
#' @author
#' Marta Karas <marta.karass@gmail.com>
#' 
#' @description 
#' - (1) Run stride pattern segmentation for validation of consistency between 
#'   ADEPT-based and manual segmentation. It splits Participants' ID into 4 folds
#'   and perform segmentation for a certain fold of Participants with 
#'   pattern template derived based on manually presegmented strides 
#'   of subjects not included in that fold. 
#'    Results are stored at "data-results/validation-consistency-with-manual-segmentation.csv". 
#'    
#' - (2) Compute table of differences between ADEPT-segmented (based on procedure
#'   described above) and manually segmented strides.
#'   Results are stored at "data-results/validation-consistency-with-manual-matched.csv".


rm(list = ls())

library(dplyr)
library(data.table)
library(ggplot2)
library(TSclust)
library(adept)
library(adeptdata)
source(file.path("R", "util.R"))

loc_id.levels  <- c("left_wrist", "left_hip","left_ankle", "right_ankle")
loc_id.labels  <- c( "Left wrist","Left hip", "Left ankle", "Right ankle")


## -----------------------------------------------------------------------------
## (1) 
## Run stride pattern segmentation for validation of consistency between 
## ADEPT-based and manual segmentation. It splits Participants' ID into 4 folds
## and perform segmentation for a certain fold of Participants with 
## pattern template derived based on manually presegmented strides 
## of subjects not included in that fold. 
## Results are stored at "data-results/validation-consistency-with-manual-segmentation.csv". 

## Collection of manually segmented strides
strides.df <- as.data.frame(fread(file.path("data", "strides-df-anonymized.csv")))

## Mutate table with raw accelerometry data 
## Compute VM accelerometry data vector for all participants, all sensor locations
acc_walking_IU <- mutate(acc_walking_IU, vm = sqrt(x^2 + y^2 + z^2))
subj_id.unique.vec <- sort(unique(acc_walking_IU$subj_id))
loc_id.unique.vec  <- unique(acc_walking_IU$loc_id)

## Split subject uniqe IDs into 4 random groups
set.seed(1)
subj_id.unq.vl <- length(subj_id.unique.vec)
subj_id.folds.idx <- split(sample(subj_id.unq.vl, subj_id.unq.vl, replace = FALSE), as.factor(1:4))
subj_id.folds <- lapply(subj_id.folds.idx, function(fold.idx) subj_id.unique.vec[fold.idx])
subj_id.folds
# $`1`
# [1] "id4ea159a8" "id34e056c8" "id86237981" "id7c20ee7a" "idfc5f05e4" "idc735fc09"
# [7] "id687ab496" "idabd0c53c"
# 
# $`2`
# [1] "id650857ca" "idc91a49d0" "id079c763c" "id3e3e50c7" "id82b9735c" "id1165e00c"
# [7] "id9603e9c3" "idb221f542"
# 
# $`3`
# [1] "id8e66893c" "idf540d82b" "id1f372081" "ida61e8ddf" "idf1ce9a0f" "id37a54bbf"
# [7] "id00b70b13" "idf5e3678b"
# 
# $`4`
# [1] "idecc9265e" "id8af5374b" "id1c7e64ad" "idff99de96" "id5993bf4a" "idbae5a811"
# [7] "idd80ac2b4" "id5308a7d6"


## -----------------------------------------------------------------------------

## Function to transform all strides (identified based on on stride ID `stride_id`)
## within a data frame
df2intrpl.mat <- function(df, nout = 200){
  strides.approx.l <- lapply(unique(df$stride_id), function(stride_id.i){
    df.sub <- df %>% filter(stride_id == stride_id.i)
    intrpl.scale.vec(df.sub$V, nout)
  })
  matrix(unlist(strides.approx.l), ncol = nout, byrow = TRUE)
}


## Function to estimate stride tempate based on k-clusters correlation clustering
estimate.stride.tmpl.1 <- function(x.mat, D.mat, strides.obs.n.df, cluster.k = 1:5){   
  ## Get cluster medoids
  medoids.idx0 <- (seq(1,cluster.k) / cluster.k) * nrow(strides.obs.n.df)
  medoids.idx0 <- round(medoids.idx0 - (min(medoids.idx0) / 2))
  medoids.idx  <- (strides.obs.n.df %>% filter(row_num %in% medoids.idx0))$stride_idx
  # Run clustering
  pam.out <- cluster::pam(D.mat, cluster.k, diss = TRUE, medoids = medoids.idx)
  # Estimate templates
  template.list <- list()
  for (cluster.k.i in 1:cluster.k){
    x.mat.i <- x.mat[which(pam.out$clustering == cluster.k.i), ]
    template.tmp <- apply(x.mat.i, 2, mean)
    template.tmp <- as.vector(scale(template.tmp))
    template.list[[cluster.k.i]] <- template.tmp
  }
  template.mat <- do.call("rbind", template.list)
  return(list(template.mat))
}

## ADEPT algorithm params
x.fs                      <- 100
pattern.dur.seq           <- seq(0.5, 1.75, length.out = 100)
similarity.measure        <- "cov"
similarity.measure.thresh <- -Inf
x.adept.ma.W              <- 0.15
finetune                  <- "maxima"
finetune.maxima.ma.W      <- 0.25
finetune.maxima.nbh.W     <- 0.6
x.cut                     <- FALSE
compute.template.idx      <- FALSE

## Object to store segmentation results
res.dt <- data.table()
stride_templates <- list()

for (fold.idx in 1:length(subj_id.folds)){
  
  subj_id.fold.i <- subj_id.folds[[fold.idx]]
  message(paste0("---> subj_id.fold.i: ", fold.idx))
  message(paste0("---> 1st fold ID: ", subj_id.fold.i[1]))

  ## Empirical pattern (stride template) estimation based on subset of data
  ## that *does not* contain subjects of the current subjects ID fold
  
  ## Subset of manually segmentes strides 
  strides.df.sub <- strides.df %>% filter(!(subj_id %in% subj_id.fold.i))
  ## Add stride ID   
  strides.df.sub <- strides.df.sub %>% mutate(stride_id = paste0(subj_id, "_", subj_stride_id))
  ## Define location-specific data frames of strides 
  lw.strides.df.sub <- strides.df.sub %>% select(subj_id, stride_id, stride_ph, V = vm_left_wrist)
  lh.strides.df.sub <- strides.df.sub %>% select(subj_id, stride_id, stride_ph, V = vm_left_hip)
  la.strides.df.sub <- strides.df.sub %>% select(subj_id, stride_id, stride_ph, V = vm_left_ankle)
  ra.strides.df.sub <- strides.df.sub %>% select(subj_id, stride_id, stride_ph, V = vm_right_ankle)
  ## Compute location-specific matrices of interpolated and rescaled strides
  lw.strides.intrpl.mat <-  df2intrpl.mat(lw.strides.df.sub)
  lh.strides.intrpl.mat <-  df2intrpl.mat(lh.strides.df.sub)
  la.strides.intrpl.mat <-  df2intrpl.mat(la.strides.df.sub)
  ra.strides.intrpl.mat <-  df2intrpl.mat(ra.strides.df.sub)
  ## Compute location-specific distance matrices of strides (similarity measure: correlation)
  lw.strides.D.mat <- TSclust::diss(lw.strides.intrpl.mat, "COR")
  lh.strides.D.mat <- TSclust::diss(lh.strides.intrpl.mat, "COR")
  la.strides.D.mat <- TSclust::diss(la.strides.intrpl.mat, "COR")
  ra.strides.D.mat <- TSclust::diss(ra.strides.intrpl.mat, "COR")
  ## Define cluster medioids to be strides corresponding to a percentile of values of
  ## stride duration time (number of accelerometry observations used to determine 
  ## duration time) 
  strides.df.sub.tmp <- lw.strides.df.sub
  strides.obs.n.vec <- sapply(unique(strides.df.sub.tmp$stride_id), function(stride_id.tmp){
    df.sub <- strides.df.sub.tmp %>% filter(stride_id == stride_id.tmp)
    length(df.sub$V)
  })
  strides.obs.n.df <- 
    data.frame(ph_n = strides.obs.n.vec, 
               stride_idx = 1:length(strides.obs.n.vec)) %>%
    arrange(ph_n) %>%
    mutate(row_num = row_number())
  ## Estimate stride template(s) via correlation clustering 
  left_wrist.tmp  <- estimate.stride.tmpl.1(lw.strides.intrpl.mat, lw.strides.D.mat, strides.obs.n.df, c(2))
  left_hip.tmp    <- estimate.stride.tmpl.1(lh.strides.intrpl.mat, lh.strides.D.mat, strides.obs.n.df, c(2))
  left_ankle.tmp  <- estimate.stride.tmpl.1(la.strides.intrpl.mat, la.strides.D.mat, strides.obs.n.df, c(2))
  right_ankle.tmp <- estimate.stride.tmpl.1(ra.strides.intrpl.mat, ra.strides.D.mat, strides.obs.n.df, c(2))
  # Final stride template list object 
  stride_template.tmp <- list(
    left_wrist = left_wrist.tmp,
    left_hip = left_hip.tmp,
    left_ankle = left_ankle.tmp,
    right_ankle = right_ankle.tmp)
  stride_templates[[length(stride_templates) + 1]] <- stride_template.tmp
  
  ## Stride segmentation for subjects who are contained 
  ## that *do* contain subjects of the current subjects ID fold
  
  for (subj_id.tmp in subj_id.fold.i){
    for (loc_id.tmp in loc_id.unique.vec){
      message(paste0("Processing: subj_id.tmp: ", subj_id.tmp, ", loc_id.tmp: ", loc_id.tmp))
      ## Pull VM accelerometry data vector from
      vm.tmp <- 
        acc_walking_IU %>% 
        filter(subj_id == subj_id.tmp, 
               loc_id == loc_id.tmp) %>%
        pull(vm)
      ## Construct list of empirical pattern templates 
      template.tmp <- list(
        stride_template.tmp[[loc_id.tmp]][[1]][1, ],  ## note we pull [[1]] 
        stride_template.tmp[[loc_id.tmp]][[1]][2, ]   ## note we pull [[1]] 
      )
      ## Segment stride pattern with ADEPT algorithm
      t1.tmp <- Sys.time()
      out.tmp <- segmentPattern(
        x = vm.tmp,
        x.fs = x.fs,
        template = template.tmp,
        pattern.dur.seq = pattern.dur.seq,
        similarity.measure = similarity.measure,
        similarity.measure.thresh = similarity.measure.thresh,
        x.adept.ma.W = x.adept.ma.W,
        finetune = finetune,
        finetune.maxima.ma.W = finetune.maxima.ma.W,
        finetune.maxima.nbh.W = finetune.maxima.nbh.W,
        x.cut = x.cut,
        compute.template.idx = compute.template.idx
      )
      t2.tmp <- Sys.time()
      exec_t.tmp <- as.numeric(t2.tmp - t1.tmp, unit = "secs")
      print(paste0("Processing time [s]: ", round(exec_t.tmp, 2)))
      ## Store segmentation results 
      out.tmp$subj_id <- subj_id.tmp
      out.tmp$loc_id  <- loc_id.tmp
      out.tmp$exec_t  <- exec_t.tmp
      print(paste0("mean out.tmp$sim_i: ", round(mean(out.tmp$sim_i), 2)))
      res.dt <- rbindlist(list(res.dt, as.data.table(out.tmp)), use.names = TRUE)
    }
  }
}


res.df <- as.data.frame(res.dt)
res.df <- select(res.df, -template_i)

## Save results to file
write.table(res.df,
            file.path("data-results", "validation-consistency-with-manual-segmentation.csv"),
            quote = FALSE, sep = ",", row.names = FALSE)




## -----------------------------------------------------------------------------
## (2) 
## Compute table of differences between ADEPT-segmented and manually segmented strides.
## Results are stored at "data-results/validation-consistency-with-manual-matched.csv".

## Data frame with manually segmented strides results
strides.df.path <- file.path("data", "strides-df-anonymized.csv")
strides.df <- as.data.frame(fread(strides.df.path))

## Data frame with ADEPT-segmented strides results
res.df.path <- file.path("data-results", "validation-consistency-with-manual-segmentation.csv")
res.df <- as.data.frame(fread(res.df.path))

## Data frame with raw accelerometry data used in both (manual and ADEPT-based)
## segmentations
acc.df <- acc_walking_IU

## Read precomputed results 
strides.df.2.path <- file.path("data-results", "strides-df-anonymized-tau_i-matched.csv")
strides.df.2 <- as.data.frame(fread(strides.df.2.path))


## Compute table of differences between ADEPT-segmented and manually segmented strides.
out.df <- data.frame()

for (subj_id.tmp in unique(strides.df$subj_id)){
  ## ADEPT-segmented strides results for one participant
  res.df_subj <- res.df %>% filter(subj_id == subj_id.tmp)
  
  ## Iterate over manually segmented strides 
  for (subj_stride_id.tmp in 1:22){
    message(paste0("subj_id: ", subj_id.tmp, ", subj_stride_id: ", subj_stride_id.tmp))
    
    ## Manually segmented strides results for one participant, one stride
    strides.df.2_subj_stride <- 
      strides.df.2 %>% 
      filter(subj_id == subj_id.tmp, 
             subj_stride_id == subj_stride_id.tmp)
    if (nrow(strides.df.2_subj_stride) == 0) next
    
    ## Manually segmented strides: min and max tau_i
    manSS.start.TMP <- min(strides.df.2_subj_stride$tau_i)
    manSS.end.TMP   <- max(strides.df.2_subj_stride$tau_i)
    
    ## For particular subject, particular manually segmented stride, 
    ## define closest stride match from ADEPT-segmented strides for
    ## all senor locations (note manually segmented strides have same 
    ## beginning/end across all 4 locations)
    res.df.tmp <- 
      res.df_subj %>% 
      group_by(loc_id) %>%
      mutate(SS_start_diff = manSS.start.TMP - tau_i) %>%
      filter(abs(SS_start_diff) == min(abs(SS_start_diff))) %>%
      mutate(SS_end_diff = manSS.end.TMP - (tau_i + T_i - 1)) %>%
      as.data.frame()
    res.df.tmp$manSS_tau_i <- manSS.start.TMP
    res.df.tmp$manSS_T_i   <- nrow(strides.df.2_subj_stride)
    ## Append results to data frame
    out.df <- rbind(out.df, res.df.tmp)
  }
}

## Save results to file
out.df.path <- file.path("data-results", "validation-consistency-with-manual-matched.csv")
write.table(out.df,
            out.df.path,
            quote = FALSE, sep = ",", row.names = FALSE)

