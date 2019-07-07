
#' @author
#' Marta Karas <marta.karass@gmail.com>
#' 
#' @description 
#' - (1) Stride pattern segmentation from Application example in the  manuscript. 
#'   Results are stored at "data-results/application-example-segmentation.csv". 
#' - (2) Compute table of differences between ADEPT-segmented and manually 
#'   segmented strides.
#'   Results are stored at "data-results/application-example-segmentation-vs-manual.csv".


rm(list = ls())

library(adeptdata)
library(dplyr)
library(data.table)
library(adept)
library(dvmisc)
source(file.path("R", "util.R"))


## -----------------------------------------------------------------------------
## (1) 
## Stride pattern segmentation from Application example in the  manuscript. 
## Results are stored at "data-results/application-example-segmentation.csv". 

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

## Compute VM accelerometry data vector for all participants, all sensor locations
acc_walking_IU <- mutate(acc_walking_IU, vm = sqrt(x^2 + y^2 + z^2))

subj_id.unique.vec <- unique(acc_walking_IU$subj_id)
loc_id.unique.vec  <- unique(acc_walking_IU$loc_id)
res.dt             <- data.table()

## Segment walking stride pattern from VM data with ADEPT algorithm
for (subj_id.tmp in subj_id.unique.vec){
  for (loc_id.tmp in loc_id.unique.vec){
    
    message(paste0("Processing: subj_id.tmp: ", subj_id.tmp, ", loc_id.tmp: ", loc_id.tmp))

    ## Pull VM accelerometry data vector from
    vm.tmp <- 
      acc_walking_IU %>% 
      filter(subj_id == subj_id.tmp, loc_id == loc_id.tmp) %>%
      pull(vm)
    
    ## Construct list of empirical pattern templates 
    template.tmp <- list(
      stride_template[[loc_id.tmp]][[2]][1, ],
      stride_template[[loc_id.tmp]][[2]][2, ]
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
    
    res.dt <- rbindlist(list(res.dt, as.data.table(out.tmp)), use.names = TRUE)
  }
}

res.df <- as.data.frame(res.dt)
res.df <- select(res.df, -template_i)

## Save results to file
write.table(res.df,
            file.path("data-results", "application-example-segmentation.csv"),
            quote = FALSE, sep = ",", row.names = FALSE)





## -----------------------------------------------------------------------------
## (2) 
## Compute table of differences between ADEPT-segmented and manually segmented strides.
## Results are stored at "data-results/application-example-segmentation-vs-manual.csv".

## Data frame with manually segmented strides results
strides.df.path <- file.path("data", "strides-df-anonymized.csv")
strides.df <- as.data.frame(fread(strides.df.path))

## Data frame with ADEPT-segmented strides results
res.df.path <- file.path("data-results", "application-example-segmentation.csv")
res.df <- as.data.frame(fread(res.df.path))

## Data frame with raw accelerometry data used in both (manual and ADEPT-based)
## segmentations
acc.df <- acc_walking_IU


## Match `tau_i` from raw accelerometry data to manually segmented strides results
acc.df <- 
  acc.df %>% 
  group_by(subj_id, loc_id) %>%
  arrange(time_s) %>%
  mutate(tau_i = row_number(),
         vm = sqrt(x^2 + y^2 + z^2)) %>%
  group_by() %>%
  arrange(subj_id, loc_id, time_s) %>%
  as.data.frame()
## Data frame to store manually segmented strides together with matched `tau_i` 
## from raw accelerometry data
strides.df.2 <- data.frame()
## Loop to perform matching
for (subj_id.tmp in unique(strides.df$subj_id)){
  for (subj_stride_id.tmp in unique(strides.df$subj_stride_id)){
    message(paste0("subj_id: ", subj_id.tmp, ", subj_stride_id: ", subj_stride_id.tmp))
    ## Subject-specific, stride-specific subset of manually segmented strides
    strides.df.sub <- 
      strides.df %>% 
      filter(subj_id == subj_id.tmp,
             subj_stride_id == subj_stride_id.tmp)
    if (!(nrow(strides.df.sub) > 0)){
      print("!(nrow(strides.df.sub) > 0)")
      next
    }
    ## Manually segmented stride VM vector
    vm.stride.tmp <- strides.df.sub$vm_left_wrist
    ## Subject-specific, location-specific subset of raw data 
    vm.raw.df.tmp <- acc.df %>% filter(subj_id == subj_id.tmp, loc_id == "left_wrist") 
    ## Make the match between raw data and manually-segmented data based on correlation
    cor.out <- sliding_cor(short = vm.stride.tmp, long = vm.raw.df.tmp$vm)
    cor.out.idx_max <- which(cor.out == max(cor.out))
    ## Assure found max is unique
    if (length(cor.out.idx_max) > 1) stop("length(cor.out.idx_max) > 1")
    if (abs(max(cor.out) - 1) > sqrt(.Machine$double.eps)) stop("abs(max(cor.out) - 1) > .Machine$double.eps")
    ## Pull `tau_i`
    tau_i.idx <- cor.out.idx_max : (cor.out.idx_max + length(vm.stride.tmp) - 1)
    tau_i.vec <- vm.raw.df.tmp[tau_i.idx, "tau_i"]
    ## Store information about `tau_i` in data frame of manually segmented strides
    strides.df.sub$tau_i <- tau_i.vec
    strides.df.2 <- rbind(strides.df.2, strides.df.sub)
  }
}
nrow(strides.df.2) == nrow(strides.df)

## Save results to file
write.table(strides.df.2,
            file.path("data-results", "strides-df-anonymized-tau_i-matched.csv"),
            quote = FALSE, sep = ",", row.names = FALSE)

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

## Look up the results
out.df %>% 
  group_by(loc_id) %>% 
  summarize(SS_start_diff = mean(abs(SS_start_diff)),
            SS_end_diff = mean(abs(SS_end_diff)))
# loc_id      SS_start_diff SS_end_diff
#   <chr>               <dbl>       <dbl>
# 1 left_ankle           2.03        2.03
# 2 left_hip             2.59        2.54
# 3 left_wrist           8.60        9.72
# 4 right_ankle          2.30        2.40

## Save results to file
out.df.path <- file.path("data-results", "application-example-segmentation-vs-manual.csv")
write.table(out.df,
            out.df.path,
            quote = FALSE, sep = ",", row.names = FALSE)

