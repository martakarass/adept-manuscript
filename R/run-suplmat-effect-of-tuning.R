
#' @author
#' Marta Karas <marta.karass@gmail.com>
#' 
#' @description 
#' Compare effect of tuning.


rm(list = ls())

library(adeptdata)
library(dplyr)
library(data.table)
library(adept)
library(dvmisc)
source(file.path("R", "util.R"))


## ADEPT algorithm params
x.fs                      <- 100
pattern.dur.seq           <- seq(0.5, 1.75, length.out = 100)
similarity.measure        <- "cov"
similarity.measure.thresh <- -Inf
x.adept.ma.W              <- 0.15
# finetune                  <- "maxima"
finetune.grid             <- c("maxima", "none")
finetune.maxima.ma.W      <- 0.25
finetune.maxima.nbh.W     <- 0.6
x.cut                     <- FALSE
compute.template.idx      <- FALSE

## Compute VM accelerometry data vector for all participants, all sensor locations
acc_walking_IU <- mutate(acc_walking_IU, vm = sqrt(x^2 + y^2 + z^2))

subj_id.unique.vec <- unique(acc_walking_IU$subj_id)
loc_id.levels  <- c("left_wrist", "left_hip","left_ankle", "right_ankle")

res.dt  <- data.table()

## Segment walking stride pattern from VM data with ADEPT algorithm
for (subj_id.tmp in subj_id.unique.vec){
  for (loc_id.tmp in loc_id.levels){
    
    ## Pull VM accelerometry data vector from
    message(paste0("Processing: subj_id.tmp: ", which(subj_id.unique.vec == subj_id.tmp), ", loc_id.tmp: ", loc_id.tmp))
    vm.tmp <- 
      acc_walking_IU %>% 
      filter(subj_id == subj_id.tmp, loc_id == loc_id.tmp) %>%
      pull(vm)
    
    ## Construct list of empirical pattern templates 
    template.tmp.x <- t(stride_template[[loc_id.tmp]][[2]])
    template.tmp <- split(template.tmp.x, rep(1:ncol(template.tmp.x), each = nrow(template.tmp.x)))
    
    for (finetune in finetune.grid){
      
      if (finetune == "none"){
        finetune.TMP <- NULL
      } else {
        finetune.TMP <- finetune
      }

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
        finetune = finetune.TMP,
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
      out.tmp$finetune <- finetune

      res.dt <- rbindlist(list(res.dt, as.data.table(out.tmp)), use.names = TRUE)
    }
  }
}

res.df <- as.data.frame(res.dt)
res.df <- select(res.df, -template_i)

## Save results to file
write.table(res.df,
            file.path("data-results", "suplmat-effect-of-tuning.csv"),
            quote = FALSE, sep = ",", row.names = FALSE)

