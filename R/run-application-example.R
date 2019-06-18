
#' @author
#' Marta Karas <marta.karass@gmail.com>
#' 
#' @description 
#' Code to perform stride pattern segmentation from Application example in the 
#' manuscript. 
#' 
#' Segmentation results are stored at 
#' "data-results/application-example-segmentation.csv".

rm(list = ls())

library(adeptdata)
library(dplyr)
library(data.table)
library(adept)
source(file.path("R", "util.R"))


## ADEPT algorithm params
x.fs                      <- 100
pattern.dur.seq           <- seq(0.5, 1.75, length.out = 30)
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
      stride_template[[loc_id.tmp]][[2]][1, ]
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

## Execution time summary 
res.df %>% 
  select(subj_id, loc_id, exec_t) %>%
  distinct() %>%
  pull(exec_t) %>%
  summary()
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.8108  0.9757  1.0790  1.1038  1.2446  1.4679 

## Save results to file
write.table(res.df,
            file.path("data-results", "application-example-segmentation.csv"),
            quote = FALSE, sep = ",", row.names = FALSE)


## -----------------------------------------------------------------------------

devtools::session_info()

# ─ Session info ───────────────────────────────────────────────────────────────────
# setting  value                       
# version  R version 3.5.2 (2018-12-20)
# os       macOS Mojave 10.14.2        
# system   x86_64, darwin15.6.0        
# ui       RStudio                     
# language (EN)                        
# collate  en_US.UTF-8                 
# ctype    en_US.UTF-8                 
# tz       Europe/Zurich               
# date     2019-06-18                  
# 
# ─ Packages ───────────────────────────────────────────────────────────────────────
# package     * version  date       lib source        
# adept       * 1.1.2    2019-06-18 [1] CRAN (R 3.5.2)
# adeptdata   * 1.0.1    2019-03-30 [1] CRAN (R 3.5.2)
# assertthat    0.2.1    2019-03-21 [1] CRAN (R 3.5.2)
# backports     1.1.4    2019-04-10 [1] CRAN (R 3.5.2)
# callr         3.2.0    2019-03-15 [1] CRAN (R 3.5.2)
# cli           1.1.0    2019-03-19 [1] CRAN (R 3.5.2)
# codetools     0.2-16   2018-12-24 [1] CRAN (R 3.5.2)
# colorspace    1.4-1    2019-03-18 [1] CRAN (R 3.5.2)
# crayon        1.3.4    2017-09-16 [1] CRAN (R 3.5.0)
# data.table  * 1.12.2   2019-04-07 [1] CRAN (R 3.5.2)
# DBI           1.0.0    2018-05-02 [1] CRAN (R 3.5.0)
# desc          1.2.0    2018-05-01 [1] CRAN (R 3.5.0)
# devtools      2.0.2    2019-04-08 [1] CRAN (R 3.5.2)
# digest        0.6.19   2019-05-20 [1] CRAN (R 3.5.2)
# dplyr       * 0.8.1    2019-05-14 [1] CRAN (R 3.5.2)
# dvmisc        1.1.3    2019-03-05 [1] CRAN (R 3.5.2)
# fs            1.3.1    2019-05-06 [1] CRAN (R 3.5.2)
# future        1.13.0   2019-05-08 [1] CRAN (R 3.5.2)
# ggplot2       3.2.0    2019-06-16 [1] CRAN (R 3.5.2)
# globals       0.12.4   2018-10-11 [1] CRAN (R 3.5.0)
# glue          1.3.1    2019-03-12 [1] CRAN (R 3.5.2)
# gtable        0.3.0    2019-03-25 [1] CRAN (R 3.5.2)
# lattice       0.20-38  2018-11-04 [1] CRAN (R 3.5.2)
# lazyeval      0.2.2    2019-03-15 [1] CRAN (R 3.5.2)
# listenv       0.7.0    2018-01-21 [1] CRAN (R 3.5.0)
# magrittr      1.5      2014-11-22 [1] CRAN (R 3.5.0)
# MASS          7.3-51.4 2019-03-31 [1] CRAN (R 3.5.2)
# Matrix        1.2-17   2019-03-22 [1] CRAN (R 3.5.2)
# memoise       1.1.0    2017-04-21 [1] CRAN (R 3.5.0)
# mitools       2.4      2019-04-26 [1] CRAN (R 3.5.2)
# munsell       0.5.0    2018-06-12 [1] CRAN (R 3.5.0)
# mvtnorm       1.0-10   2019-03-05 [1] CRAN (R 3.5.2)
# pillar        1.4.1    2019-05-28 [1] CRAN (R 3.5.2)
# pkgbuild      1.0.3    2019-03-20 [1] CRAN (R 3.5.2)
# pkgconfig     2.0.2    2018-08-16 [1] CRAN (R 3.5.0)
# pkgload       1.0.2    2018-10-29 [1] CRAN (R 3.5.0)
# prettyunits   1.0.2    2015-07-13 [1] CRAN (R 3.5.0)
# processx      3.3.1    2019-05-08 [1] CRAN (R 3.5.2)
# ps            1.3.0    2018-12-21 [1] CRAN (R 3.5.0)
# purrr         0.3.2    2019-03-15 [1] CRAN (R 3.5.2)
# R6            2.4.0    2019-02-14 [1] CRAN (R 3.5.2)
# rbenchmark    1.0.0    2012-08-30 [1] CRAN (R 3.5.0)
# Rcpp          1.0.1    2019-03-17 [1] CRAN (R 3.5.2)
# remotes       2.0.4    2019-04-10 [1] CRAN (R 3.5.2)
# rlang         0.3.4    2019-04-07 [1] CRAN (R 3.5.2)
# rprojroot     1.3-2    2018-01-03 [1] CRAN (R 3.5.0)
# rstudioapi    0.10     2019-03-19 [1] CRAN (R 3.5.2)
# scales        1.0.0    2018-08-09 [1] CRAN (R 3.5.0)
# sessioninfo   1.1.1    2018-11-05 [1] CRAN (R 3.5.0)
# survey        3.36     2019-04-27 [1] CRAN (R 3.5.2)
# survival      2.44-1.1 2019-04-01 [1] CRAN (R 3.5.2)
# testthat      2.1.1    2019-04-23 [1] CRAN (R 3.5.2)
# tibble        2.1.3    2019-06-06 [1] CRAN (R 3.5.2)
# tidyselect    0.2.5    2018-10-11 [1] CRAN (R 3.5.0)
# usethis       1.5.0    2019-04-07 [1] CRAN (R 3.5.2)
# withr         2.1.2    2018-03-15 [1] CRAN (R 3.5.0)
# yaml          2.2.0    2018-07-25 [1] CRAN (R 3.5.0)
# 
# [1] /Library/Frameworks/R.framework/Versions/3.5/Resources/library





