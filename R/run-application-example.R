
#' @author
#' Marta Karas <marta.karass@gmail.com>
#' 
#' @description 
#' - (1) Stride pattern segmentation from Application example in the  manuscript. 
#'   Results are stored at "data-results/application-example-segmentation.csv". 
#' - (2) Compute table of differences between ADEPT-segmented and manually 
#'   segmented strides.
#'   Results are stored at "data-results/application-example-segmentation-vs-manual.csv".
#'    
#' - Session info output is included at the end of the script. 

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
## (2) 
## Compute table of differences between ADEPT-segmented and manually segmented strides.
## Results are stored at "data-results/application-example-segmentation-vs-manual.csv".

## Data frame with manually segmented strides results
strides.df.path <- file.path("data", "strides-df-anonymized.csv")
strides.df <- as.data.frame(fread(strides.df.path))
str(strides.df)
# 'data.frame':	65458 obs. of  7 variables:
# $ subj_id       : chr  "idabd0c53c" "idabd0c53c" "idabd0c53c" "idabd0c53c" ...
# $ subj_stride_id: int  1 1 1 1 1 1 1 1 1 1 ...
# $ stride_ph     : num  0 0.00893 0.01786 0.02679 0.03571 ...
# $ vm_left_wrist : num  1.73 1.65 1.56 1.49 1.46 ...
# $ vm_left_hip   : num  1.603 1.022 0.631 0.459 0.655 ...
# $ vm_left_ankle : num  3.64 3.25 2.91 2.81 2.68 ...
# $ vm_right_ankle: num  2.91 3.13 3.01 2.61 2.39 ...

## Data frame with ADEPT-segmented strides results
res.df.path <- file.path("data-results", "application-example-segmentation.csv")
res.df <- as.data.frame(fread(res.df.path))
str(res.df)
# 'data.frame':	25740 obs. of  6 variables:
# $ tau_i  : int  38 146 255 363 468 572 677 780 887 993 ...
# $ T_i    : int  109 110 109 106 105 106 104 108 107 106 ...
# $ sim_i  : num  0.399 0.341 0.278 0.312 0.24 ...
# $ subj_id: chr  "id86237981" "id86237981" "id86237981" "id86237981" ...
# $ loc_id : chr  "left_wrist" "left_wrist" "left_wrist" "left_wrist" ...
# $ exec_t : num  1.16 1.16 1.16 1.16 1.16 ...

## Data frame with raw accelerometry data used in both (manual and ADEPT-based)
## segmentations
acc.df <- acc_walking_IU
str(acc.df)
# 'data.frame':	2590448 obs. of  6 variables:
# $ subj_id: chr  "id86237981" "id86237981" "id86237981" "id86237981" ...
# $ loc_id : chr  "left_wrist" "left_wrist" "left_wrist" "left_wrist" ...
# $ time_s : num  0 0.01 0.02 0.03 0.04 ...
# $ x      : num  -0.27 -0.266 -0.277 -0.277 -0.289 -0.316 -0.344 -0.375 -0.406 -0.441 ...
# $ y      : num  -0.773 -0.738 -0.707 -0.68 -0.656 -0.641 -0.625 -0.617 -0.602 -0.594 ...
# $ z      : num  -0.16 -0.172 -0.168 -0.16 -0.16 -0.164 -0.172 -0.176 -0.191 -0.211 ...


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
# 'data.frame':	65458 obs. of  8 variables:
# $ subj_id       : chr  "idabd0c53c" "idabd0c53c" "idabd0c53c" "idabd0c53c" ...
# $ subj_stride_id: int  1 1 1 1 1 1 1 1 1 1 ...
# $ stride_ph     : num  0 0.00893 0.01786 0.02679 0.03571 ...
# $ vm_left_wrist : num  1.73 1.65 1.56 1.49 1.46 ...
# $ vm_left_hip   : num  1.603 1.022 0.631 0.459 0.655 ...
# $ vm_left_ankle : num  3.64 3.25 2.91 2.81 2.68 ...
# $ vm_right_ankle: num  2.91 3.13 3.01 2.61 2.39 ...
# $ tau_i         : int  5112 5113 5114 5115 5116 5117 5118 5119 5120 5121 ...


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
#   loc_id      SS_start_diff SS_end_diff
#   <chr>               <dbl>       <dbl>
# 1 left_ankle           2.03        2.03
# 2 left_hip             2.59        2.54
# 3 left_wrist           7.36        7.68
# 4 right_ankle          2.30        2.40

## Save results to file
out.df.path <- file.path("data-results", "application-example-segmentation-vs-manual.csv")
write.table(out.df,
            out.df.path,
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
# package      * version  date       lib source        
# adept        * 1.1.2    2019-06-18 [1] CRAN (R 3.5.2)
# adeptdata    * 1.0.1    2019-03-30 [1] CRAN (R 3.5.2)
# assertthat     0.2.1    2019-03-21 [1] CRAN (R 3.5.2)
# backports      1.1.4    2019-04-10 [1] CRAN (R 3.5.2)
# callr          3.2.0    2019-03-15 [1] CRAN (R 3.5.2)
# cli            1.1.0    2019-03-19 [1] CRAN (R 3.5.2)
# cobs         * 1.3-3    2017-03-31 [1] CRAN (R 3.5.0)
# codetools      0.2-16   2018-12-24 [1] CRAN (R 3.5.2)
# colorspace     1.4-1    2019-03-18 [1] CRAN (R 3.5.2)
# crayon         1.3.4    2017-09-16 [1] CRAN (R 3.5.0)
# data.table   * 1.12.2   2019-04-07 [1] CRAN (R 3.5.2)
# DBI            1.0.0    2018-05-02 [1] CRAN (R 3.5.0)
# desc           1.2.0    2018-05-01 [1] CRAN (R 3.5.0)
# devtools       2.0.2    2019-04-08 [1] CRAN (R 3.5.2)
# digest         0.6.19   2019-05-20 [1] CRAN (R 3.5.2)
# dplyr        * 0.8.1    2019-05-14 [1] CRAN (R 3.5.2)
# dvmisc       * 1.1.3    2019-03-05 [1] CRAN (R 3.5.2)
# evaluate       0.14     2019-05-28 [1] CRAN (R 3.5.2)
# fansi          0.4.0    2018-10-05 [1] CRAN (R 3.5.0)
# fs             1.3.1    2019-05-06 [1] CRAN (R 3.5.2)
# future         1.13.0   2019-05-08 [1] CRAN (R 3.5.2)
# ggplot2      * 3.2.0    2019-06-16 [1] CRAN (R 3.5.2)
# globals        0.12.4   2018-10-11 [1] CRAN (R 3.5.0)
# glue           1.3.1    2019-03-12 [1] CRAN (R 3.5.2)
# gtable         0.3.0    2019-03-25 [1] CRAN (R 3.5.2)
# htmltools      0.3.6    2017-04-28 [1] CRAN (R 3.5.0)
# knitr          1.23     2019-05-18 [1] CRAN (R 3.5.2)
# labeling       0.3      2014-08-23 [1] CRAN (R 3.5.0)
# lattice        0.20-38  2018-11-04 [1] CRAN (R 3.5.2)
# lazyeval       0.2.2    2019-03-15 [1] CRAN (R 3.5.2)
# listenv        0.7.0    2018-01-21 [1] CRAN (R 3.5.0)
# magrittr       1.5      2014-11-22 [1] CRAN (R 3.5.0)
# MASS           7.3-51.4 2019-03-31 [1] CRAN (R 3.5.2)
# Matrix         1.2-17   2019-03-22 [1] CRAN (R 3.5.2)
# MatrixModels   0.4-1    2015-08-22 [1] CRAN (R 3.5.0)
# memoise        1.1.0    2017-04-21 [1] CRAN (R 3.5.0)
# mitools        2.4      2019-04-26 [1] CRAN (R 3.5.2)
# munsell        0.5.0    2018-06-12 [1] CRAN (R 3.5.0)
# mvtnorm        1.0-10   2019-03-05 [1] CRAN (R 3.5.2)
# pillar         1.4.1    2019-05-28 [1] CRAN (R 3.5.2)
# pkgbuild       1.0.3    2019-03-20 [1] CRAN (R 3.5.2)
# pkgconfig      2.0.2    2018-08-16 [1] CRAN (R 3.5.0)
# pkgload        1.0.2    2018-10-29 [1] CRAN (R 3.5.0)
# plyr           1.8.4    2016-06-08 [1] CRAN (R 3.5.0)
# prettyunits    1.0.2    2015-07-13 [1] CRAN (R 3.5.0)
# processx       3.3.1    2019-05-08 [1] CRAN (R 3.5.2)
# ps             1.3.0    2018-12-21 [1] CRAN (R 3.5.0)
# purrr          0.3.2    2019-03-15 [1] CRAN (R 3.5.2)
# quantreg       5.40     2019-06-03 [1] CRAN (R 3.5.2)
# R6             2.4.0    2019-02-14 [1] CRAN (R 3.5.2)
# rbenchmark   * 1.0.0    2012-08-30 [1] CRAN (R 3.5.0)
# RColorBrewer   1.1-2    2014-12-07 [1] CRAN (R 3.5.0)
# Rcpp           1.0.1    2019-03-17 [1] CRAN (R 3.5.2)
# remotes        2.0.4    2019-04-10 [1] CRAN (R 3.5.2)
# reshape2       1.4.3    2017-12-11 [1] CRAN (R 3.5.0)
# rlang          0.3.4    2019-04-07 [1] CRAN (R 3.5.2)
# rmarkdown      1.13     2019-05-22 [1] CRAN (R 3.5.2)
# rprojroot      1.3-2    2018-01-03 [1] CRAN (R 3.5.0)
# rsconnect      0.8.13   2019-01-10 [1] CRAN (R 3.5.2)
# rstudioapi     0.10     2019-03-19 [1] CRAN (R 3.5.2)
# scales         1.0.0    2018-08-09 [1] CRAN (R 3.5.0)
# sessioninfo    1.1.1    2018-11-05 [1] CRAN (R 3.5.0)
# SparseM        1.77     2017-04-23 [1] CRAN (R 3.5.0)
# stringi        1.4.3    2019-03-12 [1] CRAN (R 3.5.2)
# stringr        1.4.0    2019-02-10 [1] CRAN (R 3.5.2)
# survey         3.36     2019-04-27 [1] CRAN (R 3.5.2)
# survival       2.44-1.1 2019-04-01 [1] CRAN (R 3.5.2)
# testthat       2.1.1    2019-04-23 [1] CRAN (R 3.5.2)
# tibble         2.1.3    2019-06-06 [1] CRAN (R 3.5.2)
# tidyselect     0.2.5    2018-10-11 [1] CRAN (R 3.5.0)
# usethis        1.5.0    2019-04-07 [1] CRAN (R 3.5.2)
# utf8           1.1.4    2018-05-24 [1] CRAN (R 3.5.0)
# vctrs          0.1.0    2018-11-29 [1] CRAN (R 3.5.0)
# withr          2.1.2    2018-03-15 [1] CRAN (R 3.5.0)
# xfun           0.7      2019-05-14 [1] CRAN (R 3.5.2)
# yaml           2.2.0    2018-07-25 [1] CRAN (R 3.5.0)
# zeallot        0.1.0    2018-01-28 [1] CRAN (R 3.5.0)
# 
# [1] /Library/Frameworks/R.framework/Versions/3.5/Resources/library