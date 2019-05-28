
#' @author
#' Marta Karas <marta.karass@gmail.com>
#'
#' @description
#' Created: 2019-03-18
#' Note: It is assumed that the working directory is the current R script directory
#' 
#' This script contains the code to estimate stride pattern templates 
#' based on a data set of of manually pre-segmented strides. 
#' 
#' Specifically, this script was used to generate data `stride_template`
#' object attached to the `adeptdata` package. The result of running this script is 
#' tested to be equal to the templates from `adeptdata` package (see code
#' at the end of the script file).  

rm(list = ls())

library(data.table)
library(dplyr)
library(TSclust)
library(testthat)

## Read data 
strides.df.path <- file.path("..", "data", "strides-df-anonymized.csv")
strides.df <- as.data.frame(fread(strides.df.path))

## Add stride ID   
strides.df <- strides.df %>% mutate(stride_id = paste0(subj_id, "_", subj_stride_id))

## Function to interpolate vector to a fixed vector length and scale it to
## have mean 0 and variance 1
intrpl.scale.vec <- function(vec, nout = 200){
  vec.out <- approx(
    x = seq(0, 1, length.out = length(vec)),  
    y = vec,
    xout = seq(0, 1, length.out = nout))$y
  return(as.vector(scale(vec.out)))
}

# Function to transform all strides (identified based on on stride ID `stride_id`)
# within a data frame
df2intrpl.mat <- function(df, nout = 200){
  strides.approx.l <- lapply(unique(df$stride_id), function(stride_id.i){
    df.sub <- df %>% filter(stride_id == stride_id.i)
    intrpl.scale.vec(df.sub$V, nout)
  })
  matrix(unlist(strides.approx.l), ncol = nout, byrow = TRUE)
}

## Define location-specific data frames of strides 
lw.strides.df <- strides.df %>% select(subj_id, stride_id, stride_ph, V = vm_left_wrist)
lh.strides.df <- strides.df %>% select(subj_id, stride_id, stride_ph, V = vm_left_hip)
la.strides.df <- strides.df %>% select(subj_id, stride_id, stride_ph, V = vm_left_ankle)
ra.strides.df <- strides.df %>% select(subj_id, stride_id, stride_ph, V = vm_right_ankle)

## Compute location-specific matrices of interpolated and rescaled strides
lw.strides.intrpl.mat <-  df2intrpl.mat(lw.strides.df)
lh.strides.intrpl.mat <-  df2intrpl.mat(lh.strides.df)
la.strides.intrpl.mat <-  df2intrpl.mat(la.strides.df)
ra.strides.intrpl.mat <-  df2intrpl.mat(ra.strides.df)

## Compute location-specific distance matrices of strides (similarity measure: correlation)
lw.strides.D.mat <- TSclust::diss(lw.strides.intrpl.mat, "COR")
lh.strides.D.mat <- TSclust::diss(lh.strides.intrpl.mat, "COR")
la.strides.D.mat <- TSclust::diss(la.strides.intrpl.mat, "COR")
ra.strides.D.mat <- TSclust::diss(ra.strides.intrpl.mat, "COR")

## Define cluster medioids to be strides corresponding to a percentile of values of
## stride duration time (number of accelerometry observations used to determine 
## duration time) 
strides.df.tmp <- lw.strides.df
strides.obs.n.vec <- sapply(unique(strides.df.tmp$stride_id), function(stride_id.tmp){
  df.sub <- strides.df.tmp %>% filter(stride_id == stride_id.tmp)
  length(df.sub$V)
})
strides.obs.n.df <- 
  data.frame(ph_n = strides.obs.n.vec, 
             stride_idx = 1:length(strides.obs.n.vec)) %>%
  arrange(ph_n) %>%
  mutate(row_num = row_number())

## Function to perform location-specific clustering 
## Iterates over grid of k=1,...,5 subpopulation-specific clusters to be computed
## Returns list of five matrices, where each matrix contains 1,...,5 
## subpopulation-specific stride templates, respectively
estimate.stride.template <- function(x.mat, D.mat){   
  out.all_k <- list()
  for (cluster.k in 1:5){  
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
    ## Store estimated template(s) in a form of a martrix
    out.all_k[[cluster.k]] <- template.mat
  }
  return(out.all_k)
}

## Estimate stride template(s) via correlation clustering 
left_wrist  <- estimate.stride.template(lw.strides.intrpl.mat, lw.strides.D.mat)
left_hip    <- estimate.stride.template(lh.strides.intrpl.mat, lh.strides.D.mat)
left_ankle  <- estimate.stride.template(la.strides.intrpl.mat, la.strides.D.mat)
right_ankle <- estimate.stride.template(ra.strides.intrpl.mat, ra.strides.D.mat)

# ## Final object 
# stride_template <- list(
#   left_wrist = left_wrist,
#   left_hip = left_hip,
#   left_ankle = left_ankle,
#   right_ankle = right_ankle)



## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
## TEST RESULTS 

## Test actual versus expected values: interpolated and rescaled strides
res.actual <- c(
  mean(lw.strides.intrpl.mat), mean(lh.strides.intrpl.mat),
  mean(la.strides.intrpl.mat), mean(ra.strides.intrpl.mat))
res.exp <- c(
  -3.48530416868101e-17, -2.20918385696391e-17, 
  -4.94017429184161e-17, -4.76685765053724e-17)
tryCatch(
  expect_equivalent(res.actual, res.exp),
  error = function(e) {
    message("Disagreement in actual versus expected values: interpolated and rescaled strides")
    message(e)
  }
)

## Test actual versus expected values: stride distance matrices
res.actual <- c(
  mean(lw.strides.D.mat), mean(lh.strides.D.mat),
  mean(la.strides.D.mat), mean(ra.strides.D.mat))
res.exp <- c(
  0.981136337581524, 0.81634717819907, 
  0.585896701821308, 0.638763189391876
)
tryCatch(
  expect_equivalent(res.actual, res.exp),
  error = function(e) {
    message("Disagreement in actual versus expected values: stride distance matrices")
    message(e)
  }
)

## Test actual versus expected values: templates obtained via running this script
## versus templates from `adeptdata` package 
templates.diff <- function(res.actual, res.exp){
  max.abs.diff.vec <- numeric()
  for (i in 1:length(res.actual)){
    for (j in 1:nrow(res.actual[[i]])){
      max.abs.diff.val <- max(abs(res.actual[[i]][j, ] - res.exp[[i]][j, ]))
      max.abs.diff.vec <- c(max.abs.diff.vec, max.abs.diff.val)
    }
  }
  return(max.abs.diff.vec)
}

left_wrist.templates.diff  <- templates.diff(left_wrist,   adeptdata::stride_template$left_wrist)
left_hip.templates.diff    <- templates.diff(left_hip,     adeptdata::stride_template$left_hip)
left_ankle.templates.diff  <- templates.diff(left_ankle,   adeptdata::stride_template$left_ankle)
right_ankle.templates.diff <- templates.diff(right_ankle,  adeptdata::stride_template$right_ankle)

tryCatch(
  expect_equal(max(left_wrist.templates.diff), 0),
  error = function(e) {
    message("Disagreement in actual versus expected values: stride template for left wrist")
    message(e)
  }
)
tryCatch(
  expect_equal(max(left_hip.templates.diff), 0),
  error = function(e) {
    message("Disagreement in actual versus expected values: stride template for left hip")
    message(e)
  }
)
tryCatch(
  expect_equal(max(left_ankle.templates.diff), 0),
  error = function(e) {
    message("Disagreement in actual versus expected values: stride template for left ankle")
    message(e)
  }
)
tryCatch(
  expect_equal(max(right_ankle.templates.diff), 0),
  error = function(e) {
    message("Disagreement in actual versus expected values: stride template for right ankle")
    message(e)
  }
)


