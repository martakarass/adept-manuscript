
#' @author
#' Marta Karas <marta.karass@gmail.com>
#' 
#' @description 
#' Collection of utility functions. 


#' Generate `ggplot2` theme used for generating figures in the manuscript
#' 
#' @details 
#' Generates `ggplot2` theme used for generating figures in the manuscript. 
#' Utilizes HanjoStudy's code available on GitHub: 
#' https://github.com/HanjoStudy/quotidieR/blob/master/R/theme_publication.R
#' (commit 1f9c329 on Jun 25, 2018; accessed on May 30, 2019) 
#' with very minor modifications to the original version. 
#' 
#' Permission is hereby granted, free of charge, to any person obtaining a copy
#' of this software and associated documentation files (the "Software"), to deal
#' in the Software without restriction, including without limitation the rights
#' to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
#' copies of the Software, and to permit persons to whom the Software is
#' furnished to do so, subject to the following conditions:
#' 
#' The above copyright notice and this permission notice shall be included in all
#' copies or substantial portions of the Software.
#' 
#' THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#' IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#' FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#' AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#' LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#' OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
#' SOFTWARE.
#' 
#' @param base_size - An integer scalar. Prameter for base size of a plot font. 
#' @param base_family - A character scalar. Prameter for base family of a plot font. 
#' 
#' @return `ggplot2` theme object.
#' 
theme_Publication <- function(base_size = 13, base_family = "Helvetica") {
  theme_bw(
    base_size = base_size,
    base_family = base_family
  ) + 
    theme(
      panel.spacing.x = unit(0.5, "cm"),
      panel.background = element_rect(colour = NA),
      plot.background = element_rect(colour = NA),
      panel.border = element_rect(colour = NA),
      axis.title = element_text(face = "bold",size = rel(1)),
      axis.title.y = element_text(angle = 90, vjust = 2),
      axis.title.x = element_text(vjust = -0.2),
      axis.text = element_text(),
      axis.line = element_line(colour = "black"),
      axis.ticks = element_line(),
      panel.grid.major = element_line(colour = "#f0f0f0"),
      panel.grid.minor = element_blank(),
      legend.key = element_rect(colour = NA),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.key.size= unit(0.2, "cm"),
      legend.spacing = unit(0, "cm"),
      legend.title = element_text(face = "italic"),
      plot.margin = unit(c(10, 5, 5, 5), "mm"),
      strip.background = element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
      strip.text = element_text(face = "bold")
    )
}



#' Intepolate and standardize numeric vector
#' 
#' Interpolates linearly a numeric vector so as it has a fixed vector length and 
#' rescale it to have mean 0 and variance 1.
#' 
#' @param vec - A numeric vector.
#' @param nout - An integer scalar. A length of the vector after interpolation. 
#' @param center - A logical scalar. Whether or not to mean-center the vector. 
#' Default is TRUE.
#' @param center - A logical scalar. Whether or not to rescale the vector to 
#' have variance equal 1. Default is TRUE.
#' 
#' @return A numeric vector. 
#' 
intrpl.scale.vec <- function(vec, nout = 200, center = TRUE, scale = TRUE){
  vec.out <- approx(
    x = seq(0, 1, length.out = length(vec)),  
    y = vec,
    xout = seq(0, 1, length.out = nout))$y
  vec.out <- as.vector(scale(vec.out, center = center, scale = scale))
  return(vec.out)
}


#' FFT-based band-pass filter
#' 
#' @author
#' Jacek K. Urbanek <jurbane2@jhu.edu>
#' 
#' @param x A numeric vector.
#' @param fs A numeric scalar. Frequency of `x`` data collection. 
#' @param LD A numeric scalar. lower critical frequencies of the filter.
#' @param LU A numeric scalar. lower critical frequencies of the filter. 
#' 
#' @return A numeric vector.
#' 
dig_filter = function(x, fs, LD = 0, LU = fs/2){
  
  x[is.na(x)] = 0
  N0 = length(x)
  Nz = 2^ceiling(log2(N0))
  x = c(x,rep(0, Nz-N0)) # zero padding   df = fs/N
  
  df = fs/Nz
  
  fft_x = fft(x)
  fft_mask = rep(0,Nz)
  fft_mask[ceiling(LD/df):ceiling(LU/df)] = 2
  fft_mask[1] = 1;
  
  xf = Re(fft(fft_x*fft_mask, inverse = T))/Nz
  xf = xf[1:N0]
}



#' Estimate stride template.
#' 
#' Estimate strtide templates based on a collection of pre-segmented 
#' walking strides. 
#' 
#' @param val.vec A numeric vector. Values of vector magnitude from 
#' pre-segmented walking strides. 
#' @param id.vec A numeric/character vector. Contains value of unique ID
#' for pre-segmented walking strides whose corresponding vector magnitude
#' values are in \code{val.vec}. 
#' @param time.vec A numeric vector. Determines the order of values within each 
#' unique pre-segmented walking stride. Used to sort data (does not have to 
#' correspond to any true time of data collection).
#' @param k An integer scalar. Number of clusters we cluster the walking strides
#' into. 
#' @param nout A numeric scalar. Determines vector length to which we 
#' interpolate each walking stride before the clustering. Default is 200. 
#' 
#' @return A list with the following objects: 
#' \itemize{
#'   \item \code{x_mat} - A numeric matrix. Each row corresponds to one 
#'   stride vector, interpolated and rescaled. 
#'   \item \code{D_mat} - A numeric matrix. Strides dissimilarity matrix. 
#'   \item \code{cluster_idx} - An integer vector. Stride cluster assignment. 
#' }
#' 
estimate.stride.tmpl <- function(val.vec, id.vec, time.vec, k, nout = 200){
  require(TSclust)
  ## Strides data frame
  df <- data.frame(val = val.vec, id = id.vec, val_time = time.vec, stringsAsFactors = FALSE) %>%
    arrange(id, val_time)
  id.unique.vec <- sort(unique(df$id))
  ## Interpolated and rescaled strides matrix
  vec.intrpl.l <- lapply(id.unique.vec, function(id.tmp){
    intrpl.scale.vec(df[df$id == id.tmp, "val"], nout)
  })
  x.mat <- matrix(unlist(vec.intrpl.l), ncol = nout, byrow = TRUE)
  ## Strides distance matrix
  D.mat <- TSclust::diss(x.mat, "COR")
  ## Medoids stride index 
  medoids.nobs.idx0 <- (1:k / k) * length(id.unique.vec)
  medoids.nobs.idx <- round(medoids.nobs.idx0 - (min(medoids.nobs.idx0) / 2))
  medoids.id <- df %>%
    group_by(id) %>%
    summarize(n_obs = n()) %>%
    arrange(n_obs) %>% 
    filter(row_number() %in% medoids.nobs.idx) %>% 
    pull(id)
  medoids.idx <- which(id.unique.vec %in% medoids.id)
  ## Run PAM clustering
  pam.out <- cluster::pam(D.mat, k, diss = TRUE, medoids = medoids.idx)
  out.l <- list(x_mat = x.mat, 
                D_mat = D.mat,
                cluster_idx = pam.out$clustering)
  return(out.l)
}

