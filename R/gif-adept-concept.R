
#' @author
#' Marta Karas <marta.karass@gmail.com>
#' 
#' @description 
#' - Code to generate GIF with visualization of ADEPT method concept. 

rm(list = ls())

library(adeptdata)
library(dplyr)
library(adept)
library(ggplot2)
library(gifski)


## -----------------------------------------------------------------------------
## Stride pattern segmentation from Application example in the  manuscript. 

## ADEPT algorithm params
x.fs                      <- 100
pattern.dur.seq           <- seq(0.5, 1.75, length.out = 30)
similarity.measure        <- "cor"
similarity.measure.thresh <- -Inf
x.adept.ma.W              <- 0.15
finetune                  <- "maxima"
finetune.maxima.ma.W      <- NULL
finetune.maxima.nbh.W     <- 0.6
x.cut                     <- FALSE
compute.template.idx      <- TRUE

## Example subset 
subj_id.tmp <- "idabd0c53c"
loc_id.tmp  <- "left_hip"
acc.sub <- 
  acc_walking_IU %>%
  filter(subj_id == subj_id.tmp,
         loc_id == loc_id.tmp) %>%
  mutate(vm = sqrt(x^2 + y^2 + z^2)) %>%
  arrange(time_s)
acc.sub$vm_sm <- windowSmooth(acc.sub$vm, W = 0.15, x.fs = 100)

## Construct list of empirical pattern templates 
templ1   <- stride_template[[loc_id.tmp]][[2]][1, ]
templ2_0 <- stride_template[[loc_id.tmp]][[2]][2, ]
templ2_1 <- (approx(x = seq(0, 1, length.out = length(12:195)),
                    y = templ2_0[12:195],
                    xout = seq(0, 1, length.out = 200)))$y
templ2 <- as.vector(scale(templ2_1))
template.tmp <- list(templ1, templ2)

## Run ADEPT 
out <- segmentPattern(
  x = acc.sub$vm,
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

## Number of strides we aim to present in the pic
il <- 3

## Subset of segmentation results we use for GIF construction
out1 <- numeric()
for (i in 1:(nrow(out)-il-1)){
  idx <- i:(i+il-1)
  if (length(unique(out$template_i[idx])) == 1){
    out1 <- c(out1, NA)
  } else if (!(which.max(out$sim_i[idx]) == 2)){
    out1 <- c(out1, NA)
  } else {
    out1 <- c(out1, var(out$T_i[idx])) 
  }
}
summary(out1)
out.row.idx0 <- which((out1 > 18) & (out1 < 25))[1]
out.row.idx <- out.row.idx0:(out.row.idx0+il-1)
out.sub <- out[out.row.idx, ]

## Subset of raw data results we use for GIF construction
tau_i.min0 <- out.sub[1, "tau_i"]
tau_i.max0 <- out.sub[il, "tau_i"] + out.sub[il, "T_i"]
tau_i.min <- tau_i.min0 - 50
tau_i.max <- tau_i.max0 + 50
acc.sub2 <- 
  acc.sub %>% 
  mutate(rn = row_number()) %>%
  filter(rn > tau_i.min,
         rn < tau_i.max) %>%
  mutate(time_s = time_s - min(time_s),
         vm = vm - mean(vm)) 



## -----------------------------------------------------------------------------
## Generate plots

theme_gif <- function(base_size = 10, base_family = "Helvetica") {
  theme_minimal(
    base_size = base_size,
    base_family = base_family
  ) + 
    theme(
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      axis.text.y = element_blank(),
      # axis.text.x = element_blank(),
      axis.ticks.y.left = element_blank()
    )  
}


## Global params
initial.rn <- c(2503, 2609, 2721)
initial.tmpl.vl <- c(96, 115, 118)
ylims <- c(-1, 2)
ytext_1 <- 1.6

## First plot ("base")
plt_base_0 <- 
  ggplot(acc.sub2, aes(x = time_s, y = vm)) + 
  theme_gif() + 
  geom_line(color = "grey60") + 
  labs(x = "Time [s]",
       y = "Left hip\nvector magnitude [g]") + 
  scale_y_continuous(limits = ylims)

## Function to generate plots related to one stride segmentation
get.plots <- function(plt_base, ii){

  ## PLOT 1 
  tmpl.vec <- template.tmp[[out.sub$template_i[ii]]]
  tmpl.vl <- initial.tmpl.vl[ii]
  tmpl.vec.sc <- (approx(x = seq(0, 1, length.out = length(tmpl.vec)),
                         y = tmpl.vec,
                         xout = seq(0, 1, length.out = tmpl.vl)))$y
  ## Template df
  rn.1 <- initial.rn[ii]
  rn.vec <- rn.1:(rn.1 + tmpl.vl - 1)
  tmpl_df <- data.frame(templ = tmpl.vec.sc, rn = rn.vec)
  acc_templ <- acc.sub2 %>% left_join(tmpl_df, by = "rn")
  acc_templ_sub1 <- acc_templ %>% filter(!is.na(templ))
  acc_templ_sub2 <- acc_templ_sub1 %>% filter(row_number()==1 | row_number()==n())
  start.time_s <- round(min(acc_templ_sub2$time_s), 2)
  plt_1 <- 
    plt_base + 
    annotate("text", start.time_s + 0.03, y = ytext_1, hjust = 0, vjust = 1,
             label = "Best template match", color = "red") + 
    geom_line(data = acc_templ, aes(x = time_s, y = templ * 0.5),
              color = "red", size = 1.5, alpha = 0.4) 
  # plot(plt_1)
  
  plt_1b <- 
    plt_base + 
    annotate("text", start.time_s + 0.03, y = ytext_1, hjust = 0, vjust = 1,
             label = "Best template match", color = "red") + 
    geom_line(data = acc_templ, aes(x = time_s, y = templ * 0.5),
              color = "red", size = 1.5, alpha = 0.4) + 
    geom_vline(xintercept = acc_templ_sub2$time_s,
               linetype = 2,
               color = "red", alpha = 0.5)
  
  plt_1c <- 
    plt_base + 
    annotate("text", start.time_s + 0.03, y = ytext_1, hjust = 0, vjust = 1,
             label = "Maxima fine-tuning...", color = "blue") + 
    geom_line(data = acc_templ, aes(x = time_s, y = templ * 0.5),
              color = "red", size = 1.5, alpha = 0.4) + 
    geom_vline(xintercept = acc_templ_sub2$time_s,
               linetype = 2,
               color = "red", alpha = 0.5)
  # plot(plt_1)
  
  ## PLOT 2 
  plt_2 <- 
    plt_base + 
    annotate("text", start.time_s + 0.03, y = ytext_1, hjust = 0, vjust = 1,
             label = "Maxima fine-tuning...", color = "blue") + 
    geom_vline(xintercept = acc_templ_sub2$time_s,
               linetype = 2,
               color = "red", alpha = 0.5)
  # plot(plt_2)
  reds.xintercept <- acc_templ_sub2$time_s
  reds.start.time_s <- start.time_s
  
  ## PLOT 3
  tmpl.vec <- template.tmp[[out.sub$template_i[ii]]]
  tmpl.vl <- out.sub$T_i[ii]
  tmpl.vec.sc <- (approx(x = seq(0, 1, length.out = length(tmpl.vec)),
                         y = tmpl.vec,
                         xout = seq(0, 1, length.out = tmpl.vl)))$y
  rn.1 <- out.sub$tau_i[ii]
  rn.vec <- rn.1:(rn.1 + tmpl.vl - 1)
  tmpl_df <- data.frame(templ = tmpl.vec.sc, rn = rn.vec)
  acc_templ <- acc.sub2 %>% left_join(tmpl_df, by = "rn")
  acc_templ_sub1 <- acc_templ %>% filter(!is.na(templ))
  acc_templ_sub2 <- acc_templ_sub1 %>% filter(row_number()==1 | row_number()==n())
  start.time_s <- round(min(acc_templ_sub2$time_s), 2)
  
  plt_3a <- 
    plt_base + 
    geom_vline(xintercept = reds.xintercept,
               linetype = 2,
               color = "red", alpha = 0.5) + 
    geom_vline(xintercept = acc_templ_sub2$time_s,
               linetype = 2,
               color = "blue", alpha = 0.5) + 
    annotate("text", reds.start.time_s + 0.03, y = ytext_1, hjust = 0, vjust = 1,
             label = "Maxima fine-tuning...", color = "blue") 
  
  plt_3b <- 
    plt_base + 
    geom_vline(xintercept = acc_templ_sub2$time_s,
               linetype = 2,
               color = "blue", alpha = 0.5) + 
    annotate("text", reds.start.time_s + 0.03, y = ytext_1, hjust = 0, vjust = 1,
             label = "Maxima fine-tuning...", color = "blue") 
  
  plt_3c <- 
    plt_base + 
    geom_vline(xintercept = acc_templ_sub2$time_s,
               linetype = 2,
               color = "blue", alpha = 0.5) +
    annotate("text", reds.start.time_s + 0.03, y = ytext_1, hjust = 0, vjust = 1,
             label = "Fine-tuned", color = "blue") 
  # plot(plt_3)
  
  
  ## PLOT 4
  plt_4 <- 
    plt_base + 
    geom_vline(xintercept = acc_templ_sub2$time_s,
               linetype = 2,
               color = "blue", alpha = 0.5) +
    annotate("text", reds.start.time_s + 0.03, y = ytext_1, hjust = 0, vjust = 1,
             label = "Fine-tuned", color = "blue") +  
    geom_line(data = acc_templ_sub1,
              aes(x = time_s, y = vm), color = "blue", alpha = 0.3, size = 2)
  # plot(plt_4)
  
  
  ## PLOT 5
  plt_5 <- 
    plt_base + 
    geom_line(data = acc_templ_sub1,
              aes(x = time_s, y = vm), color = "blue", alpha = 0.3, size = 2)
  # plot(plt_5)

  return(list(plt_1, plt_1b, plt_1c, plt_2, plt_3a, plt_3b, plt_3c, plt_4, plt_5))
}


## Call plots generating function
plots.list.1 <- get.plots(plt_base_0, 2)
plots.list.2 <- get.plots(plots.list.1[[length(plots.list.1)]], 1)
plots.list.3 <- get.plots(plots.list.2[[length(plots.list.2)]], 3)

## Make two last plots
plt_base <- plots.list.3[[length(plots.list.3)]]
plt.df.last <- out.sub %>%
  mutate(rn = tau_i) %>%
  left_join(acc.sub2, by = "rn")
plt_last.0 <- plt_base + 
  geom_vline(xintercept = plt.df.last$time_s,
             linetype = 2, color = "blue", alpha = 0.5) + 
  geom_vline(xintercept = plt.df.last$time_s[3] + (plt.df.last$T_i[3]/100) - 0.01,
             linetype = 2, color = "blue", alpha = 0.5)
plt_last <- plt_last.0
for (i in 1:nrow(plt.df.last)){
  start.tmp <- plt.df.last[i, "time_s"]
  text.tmp <- paste0("Stride #", i, 
                     "\n* start: ", round(start.tmp,2), " s",
                     "\n* duration: ", round(plt.df.last$T_i[i]/100, 2), " s")
  plt_last <- plt_last +
    annotate("text", start.tmp + 0.05, y = 1.8, hjust = 0, vjust = 1,
             label = text.tmp, color = "blue")
}


## Combine all plots in one list object
plots.list.all <- 
  c(list(plt_base_0),
    plots.list.1, 
    plots.list.2,
    plots.list.3,
    list(plt_last.0,
         plt_last,
         plt_last,
         plt_last))

## Generate GIF 
gif_file <- file.path("gifs", "adept_concept.gif")
save_gif(lapply(plots.list.all, function(p) print(p)),
         delay = 1,
         gif_file = gif_file, 
         width = 1280, 
         height = 720, 
         res = 144)
