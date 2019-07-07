
#' @author
#' Marta Karas <marta.karass@gmail.com>
#' 
#' @description 
#' Code to generate Supplementary Material figure. 


rm(list = ls())

library(adeptdata)
library(dplyr)
library(data.table)
library(ggplot2)
source(file.path("R", "util.R"))

ggsave.device <- "png"
loc_id.levels  <- c("left_wrist", "left_hip","left_ankle", "right_ankle")
loc_id.labels  <- c( "Left wrist","Left hip", "Left ankle", "Right ankle")


## -----------------------------------------------------------------------------

## Figure XXX (Supplementary Material)

plt.name <- "suplmat_effect_of_tuning_part1.png"
  
res.df <- as.data.frame(fread(file.path("data-results", "suplmat-effect-of-tuning.csv")))

## Random subset of Participant ID
set.seed(1)
subj_id.sub <- sample(sort(unique(res.df$subj_id)), size = 3, replace = FALSE)
subj_id.sub
# [1] "idf5e3678b" "id37a54bbf" "idecc9265e"
subj_id.levels <- subj_id.sub
subj_id.labels <- paste0("Participant ", paste0(LETTERS[1:3]))

finetune.levels <- c("none", "maxima")
finetune.labels <- c("No tuning", "Tuning")

plt.df <- 
  res.df %>% 
  filter(subj_id %in% subj_id.sub) %>%
  mutate(loc_id = factor(loc_id, levels = loc_id.levels, labels = loc_id.labels),
         subj_id = factor(subj_id, levels = subj_id.levels, labels = subj_id.labels),
         finetune = factor(finetune, levels = finetune.levels, labels = finetune.labels))
plt <- 
  ggplot(plt.df, aes(x = factor(finetune), y = T_i/100)) + 
  geom_boxplot(outlier.size = 0.7) + 
  facet_grid(subj_id ~ loc_id) + 
  theme_Publication() + 
  labs(x = "Maxima fine-tuning employed",
       y = "Estimated stride duration [s]")
plot(plt)
ggsave(filename = file.path("figures", plt.name), plot = plt, device = ggsave.device, 
       width = 10, height = 6, units = "in")




## -----------------------------------------------------------------------------

## Figure XXX (Supplementary Material)

## Accelerometry data subset filtered to keep only data of three participants
acc.sub <- 
  acc_walking_IU %>% 
  filter(subj_id %in% subj_id.levels) %>%
  group_by(subj_id, loc_id) %>%
  arrange(time_s) %>%
  mutate(tau_i = row_number(),
         vm = sqrt(x^2 + y^2 + z^2)) %>%
  group_by() %>%
  arrange(loc_id, time_s) %>%
  as.data.frame()

## Objects to store subject- and location-specific stride pattern estimates
subj_id.vec    <- numeric()
loc_id.vec     <- numeric()
phase.vec      <- numeric()
val.vec        <- numeric()
obs_idx.vec    <- numeric()
finetune.vec   <- numeric()

itpl.n <- 200

for (subj_id.tmp in subj_id.levels){   
  for (loc_id.tmp in loc_id.levels){  
    # subj_id.tmp <-  subj_id.levels[1]; loc_id.tmp <- loc_id.levels[1]
    message(paste0("subj_id: ", subj_id.tmp, ", loc_id: ", loc_id.tmp))
    
    ## Tunning
    
    ## Subject- and location-specific subset of raw acc data 
    acc.sub.subj_loc <- 
      acc.sub %>% 
      filter(subj_id == subj_id.tmp, 
             loc_id == loc_id.tmp) %>%
      arrange(tau_i)
    ## Subject- and location-specific subset of segmentation results
    res.subj_loc <- 
      res.df %>% 
      filter(subj_id == subj_id.tmp, 
             loc_id == loc_id.tmp,
             finetune == "maxima")
    ## Skip first and last slide of a person
    for (i in 2:(nrow(res.subj_loc)-1)){ 
      # i <- 2
      tau_i.tmp  <- res.subj_loc$tau_i[i]
      T_i.tmp    <- res.subj_loc$T_i[i]
      vm_idx.tmp <- tau_i.tmp : (tau_i.tmp + T_i.tmp - 1)
      vm.tmp     <- acc.sub.subj_loc$vm[vm_idx.tmp]
      vm.approx.tmp <- (approx(
        x = seq(0, 1, length.out = length(vm.tmp)),
        y = vm.tmp,
        xout = seq(0, 1, length.out = itpl.n)
      ))$y
      ## Store results
      subj_id.vec    <- c(subj_id.vec, rep(subj_id.tmp, itpl.n))
      loc_id.vec     <- c(loc_id.vec,  rep(loc_id.tmp, itpl.n))
      phase.vec      <- c(phase.vec, seq(0, 1, length.out = itpl.n))
      val.vec        <- c(val.vec, vm.approx.tmp)
      obs_idx.vec    <- c(obs_idx.vec, rep(i, itpl.n))
      finetune.vec   <- c(finetune.vec, rep("maxima", itpl.n))
    }
      
    ## No tunning
    
    ## Subject- and location-specific subset of raw acc data 
    acc.sub.subj_loc <- 
      acc.sub %>% 
      filter(subj_id == subj_id.tmp, 
             loc_id == loc_id.tmp) %>%
      arrange(tau_i)
    ## Subject- and location-specific subset of segmentation results
    res.subj_loc <- 
      res.df %>% 
      filter(subj_id == subj_id.tmp, 
             loc_id == loc_id.tmp,
             finetune == "none")
    ## Skip first and last slide of a person
    for (i in 2:(nrow(res.subj_loc)-1)){ 
      # i <- 2
      tau_i.tmp  <- res.subj_loc$tau_i[i]
      T_i.tmp    <- res.subj_loc$T_i[i]
      vm_idx.tmp <- tau_i.tmp : (tau_i.tmp + T_i.tmp - 1)
      vm.tmp     <- acc.sub.subj_loc$vm[vm_idx.tmp]
      vm.approx.tmp <- (approx(
        x = seq(0, 1, length.out = length(vm.tmp)),
        y = vm.tmp,
        xout = seq(0, 1, length.out = itpl.n)
      ))$y
      ## Store results
      subj_id.vec    <- c(subj_id.vec, rep(subj_id.tmp, itpl.n))
      loc_id.vec     <- c(loc_id.vec,  rep(loc_id.tmp, itpl.n))
      phase.vec      <- c(phase.vec, seq(0, 1, length.out = itpl.n))
      val.vec        <- c(val.vec, vm.approx.tmp)
      obs_idx.vec    <- c(obs_idx.vec, rep(i, itpl.n))
      finetune.vec   <- c(finetune.vec, rep("none", itpl.n))
    }
  }
}

  
## Put results into data frame, format data frame
plt.df <- data.frame(
  subj_id = subj_id.vec, 
  loc_id  = loc_id.vec, 
  phase   = phase.vec, 
  val     = val.vec, 
  obs_idx = obs_idx.vec, 
  finetune = finetune.vec,
  stringsAsFactors = FALSE)

plt.df$loc_id <- factor(
  as.character(plt.df$loc_id),
  levels = loc_id.levels,
  labels = loc_id.labels)
plt.df$finetune <- factor(
  as.character(plt.df$finetune),
  levels = finetune.levels,
  labels = finetune.labels)

## Aggregate values to get estimated stride patterns
plt.df.agg <- 
  plt.df %>% 
  group_by(subj_id, loc_id, phase, finetune) %>%
  summarize(val_mean = mean(val),
            val_median = median(val))


## ------------------------

plt <- 
  ggplot(plt.df %>% filter(subj_id == subj_id.levels[1]), 
         aes(x = phase, y = val, color = loc_id, group = obs_idx)) + 
  geom_line(alpha = 0.05) + 
  geom_line(data = plt.df.agg %>% filter(subj_id == subj_id.levels[1]),  
            aes(x = phase, y = val_mean, group = 1), 
            color = "black", inherit.aes = FALSE) +
  facet_grid(finetune ~ loc_id) + 
  theme_Publication() + 
  labs(title = "", 
       x = "Stride phase", 
       y = "Vector magnitude [g]",
       title = "Participant A") + 
  theme(legend.position = 'none') 
plot(plt)

ggsave(filename = file.path("figures", "suplmat_effect_of_tuning_part2_A.png"), 
       plot = plt, 
       device = ggsave.device,
       width = 10, height = 6, units = "in")









