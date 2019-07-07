
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
loc_id.levels  <- c("left_wrist", "left_ankle")
loc_id.labels  <- c( "Left wrist", "Left ankle")


## -----------------------------------------------------------------------------

## Figure 1 (Supplementary Material)

plt.name <- "suplmat_effect_of_smoothing.png"
  
res.df <- as.data.frame(fread(file.path("data-results", "suplmat-effect-of-smoothing.csv")))

## Random subset of Participant ID
set.seed(1)
subj_id.sub <- sample(sort(unique(res.df$subj_id)), size = 3, replace = FALSE)
subj_id.sub
# [1] "idf5e3678b" "id37a54bbf" "idecc9265e"
subj_id.levels <- subj_id.sub
subj_id.labels <- paste0("Participant ", paste0(LETTERS[1:3]))

x_adept_ma_W.lvl <- sort(unique(res.df$x_adept_ma_W))
x_adept_ma_W.lbl <- paste0("Covariance max.:\n width = ", x_adept_ma_W.lvl, "s")

finetune_maxima_ma_W.lvl <- sort(unique(res.df$finetune_maxima_ma_W))
finetune_maxima_ma_W.lbl <- paste0("Fine-tuning:\n width = ", finetune_maxima_ma_W.lvl, "s")

plt.df <- 
  res.df %>% 
  mutate(loc_id = factor(loc_id, levels = loc_id.levels, labels = loc_id.labels),
         subj_id = factor(subj_id, levels = subj_id.levels, labels = subj_id.labels),
         finetune_maxima_ma_W = factor(finetune_maxima_ma_W, levels = finetune_maxima_ma_W.lvl, labels = finetune_maxima_ma_W.lbl),
         x_adept_ma_W = factor(x_adept_ma_W, levels = x_adept_ma_W.lvl, labels = x_adept_ma_W.lbl))

## PLOT A
plt <- 
  ggplot(plt.df %>% filter(subj_id == subj_id.labels[1]), 
         aes(x = factor(loc_id), y = T_i/100)) + 
  geom_boxplot(outlier.size = 0.7) + 
  facet_grid(finetune_maxima_ma_W ~ x_adept_ma_W) + 
  theme_Publication() + 
  labs(x = "",
       y = "Vector magnitude [g]", 
       title = "Participant A")
plot(plt)
ggsave(filename = file.path("figures", "suplmat_effect_of_smoothing_A.png"), 
       plot = plt,
       device = ggsave.device, 
       width = 6, height = 6, units = "in")

## PLOT B
plt <- 
  ggplot(plt.df %>% filter(subj_id == subj_id.labels[2]), 
         aes(x = factor(loc_id), y = T_i/100)) +   
  geom_boxplot(outlier.size = 0.7) + 
  facet_grid(finetune_maxima_ma_W ~ x_adept_ma_W) + 
  theme_Publication() + 
  labs(x = "",
       y = "Vector magnitude [g]", 
       title = "Participant B")
plot(plt)
ggsave(filename = file.path("figures", "suplmat_effect_of_smoothing_B.png"), 
       plot = plt,
       device = ggsave.device, 
       width = 6, height = 6, units = "in")

## PLOT C
plt <- 
  ggplot(plt.df %>% filter(subj_id == subj_id.labels[3]), 
         aes(x = factor(loc_id), y = T_i/100)) + 
  geom_boxplot(outlier.size = 0.7) + 
  facet_grid(finetune_maxima_ma_W ~ x_adept_ma_W) + 
  theme_Publication() + 
  labs(x = "",
       y = "Vector magnitude [g]", 
       title = "Participant C")
plot(plt)
ggsave(filename = file.path("figures", "suplmat_effect_of_smoothing_C.png"), 
       plot = plt,
       device = ggsave.device, 
       width = 6, height = 6, units = "in")






