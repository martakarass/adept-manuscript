
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

## Figure 1 (Supplementary Material)

plt.name <- "suplmat_effect_of_neighborhood_width.png"
  
res.df <- as.data.frame(fread(file.path("data-results", "suplmat-effect-of-neighborhood-width.csv")))

## Random subset of Participant ID
set.seed(1)
subj_id.sub <- sample(sort(unique(res.df$subj_id)), size = 3, replace = FALSE)
subj_id.sub
# [1] "idf5e3678b" "id37a54bbf" "idecc9265e"
subj_id.levels <- subj_id.sub
subj_id.labels <- paste0("Participant ", paste0(LETTERS[1:3]))

plt.df <- 
  res.df %>% 
  filter(subj_id %in% subj_id.sub) %>%
  mutate(loc_id = factor(loc_id, levels = loc_id.levels, labels = loc_id.labels),
         subj_id = factor(subj_id, levels = subj_id.levels, labels = subj_id.labels))
plt <- 
  ggplot(plt.df, aes(x = factor(nbh_W), y = T_i/100)) + 
  geom_boxplot(outlier.size = 0.7) + 
  facet_grid(subj_id ~ loc_id) + 
  theme_Publication() + 
  labs(x = "Width of neighborhood used in maxima fine-tuning procedure [s]",
       y = "Estimated stride duration [s]")
plot(plt)
ggsave(filename = file.path("figures", plt.name), plot = plt, device = ggsave.device, 
       width = 10, height = 6, units = "in")





