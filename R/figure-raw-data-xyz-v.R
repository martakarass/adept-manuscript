
#' @author
#' Marta Karas <marta.karass@gmail.com>
#' 
#' @description 
#' Code to generate manuscript figures: 
#' - "intro_3d_1d_acc.png" / "Fig1.eps"

rm(list = ls())

library(adeptdata)
library(ggplot2)
library(reshape2)
library(dplyr)
library(gridExtra)
source(file.path("R", "util.R"))

ggsave.device  <- "png"
loc_id.levels  <- c("left_wrist", "left_hip","left_ankle", "right_ankle")
loc_id.labels  <- c( "Left wrist","Left hip", "Left ankle", "Right ankle")


## -----------------------------------------------------------------------------
## Figure 

plt.name <- "intro_3d_1d_acc.png"

subj_id.sub    <- c("id3e3e50c7", "idabd0c53c")
subj_id.labels <- paste0("Participant ", 1:length(subj_id.sub))

plt.df <- 
  acc_walking_IU %>% 
  filter(subj_id %in% subj_id.sub) %>%
  melt(id.vars = c("subj_id", "loc_id", "time_s")) %>%
  mutate(subj_id = factor(subj_id, levels = subj_id.sub, labels = subj_id.labels),
         loc_id  = factor(loc_id, levels = loc_id.levels, labels = loc_id.labels))
plt_a <- 
  ggplot(plt.df, aes(x = time_s - 5, y = value, color = variable)) + 
  geom_line() + 
  facet_grid(subj_id ~ loc_id) + 
  scale_x_continuous(limits = c(0, 5)) + 
  theme_Publication() + 
  labs(x = "Time [s]", y = "Amplitude [g]", color = "Axis", title = "(a)") +  
  scale_color_manual(breaks = c("x", "y", "z"),
                     values = c("red", "blue", "green"))

plt.df <- 
  acc_walking_IU %>% 
  filter(subj_id %in% subj_id.sub) %>%
  mutate(value = sqrt(x^2 + y^2 + z^2),
         subj_id = factor(subj_id, levels = subj_id.sub, labels = subj_id.labels),
         loc_id  = factor(loc_id, levels = loc_id.levels, labels = loc_id.labels))
plt_b <- 
  ggplot(plt.df, aes(x = time_s - 5, y = value, group = 1)) + 
  geom_line(size = 0.4) + 
  facet_grid(subj_id ~ loc_id) + 
  scale_x_continuous(limits = c(0, 5)) + 
  theme_Publication() + 
  labs(x = "Time [s]", y = "Vector magnitude [g]", title = "(b)") 

plt <- grid.arrange(plt_a, plt_b, ncol=1, heights = c(53/100, 47/100))

ggsave(filename = file.path("figures", plt.name), plot = plt, device = ggsave.device, 
       width = 10, height = 9, units = "in")

## Save as eps
ggsave(filename = file.path("figures_eps", "Fig1.eps"), plot = plt, device = "eps", 
       width = 10, height = 9, units = "in")
