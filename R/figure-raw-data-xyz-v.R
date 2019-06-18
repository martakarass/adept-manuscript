
#' @author
#' Marta Karas <marta.karass@gmail.com>
#' 
#' @description 
#' Code to generate manuscript figures: 
#' - Figure 1a: 3-dimmensional acceleration time series from 5 seconds of 
#'   walking for two differentstudy participants. 
#' - Figure 1b: Same as above, but showing the vector magnitude. 

rm(list = ls())

library(adeptdata)
library(ggplot2)
library(reshape2)
library(dplyr)
source(file.path("R", "util.R"))

ggsave.device  <- "png"
loc_id.levels  <- c("left_wrist", "left_hip","left_ankle", "right_ankle")
loc_id.labels  <- c( "Left wrist","Left hip", "Left ankle", "Right ankle")
subj_id.sub    <- c("id3e3e50c7", "idabd0c53c")
subj_id.labels <- paste0("Participant ", 1:length(subj_id.sub))


## Figure 1a: 
## 3-dimmensional acceleration time series from 5 seconds of walking 
## for two different study participants
plt.name <- "3d_acc-1.png"
plt.df <- 
  acc_walking_IU %>% 
  filter(subj_id %in% subj_id.sub) %>%
  melt(id.vars = c("subj_id", "loc_id", "time_s")) %>%
  mutate(subj_id = factor(subj_id, levels = subj_id.sub, labels = subj_id.labels),
         loc_id  = factor(loc_id, levels = loc_id.levels, labels = loc_id.labels))
plt <- 
  ggplot(plt.df, aes(x = time_s - 5, y = value, color = variable)) + 
  geom_line() + 
  facet_grid(subj_id ~ loc_id) + 
  scale_x_continuous(limits = c(0, 5)) + 
  theme_Publication() + 
  labs(x = "Time [s]", y = "Amplitude [g]", color = "Axis") +  
  scale_color_manual(breaks = c("x", "y", "z"),
                     values = c("red", "blue", "green"))
# plot(plt)
ggsave(filename = file.path("figures", plt.name), plot = plt, device = ggsave.device, 
       width = 10, height = 5.5, units = "in")


## -----------------------------------------------------------------------------

## Figure 1b: 
## 1-dimmensional vector magniture time series from 5 seconds of walking 
## for two different study participants
plt.name <- "1d_acc-1.png"
plt.df <- 
  acc_walking_IU %>% 
  filter(subj_id %in% subj_id.sub) %>%
  mutate(value = sqrt(x^2 + y^2 + z^2),
         subj_id = factor(subj_id, levels = subj_id.sub, labels = subj_id.labels),
         loc_id  = factor(loc_id, levels = loc_id.levels, labels = loc_id.labels))
plt <- 
  ggplot(plt.df, aes(x = time_s - 5, y = value, group = 1)) + 
  geom_line(size = 0.4) + 
  facet_grid(subj_id ~ loc_id) + 
  scale_x_continuous(limits = c(0, 5)) + 
  theme_Publication() + 
  labs(x = "Time [s]", y = "Vector magnitude [g]") 
# plot(plt)
ggsave(filename = file.path("figures", plt.name), plot = plt, device = ggsave.device, 
       width = 10, height = 5.2, units = "in")