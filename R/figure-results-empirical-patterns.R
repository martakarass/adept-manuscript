
#' @author
#' Marta Karas <marta.karass@gmail.com>
#' 
#' @description 
#' Code to generate manusript figures: 
#' - Figure 10: Sensor location-specific sets of estimated empirical patterns of 
#'   a stride. 

rm(list = ls())

library(adeptdata)
library(ggplot2)
library(reshape2)
library(dplyr)
source(file.path("R", "util.R"))

ggsave.device  <- "png"
loc_id.levels  <- c("left_wrist", "left_hip","left_ankle", "right_ankle")
loc_id.labels  <- c( "Left wrist","Left hip", "Left ankle", "Right ankle")


## Figure 10: 
## Sensor location-specific sets of estimated empirical patterns of a stride. 
plt.name <- "results_empirical_pattern.png"

vl <- 200
plt.df <- data.frame(
  y = c(stride_template$left_wrist[[2]][1, ], 
        stride_template$left_wrist[[2]][2, ],
        stride_template$left_hip[[2]][1, ],   
        stride_template$left_hip[[2]][2, ],
        stride_template$left_ankle[[2]][1, ],
        stride_template$left_ankle[[2]][2, ],
        stride_template$right_ankle[[2]][1, ],
        stride_template$right_ankle[[2]][2, ]),
  x = as.vector(replicate(8, seq(0, 1, length.out = vl))),
  loc_id = as.vector(sapply(loc_id.labels, function(loc_id.tmp) rep(loc_id.tmp, 2 * vl))),
  cluster_id = as.vector(replicate(4, c(rep(1, vl), rep(2, vl)))),
  stringsAsFactors = FALSE
)
plt.df$loc_id <- factor(plt.df$loc_id, levels = loc_id.labels)
plt.df$cluster_id <- factor(
  plt.df$cluster_id, 
  levels = c(1,2), 
  labels = paste0("Empirical pattern ", c(1,2)))

plt <- 
  ggplot(plt.df, aes(x = x, y = y, group = 1, color = loc_id)) + 
  geom_hline(yintercept = 0, linetype = 2, color = "darkgrey") + 
  geom_line(size = 1) + 
  facet_grid(cluster_id ~ loc_id) +
  theme_Publication() + 
  labs(x = "Stride phase", 
       y = "Vector magnitude [g]", 
       color = "Sensor location") + 
  theme(legend.position="none") 
# plot(plt)
ggsave(filename = file.path("figures", plt.name), plot = plt, device = ggsave.device, 
       width = 10, height = 4.2, units = "in")  
