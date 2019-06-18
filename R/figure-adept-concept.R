
#' @author
#' Marta Karas <marta.karass@gmail.com>
#' 
#' @description 
#' Code to generate manuscript figures: 
#' - Figure 2: visualize the translation and scaling operations on the data. 
#' - Figure 4: acceleration time series for two subsequent strides, where the 
#'   beginning of a stride is marked.

rm(list = ls())

library(adeptdata)
library(ggplot2)
library(reshape2)
library(runstats)
library(latex2exp)
library(adept)
library(dplyr)
source(file.path("R", "util.R"))

ggsave.device  <- "png"
subj_id.tmp    <- "id3e3e50c7"
loc_id.tmp     <- "left_hip"


## -----------------------------------------------------------------------------

## Figure 2:
## visualize the translation and scaling operations on the data. 
plt.name <- "adaptive_movelets_concept-1.png"

acc.sub <- 
  acc_walking_IU %>%
  filter(subj_id == subj_id.tmp, loc_id == loc_id.tmp) %>%
  arrange(time_s) %>%
  mutate(vm = sqrt(x^2 + y^2 + z^2)) %>%
  select(time_s, vm)
  
tmpl0 <- stride_template$left_hip[[1]][1, ]
tmpl1 <- intrpl.scale.vec(tmpl0, nout = 45, scale = FALSE, center = FALSE)
tmpl2 <- intrpl.scale.vec(tmpl0, nout = 90, scale = FALSE, center = FALSE)
tmpl3 <- intrpl.scale.vec(tmpl0, nout = 135, scale = FALSE, center = FALSE)

acc.sub$cov_tmpl1 <- RunningCov(acc.sub$vm, tmpl1)
acc.sub$cov_tmpl2 <- RunningCov(acc.sub$vm, tmpl2)
acc.sub$cov_tmpl3 <- RunningCov(acc.sub$vm, tmpl3)

var.levels <- c("vm", paste0("cov_tmpl", 1:3))
var.labels <- c("Vector\nmagnitude [g]", "Cov.", " Cov. ", "  Cov.  ")

# Data frame with walking VM data and running correlation values
plt.df <- 
  acc.sub %>% 
  melt(id.vars = c("time_s")) %>%
  mutate(value = ifelse(variable != "vm", value, (value - 1.2) * 0.45),
         value_color = ifelse(variable == "vm", "black", "grey"),
         variable = factor(as.character(variable), levels =  var.levels, labels = var.labels))

# Data frame with templates 
tmpl.df <- data.frame()
tmpl.l <- list(tmpl1, tmpl2, tmpl3)
time_s.start.grid <- c(105.61, 108.1)
for (tmpl.idx in 1:length(tmpl.l)){
  tmpl.tmp <- tmpl.l[[tmpl.idx]]
  for (time_s.start.tmp in time_s.start.grid){
    df.tmp <- data.frame(
      seq(from = time_s.start.tmp, by = 0.01, length.out = length(tmpl.tmp)),
      paste0("cov_tmpl", tmpl.idx),
      tmpl.tmp,
      time_s.start.tmp, 
      stringsAsFactors = FALSE
    )
    tmpl.df <- rbind(tmpl.df, df.tmp)
  }
}
names(tmpl.df) <- c("time_s", "variable", "value", "time_s_start")
plt.tmpl.df <- 
  tmpl.df %>%
  mutate(variable = factor(as.character(variable), levels =  var.levels, labels = var.labels))

time_s.shift <- 104.4
xinterc.vec1 <- 1.21 + c(0, 0.9) 
xinterc.vec2 <- 3.7 + c(0, 0.9) 

plt <- 
  ggplot(plt.df, aes(x = time_s - time_s.shift, y = value, group = 1)) + 
  geom_line(aes(color = value_color), size = 0.4) + 
  facet_grid(variable ~ ., switch = "y") + 
  scale_colour_manual(values = c("black", "gray85")) + 
  scale_x_continuous(limits = c(0, 5.5), expand = c(0,0)) + 
  scale_y_continuous(limits = c(-0.6, 0.7)) + 
  
  geom_line(data = plt.tmpl.df %>% filter(time_s_start == time_s.start.grid[1]),
            aes(x = time_s - time_s.shift, y = value * 0.27, group = 1), 
            size = 1, alpha = 0.8, color = "green") + 
  geom_line(data = plt.tmpl.df %>% filter(time_s_start == time_s.start.grid[2]),
            aes(x = time_s - time_s.shift, y = value * 0.27, group = 1), 
            size = 1, alpha = 0.8, color = "red") + 
  geom_vline(xintercept = xinterc.vec1, linetype = 2, color = "gray40") + 
  geom_vline(xintercept = xinterc.vec2, linetype = 2, color = "gray40") + 
  theme_Publication() + 
  theme(legend.position = "none",
        strip.background = element_rect(fill="white")) + 
  labs(x = "Time [s]", y = "") 
# plot(plt)
ggsave(filename = file.path("figures", plt.name), plot = plt, device = ggsave.device, 
       width = 8, height = 5.7, units = "in")  


## -----------------------------------------------------------------------------

## Figure 4:
## acceleration time series for two subsequent strides, where the beginning of a
## stride is marked.
plt.name <- "stride_visualization-1.png"

plt.df <- acc.sub
plt.df$vm_smoothed <- dig_filter(plt.df$vm, 100, LD = 0, LU = 4)

## Plot objects/variables
my.arrow <- arrow(
  type = "closed",
  ends = "both",
  angle = seq(10, 80, length = 10),
  length = unit(seq(4, 3, length = 10), "mm"))

xip <- c(5.55, 6.48, 7.42)
tm <- 4.95
ym <- 2.7

plt <- 
  ggplot(plt.df, aes(x = time_s - tm, y = vm, group = 1)) + 
  geom_line(size = 0.4) + 
  geom_line(data = plt.df, aes(x = time_s - tm, y = vm_smoothed, group = 1), 
            size = 1, alpha = 0.5, color = "red") + 
  scale_x_continuous(limits = c(0, 3), expand = c(0,0))  + 
  scale_y_continuous(limits = c(-0.3, 3.5), expand = c(0,0))  + 
  geom_vline(xintercept = xip - tm, linetype = 2, color = "gray40") + 
  ## arrows 
  annotate("segment", x = (xip[1] - tm + 0.01), xend = (xip[2] - tm - 0.01), y = ym, yend = ym, colour = "gray40", size = 0.6, arrow = my.arrow) + 
  annotate("segment", x = (xip[2] - tm + 0.01), xend = (xip[3] - tm - 0.01), y = ym, yend = ym, colour = "gray40", size = 0.6, arrow = my.arrow) + 
  ## \tau_{ij} text
  annotate("text", x = (xip[1] - tm - 0.07), y = -0.05, label = TeX("$\\tau_{i1}$", output = 'character'), parse = TRUE, size = 5) + 
  annotate("text", x = (xip[2] - tm - 0.07), y = -0.05, label = TeX("$\\tau_{i2}$", output = 'character'), parse = TRUE, size = 5) + 
  annotate("text", x = (xip[3] - tm - 0.07), y = -0.05, label = TeX("$\\tau_{i3}$", output = 'character'), parse = TRUE, size = 5) + 
  ## T_{ij} text
  annotate("text", x = (xip[1] - tm + 0.48), y = 2.9, label = TeX("$T_{i1}$", output = 'character'), parse = TRUE, size = 5) + 
  annotate("text", x = (xip[2] - tm + 0.48), y = 2.9, label = TeX("$T_{i2}$", output = 'character'), parse = TRUE, size = 5) + 
  labs(x = "Time [s]", 
       y = "Vector magnitude [g]") +
  theme_Publication() 
# plot(plt)
ggsave(filename = file.path("figures", plt.name), plot = plt, device = ggsave.device, 
       width = 8, height = 3.7, units = "in")  