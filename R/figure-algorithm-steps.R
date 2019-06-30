
#' @author
#' Marta Karas <marta.karass@gmail.com>
#' 
#' @description 
#' Code to generate manuscript figures: 
#' - Figure 7: demonstration of window size effect on smoothing. 
#' - Figure 8: visualization of the ADEPT covariance matrix. 
#' - Figure 9: visualization of the algorithm tuning step.


rm(list = ls())

library(adept)
library(adeptdata)
library(ggplot2)
library(dplyr)

source(file.path("R", "util.R"))
ggsave.device <- "png"

loc_id.levels  <- c("left_wrist", "left_hip","left_ankle", "right_ankle")
loc_id.labels  <- c( "Left wrist","Left hip", "Left ankle", "Right ankle")


## -----------------------------------------------------------------------------

## Figure 7: 
## demonstration of window size effect on smoothing. 
plt.name <- "methods_smoothing_w_param_effect.png"
subj_id.sub <- c("idabd0c53c")

plt.df.0 <- 
  acc_walking_IU %>% 
  filter(subj_id %in% subj_id.sub) %>%
  mutate(vm = sqrt(x^2 + y^2 + z^2)) %>% 
  select(-subj_id, -x, -y, -z) %>%
  arrange(loc_id, time_s) %>%
  mutate(vm_sm015 = windowSmooth(vm, W = 0.15, x.fs = 100),
         vm_sm025 = windowSmooth(vm, W = 0.25, x.fs = 100),
         loc_id  = factor(loc_id, levels = loc_id.levels, labels = loc_id.labels)) 

variable.levels <- c("vm", "vm_sm015", "vm_sm025")
variable.labels <- c("original signal", "w = 0.15s", "w = 0.25s")

plt.df <- 
  plt.df.0 %>%  
  melt(id.vars = c("loc_id", "time_s")) %>%
  mutate(variable = factor(variable, levels = variable.levels, labels = variable.labels))

time_s.peaks.lw <- c(5.40, 6.44, 7.49, 8.51)
time_s.peaks.lh <- c(5.36, 6.41, 7.45, 8.49)
time_s.peaks.la <- c(5.36, 6.42, 7.46, 8.48)
time_s.peaks.ra <- c(5.35, 6.40, 7.43, 8.48)
loc_id.vec <- as.vector(sapply(loc_id.levels, function(val) rep(val, 4)))
plt.df.peaks <- data.frame(
  time_s = c(time_s.peaks.lw, time_s.peaks.lh, time_s.peaks.la, time_s.peaks.ra),
  loc_id = factor(loc_id.vec, levels = loc_id.levels, labels = loc_id.labels)) 

plt <- 
  ggplot(plt.df, aes(x = time_s - 5, y = value, group = 1)) + 
  geom_vline(data = plt.df.peaks, aes(xintercept = time_s - 5), alpha = 0.3, color = "blue") +
  geom_line(size = 0.4) + 
  facet_grid(variable ~ loc_id) + 
  scale_x_continuous(limits = c(0, 3)) + 
  scale_y_continuous(limits = c(0, 4.7)) + 
  theme_Publication() + 
  labs(x = "Time [s]", y = "Vector magnitude [g]") 
# plot(plt)

ggsave(filename = file.path("figures", plt.name), plot = plt, device = ggsave.device, 
       width = 10, height = 5.5, units = "in")


## -----------------------------------------------------------------------------

## Figure 8: 
## visualization of the ADEPT covariance matrix. 

plt.name <- "methods_adept_similarity_matrix.png"

subj_id.sub <- c("idabd0c53c")
loc_id.sub  <- c("left_ankle")

## Get VM time-series for data collected at left wrist for 1 participant
vm <- 
  acc_walking_IU %>% 
  filter(subj_id %in% subj_id.sub,
         loc_id %in% loc_id.sub) %>%
  mutate(vm = sqrt(x^2 + y^2 + z^2)) %>% 
  arrange(time_s) %>%
  pull(vm)
## Replace NA's to make similarityMatrix work 
vm[which(is.na(vm))] <- mean(vm, na.rm = TRUE)

## Define 1-element list of wrist-specific templates 
templ.list <- list(stride_template$left_ankle[[1]][1, ])

## Compute a list of scaled templates via linear interpolation
templ.vl <- seq(50, 176, 1)
stempl <- scaleTemplate(template = templ.list, template.vl = templ.vl)

## Compute ADEPT similarity matrix between VM time-series and a collection of scaled templates
smat <- similarityMatrix(x = vm, template.scaled = stempl, similarity.measure = "cov")

## Convert ADEPT similarity matrix to ggplot-ready data frame
rownames(smat) <- templ.vl
colnames(smat) <- seq(0.01, by = 0.01, length.out = ncol(smat))
smat.sub.row <- seq(1, nrow(smat), 3)
smat.sub.col <- seq(500, 900, 1)
smat.sub     <- smat[smat.sub.row, smat.sub.col]

plt.df <- 
  melt(smat.sub) %>%
  rename_(templ_vl = names(.)[1],
          time_s   = names(.)[2]) %>%
  mutate(templ_vl = as.numeric(as.character(templ_vl)),
         time_s   = as.numeric(as.character(time_s)),
         stride_s = (templ_vl - 1)/100,
         cad = 2 / stride_s,
         loc_id = factor("left_ankle", levels = loc_id.levels, labels = loc_id.labels))

plt.df.maxval <- plt.df %>% filter(value == max(value))
print(plt.df.maxval)

limits.tmp  <- c(-1, 1) * max(abs(smat.sub))
colours.tmp <- c("blue", "white", "red")
s_shift <- 5

plt <- 
  ggplot(plt.df, aes(time_s - s_shift, stride_s)) +
  geom_tile(aes(fill = value), colour = "grey90") +
  facet_grid(. ~ loc_id) + 
  scale_fill_gradientn(colours = colours.tmp,
                       limits = limits.tmp) + 
  geom_point(data = plt.df.maxval, aes(time_s - s_shift, stride_s), colour = "black", size = 1) + 
  geom_vline(data = plt.df.maxval, aes(xintercept = time_s - s_shift), colour = "black", size = 0.1) +   
  geom_hline(data = plt.df.maxval, aes(yintercept = stride_s), colour = "black", size = 0.1) + 
  theme_Publication() + 
  labs(x = "Time [s]", 
       y = "Stride duration [s]",
       fill = "Sample\ncovariance") + 
  scale_x_continuous(expand = c(0, 0.01), breaks = c(seq(0, 100, 1), plt.df.maxval$time_s - 5)) +
  scale_y_continuous(expand = c(0, 0), breaks = c(seq(0.5, 111.75, 0.25), plt.df.maxval$stride_s)) + 
  theme(legend.key.size = unit(0.5, "cm"),
        legend.spacing = unit(0, "cm"))
# plot(plt)

ggsave(filename = file.path("figures", plt.name), plot = plt, device = ggsave.device, 
       width = 10, height = 6, units = "in")


## -----------------------------------------------------------------------------

## Figure 9: 
## visualization of the algorithm tuning step.

plt.name.1 <- "methods_fine_tuning_part1.png"
plt.name.2 <- "methods_fine_tuning_part2.png"

vm_sm025 <- windowSmooth(vm, W = 0.25, x.fs = 100)
plt.df <- data.frame(
  x = seq(0.01, by = 0.01, length.out = length(vm)),
  y = vm,
  y2 = vm_sm025,
  loc_id = factor("left_ankle", levels = loc_id.levels, labels = loc_id.labels))

alpha.sh <- 0.05

plt <- 
  ggplot(plt.df, aes(x = x -5 , y = y, group = 1)) + 
  annotate("rect", xmin = 6.42 - 5 - 0.3, xmax = 6.42 - 5 + 0.3, ymin = -Inf, ymax = Inf, 
           alpha = alpha.sh, fill = "blue")  + 
  annotate("rect", xmin = 6.42 - 5 + 1.06 - 0.3, xmax = 6.42 - 5 + 1.06  + 0.3, ymin = -Inf, ymax = Inf, 
           alpha = alpha.sh, fill = "blue")  + 
  geom_line(alpha = 0.8) + 
  scale_x_continuous( limits = c(1.1, 2.85),  breaks = c(2.0, 1.42, 2.48)) + 
  scale_y_continuous( limits = c(0, 5)) + 
  geom_vline(xintercept = c(6.42 - 5, 6.42 + 1.06 - 5), color = "blue") + 
  facet_grid(. ~ loc_id) + 
  theme_Publication() + 
  labs(x = "Time [s]", y = "Vector magnitude [g]")
# plot(plt)
ggsave(filename = file.path("figures", plt.name.1), plot = plt, device = ggsave.device, 
         width = 5, height = 4, units = "in")

  
plt <- 
  ggplot(plt.df, aes(x = x -5 , y = y, group = 1)) + 
  annotate("rect", xmin = 6.42 - 5 - 0.3, xmax = 6.42 - 5 + 0.3, ymin = -Inf, ymax = Inf, 
           alpha = alpha.sh, fill = "blue")  + 
  annotate("rect", xmin = 6.42 - 5 + 1.06 - 0.3, xmax = 6.42 - 5 + 1.06  + 0.3, ymin = -Inf, ymax = Inf, 
           alpha = alpha.sh, fill = "blue")  + 
  geom_line(alpha = 0.8) + 
  geom_line(data = plt.df,  aes(x = x -5 , y = y2, group = 1), color = "red", inherit.aes = FALSE, size = 0.8, alpha = 0.8) +
  geom_vline(xintercept = c(6.43 - 5, 7.46 - 5), color = "red") + 
  facet_grid(. ~ loc_id) + 
  scale_x_continuous( limits = c(1.1, 2.85),  breaks = c(2.0, 1.43, 2.46)) + 
  scale_y_continuous( limits = c(0, 5)) + 
  theme_Publication() + 
  labs(x = "Time [s]", y = "Vector magnitude [g]") 
# plot(plt)
ggsave(filename = file.path("figures", plt.name.2), plot = plt, device = ggsave.device, 
       width = 5, height = 4, units = "in")

