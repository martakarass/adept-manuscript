
#' @author
#' Marta Karas <marta.karass@gmail.com>
#' 
#' @description 
#' Code to generate manuscript figures: 
#' - Figure 11: Heatmap  of  walking  cadence  estimates  using  ADEPT  on  left  
#'   ankle  VM  data. 
#' - Figure 12: Estimated start point (x-axis) and duration time (y-axis) of all 
#'   estimated strides for onestudy participant across four sensor locations. 
#' - Figure 13: Comparison of stride segmentation using the manual and ADEPT 
#'   approaches for four different body locations.


rm(list = ls())

library(data.table)
library(ggplot2)
library(dplyr)
library(cobs)
library(adeptdata)
library(scales)
source(file.path("R", "util.R"))

ggsave.device  <- "png"
loc_id.levels  <- c("left_wrist", "left_hip","left_ankle", "right_ankle")
loc_id.labels  <- c( "Left wrist","Left hip", "Left ankle", "Right ankle")


## -----------------------------------------------------------------------------

## Figure 11: 
## Sensor location-specific sets of estimated empirical patterns of a stride. 
plt.name <- "results_cadence_estimates_heatmap.png"

res.df <- as.data.frame(fread(file.path("data-results", "application-example-segmentation.csv")))

## Subset results to keep left ankle only
res.df.sub <- 
  res.df %>%
  filter(loc_id == "left_ankle") %>% 
  mutate(tau_i_plus_T_i_by2 = tau_i + T_i/2,
         tau_i_plus_T_i = (tau_i + T_i - 1),
         cadence_est = (2 * 100)/T_i) 

## Interpolate results so as for each subject, we have cadence estimate 
## for the same time point
plt.df <- data.frame()
for (subj_id.tmp in unique(res.df.sub$subj_id)){ 
  plt.df.tmp      <- res.df.sub %>% filter(subj_id == subj_id.tmp)
  ## Use time of walking of this particular participant as max value in a 
  ## time grid we interpolate cadence values onto
  t_i.seq.tmp.max <- max(plt.df.tmp$tau_i_plus_T_i)
  t_i.seq.tmp     <- seq(0, t_i.seq.tmp.max, 10)
  cadence_est.tmp <- approx(
    plt.df.tmp$tau_i_plus_T_i_by2,  
    plt.df.tmp$cadence_est,
    t_i.seq.tmp)$y
  plt.df.tmp <- data.frame(t_i = t_i.seq.tmp, cadence_est = cadence_est.tmp)
  plt.df.tmp$subj_id <- subj_id.tmp
  plt.df.tmp <- plt.df.tmp %>% filter(complete.cases(.))
  plt.df <- rbind(plt.df, plt.df.tmp)
}

## Define participant ID levels based on participant's median cadence 
subj_id.df <- 
  res.df.sub %>%
  group_by(subj_id) %>%
  filter(tau_i != min(tau_i),
         tau_i != max(tau_i)) %>%
  summarize(cad_median = median(cadence_est)) %>%
  arrange(desc(cad_median)) 
subj_id.levels <- subj_id.df %>% pull(subj_id)
subj_id.labels <- paste0(1:32)

## Format variables 
plt.df$subj_id <- factor(
  as.character(plt.df$subj_id),
  levels = subj_id.levels,
  labels = subj_id.labels)
plt.df$loc_id <- "Left ankle"

plt <- 
  ggplot(plt.df, aes(t_i / (100 * 60), subj_id)) +
  geom_tile(aes(fill = cadence_est))  + 
  scale_fill_distiller(palette = "Spectral") + 
  facet_grid(. ~ loc_id) +
  theme_Publication() + 
  theme(legend.key.size = unit(0.7, "cm")) + 
  labs(title = "", 
       x = "Time [min]", 
       y = "Study participant", 
       fill = "Cadence\nestimate")   + 
  scale_y_discrete(expand = c(0, 1)) +
  scale_x_continuous(expand = c(0, 0.01), breaks = c(0, 1, 2, 3, 4)) +
  geom_line(data = data.frame(x = c(0, 4), y = rep(1:33, each = 2) - 0.5),
            aes(x = x, y = y, group = y), 
            color = "black", 
            alpha = 0.3)
# plot(plt)
ggsave(filename = file.path("figures", plt.name), plot = plt, device = ggsave.device, 
       width = 10, height = 7, units = "in") 


## -----------------------------------------------------------------------------

## Figure 12: 
## Estimated start point (x-axis) and duration time (y-axis) of all
## estimated strides for onestudy participant across four sensor locations. 
plt.name <- "results_stride_duration_bubble_plot.png"

subj_id.tmp.idx <- 27
subj_id.tmp <- subj_id.levels[subj_id.tmp.idx]
bubble.df   <- res.df %>% filter(subj_id == subj_id.tmp)
bubble.df$subj_id <- paste0("Participant ", subj_id.tmp.idx)
bubble.df$loc_id <- factor(
  as.character(bubble.df$loc_id),
  levels = loc_id.levels,
  labels = loc_id.labels)

## Fit constrained quantile curves using linear or quadratic splines 
## to the estimated stride duration 
cobs.out <- cobs(x = bubble.df$tau_i, 
                 y = bubble.df$T_i, 
                 constraint = "none",
                 nknots = 50,
                 method = "quantile")
## Use splines fitted object to make prediction of estimated stride duration 
splines.pred <- predict(cobs.out, 1:max(bubble.df$tau_i))[, 2]
splines.pred.df <- data.frame(tau_i = 1:max(bubble.df$tau_i),
                              T_i = splines.pred)
splines.pred.df$subj_id <- paste0("Participant ", subj_id.tmp.idx)

## Define experiment time split 
split.n <- 2
bubble.df <- bubble.df %>% mutate(
  time_split = floor(tau_i / ((1/split.n) * (max(bubble.df$tau_i) + 1))))
splines.pred.df <- splines.pred.df %>% mutate(
  time_split =  floor(tau_i / ((1/split.n) * (max(splines.pred.df$tau_i) + 1))))
table(bubble.df$time_split)
table(splines.pred.df$time_split)

plt <- 
  ggplot(bubble.df, aes(x = tau_i / (100 * 60), y = T_i / 100,
                        color = loc_id,
                        group = loc_id)) +
  geom_point(aes(size = sim_i), alpha = 0.25) +
  scale_size(range = c(0, 2.5)) + 
  geom_line(alpha = 0.25) + 
  geom_line(data = splines.pred.df, aes(x = tau_i / (100 * 60), y = T_i / 100, group = 1), 
            inherit.aes = FALSE) +
  facet_wrap(~ time_split, scales = "free_x", nrow = split.n) +
  labs(x = "Time [min]", 
       y = "Stride duration [s]", 
       color = "Sensor\nlocation", 
       shape = "Sensor\nlocation", 
       size = "Covariance\nvalue", 
       title = "Participant 27") +
  theme_Publication() +
  # scale_y_continuous(limits = c(0.5, 1.75)) +
  coord_cartesian(ylim=c(0.7, 1.5)) + 
  theme(legend.key.size = unit(3, "point"),
        strip.background = element_blank(),
        strip.text.x = element_blank())
# plot(plt)
ggsave(filename = file.path("figures", plt.name), plot = plt, device = ggsave.device,
       width = 10, height = 6, units = "in")


## -----------------------------------------------------------------------------

## Figure 13: 
## Comparison of stride segmentation using the manual and ADEPT approaches for 
## four different body locations.
plt.name <- "results_manual_vs_adept_segmentation.png"

subj_id.tmp <- "idf5e3678b"

## Read data frame with precomputed results of difference between manual and 
## ADEPT-based segmentation
diffs.df <- as.data.frame(fread(file.path("data-results", "application-example-segmentation-vs-manual.csv")))

## Subset raw accelerometry data to keep observations for selected participant
## Add `tau_i` observation index 
plt.df.vm <- 
  acc_walking_IU %>% 
  filter(subj_id == subj_id.tmp) %>%
  group_by(loc_id) %>%
  arrange(time_s) %>%
  mutate(tau_i = row_number(),
         vm = sqrt(x^2 + y^2 + z^2)) %>%
  group_by() %>%
  arrange(loc_id, time_s) %>%
  as.data.frame()
plt.df.vm$vm_sm0_25 <- windowSmooth(plt.df.vm$vm, W = 0.25, x.fs = 100)
# plt.df.vm == r.df.subj

## Select some particular stride out of 20 for this Participant 
manSS_tau_i.tmp <- 7948

## Create data frame with data for VM / smoothed VM lines  
diffs.df.sub <- 
  diffs.df %>% 
  mutate(tau_i_T_i_min1 = tau_i + T_i - 1,
         manSS_tau_i_T_i_min1 = manSS_tau_i + manSS_T_i - 1) %>%
  filter(subj_id == subj_id.tmp,
         manSS_tau_i == manSS_tau_i.tmp) %>%
  mutate(tau_i_MIN = min(tau_i, manSS_tau_i),
         tau_i_T_i_min1_MAX = max(tau_i_T_i_min1, manSS_tau_i_T_i_min1) ) %>%
  as.data.frame()

## Number of indices of VM margin before and after the stride to be shown
thres <- 20

## Merge VM raw data with segmentation results
diffs.df.sub.lj <- 
  diffs.df.sub %>% 
  dplyr::select(loc_id, tau_i_MIN, tau_i_T_i_min1_MAX, tau_i, tau_i_T_i_min1)
r.df.subj2 <- 
  plt.df.vm %>% 
  left_join(diffs.df.sub.lj, by = "loc_id",  suffix = c("", "_y")) %>%
  filter(tau_i >= tau_i_MIN - thres,
         tau_i <= tau_i_T_i_min1_MAX + thres)
r.df.subj2$loc_id <- factor(
  as.character(r.df.subj2$loc_id),
  levels = loc_id.levels,
  labels = loc_id.labels)

## Data frame with data for vertical peaks 
diffs.df.sub2A <- 
  diffs.df.sub %>% 
  select(loc_id, tau_i, tau_i_T_i_min1) %>% 
  melt(id.var = "loc_id") %>%
  mutate(segmentation = " with ADEPT")
diffs.df.sub2B <- 
  diffs.df.sub %>% 
  select(loc_id, manSS_tau_i, manSS_tau_i_T_i_min1) %>% 
  melt(id.var = "loc_id") %>%
  mutate(segmentation = "manual")
diffs.df.sub2 <- rbind(diffs.df.sub2A, diffs.df.sub2B)
diffs.df.sub2$loc_id <- factor(
  as.character(diffs.df.sub2$loc_id),
  levels = loc_id.levels,
  labels = loc_id.labels)

cols.vec <- scales::hue_pal()(4)

plt <- 
  ggplot() +  
  ## 
  ## VM smoothed line
  geom_line(data = r.df.subj2, aes(x = tau_i / (100) , y = vm_sm0_25, group = 1),
            color = "black",  alpha = 0.5, linetype = 1) +
  ##
  ## VM raw: thin lines
  geom_line(data = r.df.subj2 %>% filter(loc_id == loc_id.labels[1]), aes(x = tau_i / (100) , y = vm, group = 1),
            color = cols.vec[1],  alpha = 0.7) + 
  geom_line(data = r.df.subj2 %>% filter(loc_id == loc_id.labels[2]), aes(x = tau_i / (100) , y = vm, group = 1),
            color = cols.vec[2],  alpha = 0.7) + 
  geom_line(data = r.df.subj2 %>% filter(loc_id == loc_id.labels[3]), aes(x = tau_i / (100) , y = vm, group = 1),
            color = cols.vec[3],  alpha = 0.7) + 
  geom_line(data = r.df.subj2 %>% filter(loc_id == loc_id.labels[4]), aes(x = tau_i / (100) , y = vm, group = 1),
            color = cols.vec[4],  alpha = 0.7) +
  ##
  ## VM thick (where the strides are): thin lines
  geom_line(data = r.df.subj2 %>% filter(loc_id == loc_id.labels[1], tau_i >= tau_i_y, tau_i <= tau_i_T_i_min1),
            aes(x = tau_i / (100) , y = vm, group = 1), 
            color = cols.vec[1], alpha = 0.25, size = 3) +
  geom_line(data = r.df.subj2 %>% filter(loc_id == loc_id.labels[2], tau_i >= tau_i_y, tau_i <= tau_i_T_i_min1),
            aes(x = tau_i / (100) , y = vm, group = 1), 
            color = cols.vec[2], alpha = 0.25, size = 3) +
  geom_line(data = r.df.subj2 %>% filter(loc_id == loc_id.labels[3], tau_i >= tau_i_y, tau_i <= tau_i_T_i_min1),
            aes(x = tau_i / (100) , y = vm, group = 1), 
            color = cols.vec[3], alpha = 0.25, size = 3) +
  geom_line(data = r.df.subj2 %>% filter(loc_id == loc_id.labels[4], tau_i >= tau_i_y, tau_i <= tau_i_T_i_min1),
            aes(x = tau_i / (100) , y = vm, group = 1), 
            color = cols.vec[4], alpha = 0.25, size = 3) +
  ##
  ## Vertical lines denoting segmentation
  geom_vline(data = diffs.df.sub2, aes(xintercept = value / (100), color = segmentation)) + 
  scale_colour_manual(values = c("red", "blue")) + 
  facet_grid(loc_id ~ ., scales = "free") + 
  theme_Publication() + 
  theme(legend.key.size = unit(0.6, "line")) + 
  labs(x = "Time [s]", 
       y = "Vector magnitude [g]",  
       color = "Segmentation technique: ", ## legend title
       title  = "Participant 27") 
# plot(plt)
ggsave(filename = file.path("figures", plt.name), plot = plt, device = ggsave.device,
       width = 6.5, height = 7, units = "in")








