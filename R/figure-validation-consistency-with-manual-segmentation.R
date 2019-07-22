
#' @author
#' Marta Karas <marta.karass@gmail.com>
#' 
#' @description 
#' Code to generate manuscript figures: 
#' - "validation_consistency_with_manual_segmentation_all.png"
#' - "validation_consistency_with_manual_segmentation_1.png" / "Fig4.eps"

rm(list = ls())

library(dplyr)
library(data.table)
library(ggplot2)
library(tidyr)
source(file.path("R", "util.R"))

ggsave.device  <- "png"
loc_id.levels  <- c("left_wrist", "left_hip","left_ankle", "right_ankle")
loc_id.labels  <- c( "Left wrist","Left hip", "Left ankle", "Right ankle")


## -----------------------------------------------------------------------------
## Figure 

plt.name <- "validation_consistency_with_manual_segmentation_all.png"

diffs.df.path <- file.path("data-results", "validation-consistency-with-manual-matched.csv")
diffs.df <- as.data.frame(fread(diffs.df.path))
str(diffs.df)

diffs.subj <- 
  diffs.df %>% 
  select(loc_id, SS_start_diff, SS_end_diff) %>%
  melt(id.vars = "loc_id") %>%
  tidyr::separate(variable, c("foo", "variable"), "SS_")  %>%
  tidyr::separate(variable, c("variable", "foo"), "_diff") %>%
  mutate(loc_id = factor(loc_id, 
                         levels = loc_id.levels, 
                         labels = loc_id.labels), 
         variable = factor(variable, 
                           levels = c("start", "end"), 
                           labels = c("Stride start", "Stride end")))

plt <- 
  ggplot(diffs.subj, aes(value / 100)) + 
  geom_vline(xintercept = 0, linetype = 2, color = "blue", alpha = 0.5, size = 0.7) + 
  geom_density(aes(fill = loc_id), position = "identity", alpha = 0.3) +
  facet_grid(loc_id ~ variable) + 
  scale_x_continuous(limits = c(-0.6, 0.6)) + 
  theme_Publication() + 
  labs(x = "Time difference [s]", 
       y = "Distribution density", 
       fill = "Sensor\nlocation") + 
  theme(legend.position = "none") 

# plot(plt)
ggsave(filename = file.path("figures", plt.name), plot = plt, device = ggsave.device, 
       width = 10, height = 5, units = "in")


## -----------------------------------------------------------------------------
## Figure

plt.name <- "validation_consistency_with_manual_segmentation_1.png"

## Selected participant ID
subj_id.tmp <- "idf5e3678b"

## Read data frame with precomputed results of difference between manual and 
## ADEPT-based segmentation
diffs.df <- as.data.frame(fread(file.path("data-results",  "validation-consistency-with-manual-matched.csv")))

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

## Save as eps
ggsave(filename = file.path("figures_eps", "Fig4.eps"), plot = plt, 
       device=cairo_ps, fallback_resolution = 600,
       width = 6.5, height = 7, units = "in")
