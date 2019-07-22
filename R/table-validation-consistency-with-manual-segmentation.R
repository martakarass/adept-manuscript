
#' @author
#' Marta Karas <marta.karass@gmail.com>
#' 
#' @description 
#' Code to generate manuscript tables: 
#' - Table 4: The mean and standard deviation of estimated stride start time 
#'   and end time difference values between manually and automatically segmented strides.
#'   ADEPT segmentation results were obtained as described in a validation
#'   procedure in Section 6.2 in the Manuscript.
#' - Table 5: The mean and standard deviation of estimated stride start time 
#'   and end time difference values between manually and automatically segmented strides.
#'   ADEPT segmentation results were obtained with stride patterns estimated based 
#'   on all study participants.


rm(list = ls())

library(dplyr)
library(data.table)
library(stargazer)

loc_id.levels  <- c("left_wrist", "left_hip","left_ankle", "right_ankle")
loc_id.labels  <- c( "Left wrist","Left hip", "Left ankle", "Right ankle")


## -----------------------------------------------------------------------------
## Table 4

diffs.df.path <- file.path("data-results", "validation-consistency-with-manual-matched.csv")
diffs.df <- as.data.frame(fread(diffs.df.path))

## Participant 27 only
subj_id.tmp <- "idf5e3678b"
diffs.df_subj <- 
  diffs.df %>% 
  filter(subj_id == subj_id.tmp) %>%
  group_by(loc_id) %>%
  mutate(SS_start_diff = SS_start_diff/100,
         SS_end_diff = SS_end_diff/100) %>%
  summarise(SS_start_diff_mean = mean(SS_start_diff),
            SS_start_diff_sd   = sd(SS_start_diff),
            SS_end_diff_mean   = mean(SS_end_diff),
            SS_end_diff_sd     = sd(SS_end_diff)) %>%
  mutate(SS_start_diff_mean = round(SS_start_diff_mean, 2),
         SS_start_diff_sd = round(SS_start_diff_sd, 2),
         SS_end_diff_mean = round(SS_end_diff_mean, 2),
         SS_end_diff_sd = round(SS_end_diff_sd, 2),
         loc_id = factor(loc_id, 
                         levels = loc_id.levels, 
                         labels = loc_id.labels)) %>% 
  arrange(loc_id) %>%
  as.data.frame()

## Average stride length for that participant 
res.df <- as.data.frame(fread(file.path("data-results", "validation-consistency-with-manual-segmentation.csv")))
res.df %>%
  filter(subj_id == subj_id.tmp) %>%
  group_by(loc_id) %>%
  filter(tau_i != min(tau_i),
         tau_i != max(tau_i)) %>%
  mutate(dur_s = T_i / 100) %>%
  group_by() %>%
  summarize(dur_s = round(mean(dur_s), 2)) %>%
  pull(dur_s)

diffs.df.path <- file.path("data-results", "validation-consistency-with-manual-matched.csv")
diffs.df <- as.data.frame(fread(diffs.df.path))

diffs.df_subj_A <- 
  diffs.df %>% 
  group_by(loc_id) %>%
  mutate(SS_start_diff = SS_start_diff/100,
         SS_end_diff = SS_end_diff/100) %>%
  summarise(SS_start_diff_mean = mean(SS_start_diff),
            SS_start_diff_sd   = sd(SS_start_diff),
            SS_end_diff_mean   = mean(SS_end_diff),
            SS_end_diff_sd     = sd(SS_end_diff)) %>%
  mutate(SS_start_diff_mean = round(SS_start_diff_mean, 2),
         SS_start_diff_sd = round(SS_start_diff_sd, 2),
         SS_end_diff_mean = round(SS_end_diff_mean, 2),
         SS_end_diff_sd = round(SS_end_diff_sd, 2),
         loc_id = factor(loc_id, 
                         levels = loc_id.levels, 
                         labels = loc_id.labels)) %>% 
  arrange(loc_id) %>%
  as.data.frame()

diffs.df_subj_A
stargazer(diffs.df_subj_A, summary = FALSE, rownames = FALSE, digits = 2, font.size = "small")


## -----------------------------------------------------------------------------
## Table 5

diffs.df.path <- file.path("data-results", "application-example-segmentation-vs-manual.csv")
diffs.df <- as.data.frame(fread(diffs.df.path))
diffs.df_subj_B <- 
  diffs.df %>% 
  group_by(loc_id) %>%
  mutate(SS_start_diff = SS_start_diff/100,
         SS_end_diff = SS_end_diff/100) %>%
  summarise(SS_start_diff_mean = mean(SS_start_diff),
            SS_start_diff_sd   = sd(SS_start_diff),
            SS_end_diff_mean   = mean(SS_end_diff),
            SS_end_diff_sd     = sd(SS_end_diff)) %>%
  mutate(SS_start_diff_mean = round(SS_start_diff_mean, 2),
         SS_start_diff_sd = round(SS_start_diff_sd, 2),
         SS_end_diff_mean = round(SS_end_diff_mean, 2),
         SS_end_diff_sd = round(SS_end_diff_sd, 2),
         loc_id = factor(loc_id, 
                         levels = loc_id.levels, 
                         labels = loc_id.labels)) %>% 
  arrange(loc_id) %>%
  as.data.frame()

diffs.df_subj_B
stargazer(diffs.df_subj_B, summary = FALSE, rownames = FALSE, digits = 2, font.size = "small")
