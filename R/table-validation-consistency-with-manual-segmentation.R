
#' @author
#' Marta Karas <marta.karass@gmail.com>
#' 
#' @description 
#' Code to generate manuscript tables: 
#' - Table XXX

rm(list = ls())

library(dplyr)
library(data.table)
library(stargazer)

loc_id.levels  <- c("left_wrist", "left_hip","left_ankle", "right_ankle")
loc_id.labels  <- c( "Left wrist","Left hip", "Left ankle", "Right ankle")


## -----------------------------------------------------------------------------

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
diffs.df_subj
#        loc_id SS_start_diff_mean SS_start_diff_sd SS_end_diff_mean SS_end_diff_sd
# 1  Left wrist              -0.04             0.06             0.03           0.18
# 2    Left hip              -0.01             0.01            -0.01           0.01
# 3  Left ankle               0.00             0.01             0.00           0.01
# 4 Right ankle               0.00             0.01             0.00           0.02


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
# [1] 1.06


## -----------------------------------------------------------------------------

## Table 4: 

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
# loc_id SS_start_diff_mean SS_start_diff_sd SS_end_diff_mean SS_end_diff_sd
# 1  Left wrist               0.00             0.16             0.02           0.19
# 2    Left hip               0.00             0.03             0.00           0.03
# 3  Left ankle               0.01             0.03             0.01           0.03
# 4 Right ankle               0.01             0.03             0.01           0.03


# \begin{table}[!htbp] \centering 
# \centering 
# \tblcaption{The mean and standard deviation of estimated stride start time (first column) and end time (second column) difference values between manually and automatically segmented strides. Sample statistics were computed out of $642$ matched stride pairs from all study participants for each four sensor locations.  ADEPT segmentation results were obtained as described in a validation procedure in Sect.~\ref{subsec:manual}. } 
# \small 
# \begin{tabular}{@{\extracolsep{5pt}} l|rr} 
# \hline 
# \hline 
# Sensor location  & Stride start & Stride end \\ 
# & mean (sd) & mean (sd) \\ 
# \hline 
# Left wrist & $0.00$ ($0.16$) & $0.02$ ($0.19$) \\ 
# Left hip & $0.00$ ($0.03$) & $0.00$ ($0.03$) \\ 
# Left ankle & $0.01$ ($0.03$) & $0.01$ ($0.03$) \\ 
# Right ankle & $0.01$ ($0.03$) & $0.01$ ($0.03$) \\ 
# \hline 
# \end{tabular} 
# \label{table:identified_strides_duration}
# \end{table} 




## -----------------------------------------------------------------------------

## Table 5: 

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
#        loc_id SS_start_diff_mean SS_start_diff_sd SS_end_diff_mean SS_end_diff_sd
# 1  Left wrist               0.00             0.17             0.01           0.19
# 2    Left hip               0.00             0.03             0.00           0.03
# 3  Left ankle               0.01             0.03             0.01           0.03
# 4 Right ankle               0.01             0.03             0.01           0.03

stargazer(diffs.df_subj_B, summary = FALSE, rownames = FALSE, digits = 2, font.size = "small")


# \begin{table}[!htbp] \centering 
# \centering 
# \tblcaption{The mean and standard deviation of estimated stride start time (first column) and end time (second column) difference values between manually and automatically segmented strides. Sample statistics were computed out of $642$ matched stride pairs from all study participants for each four sensor locations. ADEPT segmentation results were obtained with stride patterns estimated based on all study participants. } 
# \small 
# \begin{tabular}{@{\extracolsep{5pt}} l|rr} 
# \hline 
# \hline 
# Sensor location  & Stride start & Stride end \\ 
# & mean (sd) & mean (sd) \\ 
# \hline 
# Left wrist & $0.00$ ($0.17$) & $0.01$ ($0.19$) \\ 
# Left hip & $0.00$ ($0.03$) & $0.00$ ($0.03$) \\ 
# Left ankle & $0.01$ ($0.03$) & $0.01$ ($0.03$) \\ 
# Right ankle & $0.01$ ($0.03$) & $0.01$ ($0.03$) \\ 
# \hline 
# \end{tabular} 
# \label{table:identified_strides_duration}
# \end{table} 




