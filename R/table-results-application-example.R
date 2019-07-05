
#' @author
#' Marta Karas <marta.karass@gmail.com>
#' 
#' @description 
#' Code to generate manuscript tables: 
#' - Table 1: Summary of a number of segmented strides per person out of 32 study 
#'   participants, grouped by sensor location.
#' - Table 2: Summary of stride duration time (in seconds) out of all segmented 
#'   strides from 32 studyparticipants, grouped by sensor location.


rm(list = ls())

library(dplyr)
library(data.table)
library(stargazer)

loc_id.levels  <- c("left_wrist", "left_hip","left_ankle", "right_ankle")
loc_id.labels  <- c( "Left wrist","Left hip", "Left ankle", "Right ankle")


## -----------------------------------------------------------------------------

## Table 1: 
## Summary of a number of segmented strides per person out of 32 study 
## participants, grouped by sensor location.

res.df <- as.data.frame(fread(file.path("data-results", "application-example-segmentation.csv")))

tbl.df <- 
  res.df %>%
  group_by(subj_id, loc_id) %>%
  summarize(cnt = n()) %>%
  group_by(loc_id) %>%
  summarise(
    cnt_min = min(cnt),
    `25%` = quantile(cnt, probs = 0.25),
    `50%` = median(cnt),
     cnt_mean = mean(cnt),
    `75%` = max(cnt, probs = 0.75),
    cnt_max = max(cnt)) %>%
  mutate(loc_id = factor(loc_id, levels = loc_id.levels, labels = loc_id.labels)) %>%
  arrange(loc_id) %>%
  as.data.frame()

tbl.df[, c(3,4,5,6)] <- round(tbl.df[, c(3,4,5,6)], 1)
tbl.df

stargazer(tbl.df, summary = FALSE, rownames = FALSE, digits = 1, font.size = "small")


## -----------------------------------------------------------------------------

## Table 2: 
## Summary of stride duration time (in seconds) out of all segmented 
## strides from 32 studyparticipants, grouped by sensor location.

tbl.df <- 
  res.df %>%
  mutate(T_i_sek = T_i / 100) %>%
  group_by(loc_id) %>%
  summarise(
    T_i_sek_min = min(T_i_sek),
    `25%` = quantile(T_i_sek, probs = 0.25),
    `50%` = median(T_i_sek),
    T_i_sek_mean = mean(T_i_sek),
    `75%` = max(T_i_sek, probs = 0.75),
    cnt_max = max(T_i_sek)) %>%
  mutate(loc_id = factor(loc_id, levels = loc_id.levels, labels = loc_id.labels)) %>%
  arrange(loc_id) %>%
  as.data.frame()

tbl.df[, c(3,4,5,6)] <- round(tbl.df[, c(3,4,5,6)], 3)
tbl.df

stargazer(tbl.df, summary = FALSE, rownames = FALSE, digits = 3, font.size = "small")

