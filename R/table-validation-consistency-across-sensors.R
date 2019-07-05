
#' @author
#' Marta Karas <marta.karass@gmail.com>
#' 
#' @description 
#' Code to generate manuscript tables: 
#' - Table 3: Coefficient of variation (CV), expressed in percentage, of strides 
#'   estimation for each study participant across four sensor locations, together 
#'   with average CV value.


rm(list = ls())

library(dplyr)
library(data.table)
library(stargazer)

loc_id.levels  <- c("left_wrist", "left_hip","left_ankle", "right_ankle")
loc_id.labels  <- c( "Left wrist","Left hip", "Left ankle", "Right ankle")


## -----------------------------------------------------------------------------

## Table 3: 
## Coefficient of variation (CV), expressed in percentage, of strides 
## estimation for each study participant across four sensor locations, together 
## with average CV value.

res.df <- as.data.frame(fread(file.path("data-results", "validation-consistency-across-sensors.csv")))

tbl.df <- 
  res.df %>%
  mutate(loc_id = factor(loc_id, levels = loc_id.levels, labels = loc_id.labels)) %>%
  dplyr::select(-subj_id) %>%
  dcast(subj_id_label ~ loc_id, value.var = "mae")
  
stargazer(tbl.df, summary = FALSE, rownames = FALSE, digits = 2, font.size = "small")

## Participant 27
tbl.df %>% 
  filter(subj_id_label == "27") %>%
  stargazer( summary = FALSE, rownames = FALSE, digits = 2, font.size = "small")

## Average CV across all participants
tbl.df %>% 
  summarize_all(mean) %>%
  stargazer( summary = FALSE, rownames = FALSE, digits = 2, font.size = "small")



