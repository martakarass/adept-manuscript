
#' @author
#' Marta Karas <marta.karass@gmail.com>
#' 
#' @description 
#' Code to perform analysis of the consistency of ADEPT segmentation across 
#' sensor location. Analysis results are stored at 
#' "data-results/validation-consistency-across-sensors.csv".

rm(list = ls())

library(dplyr)
library(data.table)
library(cobs)
library(ggplot2)

loc_id.levels  <- c("left_wrist", "left_hip","left_ankle", "right_ankle")
loc_id.labels  <- c( "Left wrist","Left hip", "Left ankle", "Right ankle")


## -----------------------------------------------------------------------------

res.df <- as.data.frame(fread(file.path("data-results", "application-example-segmentation.csv")))

## Define participant ID levels based on participant's median cadence 
subj_id.df <- 
  res.df %>%
  filter(loc_id == "left_ankle") %>% 
  mutate(cadence_est = (2 * 100)/T_i) %>% 
  group_by(subj_id) %>%
  filter(tau_i != min(tau_i),
         tau_i != max(tau_i)) %>%
  summarize(cad_median = median(cadence_est)) %>%
  arrange(desc(cad_median)) 
subj_id.levels <- subj_id.df %>% pull(subj_id)
subj_id.labels <- paste0(1:32)


subj_id.tmp <- subj_id.levels[1]

## Data frame with cadence estimates (y) 
value.df <- 
  res.df %>%
  group_by(loc_id, subj_id) %>%
  ## Filter out 1st and last stride per participant, per sensor location
  filter(tau_i != min(tau_i),
         tau_i != max(tau_i)) %>%
  mutate(tau_1 = tau_i,
         tau_2 = tau_i + T_i - 1,
         x = (tau_1 + tau_2)/2,
         y = (2 * 100)/T_i) %>%    ## cadence estimate
  as.data.frame()


## Fitt values for each observation in segmentation results 
out.df <- data.frame()

for (subj_id.tmp in subj_id.levels){
  message(paste0("subj_id: ", subj_id.tmp))
  
  ## Subject-specific data with segmentation results
  value.df_subj <- value.df %>% filter(subj_id == subj_id.tmp)
  ## Subject-specific fit of constrained quantile curves
  cobs.fit_subj <- cobs(
    x = value.df_subj$x, 
    y = value.df_subj$y, 
    constraint = "none",
    nknots = 50,
    method = "quantile")
  
  for (loc_id.tmp in loc_id.levels){
    ## Fitted values from constrained quantile curves
    value.df_subj_loc <- value.df_subj %>% filter(loc_id == loc_id.tmp)
    value.df_subj_loc$pred <- predict(cobs.fit_subj, value.df_subj_loc$x)[, 2]
    out.df <- rbind(out.df, value.df_subj_loc)
  }
}


## Sanity check: 
## Plot segmentation results (cadence) aganist fitted curves
for (subj_id.tmp in subj_id.levels){
  message(paste0("subj_id: ", subj_id.tmp))
  ## Subject-specific data with results
  out.df_subj <- out.df %>% filter(subj_id == subj_id.tmp)
  plt <- 
    ggplot(out.df_subj, aes(x = x, y = y, color = loc_id)) + 
    geom_point(alpha = 0.3) + 
    geom_line(data = out.df_subj, aes(x = x, y = pred, group = 1), 
              color = "black",
              inherit.aes = FALSE) + 
    scale_y_continuous(limits = c(1, 2.5)) + 
    labs(title = subj_id.tmp) + 
    theme_bw(base_size = 10)
  plot(plt)
}


## -----------------------------------------------------------------------------
## Compute iPAD / estimator  of  the  coefficient  of  variation for 
## each participant, for each sensor location

out.df.agg <-
  out.df %>%
  mutate(ae = abs(y - pred)/pred) %>%
  group_by(subj_id, loc_id) %>%
  summarize(mae = mean(ae) * 100) %>%
  mutate(subj_id_label = subj_id,
         subj_id_label = factor(
           subj_id_label, 
           levels = subj_id.levels,
           labels = subj_id.labels)) %>%
  as.data.frame()

## Save results to file
out.df.path <- file.path("data-results", "validation-consistency-across-sensors.csv")
write.table(out.df.agg,
            out.df.path,
            quote = FALSE, sep = ",", row.names = FALSE)

