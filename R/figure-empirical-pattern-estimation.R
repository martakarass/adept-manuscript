
#' @author
#' Marta Karas <marta.karass@gmail.com>
#' 
#' @description 
#' Code to generate manusript figures: 
#' - Figure 5a: 200 manually segmented strides from right ankle. 
#' - Figure 5b: 200 manually segmented strides from right ankle after linear 
#'   interpolation and normalization to have meanzero and variance one.
#' - Figure 6a: 642 normalized strides clustered into two groups 
#'   based on their correlation similarity. 
#' - Figure 6b: 642 normalized strides clustered into three groups 
#'   based on their correlation similarity. 

rm(list = ls())

library(data.table)
library(ggplot2)
library(dplyr)
library(cluster)

source(file.path("R", "util.R"))
ggsave.device <- "png"


## Figure 5a:
## 200 manually segmented strides from right ankle. 
plt.name <- "empirical_pattern_FINAL-1.png"

strides.df.path <- file.path("data", "strides-df-anonymized.csv")
strides.df <- as.data.frame(fread(strides.df.path))
ra.strides.df <- 
  strides.df %>% 
  mutate(stride_id = paste0(subj_id, "_", subj_stride_id)) %>%
  select(subj_id, stride_ph, stride_id, V = vm_right_ankle) %>%
  group_by(stride_id) %>%
  arrange(stride_id, stride_ph) %>%
  mutate(rn = row_number())

set.seed(1)
stride_id.sub <- sample(sort(unique(ra.strides.df$stride_id)), 200)
ra.strides.df.sub <- ra.strides.df %>% filter(stride_id %in% stride_id.sub)
plt.df <- ra.strides.df.sub %>% mutate(loc_id = "Right ankle")

plt <- 
  ggplot(plt.df,
       aes(x = (rn-1)/100, y = V, group = stride_id)) + 
  geom_line(size = 0.5, alpha = 0.1) + 
  facet_grid(. ~ loc_id) +
  theme_Publication() + 
  labs(x = "Time [s]", y = "Vector magnitude [g]")
# plot(plt)
ggsave(filename = file.path("figures", plt.name), plot = plt, device = ggsave.device, 
       width = 5, height = 4, units = "in")  


## -----------------------------------------------------------------------------

## Figure 5b:
## 200 manually segmented strides from right ankle after linear interpolation 
## and normalization to have meanzero and variance one.
plt.name <- "empirical_pattern_FINAL-2.png"

V.intrpl.l <- lapply(stride_id.sub, function(stride_id.tmp){
  V.vec <- ra.strides.df.sub %>%
    filter(stride_id == stride_id.tmp) %>%
    arrange(stride_ph) %>%
    pull(V)
  intrpl.scale.vec(V.vec)
})
plt.df <- data.frame(
  V_intrpl = unlist(V.intrpl.l),
  stride_ph = as.vector(replicate(length(V.intrpl.l), seq(0, 1, length.out = 200))),
  stride_id = unlist(lapply(stride_id.sub, function(i) rep(i, 200)))
) 
plt.df <- plt.df %>% mutate(loc_id = "Right ankle")
plt.df.agg <- plt.df %>% 
  group_by(stride_ph) %>% 
  summarize(V_intrpl_mean = mean(V_intrpl))

plt <- 
  ggplot(plt.df,
         aes(x = stride_ph, y = V_intrpl, group = stride_id)) + 
  geom_line(size = 0.5, alpha = 0.1) + 
  geom_line(data = plt.df.agg, aes(x = stride_ph, y = V_intrpl_mean, group = 1), 
            color = "red", size = 1) +
  facet_grid(. ~ loc_id) +
  theme_Publication() + 
  labs(x = "Stride phase", y = "Vector magnitude [g]") 
# plot(plt)
ggsave(filename = file.path("figures", plt.name), plot = plt, device = ggsave.device, 
       width = 5, height = 4, units = "in")  


## -----------------------------------------------------------------------------

## Figure 6a:
## 642 normalized strides clustered into two groups based on their correlation
## similarity. 
plt.name <- "empirical_pattern_multiple2-1.png"

k.tmp <- 2
tmpl.out <- estimate.stride.tmpl(
  ra.strides.df$V, ra.strides.df$stride_id, ra.strides.df$stride_ph, k.tmp)
str(tmpl.out)
plt.df <- data.frame(
  V_intrpl = as.vector(t(tmpl.out$x_mat)),
  stride_id = unlist(lapply(paste0("id", 1:nrow(tmpl.out$x_mat)), function(i) rep(i, 200))),
  cluster_idx = unlist(lapply(tmpl.out$cluster_idx, function(i) rep(i, 200))),
  stride_ph = as.vector(replicate(nrow(tmpl.out$x_mat), seq(0, 1, length.out = 200))),
  loc_id = "Right ankle")
plt.df.agg <- plt.df %>%
  group_by(stride_ph, cluster_idx) %>%
  summarise(V_intrpl_mean = mean(V_intrpl)) 
plt.df.agg.max <- 
  plt.df.agg %>%
  filter(stride_ph > 0.25, stride_ph < 0.75) %>%
  group_by(cluster_idx) %>%
  filter(V_intrpl_mean == max(V_intrpl_mean))

plt <- 
  ggplot(plt.df, aes(x = stride_ph, y = V_intrpl, group = stride_id)) + 
  geom_line(size = 0.5, alpha = 0.05) + 
  geom_line(data = plt.df.agg, aes(x = stride_ph, y = V_intrpl_mean, group = 1), 
            color = "red", size = 1) +
  geom_vline(data = plt.df.agg.max, aes(xintercept = stride_ph),
             size = 0.8, linetype = 1, color = "blue", alpha = 0.3) +
  facet_grid(cluster_idx ~ loc_id) + 
  theme_Publication() + 
  labs(x = "Stride phase", y = "Vector magnitude [g]")
# plot(plt)
ggsave(filename = file.path("figures", plt.name), plot = plt, device = ggsave.device, 
       width = 5, height = 4.8, units = "in")  


## -----------------------------------------------------------------------------

## Figure 6b:
## 642 normalized strides clustered into three groups based on their correlation
## similarity. 
plt.name <- "empirical_pattern_multiple3-1.png"

k.tmp <- 3
tmpl.out <- estimate.stride.tmpl(
  ra.strides.df$V, ra.strides.df$stride_id, ra.strides.df$stride_ph, k.tmp)
str(tmpl.out)
plt.df <- data.frame(
  V_intrpl = as.vector(t(tmpl.out$x_mat)),
  stride_id = unlist(lapply(paste0("id", 1:nrow(tmpl.out$x_mat)), function(i) rep(i, 200))),
  cluster_idx = unlist(lapply(tmpl.out$cluster_idx, function(i) rep(i, 200))),
  stride_ph = as.vector(replicate(nrow(tmpl.out$x_mat), seq(0, 1, length.out = 200))),
  loc_id = "Right ankle")
plt.df$cluster_idx <- factor(plt.df$cluster_idx, levels = c(2,1,3), labels = c(1,2,3))
plt.df.agg <- plt.df %>%
  group_by(stride_ph, cluster_idx) %>%
  summarise(V_intrpl_mean = mean(V_intrpl)) 
plt.df.agg.max <- 
  plt.df.agg %>%
  filter(stride_ph > 0.25, stride_ph < 0.75) %>%
  group_by(cluster_idx) %>%
  filter(V_intrpl_mean == max(V_intrpl_mean))

plt <- 
  ggplot(plt.df, aes(x = stride_ph, y = V_intrpl, group = stride_id)) + 
  geom_line(size = 0.5, alpha = 0.05) + 
  geom_line(data = plt.df.agg, aes(x = stride_ph, y = V_intrpl_mean, group = 1), 
            color = "red", size = 1) +
  geom_vline(data = plt.df.agg.max, aes(xintercept = stride_ph),
             size = 0.8, linetype = 1, color = "blue", alpha = 0.3) +
  facet_grid(cluster_idx ~ loc_id) + 
  theme_Publication() + 
  labs(x = "Stride phase", y = "Vector magnitude [g]")
# plot(plt)
ggsave(filename = file.path("figures", plt.name), plot = plt, device = ggsave.device, 
       width = 5, height = 6, units = "in")  










