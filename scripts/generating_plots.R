# generating graphs
# 2/14/2017

# LIBRARIES

library(tidyr)
library(ggplot2)

# DATA

meta <- read.csv("data/metacom_full.csv", header = TRUE)

# PLOTS

# plot of metrics and significance

meta_df <- meta[,-c(1,9)]
meta_df <- gather(meta_df, 2:7, key = metric, value = value)

meta_df$metric_type <- meta_df$metric
for (i in 1:length(meta_df$metric)) {
  # add column designating value as coherence, tolerance, or boundary
  if (meta_df$metric[i] %in% c('coherence_z', 'coherence_pvalue')) {
    meta_df$metric_type[i] = 'coherence'
  } else if (meta_df$metric[i] %in% c('turnover_z', 'turnover_pvalue')) {
    meta_df$metric_type[i] = 'turnover'
  } else {
    meta_df$metric_type[i] = 'boundary'
  }
}

meta_df$significance <- meta_df$metric
for (i in 1:length(meta_df$metric)) {
  # add column desginating whether value in metric or significance value
  if (meta_df$metric[i] %in% c('coherence_z', 'turnover_z', 'boundary_index')) {
    meta_df$significance[i] = 'metric'
  } else {
    meta_df$significance[i] = 'significance'
  }
}

ggplot(meta_df, aes(x = period, y = value)) +
  geom_area() +
  geom_smooth(method = "loess") +
  facet_grid(metric_type ~ significance, scales = "free") +
  theme_bw()

# plot metrics, including significance as color

meta_df2 <- spread(meta_df[,-2], key = significance, value = value)

meta_df2$sig_or_NS <- NA
for (i in 1:length(meta_df2$metric_type)){
  if (meta_df2$significance[i] > 0.055 | is.na(meta_df2$significance[i])){
    meta_df2$sig_or_NS[i] = 'NS'
  } else {
    meta_df2$sig_or_NS[i] = 'sig'
  }
}

ggplot(meta_df2, aes(x = period, y = metric)) +
  geom_col(aes(fill = sig_or_NS)) +
  geom_smooth(method = "loess") +
  facet_grid(. ~ metric_type, scales = "free") +
  theme_bw()

# show numbers of metacom categories

ggplot(meta, aes(x = pattern)) +
  geom_bar()
