# Practicing with metacom Package - by period
# EKB
# February 2017

#########################
# LIBRARIES

library(dplyr)
library(reshape2)
library(tibble)
library(metacom)
library(ggplot2)
library(testit)

########################
# DATA

# remove periods where one day of trapping is missing
trap_hist <- read.csv("data/Portal_rodent_trapping.csv", header = TRUE)
trap_hist <- aggregate(trap_hist$Sampled, by=list(trap_hist$Period), FUN=sum)
all_trap <- trap_hist %>% filter(x >= 20)
periods <- unique(all_trap$Group.1)

# select Portal data to use
portal <- read.csv("data/Portal_rodent.csv", header = TRUE, na.strings = c(""))
portal <- subset(portal, period %in% periods) # could also use match()
portal <- portal[!(is.na(portal$plot)) & !(is.na(portal$period)) & !(is.na(portal$species)),]

portal_full <- select(portal, period, plot, species) %>% 
  filter(period > 0, 
         species == 'NA' | species == 'DM' | species == 'PF' | species == 'PE' |
           species == 'DS' | species == 'PP' | species == 'SH' | species =='OT' | species == 'DO' |
           species == 'OL' | species == 'RM' | species == 'PM' | species == 'RF' | species == 'PH'|
           species == 'BA' | species == 'SF' | species == 'RO' | species == 'SO' | species == 'PI' |
           species == 'PB' | species == 'PL') %>% 
  group_by(period, plot)

portal_long <- select(portal, period, plot, species) %>% 
  filter(period > 0, 
         species == 'NA' | species == 'DM' | species == 'PF' | species == 'PE' |
           species == 'DS' | species == 'PP' | species == 'SH' | species =='OT' | species == 'DO' |
           species == 'OL' | species == 'RM' | species == 'PM' | species == 'RF' | species == 'PH'|
           species == 'BA' | species == 'SF' | species == 'RO' | species == 'SO' | species == 'PI' |
           species == 'PB' | species == 'PL', 
         plot == 4 | plot == 11 | plot == 19 | plot == 20 | plot == 21 | plot == 3 | plot == 14 |
           plot == 15 | plot == 17) %>% 
  group_by(period, plot)

##########################

periods <- unique(portal_full$period)

meta_df <- data.frame(period = numeric(435), coherence_z = numeric(435), coherence_pvalue = numeric(435),
                      turnover_z = numeric(435), turnover_pvalue = numeric(435), boundary_index = numeric(435),
                      boundary_P = numeric(435))

# for loop ignoring errors
for (i in 387:length(periods)){
  per <- periods[i]
  meta_df$period[i] <- per
  
  data <- portal_full %>% filter(period == per)
  matrix <- data[,-1]
  matrix <- dcast(matrix, formula = plot ~ species, fun.aggregate = length, value.var = "species")
  matrix <- tibble::column_to_rownames(as.data.frame(matrix), "plot")
  matrix <- as.matrix(as.data.frame(lapply(matrix, as.numeric)))
  
  if (has_error(Coherence(matrix))){
    meta_df$coherence_z[i] <- NA
    meta_df$coherence_pvalue[i] <- NA
  } else {
    coherence = Coherence(matrix)
    meta_df$coherence_z[i] <- coherence$z
    meta_df$coherence_pvalue[i] <- coherence$pval
  }
  
  if (has_error(Turnover(matrix))){
    meta_df$turnover_z[i] <- NA
    meta_df$turnover_pvalue[i] <- NA
  } else {
    turnover = Turnover(matrix)
    meta_df$turnover_z[i] <- turnover$z
    meta_df$turnover_pvalue[i] <- turnover$pval
  }
  
  if (has_error(BoundaryClump(matrix))){
    meta_df$boundary_z[i] <- NA
    meta_df$boundary_pvalue[i] <- NA
  } else {
    boundary = BoundaryClump(matrix)
    meta_df$boundary_index[i] <- boundary$index
    meta_df$boundary_P[i] <- boundary$P
  }
}

meta_df$pattern <- NA
for (i in 1:length(meta_df$period)){
  meta_df$pattern[i] <- ifelse(-is.na(meta_df$coherence_pvalue[i]) | meta_df$coherence_pvalue[i] > 0.055, "random", 
                               ifelse(meta_df$coherence_z[i] < 0, "checkerboard",
                               ifelse(-is.na(meta_df$turnover_pvalue[i]) | meta_df$turnover_pvalue[i] > 0.055, "unknown",
                               ifelse(meta_df$turnover_z[i] < 0 | is.na(meta_df$turnover_z[i]), "nested_subsets",
                               ifelse(meta_df$boundary_P[i] > 0.055, "Gleasonian",
                               ifelse(meta_df$boundary_index[i] > 0, "evenly_spaced_gradients", "Clementsian"))))))
}

write.csv(meta_df, "data/metacom_full.csv")


#############################################
# WORKING AREA

# testing
i = 15
per <- periods[i]

data <- portal_long %>% filter(period == per)
matrix <- data[,-1]
matrix <- dcast(matrix, formula = plot ~ species, fun.aggregate = length, value.var = "species")
matrix <- tibble::column_to_rownames(as.data.frame(matrix), "plot")
matrix <- as.matrix(as.data.frame(lapply(matrix, as.numeric)))

if (has_error(Coherence(matrix))){
  meta_df$coherence_z[i] <- NA
  meta_df$coherence_pvalue[i] <- NA
} else {
  coherence = Coherence(matrix)
  meta_df$coherence_z[i] <- coherence$z
  meta_df$coherence_pvalue[i] <- coherence$pval
}

meta_df$turnover[i] <- ifelse(has_error(Turnover(matrix)), NA, Turnover(matrix)$z)
meta_df$boundary_clump[i] <- ifelse(has_error(BoundaryClump(matrix)), NA, BoundaryClump(matrix)$index)

