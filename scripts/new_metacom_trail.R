# Practicing with metacom Package
# EKB
# October 2016

#########################
# LIBRARIES

library(dplyr)
library(reshape2)
library(tibble)
library(metacom)
library(ggplot2)

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

meta_df <- data.frame(period = character(435), coherence = numeric(435), 
                        turnover = numeric(435), boundary_clump = numeric(435))

# turnover
for (i in 1:435){
  data <- portal_full %>% filter(period == i)
  meta_df$period[i] <- as.numeric(data[1,1])
  
  matrix <- data[,-1]
  matrix <- dcast(matrix, formula = plot ~ species)
  matrix <- tibble::column_to_rownames(as.data.frame(matrix), "plot")
  
  t <- Turnover(comm = matrix)
  meta_df$turnover[i] <- t$z
}

Coherence <- rep(0,435)
# coherence
for (i in 1:435){
  data <- portal_full %>% filter(period == i)
  meta_df$period[i] <- as.numeric(data[1,1])
  
  matrix <- data[,-1]
  matrix <- dcast(matrix, formula = plot ~ species)
  matrix <- tibble::column_to_rownames(as.data.frame(matrix), "plot")
  
  t <- Turnover(comm = matrix)
  meta_df$turnover[i] <- t$z
}

# metacom function
for (i in 1:435){
  data <- portal_full %>% filter(period == i)
  meta_df$period[i] <- as.numeric(data[1,1])
  
  matrix <- data[,-1]
  matrix <- dcast(matrix, formula = plot ~ species)
  matrix <- tibble::column_to_rownames(as.data.frame(matrix), "plot")
  matrix <- as.matrix(matrix)
  
  m <- Metacommunity(comm = matrix)
  meta_df$turnover[i] <- as.numeric(paste(m$Turnover[2]))
  meta_df$coherence[i] <- as.numeric(paste(m$Coherence[2]))
  meta_df$boundary_clump[i] <- m$Boundary[2]
}

##########
test <- portal_full %>% filter(period == 444)

