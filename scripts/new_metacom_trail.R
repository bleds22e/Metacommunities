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

meta_df <- data.frame(period = numeric(435), coherence = numeric(435), 
                      turnover = numeric(435), boundary_clump = numeric(435))

Full <- split(portal_full, f = portal_full$period)

LongTerm <- split(portal_long, f = portal_long$period)


# for loop ignoring errors
for (i in 37:435){
  
  data <- portal_full %>% filter(period == i)
  meta_df$period[i] <- data[1,1]
  matrix <- data[,-1]
  matrix <- dcast(matrix, formula = plot ~ species, fun.aggregate = length, value.var = "species")
  matrix <- tibble::column_to_rownames(as.data.frame(matrix), "plot")
  matrix <- as.matrix(as.data.frame(lapply(matrix, as.numeric)))
  
  meta_df$coherence[i] <- ifelse(has_error(Coherence(matrix)), NA, Coherence(matrix)$z)
  meta_df$turnover[i] <- ifelse(has_error(Turnover(matrix)), NA, Turnover(matrix)$z)
  meta_df$boundary_clump[i] <- ifelse(has_error(BoundaryClump(matrix)), NA, BoundaryClump(matrix)$index)
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





#############################################
# WORKING AREA

test <- portal_full %>% filter(period == 19)
test_df <- data.frame(period = character(435), coherence = numeric(435), 
                      turnover = numeric(435), boundary_clump = numeric(435))

test_df$period <- as.numeric(test[1,1])

test <- Full[[19]]
meta_df$period[19] <- data$period[1]

matrix <- test[,-1]
matrix <- dcast(matrix, formula = plot ~ species, fun.aggregate = length, value.var = "species")
matrix <- tibble::column_to_rownames(as.data.frame(matrix), "plot")
matrix <- as.matrix(as.data.frame(lapply(matrix, as.numeric)))

c <- Coherence(matrix)
test_df$coherence <- c$z
t <- Turnover(matrix)
test_df$turnover <- t$z
b <- BoundaryClump(matrix)
test_df$boundary_clump <- b$index

co <- ifelse(has_error(Coherence(matrix)), 0, Coherence(matrix)$z)
