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

meta_df <- data.frame(period = numeric(435), coherence_z = numeric(435), coherence_pvalue = numeric(435),
                      turnover = numeric(435), boundary_clump = numeric(435))

# for loop ignoring errors
for (i in 382:length(periods)){
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
  
  meta_df$turnover[i] <- ifelse(has_error(Turnover(matrix)), NA, Turnover(matrix)$z)
  meta_df$boundary_clump[i] <- ifelse(has_error(BoundaryClump(matrix)), NA, BoundaryClump(matrix)$index)
  
}

write.csv(meta_df, "data/metacom_full.csv")


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


##############
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
