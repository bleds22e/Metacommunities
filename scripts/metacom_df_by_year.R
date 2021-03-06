# Metacommunity Dataframe by Year
# EKB
# February 2017

#########################
# LIBRARIES

library(dplyr)
library(reshape2)
library(tibble)
library(metacom)

########################
# DATA

# select Portal data to use
portal <- read.csv("data/Portal_rodent.csv", header = TRUE, na.strings = c(""))
portal <- portal[!(is.na(portal$plot)) &
                 !(is.na(portal$yr)) &
                 !(is.na(portal$species)) & 
                 !(is.na(portal$period)), ]

portal_full <- select(portal, year = yr, period, plot, species) %>% 
  filter(period > 0, 
         species %in% c('NA', 'DM', 'PF', 'PE', 'DS', 'PP', 'SH', 'OT', 'DO', 
                        'OL', 'RM', 'PM', 'RF', 'PH', 'BA', 'SF', 'RO', 'SO', 
                        'PI', 'PB', 'PL')) %>% 
  group_by(year, plot) # removals?

portal_long <- select(portal, year = yr, period, plot, species) %>% 
  filter(period > 0, 
         species  %in% c('NA', 'DM', 'PF', 'PE', 'DS', 'PP', 'SH', 'OT', 'DO', 
                         'OL', 'RM', 'PM', 'RF', 'PH', 'BA', 'SF', 'RO', 'SO', 
                         'PI', 'PB', 'PL'),
         plot %in% c(4, 11, 19, 20, 21, 3, 14, 15, 17)) %>% # add LT removals?
  group_by(year, plot)

##########################
# Dataframes

# full (all plots)

meta_df_full <-
  data.frame(
    year = numeric(40),
    coherence_z = numeric(40),
    coherence_pvalue = numeric(40),
    turnover_z = numeric(40),
    turnover_pvalue = numeric(40),
    boundary_index = numeric(40),
    boundary_P = numeric(40),
    pattern = NA
  )

years <- unique(portal_full$year)

for (i in 1:length(years)){
  
  meta_df_full$year[i] <- years[i]
  
  data <- select(portal_full, -period) %>% filter(year == year[i])
  matrix <- data[,-1]
  matrix <- dcast(matrix, formula = plot ~ species, fun.aggregate = length, value.var = "species")
  matrix <- tibble::column_to_rownames(as.data.frame(matrix), "plot")
  matrix <- as.matrix(as.data.frame(lapply(matrix, as.numeric)))
  
  metacom <- Metacommunity(matrix)
  meta_df_full$coherence_z[i] <- as.numeric(paste(metacom$Coherence[2]))
  meta_df_full$coherence_pvalue[i] <- as.numeric(paste(metacom$Coherence[3]))
  meta_df_full$turnover_z[i] <- as.numeric(paste(metacom$Turnover[2]))
  meta_df_full$turnover_pvalue[i] <- as.numeric(paste(metacom$Turnover[3]))
  meta_df_full$boundary_index[i] <- metacom$Boundary$index
  meta_df_full$boundary_P[i] <- metacom$Boundary$P
  meta_df_full$pattern[i] <- IdentifyStructure(metacom)
  
}

write.csv(meta_df_full, "data/metacom_year_full_function.csv")

# long-term plots only

meta_df_long <-
  data.frame(
    year = numeric(40),
    coherence_z = numeric(40),
    coherence_pvalue = numeric(40),
    turnover_z = numeric(40),
    turnover_pvalue = numeric(40),
    boundary_index = numeric(40),
    boundary_P = numeric(40),
    pattern = NA
  )

years <- unique(portal_long$year)

for (i in 1:length(years)){
  
  meta_df_long$year[i] <- years[i]
  
  data <- select(portal_long, -period) %>% filter(year == year[i])
  matrix <- data[,-1]
  matrix <- dcast(matrix, formula = plot ~ species, fun.aggregate = length, value.var = "species")
  matrix <- tibble::column_to_rownames(as.data.frame(matrix), "plot")
  matrix <- as.matrix(as.data.frame(lapply(matrix, as.numeric)))
  
  metacom <- Metacommunity(matrix)
  meta_df_long$coherence_z[i] <- as.numeric(paste(metacom$Coherence[2]))
  meta_df_long$coherence_pvalue[i] <- as.numeric(paste(metacom$Coherence[3]))
  meta_df_long$turnover_z[i] <- as.numeric(paste(metacom$Turnover[2]))
  meta_df_long$turnover_pvalue[i] <- as.numeric(paste(metacom$Turnover[3]))
  meta_df_long$boundary_index[i] <- metacom$Boundary$index
  meta_df_long$boundary_P[i] <- metacom$Boundary$P
  meta_df_long$pattern[i] <- IdentifyStructure(metacom)
  
}

write.csv(meta_df_long, "data/metacom_year_long_function.csv")
