# Portal Metacommunities by Season
# EKB
# February 2017

#########################
# LIBRARIES

library(dplyr)
library(reshape2)
library(tibble)
library(metacom)
source("scripts/season_function.R")

########################
# DATA

### Select Portal Data ###

portal <- read.csv("data/Portal_rodent.csv", header = TRUE, na.strings = c(""))
portal <- portal[!(is.na(portal$plot)) &
                   !(is.na(portal$yr)) &
                   !(is.na(portal$species)) & 
                   !(is.na(portal$period)), ]

# all sites
portal_full <- select(portal, year = yr, month = mo, period, plot, species) %>% 
  filter(period > 0, 
         species %in% c('NA', 'DM', 'PF', 'PE', 'DS', 'PP', 'SH', 'OT', 'DO', 
                        'OL', 'RM', 'PM', 'RF', 'PH', 'BA', 'SF', 'RO', 'SO', 
                        'PI', 'PB', 'PL'))

# long-term sites only
portal_long <- select(portal, year = yr, month = mo, period, plot, species) %>% 
  filter(period > 0, 
         species  %in% c('NA', 'DM', 'PF', 'PE', 'DS', 'PP', 'SH', 'OT', 'DO', 
                         'OL', 'RM', 'PM', 'RF', 'PH', 'BA', 'SF', 'RO', 'SO', 
                         'PI', 'PB', 'PL'),
         plot %in% c(3, 4, 10, 11, 14, 15, 16, 17, 19, 20, 21, 23)) 

### Add Season ###

# four seasons
full_4 <- add_seasons(portal_full, 4) %>%
  group_by(species, plot, season4_yr, season4) %>% 
  tidyr::unite(season_yr, season4_yr, season4, sep = "_")
long_4 <- add_seasons(portal_long, 4) %>%
  group_by(species, plot, season4_yr, season4) %>% 
  tidyr::unite(season_yr, season4_yr, season4, sep = "_")

# two seasons
full_2 <- add_seasons(portal_full, 2) %>%
  group_by(species, plot, season2_yr, season2) %>% 
  tidyr::unite(season_yr, season2_yr, season2, sep = "_")
long_2 <- add_seasons(portal_long, 2) %>%
  group_by(species, plot, season2_yr, season2) %>% 
  tidyr::unite(season_yr, season2_yr, season2, sep = "_")

##########################
# DATAFRAMES

### Four Seasons

# full (all plots)
timestep <- unique(full_4$season_yr)

meta_full_s4 <-
  data.frame(
    season_yr = NA,
    coherence_z = numeric(length(timestep)),
    coherence_pvalue = numeric(length(timestep)),
    turnover_z = numeric(length(timestep)),
    turnover_pvalue = numeric(length(timestep)),
    boundary_index = numeric(length(timestep)),
    boundary_P = numeric(length(timestep)),
    pattern = NA
  )

for (i in 1:length(timestep)){
  
  meta_full_s4$season_yr[i] <- timestep[i]
  
  data <- filter(full_4, season_yr == timestep[i])
  matrix <- data[,-c(1:3,6)]
  matrix <- dcast(matrix, formula = plot ~ species, fun.aggregate = length, value.var = "species")
  matrix <- tibble::column_to_rownames(as.data.frame(matrix), "plot")
  matrix <- as.matrix(as.data.frame(lapply(matrix, as.numeric)))
  
  metacom <- Metacommunity(matrix)
  meta_full_s4$coherence_z[i] <- as.numeric(paste(metacom$Coherence[2]))
  meta_full_s4$coherence_pvalue[i] <- as.numeric(paste(metacom$Coherence[3]))
  meta_full_s4$turnover_z[i] <- as.numeric(paste(metacom$Turnover[2]))
  meta_full_s4$turnover_pvalue[i] <- as.numeric(paste(metacom$Turnover[3]))
  meta_full_s4$boundary_index[i] <- metacom$Boundary$index
  meta_full_s4$boundary_P[i] <- metacom$Boundary$P
  meta_full_s4$pattern[i] <- IdentifyStructure(metacom)
  
}

write.csv(meta_full_s4, "data/metacom_season4_full.csv")

# long-term plots only
timestep <- unique(long_4$season_yr)

meta_long_s4 <-
  data.frame(
    season_yr = NA,
    coherence_z = numeric(length(timestep)),
    coherence_pvalue = numeric(length(timestep)),
    turnover_z = numeric(length(timestep)),
    turnover_pvalue = numeric(length(timestep)),
    boundary_index = numeric(length(timestep)),
    boundary_P = numeric(length(timestep)),
    pattern = NA
  )

for (i in 148:length(timestep)){
  
  meta_long_s4$season_yr[i] <- timestep[i]
  
  data <- filter(long_4, season_yr == timestep[i]) 
  matrix <- data[,-c(1:3,6)]
  matrix <- dcast(matrix, formula = plot ~ species, fun.aggregate = length, value.var = "species")
  matrix <- tibble::column_to_rownames(as.data.frame(matrix), "plot")
  matrix <- as.matrix(as.data.frame(lapply(matrix, as.numeric)))
  
  metacom <- Metacommunity(matrix)
  meta_long_s4$coherence_z[i] <- as.numeric(paste(metacom$Coherence[2]))
  meta_long_s4$coherence_pvalue[i] <- as.numeric(paste(metacom$Coherence[3]))
  meta_long_s4$turnover_z[i] <- as.numeric(paste(metacom$Turnover[2]))
  meta_long_s4$turnover_pvalue[i] <- as.numeric(paste(metacom$Turnover[3]))
  meta_long_s4$boundary_index[i] <- metacom$Boundary$index
  meta_long_s4$boundary_P[i] <- metacom$Boundary$P
  meta_long_s4$pattern[i] <- IdentifyStructure(metacom)
  
}

write.csv(meta_long_s4, "data/metacom_season4_long.csv")

### Two Seasons

# full (all plots)
timestep <- unique(full_2$season_yr)

meta_full_s2 <-
  data.frame(
    season_yr = NA,
    coherence_z = numeric(length(timestep)),
    coherence_pvalue = numeric(length(timestep)),
    turnover_z = numeric(length(timestep)),
    turnover_pvalue = numeric(length(timestep)),
    boundary_index = numeric(length(timestep)),
    boundary_P = numeric(length(timestep)),
    pattern = NA
  )

for (i in 1:length(timestep)){
  
  meta_full_s2$season_yr[i] <- timestep[i]
  
  data <- filter(full_2, season_yr == timestep[i])
  matrix <- data[,-c(1:3,6)]
  matrix <- dcast(matrix, formula = plot ~ species, fun.aggregate = length, value.var = "species")
  matrix <- tibble::column_to_rownames(as.data.frame(matrix), "plot")
  matrix <- as.matrix(as.data.frame(lapply(matrix, as.numeric)))
  
  metacom <- Metacommunity(matrix)
  meta_full_s2$coherence_z[i] <- as.numeric(paste(metacom$Coherence[2]))
  meta_full_s2$coherence_pvalue[i] <- as.numeric(paste(metacom$Coherence[3]))
  meta_full_s2$turnover_z[i] <- as.numeric(paste(metacom$Turnover[2]))
  meta_full_s2$turnover_pvalue[i] <- as.numeric(paste(metacom$Turnover[3]))
  meta_full_s2$boundary_index[i] <- metacom$Boundary$index
  meta_full_s2$boundary_P[i] <- metacom$Boundary$P
  meta_full_s2$pattern[i] <- IdentifyStructure(metacom)
  
}

write.csv(meta_full_s2, "data/metacom_season2_full.csv")

# long-term plots only
timestep <- unique(long_2$season_yr)

meta_long_s2 <-
  data.frame(
    season_yr = NA,
    coherence_z = numeric(length(timestep)),
    coherence_pvalue = numeric(length(timestep)),
    turnover_z = numeric(length(timestep)),
    turnover_pvalue = numeric(length(timestep)),
    boundary_index = numeric(length(timestep)),
    boundary_P = numeric(length(timestep)),
    pattern = NA
  )

for (i in 70:length(timestep)){
  
  meta_long_s2$season_yr[i] <- timestep[i]
  
  data <- filter(long_2, season_yr == timestep[i]) 
  matrix <- data[,-c(1:3,6)]
  matrix <- dcast(matrix, formula = plot ~ species, fun.aggregate = length, value.var = "species")
  matrix <- tibble::column_to_rownames(as.data.frame(matrix), "plot")
  matrix <- as.matrix(as.data.frame(lapply(matrix, as.numeric)))
  
  metacom <- Metacommunity(matrix)
  meta_long_s2$coherence_z[i] <- as.numeric(paste(metacom$Coherence[2]))
  meta_long_s2$coherence_pvalue[i] <- as.numeric(paste(metacom$Coherence[3]))
  meta_long_s2$turnover_z[i] <- as.numeric(paste(metacom$Turnover[2]))
  meta_long_s2$turnover_pvalue[i] <- as.numeric(paste(metacom$Turnover[3]))
  meta_long_s2$boundary_index[i] <- metacom$Boundary$index
  meta_long_s2$boundary_P[i] <- metacom$Boundary$P
  meta_long_s2$pattern[i] <- IdentifyStructure(metacom)
  
}

write.csv(meta_long_s2, "data/metacom_season2_long.csv")
