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
         plot %in% c(4, 11, 19, 20, 21, 3, 14, 15, 17)) 

### Add Season ###

# four seasons
full_4 <- add_seasons(portal_full, 4) %>%
  group_by(species, plot, season4_yr, season4) %>%
  summarise(count = n())
long_4 <- add_seasons(portal_long, 4) %>%
  group_by(species, plot, season4_yr, season4) %>%
  summarise(count = n())

# two seasons
full_2 <- add_seasons(portal_full, 2) %>%
  group_by(species, plot, season2_yr, season2) %>%
  summarise(count = n())
long_2 <- add_seasons(portal_long, 2) %>%
  group_by(species, plot, season2_yr, season2) %>%
  summarise(count = n())

# NEED TO SUMMARIZE BY SPECIES COUNTS BY SEASON BY YEAR

##########################
# DATAFRAMES

# four seasons
  # full (all plots)

meta_full_s4 <-
  data.frame(
    year4 = NA,
    coherence_z = NA,
    coherence_pvalue = NA,
    turnover_z = NA,
    turnover_pvalue = NA,
    boundary_index = NA,
    boundary_P = NA,
    pattern = NA
  )

full_s4yr <- unique(interaction(full_4$season4_yr, full_4$season4))

for (i in 1:length(full_s4yr)){
  
  meta_full_s4$year4[i] <- full_s4yr[i]
  
  data <- select(full_4) %>% filter(season4_yr == full_s4yr[i]) # stopped editing here--filter by year and season--maybe combine the column like in full_s4yr?
  matrix <- data[,-1]
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
