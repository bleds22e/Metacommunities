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

# elect Portal data to use
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

# list of dataframes by periods
Full <- split(portal_full, f = portal_full$period)

LongTerm <- split(portal_long, f = portal_long$period)

### Full
# loop metacom package

empty_list <- list()

for (i in 1:length(Full)){  
  matrix <- Full[[i]]
  period <- matrix$period[1]
  matrix <- matrix[,-1]
  matrix <- dcast(matrix, formula = plot ~ species)
  matrix <- tibble::column_to_rownames(as.data.frame(matrix), "plot")
  
  metacom <- Metacommunity(matrix, order = TRUE)
  
  c <- as.data.frame(metacom$Coherence)
  coherence <- as.numeric(paste(c[2,1]))
  
  t <- as.data.frame(metacom$Turnover)
  turnover <- as.numeric(paste(t[2,1]))
  
  boundary <- metacom$Boundary$index
  
  empty_list[[i]] <- c(period, coherence, turnover, boundary)
}

# dataframe of metacom results

full_metacom <- do.call("rbind", empty_list)
colnames(full_metacom) <- c("period", "coherence", "turnover", "boundary")

### Long Term
# loop metacom package

empty_list <- list()

for (i in 1:length(LongTerm)){  
  matrix <- Full[[i]]
  period <- matrix$period[1]
  matrix1 <- matrix[,-1]
  matrix2 <- dcast(matrix1, formula = plot ~ species)
  matrix3 <- tibble::column_to_rownames(as.data.frame(matrix2), "plot")
  
  metacom <- Metacommunity(matrix3, order = TRUE, binary = FALSE)
  
  c <- as.data.frame(metacom$Coherence)
  coherence <- as.numeric(paste(c[2,1]))
  
  t <- as.data.frame(metacom$Turnover)
  turnover <- as.numeric(paste(t[2,1]))
  
  boundary <- metacom$Boundary$index
  
  empty_list[[i]] <- c(period, coherence, turnover, boundary)
}

# Dataframe of Metacom Results

long_metacom <- do.call("rbind", empty_list)
colnames(long_metacom) <- c("period", "coherence", "turnover", "boundary")

#######################
# WORK AREA

# FULL (all plots)

f_matrix_list <- list()
f_period_list <- list()
for (i in 1:length(Full)){  
  matrix <- Full[[i]]
  period <- matrix$period[1]
  matrix1 <- matrix[,-1]
  matrix2 <- dcast(matrix1, formula = plot ~ species)
  matrix3 <- tibble::column_to_rownames(as.data.frame(matrix2), "plot")
  f_matrix_list[[i]] <- matrix3
  f_period_list[[i]] <- period
}

#f_coherence_list <- list()
#for (i in 1:length(f_matrix_list)){
#  co <- Coherence(f_matrix_list[[i]])
#  f_coherence_list <- co$z
#}
  
f_turnover_list <- list()
for (i in 1:length(f_matrix_list)){
  t <- Turnover(f_matrix_list[[i]])
  turnover <- t$z
  f_turnover_list[[i]] <- turnover
}
 
#boundary_list <- list()
#for (i in 1:length(f_matrix_list)){
#  b <- BoundaryClump(f_matrix_list[[i]])
#  boundary <- b$index
#  f_boundary_list[[i]] <- boundary
#}
  
# dataframe of turnover results

f_turnover_df <- as.data.frame(do.call("rbind", f_turnover_list))
f_period_df <- as.data.frame(do.call("rbind", f_period_list))
#f_coherence_df <- as.data.frame(do.call("rbind", f_coherence_list))
#f_boundary_df <- as.data.frame(do.call("rbind", f_boundary_list))

f_df <- bind_cols(f_period_df, f_turnover_df)
colnames(f_df) <- c("period", "turnover")

# plotting

ggplot(f_df, aes(x = period, y = turnover)) +
  geom_area() +
  geom_smooth() +
  theme_bw() +
  ggtitle("All Plots")

# LONG-TERM

lt_matrix_list <- list()
lt_period_list <- list()
for (i in 1:length(LongTerm)){  
  matrix <- LongTerm[[i]]
  period <- matrix$period[1]
  matrix1 <- matrix[,-1]
  matrix2 <- dcast(matrix1, formula = plot ~ species)
  matrix3 <- tibble::column_to_rownames(as.data.frame(matrix2), "plot")
  lt_matrix_list[[i]] <- matrix3
  lt_period_list[[i]] <- period
}

#lt_coherence_list <- list()
#for (i in 1:length(lt_matrix_list)){
#  co <- Coherence(lt_matrix_list[[i]])
#  lt_coherence_list <- co$z
#}

lt_turnover_list <- list()
for (i in 1:length(lt_matrix_list)){
  t <- Turnover(lt_matrix_list[[i]])
  turnover <- t$z
  lt_turnover_list[[i]] <- turnover
}

#lt_boundary_list <- list()
#for (i in 1:length(lt_matrix_list)){
#  b <- BoundaryClump(lt_matrix_list[[i]])
#  boundary <- b$index
#  lt_boundary_list[[i]] <- boundary
#}

# dataframe of turnover results

lt_turnover_df <- as.data.frame(do.call("rbind", lt_turnover_list))
lt_period_df <- as.data.frame(do.call("rbind", lt_period_list))
#lt_coherence_df <- as.data.frame(do.call("rbind", lt_coherence_list))
#lt_boundary_df <- as.data.frame(do.call("rbind", lt_boundary_list))

lt_df <- bind_cols(lt_period_df, lt_turnover_df)
colnames(lt_df) <- c("period", "turnover")

# plotting

ggplot(lt_df, aes(x = period, y = turnover)) +
  geom_area() +
  geom_smooth() +
  theme_bw() +
  ggtitle("Long-Term Plots")

# CONTROLS
