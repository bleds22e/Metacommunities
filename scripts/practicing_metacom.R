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

##########
matrix_list <- list()
period_list <- list()
for (i in 1:length(Full)){  
  matrix <- Full[[i]]
  period <- matrix$period[1]
  matrix1 <- matrix[,-1]
  matrix2 <- dcast(matrix1, formula = plot ~ species)
  matrix3 <- tibble::column_to_rownames(as.data.frame(matrix2), "plot")
  matrix_list[[i]] <- matrix3
  period_list[[i]] <- period
}

coherence_list <- list()
for (i in 1:length(matrix_list)){
  co <- Coherence(matrix_list[[i]])
  coherence <- co$z
}
  
turnover_list <- list()
for (i in 1:length(matrix_list)){
   t <- Turnover(matrix_list[[i]])
    turnover <- t$z
  turnover_list[[i]] <- turnover
}
 
boundary_list <- list()
for (i in 1:length(matrix_list)){
  b <- BoundaryClump(matrix_list[[i]])
  boundary <- b$index
  boundary_list[[i]] <- boundary
}
  
# dataframe of turnover results

turnover_df <- as.data.frame(do.call("rbind", turnover_list))
period_df <- as.data.frame(do.call("rbind", period_list))
#coherence_df <- as.data.frame(do.call("rbind", coherence_list))
#boundary_df <- as.data.frame(do.call("rbind", boundary_list))

df <- bind_cols(period_df, turnover_df)
colnames(df) <- c("period", "turnover")

# plotting

ggplot(df, aes(x = period, y = turnover)) +
  geom_area() +
  geom_smooth() +
  theme_bw()


###########
