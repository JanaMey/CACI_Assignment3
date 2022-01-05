# Install and load the required libraries ----------------------------------------
pacman::p_load(reshape2, ggplot2, dplyr, stringr, corrplot)

# data.survey ist der original Datensatz
# data.categories ist der Datensatz nur mit Kategorien und keine Dummies
# data.dummies ist der Datensatz nur mit Dummies und ohne Kategorien

# Load required dataset ----------------------------------------------------------
urlfile<-'https://raw.githubusercontent.com/JanaMey/CACI_Assignment3/main/smartwatch_survey.csv'
data.survey <-read.csv(urlfile)

urlfile<-'https://raw.githubusercontent.com/JanaMey/CACI_Assignment3/main/data.categories.csv'
data.categories <-read.csv(urlfile)

urlfile<-'https://raw.githubusercontent.com/JanaMey/CACI_Assignment3/main/data.dummies.csv'
data.dummies <-read.csv(urlfile)

str(data.dummies)
#TODO: richtigen datensatz einlesen

# # Split the data into training and test sets -----------------------------------
# set.seed(04625)   # fix the seed for reproducability
# train.pop <- 0.65 # we will use 65-35% split
# N <- nrow(seg.df) # total sample size
# 
# # randomly sample 65\% of observations
# train.cases <- sample(N, N*train.pop)
# 
# # assign the randomly sampled 65% of obs. to training dataset
# seg.df.train <- seg.df[train.cases, ] 
# nrow(seg.df.train)
# 
# # assign the rest to test dataset
# seg.df.test <- seg.df[-train.cases, ]
# nrow(seg.df.test)