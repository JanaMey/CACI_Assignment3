# Install and load the required libraries ----------------------------------------
pacman::p_load(reshape2, ggplot2, dplyr, stringr, corrplot)

# Load required dataset ----------------------------------------------------------
urlfile<-'https://raw.githubusercontent.com/JanaMey/CACI_Assignment3/main/smartwatch_survey.csv'
data.survey <-read.csv(urlfile)
str(data.survey)
head(data.survey)
dim(data.survey) # 1000 38
