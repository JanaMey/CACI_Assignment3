# Install and load the required libraries ----------------------------------------
pacman::p_load(reshape2, ggplot2, dplyr, stringr, corrplot)

# Load required dataset ----------------------------------------------------------
urlfile<-'https://raw.githubusercontent.com/JanaMey/CACI_Assignment3/main/smartwatch_survey.csv'
data.survey <-read.csv(urlfile)
str(data.survey)
head(data.survey)
dim(data.survey) # 1000 38
any(is.na(data.survey)) # FALSE --> no missing values
summary(data.survey)
names(data.survey)


# Age
#plot(data.survey$Age) # 20 - 50 Jahre
#boxplot(data.survey$Age) # keine Outlier
ggplot(data = data.survey, aes(x = Age)) + #fill: variable for differencing ('target)
  geom_histogram(bins = 45, col = "white", fill ="turquoise4") + # position dodge: next to each other
  labs(x = "Age", y = "Frequency") +
  grid(TRUE)+
  scale_x_continuous(breaks = seq(0, 50, by = 1)) +
  theme_classic(base_size = 10)# change size of text
#ggsave(file="age_ditribution.png", width=8, height=3, dpi=600) 
# --> keine normale Distribution

# WTP
#plot(data.survey$WTP) # 100 - 400
#boxplot(data.survey$WTP) # Outlier ca 340 - 400, aber nicht so wild
ggplot(data = data.survey, aes(x = WTP)) + #fill: variable for differencing ('target)
  geom_histogram(bins = 45, col = "white", fill ="turquoise4") + # position dodge: next to each other
  labs(x = "WTP", y = "Frequency") +
  grid(TRUE)+
  scale_x_continuous(breaks = seq(0, 400, by = 20)) +
  theme_classic(base_size = 10)# change size of text
#ggsave(file="wtp_ditribution.png", width=8, height=3, dpi=600) 