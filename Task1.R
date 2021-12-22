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

# Variable Degree und Income sind noch Categories
# Kopie von Data Set erstellen nur mit Categories, original Data Set enthält dann nur noch dummies
data.categories <- data.survey
data.survey$Degree <- ifelse(data.survey$Degree == 1, 1, 0)
# Dummies für Income -- 5 ist die reference category
data.survey$Income_Lower40k <- ifelse(data.survey$Income == 1, 1, 0)
data.survey$Income_40k_to_70k <- ifelse(data.survey$Income == 2, 1, 0)
data.survey$Income_71k_to_100k <- ifelse(data.survey$Income == 3, 1, 0)
data.survey$Income_101k_to_157k <- ifelse(data.survey$Income == 4, 1, 0)
data.survey$Income <- NULL

# Categories erstellen
data.categories$iPhone <- ifelse(data.categories$iPhone == 1, "iPhone", "other smartphone")
data.categories$CompBuy <- ifelse(data.categories$CompBuy == 1, "Yes", "No")
data.categories$Occupation <- ifelse(data.categories$Occup_Health == 1, "Health services", 
                              ifelse(data.categories$Occup_Finc == 1, "Financial services",
                              ifelse(data.categories$Occup_Sales == 1, "Sales",
                              ifelse(data.categories$Occup_Advt == 1, "Advertising/public relations",
                              ifelse(data.categories$Occup_Edu == 1, "Education", 
                              ifelse(data.categories$Occup_Cons == 1, "Construction/transportation/manufacturing/logistics",
                              ifelse(data.categories$Occup_Eng == 1, "Engineering",
                              ifelse(data.categories$Occup_Tech == 1, "Technology",
                              ifelse(data.categories$Occup_Retail == 1, "Retailing/services/restaurant",
                              ifelse(data.categories$Occup_SMB == 1, "Small-medium business/self-employed", "Other/family caretaker"))))))))))
data.categories$Occup_Health <- NULL
data.categories$Occup_Finc <- NULL
data.categories$Occup_Sales <- NULL
data.categories$Occup_Advt <- NULL
data.categories$Occup_Edu <- NULL
data.categories$Occup_Cons <- NULL
data.categories$Occup_Eng <- NULL
data.categories$Occup_Tech <- NULL
data.categories$Occup_Retail <- NULL
data.categories$Occup_SMB <- NULL

data.categories$MediaUse <- ifelse(data.categories$FB_Insta == 1, "Facebook/Instagram",
                            ifelse(data.categories$Twit == 1, "Snap",
                            ifelse(data.categories$YouTube == 1, "YouTube/Netflix/Hulu",
                            ifelse(data.categories$Pod_radio == 1, "Radio/podcasts",
                            ifelse(data.categories$TV == 1, "TV", "Newspapers or magazines")))))
data.categories$FB_Insta <- NULL
data.categories$Twit <- NULL
data.categories$YouTube <- NULL
data.categories$Pod_radio <- NULL
data.categories$TV <- NULL
data.categories$NewsP <- NULL


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