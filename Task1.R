# Install and load the required libraries ----------------------------------------
pacman::p_load(reshape2, ggplot2, dplyr, stringr, corrplot)

# data.survey ist der original Datensatz
# data.categories ist der Datensatz nur mit Kategorien und keine Dummies
# data.dummies ist der Datensatz nur mit Dummies und ohne Kategorien

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
data.dummies <- data.survey
data.dummies$Degree <- ifelse(data.survey$Degree == 1, 1, 0)
# Dummies für Income -- 5 ist die reference category
data.dummies$Income_Lower40k <- ifelse(data.survey$Income == 1, 1, 0)
data.dummies$Income_40k_to_70k <- ifelse(data.survey$Income == 2, 1, 0)
data.dummies$Income_71k_to_100k <- ifelse(data.survey$Income == 3, 1, 0)
data.dummies$Income_101k_to_157k <- ifelse(data.survey$Income == 4, 1, 0)
data.dummies$Income <- NULL

# Categories erstellen
data.categories$iPhone <- ifelse(data.survey$iPhone == 1, "iPhone", "other smartphone")
data.categories$Degree <- ifelse(data.survey$Degree == 1, "Undergraduate degree", "Master’s degree or higher")
data.categories$CompBuy <- ifelse(data.survey$CompBuy == 1, "Yes", "No")
data.categories$Income <- ifelse(data.survey$Income == 1, "< $40k",
                          ifelse(data.survey$Income == 2, "$40k - $70k",
                          ifelse(data.survey$Income == 3, "$71k – $100k",
                          ifelse(data.survey$Income == 4, "$101k - $175k", "> $175k"))))

data.categories$Occupation <- ifelse(data.survey$Occup_Health == 1, "Health services", 
                              ifelse(data.survey$Occup_Finc == 1, "Financial services",
                              ifelse(data.survey$Occup_Sales == 1, "Sales",
                              ifelse(data.survey$Occup_Advt == 1, "Advertising/public relations",
                              ifelse(data.survey$Occup_Edu == 1, "Education", 
                              ifelse(data.survey$Occup_Cons == 1, "Construction/transportation/manufacturing/logistics",
                              ifelse(data.survey$Occup_Eng == 1, "Engineering",
                              ifelse(data.survey$Occup_Tech == 1, "Technology",
                              ifelse(data.survey$Occup_Retail == 1, "Retailing/services/restaurant",
                              ifelse(data.survey$Occup_SMB == 1, "Small-medium business/self-employed", "Other/family caretaker"))))))))))
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

data.categories$FB_Insta <- ifelse(data.survey$FB_Insta == 1, "Yes", "No")
data.categories$Twit <- ifelse(data.survey$Twit == 1, "Yes", "No")
data.categories$Snap <- ifelse(data.survey$Snap == 1, "Yes", "No")
data.categories$YouTube <- ifelse(data.survey$YouTube == 1, "Yes", "No")
data.categories$Pod_radio <- ifelse(data.survey$Pod_radio == 1, "Yes", "No")
data.categories$TV <- ifelse(data.survey$TV == 1, "Yes", "No")
data.categories$NewsP <- ifelse(data.survey$NewsP == 1, "Yes", "No")

data.categories$AmznP <- ifelse(data.survey$AmznP == 1, "Yes", "No")

data.categories$Gender <- ifelse(data.survey$Female == 1, "Female", "Male")
data.categories$Female <- NULL

head(data.survey)
any(is.na(data.survey)) # FALSE --> no missing values
summary(data.survey)
names(data.survey)

head(data.dummies)
any(is.na(data.dummies)) # FALSE --> no missing values
summary(data.dummies)
names(data.dummies)
# Data Set mit Dummies speichern
write.csv(data.dummies, file = "data.dummies.csv", row.names = FALSE)

head(data.categories)
any(is.na(data.categories)) # FALSE --> no missing values
summary(data.categories)
names(data.categories)
# Data Set mit Kategorien speichern
write.csv(data.categories, file = "data.categories.csv", row.names = FALSE)

# Age
# Average Age
summary(data.survey$Age) # Mean 35,52 ; Median 36
#plot(data.survey$Age) # ca 20 - 50 Jahre
max(data.survey$Age) #47
min(data.survey$Age) #24
#boxplot(data.survey$Age) # keine Outlier
ggplot(data = data.survey, aes(x = Age)) + #fill: variable for differencing ('target)
  geom_histogram(bins = 45, col = "white", fill ="grey24") + # position dodge: next to each other
  labs(x = "Age", y = "Frequency") +
  grid(TRUE)+
  scale_x_continuous(breaks = seq(0, 50, by = 1)) +
  scale_y_continuous(breaks = seq(0, 120, by = 10)) +
  theme_classic(base_size = 15) # change size of text 
  ggsave(file="ageDitribution.png", width=8, height=3, dpi=600) 
# --> keine normale Distribution

# WTP --> Willingness to pay
# Average WTP
summary(data.survey$WTP) # Mean 212,9 ; Median 210
#plot(data.survey$WTP) # ca 100 - 400
max(data.survey$WTP) #390
min(data.survey$WTP) #100
#boxplot(data.survey$WTP) # Outlier ca 350 - 400, aber nicht so schlimm
ggplot(data = data.survey, aes(x = WTP)) + #fill: variable for differencing ('target)
  geom_histogram(bins = 45, col = "white", fill ="grey24") + # position dodge: next to each other
  labs(x = "WTP", y = "Frequency") +
  grid(TRUE)+
  scale_x_continuous(breaks = seq(0, 400, by = 20)) +
  theme_classic(base_size = 10)# change size of text
ggsave(file="wtp_ditribution.png", width=8, height=3, dpi=600) 

# Gender
data.categories[,"Gender"] <- as.factor(data.categories[,"Gender"])
#plot(data.categories$Gender)
summary(data.categories$Gender) #566 Female, 434 Male
ggplot(data = data.categories, aes(x = Gender)) +
  geom_histogram(bins = 43, binwidth = 10, col = "white", fill ="grey24", stat="count")+
  labs(x = "Gender", y = "Frequency") +
  grid(TRUE)+
  #scale_x_continuous(breaks = seq(0, 600, by = 50)) +
  theme_classic(base_size = 10)# change size of text
prop.table(table(data.categories$Gender))

# Occupation
data.categories[,"Occupation"] <- as.factor(data.categories[,"Occupation"])
summary(data.categories$Occupation)
# 103 Advertising/public relations 
# 73 Construction/transportation/manufacturing/logistics 
# 85 Education
# 52 Engineering
# 126 Financial services 
# 64 Health services
# 74 Other/family caretaker  
# 77 Retailing/services/restaurant  
# 137 Sales 
# 93 Small-medium business/self-employed 
# 100 Technology
#plot(data.categories$Occupation)
prop.table(table(data.categories$Occupation))
head(data.categories)

# iPhone
data.categories[,"iPhone"] <- as.factor(data.categories[,"iPhone"])
#plot(data.categories$iPhone)
summary(data.categories$iPhone) # iPhone bischen höher als other


# CompBuy
data.categories[,"CompBuy"] <- as.factor(data.categories[,"CompBuy"])
#plot(data.categories$CompBuy)
summary(data.categories$CompBuy)
prop.table(table(data.categories$CompBuy)) # 80% nein, fast 20% bekommen Technology von Arbeitgeber gestellt

# AmznP
data.categories[,"AmznP"] <- as.factor(data.categories[,"AmznP"])
#plot(data.categories$AmznP)
summary(data.categories$AmznP) # ca 100 Leute mehr haben Amazon Prime Account

# Degree
data.categories[,"Degree"] <- as.factor(data.categories[,"Degree"])
#plot(data.categories$Degree)
summary(data.categories$Degree)
# undergraduate degree ca doppelt so viel wie master or higher

# Income
data.categories[,"Income"] <- as.factor(data.categories[,"Income"])
#plot(data.categories$Income)
summary(data.categories$Income)
# most peopke $71k-$100k, only few <40k
prop.table(table(data.categories$Income))


# MediaUse
data.categories[,"MediaUse"] <- as.factor(data.categories[,"MediaUse"])
#plot(data.categories$MediaUse)
summary(data.categories$MediaUse)
prop.table(table(data.categories$MediaUse))
# almost all Facebook/ Instagram

