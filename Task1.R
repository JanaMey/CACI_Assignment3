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
#write.csv(data.dummies, file = "data.dummies.csv", row.names = FALSE)

head(data.categories)
any(is.na(data.categories)) # FALSE --> no missing values
summary(data.categories)
names(data.categories)
# Data Set mit Kategorien speichern
#write.csv(data.categories, file = "data.categories.csv", row.names = FALSE)

# Age
# Average Age
summary(data.survey$Age) # Mean 35,52 ; Median 36
#plot(data.survey$Age) # ca 20 - 50 Jahre
max(data.survey$Age) #47
min(data.survey$Age) #24
female <- subset(data.survey, data.survey$Female == 1) 
summary(female$Age) # mean 34.9
male <- subset(data.survey, data.survey$Female == 0) 
summary(male$Age) # mean 36.32
#boxplot(data.survey$Age) # keine Outlier
ggplot(data = data.survey, aes(x = Age)) + #fill: variable for differencing ('target)
  geom_histogram(bins = 45, col = "white", fill ="grey24") + # position dodge: next to each other
  labs(x = "Age", y = "Frequency") +
  grid(TRUE)+
  scale_x_continuous(breaks = seq(0, 50, by = 1)) +
  scale_y_continuous(breaks = seq(0, 120, by = 10)) +
  theme_classic(base_size = 15) # change size of text 
#ggsave(file="ageDitribution.png", width=8, height=3, dpi=600) 
# --> keine normale Distribution

# WTP --> Willingness to pay
# Average WTP
summary(data.survey$WTP) # Mean 212,9 ; Median 210
#plot(data.survey$WTP) # ca 100 - 400
max(data.survey$WTP) #390
min(data.survey$WTP) #100
boxplot(data.survey$WTP) # Outlier ca 350 - 400, aber nicht so schlimm
ggplot(data = data.survey, aes(x = WTP)) + #fill: variable for differencing ('target)
  geom_histogram(bins = 45, col = "white", fill ="grey24") + # position dodge: next to each other
  labs(x = "WTP", y = "Frequency") +
  grid(TRUE)+
  scale_x_continuous(breaks = seq(0, 400, by = 20)) +
  theme_classic(base_size = 10)# change size of text
#ggsave(file="wtp_ditribution.png", width=8, height=3, dpi=600) 

# Gender
data.categories[,"Gender"] <- as.factor(data.categories[,"Gender"])
plot(data.categories$Gender)
summary(data.categories$Gender) #566 Female, 434 Male
ggplot(data = data.categories, aes(x = Gender)) +
  geom_histogram(bins = 43, binwidth = 10, col = "white", fill ="grey24", stat="count")+
  labs(x = "Gender", y = "Frequency") +
  grid(TRUE)+
  #scale_x_continuous(breaks = seq(0, 600, by = 50)) +
  theme_classic(base_size = 10)# change size of text
prop.table(table(data.categories$Gender)) # Female 0.566 - Male 0.434

# Occupation
data.categories[,"Occupation"] <- as.factor(data.categories[,"Occupation"])
summary(data.categories$Occupation)
female.categories <- subset(data.categories, data.categories$Gender == "Female") 
female.categories[,"Income"] <- as.factor(female.categories[,"Income"])
summary(female.categories$Income)

# 103 Advertising/public relations 0.104
# 73 Construction/transportation/manufacturing/logistics 0.074
# 85 Education 0.085
# 52 Engineering 0.052
# 126 Financial services 0.128
# 64 Health services 0.065
# 74 Other/family caretaker 0.075
# 77 Retailing/services/restaurant 0.078
# 137 Sales 0.140
# 93 Small-medium business/self-employed 0.098
# 100 Technology 0.101
#plot(data.categories$Occupation)
prop.table(table(data.categories$Occupation))
head(data.categories)

# iPhone
data.categories[,"iPhone"] <- as.factor(data.categories[,"iPhone"])
plot(data.categories$iPhone)
summary(data.categories$iPhone) # iPhone bischen höher als other
# iPhone 534  0.534 - other 466  0.466
prop.table(table(data.categories$iPhone))

# CompBuy
data.categories[,"CompBuy"] <- as.factor(data.categories[,"CompBuy"])
plot(data.categories$CompBuy)
summary(data.categories$CompBuy) # No 799 - Yes 201
prop.table(table(data.categories$CompBuy)) # 80% nein, fast 20% bekommen Technology von Arbeitgeber gestellt
CompBuy.categories <- subset(data.categories, data.categories$CompBuy == "Yes") 
CompBuy.categories[,"Occupation"] <- as.factor(CompBuy.categories[,"Occupation"])
summary(CompBuy.categories$Occupation)


# AmznP
data.categories[,"AmznP"] <- as.factor(data.categories[,"AmznP"])
plot(data.categories$AmznP)
summary(data.categories$AmznP) # ca 100 Leute mehr haben Amazon Prime Account
# No 436 - Yes 564

# Degree
data.categories[,"Degree"] <- as.factor(data.categories[,"Degree"])
plot(data.categories$Degree)
summary(data.categories$Degree) #Undergraduate 668 - master 332
# undergraduate degree ca doppelt so viel wie master or higher

# Income
data.categories[,"Income"] <- as.factor(data.categories[,"Income"])
plot(data.categories$Income)
summary(data.categories$Income)
# most peopke $71k-$100k, only few <40k
prop.table(table(data.categories$Income))

# MediaUse - aufgeteilt in einzelne Medien
# FB_Insta
data.categories[,"FB_Insta"] <- as.factor(data.categories[,"FB_Insta"])
plot(data.categories$FB_Insta)
summary(data.categories$FB_Insta) # No 257 - Yes 743

# Twit
data.categories[,"Twit"] <- as.factor(data.categories[,"Twit"])
plot(data.categories$Twit)
summary(data.categories$Twit) # No 509 - Yes 491

# Snap
data.categories[,"Snap"] <- as.factor(data.categories[,"Snap"])
plot(data.categories$Snap)
summary(data.categories$Snap) # No 635 - Yes 365

# YouTube
data.categories[,"YouTube"] <- as.factor(data.categories[,"YouTube"])
plot(data.categories$YouTube)
summary(data.categories$YouTube) # No 433 - Yes 567

# Pod_radio
data.categories[,"Pod_radio"] <- as.factor(data.categories[,"Pod_radio"])
plot(data.categories$Pod_radio)
summary(data.categories$Pod_radio) # No 414 - Yes 586

# TV
data.categories[,"TV"] <- as.factor(data.categories[,"TV"])
plot(data.categories$TV)
summary(data.categories$TV) # No 207 - Yes 793

# NewsP
data.categories[,"NewsP"] <- as.factor(data.categories[,"NewsP"])
plot(data.categories$NewsP)
summary(data.categories$NewsP) # No 376 - Yes 624


names(data.categories)

# Grouped Histogram
# Age + Degree -> nicht aussagekräftig

# Age + CompBuy
ggplot(data = data.categories, aes(x = Age, fill = CompBuy)) +
  geom_histogram(bins = 30, col = "white") +
  labs(x = "Age", y = "Frequency", fill = "Company provides Technology") +
  scale_y_continuous(limits = c(0, 105), 
                     breaks = seq(0, 105, by = 5)) +
  scale_x_continuous(breaks = seq(0, 50, by = 1)) +
  scale_fill_manual(values = c("steelblue4", "skyblue")) +
  theme_classic()
ggsave(file="ageFrequencyByCombBuy.png", width=8, height=3, dpi=600)

ggplot(data.categories, aes(Age, colour = CompBuy)) +
  geom_freqpoly(binwidth = 5)+
  labs(x = "Willingness to Pay", y = "Frequency", fill = "Income") +
  scale_y_continuous(limits = c(0, 300), 
                     breaks = seq(0, 300, by = 5)) +
  scale_x_continuous(breaks = seq(0, 100, by = 1)) +
  scale_fill_manual(values = c("steelblue4", "skyblue", "darkblue")) +
  theme_classic()

# Age + Occupation -> nicht aussagekräftig, da zu viele Kategorien in Occupation

# Age + Gender
ggplot(data = data.categories, aes(x = Age, fill = Gender)) +
  geom_histogram(bins = 20, col = "white") +
  labs(x = "Age", y = "Frequency", fill = "Gender") +
  scale_y_continuous(limits = c(0, 110), 
                     breaks = seq(0, 110, by = 10)) +
  scale_x_continuous(breaks = seq(0, 50, by = 1)) +
  scale_fill_manual(values = c("steelblue4", "skyblue")) +
  theme_classic()
ggsave(file="ageFrequencyByGender.png", width=8, height=3, dpi=600) 

# Age + iPhone ?
ggplot(data = data.categories, aes(x = Age, fill = iPhone)) +
  geom_histogram(bins = 20, col = "white") +
  labs(x = "Age", y = "Frequency", fill = "iPhone") +
  scale_y_continuous(limits = c(0, 110), 
                     breaks = seq(0, 110, by = 10)) +
  scale_x_continuous(breaks = seq(0, 50, by = 1)) +
  scale_fill_manual(values = c("steelblue4", "skyblue")) +
  theme_classic()
#ggsave(file="ageFrequencyByiPhone.png", width=8, height=3, dpi=600) 

# Age + AmznP ?
ggplot(data = data.categories, aes(x = Age, fill = AmznP)) +
  geom_histogram(bins = 30, col = "white") +
  labs(x = "Age", y = "Frequency", fill = "Amazon Prime Account") +
  scale_y_continuous(limits = c(0, 105), 
                     breaks = seq(0, 105, by = 10)) +
  scale_x_continuous(breaks = seq(0, 50, by = 1)) +
  scale_fill_manual(values = c("steelblue4", "skyblue")) +
  theme_bw()
ggsave(file="ageFrequencyByAmznP.png", width=8, height=3, dpi=600) 

# Age + AmazonPrime
ggplot(data.categories, aes(Age, colour = AmznP)) +
  geom_freqpoly(binwidth = 2)+
  labs(x = "Age", y = "Frequency", fill = "Amazon Prime") +
  scale_y_continuous(limits = c(0, 90), 
                     breaks = seq(0, 90, by = 5)) +
  scale_x_continuous(breaks = seq(0, 50, by = 2)) +
  scale_color_discrete(name="Amazon Prime Account", labels = c("No", "Yes"))
  theme_classic()
  

#Age + WTP
ggplot(data.categories, aes(Age, colour = WTP)) +
  geom_freqpoly(binwidth = 2)+
  labs(x = "Age", y = "Frequency", fill = "WTP",
                       breaks = seq(0, 90, by = 5)) +
  scale_x_continuous(breaks = seq(0, 50, by = 2)) +
theme_classic()
  

# Age + Income ?
ggplot(data.categories, aes(Age, colour = Income)) +
  geom_freqpoly(binwidth = 2)+
  labs(x = "Age", y = "Frequency", fill = "Income") +
  scale_y_continuous(limits = c(0, 70), 
                     breaks = seq(0, 70, by = 5)) +
  scale_x_continuous(breaks = seq(0, 50, by = 2)) +
  scale_fill_manual(values = c("steelblue4", "skyblue", "darkblue")) +
  theme_bw()

# WTP + iPhone
ggplot(data = data.categories, aes(x = WTP, fill = iPhone)) +
  geom_histogram(bins = 20, col = "white") +
  labs(x = "WTP", y = "Frequency", fill = "iPhone") +
  scale_y_continuous(limits = c(0, 160), 
                     breaks = seq(0, 160, by = 5)) +
  scale_x_continuous(breaks = seq(0, 400, by = 20)) +
  scale_fill_manual(values = c("steelblue4", "skyblue")) +
  theme_classic()
#ggsave(file="WTFFrequencyByiPhone.png", width=8, height=3, dpi=600) 

# WTP + income
data.categories$Income <- ifelse(data.categories$Income == "< $40k", "< $70k",
                                 ifelse(data.categories$Income == "$40k - $70k", "< $70k",
                                        ifelse(data.categories$Income == "> $175k", "> $100k",
                                               ifelse(data.categories$Income == "$101k - $175k", "> $100k", "$71k – $100k"))))

ggplot(data.categories, aes(WTP, colour = Income)) +
  geom_freqpoly(binwidth = 20)+
  labs(x = "Willingness to Pay", y = "Frequency", fill = "Income") +
  scale_y_continuous(limits = c(0, 70), 
                     breaks = seq(0, 70, by = 10)) +
  scale_x_continuous(breaks = seq(0, 400, by = 20)) +
  scale_fill_manual(values = c("steelblue4", "skyblue", "darkblue")) +
  theme_bw()
ggsave(file="WTFFrequencyByIncome.png", width=8, height=3, dpi=600) 



names(data.categories)

