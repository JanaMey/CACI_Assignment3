# (Install and) Load the necessary packages ------------------------------------
pacman::p_load(reshape2, ggplot2, psych, corrplot, fpc, cluster,
               nnet, mclust, e1071, randomForest)

# data.survey ist der original Datensatz
# data.categories ist der Datensatz nur mit Kategorien und keine Dummies
# data.dummies ist der Datensatz nur mit Dummies und ohne Kategorien

# Load required dataset ----------------------------------------------------------
urlfile<-'https://raw.githubusercontent.com/JanaMey/CACI_Assignment3/main/smartwatch_survey.csv'
data.survey <-read.csv(urlfile)
head(data.survey)

# 
# urlfile<-'https://raw.githubusercontent.com/JanaMey/CACI_Assignment3/main/data.categories.csv'
# data.categories <-read.csv(urlfile)
# head(data.categories)
# 
# urlfile<-'https://raw.githubusercontent.com/JanaMey/CACI_Assignment3/main/data.dummies.csv'
# data.dummies <-read.csv(urlfile)

urlfile<-'https://raw.githubusercontent.com/JanaMey/CACI_Assignment3/main/data.classification.csv'
seg.df <-read.csv(urlfile)
head(seg.df)

###TEST INPUT DATA##### 
# naive bayes need category input data
####new column segment to data set
# data.categories$segment <- seg.df$cluster_kmeans
# head(data.categories)
# seg.df <- data.categories
# str(seg.df)

###test
data.survey$segment <- seg.df$cluster_kmeans
head(data.survey)
seg.df <- data.survey
str(seg.df)

# predictors? ==================================================================
# predictors1 <- names(seg.df)[14:26]
# predictors2 <- names(seg.df)[28:30]
# seg.df <- seg.df[, c(predictors1,predictors2)]
# #colnames(seg.df)[20]  <- "segment"
# seg.df$segment <- as.factor(seg.df$segment) # for boxplot

#test
predictors <- names(seg.df)[14:37]
seg.df <- seg.df[, c(predictors, "segment")]
# #colnames(seg.df)[20]  <- "segment"
seg.df$segment <- as.factor(seg.df$segment) # for boxplot

names <- c(names(seg.df)[2:21], "Female", "Degree","segment")
seg.df[,names] <- lapply(seg.df[,names] , factor)
str(seg.df)

# Descriptive analysis =========================================================
#####eventuell TODO
#segmente an data.categories pinnen und printen

#corrplot
# cor(seg.df[,1], seg.df[,12], method = "pearson")
# #-0.00118738 => no correlation
# corrplot(cor(seg.df[, c(1,12)]),
#                   method = 'number',
#                   type = "upper",
#                   number.cex = 0.9,
#                   tl.cex = 0.9)

# size segment + plot
table(seg.df$segment)
# 
ggplot(data = seg.df) +
  geom_bar(mapping = aes(x = segment, y = ..prop.., group = 1), stat = "count") +
  labs(y = "Relative Frequency") +
  #scale_y_continuous(limits = c(0, 0.4)) +
  theme_classic()
# 
# # Grouped by iPhone
# ggplot(data = seg.df, aes(x = segment, y = ..prop.., group = 1)) +
#   geom_bar(stat = "count") +
#   labs(y = "Relative Frequency for iPhone") +
#   facet_wrap(.~iPhone) +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 90))
# 
# # Grouped by degree
# ggplot(data = seg.df, aes(x = segment, y = ..prop.., group = 1)) +
#   geom_bar(stat = "count") +
#   labs(y = "Relative Frequency for Degree") +
#   facet_wrap(.~Degree) +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 90))
# 
# 
# Grouped by AmznP
# ggplot(data = seg.df, aes(x = segment, y = ..prop.., group = 1)) +
#   geom_bar(stat = "count") +
#   labs(y = "Relative Frequency for AmznP") +
#   facet_wrap(.~AmznP) +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 90))
# 
# 
# # Grouped by CompBuy
# ggplot(data = seg.df, aes(x = segment, y = ..prop.., group = 1)) +
#   geom_bar(stat = "count") +
#   labs(y = "Relative Frequency for CompanyBuy") +
#   facet_wrap(.~CompBuy) +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 90))
# 
#Grouped by gender
ggplot(data = seg.df, aes(x = segment, y = ..prop.., group = 1)) +
  geom_bar(stat = "count") +
  labs(y = "Relative Frequency for Gender") +
  facet_wrap(.~Female) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90))
  
# ######TODO
# # Grouped by media use
# 
# # Grouped by occupation
# 
# # Grouped by income
# 
# 
# Age distribution by segments
ggplot(data = seg.df, aes(x = segment, y = Age)) +
  geom_boxplot() +
  labs(y = "Age") +
  theme_classic()

ggplot(data = seg.df, aes(x = segment, y = WTP, fill = segment)) + # Manually specified filling color
  geom_boxplot() +
  labs(y = "Willingness to Pay",x = "Cluster") +
  scale_y_continuous(limits = c(100, 390), 
                   breaks = seq(100, 390, by = 50)) +
  scale_fill_manual(values = c("grey","steelblue4", "skyblue")) +
  guides(fill=guide_legend(title="Cluster"))+
  theme_bw(base_size = 15)
ggsave(file="WTP.png", width=6, height=3, dpi=300)




#######standardization
# standardize age
seg.df$Age <- scale(seg.df$Age)
seg.df$WTP <- scale(seg.df$WTP)
summary(seg.df)


# Step 1: Split the data into training and test sets ===========================
set.seed(04625)   # fix the seed for reproducability
train.pop <- 0.60 # we will use 60-40% split
N <- nrow(seg.df) # total sample size

# randomly sample 65\% of observations
train.cases <- sample(N, N*train.pop)

# assign the randomly sampled 65% of obs. to training dataset
train.df <- seg.df[train.cases, ]
nrow(train.df)

# assign the rest to test dataset
test.df <- seg.df[-train.cases, ]
nrow(test.df)


# Step 2: Train the prediction model ===========================================
##### Regression
##TODO: ohne Income, media use, occupation
logistic <- multinom(segment ~ ., data = train.df)
#logistic <- multinom(segment ~ WTP + iPhone+ CompBuy + Age + AmznP + Degree + Gender , data = train.df)#+FB_Insta + Twit + Snap + YouTube + Pod_radio + TV + NewsP
summary(logistic)
#tetst
# ohne occup, media_use, income: 74.25
# ohne occup, income: 77.5
# ohne income: 78.00
# ohne occup: 77.5

str(train.df)
# Coefficents
coeff = (round(summary(logistic)$coefficients, 2)) # t for transpose
coeff <-as.data.frame(coeff) 
coeff
write.csv(coeff,"C:/Users/Lilli/Google Drive/2021CACI/LogReg.csv", row.names = TRUE)

# standard errors
#error<-as.data.frame(t(round(summary(logistic)$standard.errors, 3)))
#write.csv(error,"C:/Users/Lilli/Google Drive/2021CACI/LogError.csv", row.names = FALSE)FB_Insta


# p-value calculation for the regression coefficients,
# so we calculate p-values using Wald tests (here z-tests).
z <- summary(logistic)$coefficients/summary(logistic)$standard.errors
# 2-tailed z test
p <- (1 - pnorm(abs(z), 0, 1)) * 2
pValue <-as.data.frame(round(t(p), 3))
pValue
write.csv(pValue,"C:/Users/Lilli/Google Drive/2021CACI/Log_pValue.csv", row.names = FALSE)

# odds ratio
x = exp(summary(logistic)$coefficients)
#round(x[1, -1], 2)
odds <-as.data.frame(round(t(x), 2))
odds
write.csv(odds,"C:/Users/Lilli/Google Drive/2021CACI/LogOdds.csv", row.names = FALSE)

##### Naive Bayes 
nb <- naiveBayes(segment ~ ., data = train.df)
(nb)


##### Random Forest 
set.seed(98040)
head(train.df)

# to avoid erro message=> to tell RF to perform classification
train.df$segment <- as.factor(train.df$segment)

rf <- randomForest(segment ~ ., data = train.df, ntree = 10000)
(rf)


# segment allocation
head(test.df)
test.df$seg_log <- predict(logistic, newdata = test.df)
test.df$seg_nb <- predict(nb, test.df)
test.df$seg_rf <- predict(rf, test.df)


# How well did the models predict?
fit <- data.frame(model = c("Logistic", "Naive Bayes", "Random Forest"),
                  hitrate = c(mean(test.df$segment == test.df$seg_log),
                              mean(test.df$segment == test.df$seg_nb),
                              mean(test.df$segment == test.df$seg_rf)) * 100,
                  vs_chance = c(adjustedRandIndex(test.df$seg_log, test.df$segment),
                                adjustedRandIndex(test.df$seg_nb, test.df$segment),
                                adjustedRandIndex(test.df$seg_rf, test.df$segment)) * 100)

fit
str(test.df[, 1:27])



# # Comparison of methods --------------------------------------------------------
# clusplot(test.df[, 1:27], test.df$seg_log,
#          color = TRUE, shade = TRUE,
#          labels = 2, lines = 0,
#          main = "Logistic Regression classification")
# 
# clusplot(test.df[, 1:27], test.df$seg_nb,
#          color = TRUE, shade = TRUE,
#          labels = 2, lines = 0,
#          main = "Naive Bayes classification")
# 
# clusplot(test.df[, 1:27], test.df$seg_rf,
#          color = TRUE, shade = TRUE,
#          labels = 2, lines = 0,
#          main = "Random Forest classification")

