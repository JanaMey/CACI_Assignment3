# (Install and) Load the necessary packages ------------------------------------
pacman::p_load(reshape2, ggplot2, psych, corrplot, fpc, cluster,
               nnet, mclust, e1071, randomForest)

# data.survey ist der original Datensatz
# data.categories ist der Datensatz nur mit Kategorien und keine Dummies
# data.dummies ist der Datensatz nur mit Dummies und ohne Kategorien

# Load required dataset ----------------------------------------------------------
# urlfile<-'https://raw.githubusercontent.com/JanaMey/CACI_Assignment3/main/smartwatch_survey.csv'
# data.survey <-read.csv(urlfile)
# 
# urlfile<-'https://raw.githubusercontent.com/JanaMey/CACI_Assignment3/main/data.categories.csv'
# data.categories <-read.csv(urlfile)
# 
# urlfile<-'https://raw.githubusercontent.com/JanaMey/CACI_Assignment3/main/data.dummies.csv'
# data.dummies <-read.csv(urlfile)

urlfile<-'https://raw.githubusercontent.com/JanaMey/CACI_Assignment3/main/data.classification.csv'
seg.df <-read.csv(urlfile)
head(seg.df)

# predictors?
#wtp bringt 0.5 punkte accurancy
predictors <- names(seg.df)[15:42]
predictors
seg.df <- seg.df[, c(predictors)]
colnames(seg.df)[28]  <- "segment"
head(seg.df)


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
logistic <- multinom(segment ~ ., data = train.df)
summary(logistic)

# Naive Bayes 
nb <- naiveBayes(segment ~ ., data = train.df)
(nb)

# Random Forest 
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

