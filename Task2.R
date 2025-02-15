# Install and load the required libraries ----------------------------------------
pacman::p_load(reshape2, ggplot2, psych, corrplot, fpc, cluster,
               nnet, mclust, e1071, randomForest)

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

head(data.survey)

names(data.survey)[2] <- "Innovation"
names(data.survey)[3] <- "Constant C."
names(data.survey)[4] <- "Creative C."
names(data.survey)[5] <- "Timely Info"
names(data.survey)[6] <- "Save Money: Trans."
names(data.survey)[7] <- "Save Money: Life"
names(data.survey)[8] <- "Task Management"
names(data.survey)[9] <- "Sturdiness"
names(data.survey)[10] <- "Photo"
names(data.survey)[11] <- "Wellness"
names(data.survey)[12] <- "Athlete"
names(data.survey)[13] <- "Style"

# Plot the distribution of importance ratings
# using pairs.panels function in psych package
pairs.panels(data.survey[, 2:14],
             method = "pearson",  # correlation method
             hist.col = "grey60", # color of hist. bins
             density = TRUE,      # show density plots
             lm = TRUE)           # plot the linear fit

# Clustering Consumers based on importance ratings ===================================
# Hierarchical Clustering
# What proximity measure is appropriate? ---------------------------------------
# Standardize the data -> because means and scale differs
data.survey.sc = data.survey[, 2:14] #only the evals
data.survey.sc = scale(data.survey.sc) #center 0 and s.d. is 1
head(data.survey.sc)
summary(data.survey.sc)

# Compute Euclidean distance
dist.eucl <- dist(data.survey.sc) #method by default is euclidean, you can specify manhattan etc.
as.matrix(dist.eucl)[1:6, 1:6] 


# To conduct hierarchical cluster analysis 
?hclust()

#whats the best?
cl.single <- hclust(dist.eucl, method = "single")     # single linkage method
cl.complete <- hclust(dist.eucl, method = "complete") # complete linkage method
cl.average <- hclust(dist.eucl, method = "average")   # average linkage method
cl.centroid <- hclust(dist.eucl, method = "centroid") # centroid linkage method
cl.median <- hclust(dist.eucl, method = "median")     # median linkage method
cl.ward <- hclust(dist.eucl, method = "ward.D2")      # ward's method


# Let's investigate the solutions
# single linkage
plot(as.dendrogram(cl.single), ylim = c(0, 3))
plot(as.dendrogram(cl.single), ylim = c(0, 3),
     leaflab = "none") # suppress the x-axis labels
# size of clusters?
table(cutree(cl.single, 30)) 
#not good

# average linkage
plot(as.dendrogram(cl.average))
rect.hclust(cl.average, k = 6, border = "darkred") 
# size of clusters?
table(cutree(cl.average, 6))
#not good

# centroid linkage
plot(as.dendrogram(cl.centroid))
rect.hclust(cl.centroid, k = 3, border = "darkred") 
# size of clusters?
table(cutree(cl.centroid, 6))
#not good

# median linkage
plot(as.dendrogram(cl.median))
rect.hclust(cl.median, k = 3, border = "darkred") 
# size of clusters?
table(cutree(cl.median, 6))
#not good

# complete linkage
plot(as.dendrogram(cl.complete))
rect.hclust(cl.complete, k = 4, border = "darkred") 
# size of clusters?
table(cutree(cl.complete,6))
#   1   2   3   4   5   6
# 197 62 206 128 185 122
table(cutree(cl.complete,3))
# 382 184 434
# 197 184 434 185

# ward's method
# minimizing the variance within clusters, aim for tighter clusters
# spherical or round clusters
plot(as.dendrogram(cl.ward), leaflab = "none", ylim = c(0, 70))
rect.hclust(cl.ward, k = 3, border = "darkred")
#looks reasonable
# size of clusters?
table(cutree(cl.ward, 3))
#398 189 413
#398 189 284 129 -> BEST
table(cutree(cl.ward, 5))
# 398 189 196 88 129


#WARD's METHOD and COMPLETE LINKAGE both works!
#Wards best
#lets go with Wards Method and 4 Clusters?


# Variance ratio criterion or Calinski-Harabasz (CH) index ---------------------
# we will use fpc package
#additionally help decide on the number of clusters
#vector 2:10, 9 elements
VRC.ward = rep(0, length(2:10)) # initialize
for(k in 2:10){
  VRC.ward[k] <- cluster.stats(d = dist.eucl, 
                               clustering = cutree(cl.ward, k))$ch
}
VRC.ward = VRC.ward[-1]
VRC.ward

VRC.complete = rep(0, length(2:10)) # initialize
for(k in 2:10){
  VRC.complete[k] <- cluster.stats(d = dist.eucl, 
                                   clustering = cutree(cl.complete, k))$ch
}
VRC.complete = VRC.complete[-1]
VRC.complete

# save as a data frame
VRC = data.frame(K = 2:10,  ward = VRC.ward) #complete = VRC.complete,

# reshape to long
VRC = melt(VRC, id.vars = "K")

# plot
ggplot(VRC, aes(x = K, y = value)) +
  geom_point() + geom_line() +
  facet_grid(variable~.) +
  labs(x = "Number of Clusters", 
       y = "Variance Ratio Index") +
  theme_bw(base_size = 14)
ggsave(file="VRC.png", width=8, height=3, dpi=600)  

#6 because theres a peak in the complete linkage?

# Describe the clusters on observable characteristic ---------------------------
# We proceed with complete linkage and 6-cluster solution
# combine cluster solutions with initial data frame
data.survey$cluster <- cutree(cl.ward, 3)
head(data.survey)
dim(data.survey)


#compute the mean
clust.mean <- aggregate(data.survey[, -c(1,39)], 
                        by = list(cluster = data.survey$cluster), 
                        function(x)c(mean = round(mean(x), 2)))
clust.mean
dim(clust.mean)


# visualize differences for ratings
clust.mean_long <- melt(clust.mean[, -c(14:38)], id.vars = "cluster") 
head(clust.mean_long)

ggplot(data = clust.mean_long, aes(x = variable, y = value, 
                                   fill = as.factor(cluster))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "Mean Importance Rating", fill = "Cluster") +
  #scale_fill_grey(start = 0.2, end = 0.8) +
  scale_fill_manual(values = c("grey","steelblue4", "skyblue"))+
  scale_y_continuous(limits = c(0, +6), 
                     breaks = seq(0, 6, by = 1)) +
  theme_bw(base_size = 16)+
  theme(axis.text.x=element_text(angle = 20, hjust = 1))
ggsave(file="MeanRating.png", width=9, height=5, dpi=600)
#"steelblue4", "skyblue"


# WTP distibution across clusters
#ggplot(data = data.survey, aes(x = log(WTP))) +
#  geom_histogram() +
#  facet_grid(cluster~.) +
#  labs(x = "log(Distance traveled)", y = "Absolute Freqeuncy") +
#  theme_bw()


# Non-hierarchical: K-mean Clustering ==========================================
?kmeans
# Input for k-means are the original variables, not distance matrix. 
# need metric x!
# importance rating is metric, works.

# As the k-means initial partition is random, fix the seed for reproducability
set.seed(185) #random number 
cl.kmeans <- kmeans(data.survey.sc, centers = 3) #we conitnue with 3 clusters like before

str(cl.kmeans)

# cluster assignments
cl.kmeans$cluster

# combine with the original data
data.survey$cluster_kmeans <- cl.kmeans$cluster
head(data.survey)
dim(data.survey)

clust.kmean <- aggregate(data.survey[, -c(38,39,40)], 
                         by = list(cluster = data.survey$cluster_kmeans), 
                         function(x)c(mean = round(mean(x), 2)))
clust.kmean
dim(clust.mean)
# visualize differences for satisfaction ratings
clust.kmean_long <- melt(clust.kmean[, -c(2,15:38)], id.vars = "cluster") 

ggplot(data = clust.kmean_long, aes(x = variable, y = value, 
                                    fill = as.factor(cluster))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "Mean Importance Rating", fill = "Cluster") +
  #scale_fill_grey(start = 0.2, end = 0.8) +
  #theme_classic()
  scale_fill_manual(values = c("grey","steelblue4", "skyblue"))+
  scale_y_continuous(limits = c(0, +6), 
                     breaks = seq(0, 6, by = 1)) +
  theme_bw(base_size = 16)+
  theme(axis.text.x=element_text(angle = 20, hjust = 1))
ggsave(file="MeanRating.png", width=9, height=5, dpi=600)



#Look at differences in clusters with Ward
head(data.survey)
dim(data.survey)
t(aggregate(data.survey[, -c(1, 15:40)], 
            by = list(cluster = data.survey$cluster), 
            function(x)c(mean = round(mean(x), 2))))

#Look at differences in clusters with K-Means
t(aggregate(data.survey[, -c(1, 15:40)], 
            by = list(cluster = data.survey$cluster_kmeans), 
            function(x)c(mean = round(mean(x), 2))))

# how well clusters are separate with ward and k-means
clusplot(data.survey.sc, cutree(cl.ward, 4), color = TRUE , shade = TRUE ,
         labels = 6, lines = 0, main = "Ward's method plot")

# k-means
clusplot(data.survey.sc, cl.kmeans$cluster, color = TRUE , shade = TRUE ,
         labels = 6, lines = 0, main = "K-means cluster plot")


table(data.survey$cluster)
table(data.survey$cluster_kmeans) 

head(data.survey)
dim(data.survey)
#fertiger Datensatz

data.classification <- data.survey[,-39]
head(data.classification)
getwd()
write.csv(data.classification, file = "data.classification.csv", row.names = FALSE)

