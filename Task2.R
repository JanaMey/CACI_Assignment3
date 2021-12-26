#load packages
pacman::p_load(reshape2, ggplot2, psych, corrplot, fpc, cluster)

#import data
#sat.df <- read.csv("link einfügen")

# standardize data (mean = 0)
sat.sc = sat.df[, 4:8] #welche zahlen satt 4:8? welche columns? in beispiel von variable 4 bis 8
sat.sc = scale(sat.sc)
head(sat.sc)
summary(sat.sc)

#compute euclidean distance (always eulidean by default with dist-function) --> distances between observations
dist.eucl <- dist(sat.sc)
as.matrix(dist.eucl)[1:6, 1:6]

#mixed data?
dist.gower <- daisy(sat.sc, metric = "gower")
as.matrix(dist.gower)[1:6, 1:6]

#hierarchical cluster analysis --> select best fitting option
cl.single <- hclust(dist.eucl, method = "single")     # single linkage method
cl.complete <- hclust(dist.eucl, method = "complete") # complete linkage method
cl.average <- hclust(dist.eucl, method = "average")   # average linkage method
cl.centroid <- hclust(dist.eucl, method = "centroid") # centroid linkage method
cl.median <- hclust(dist.eucl, method = "median")     # median linkage method
cl.ward <- hclust(dist.eucl, method = "ward.D2")      # ward's method

# method 1: single linkage
plot(as.dendrogram(cl.single), ylim = c(0, 3))
plot(as.dendrogram(cl.single), ylim = c(0, 3),
     leaflab = "none")
     
# size of clusters?
table(cutree(cl.single, 20)) # use different numbers to see chaining effect

#method 2: complete linkage
plot(as.dendrogram(cl.complete))
rect.hclust(cl.complete, k = 3, border = "darkred") #try out with different numbers for k

table(cutree(cl.complete, 3)) #decent Verteilung or not?

#method 3: average linkage
plot(as.dendrogram(cl.average))
rect.hclust(cl.average, k = 3, border = "darkred") 

# size of clusters?
table(cutree(cl.average, 3))

#method 4: centroid linkage
plot(as.dendrogram(cl.centroid))
rect.hclust(cl.centroid, k = 3, border = "darkred") 

# size of clusters?
table(cutree(cl.centroid, 3))

#method 5: median linkage
plot(as.dendrogram(cl.median))
rect.hclust(cl.median, k = 3, border = "darkred") 

# size of clusters?
table(cutree(cl.median, 3))

#method 6: ward's method #should look more reasonable
plot(as.dendrogram(cl.ward))
rect.hclust(cl.ward, k = 3, border = "darkred") 

# size of clusters?
table(cutree(cl.ward, 3))

# Zoom in to the left-most cluster on the dendrogram
plot(cut(as.dendrogram(cl.ward), h = 20)$lower[[1]])

# Variance ratio criterion or Calinski-Harabasz (CH) index --> validate best fitting option (compare ward and complete linkage)
VRC.ward = rep(0, length(2:10)) 
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

# save as a data frame
VRC = data.frame(K = 2:10, complete = VRC.complete, ward = VRC.ward)

# reshape to long
VRC = melt(VRC, id.vars = "K")

# plot
ggplot(VRC, aes(x = K, y = value)) +
  geom_point() + geom_line() +
  facet_grid(variable~.) +
  labs(x = "Number of Clusters", 
       y = "Variance Ratio (Calinski-Harabasz) index") +
  theme_bw()
  
# We proceed with Ward's method and 3-cluster solution
# combine cluster solutions with initial data frame
sat.df$cluster <- cutree(cl.ward, 3)
head(sat.df)
dim(sat.df)

# recode weekend into a dummy --> auf example bezogen
sat.df$weekend <- ifelse(sat.df$weekend == "yes", 1, 0)

clust.mean <- aggregate(sat.df[, -9], 
                        by = list(cluster = sat.df$cluster), 
                        function(x)c(mean = round(mean(x), 2)))
clust.mean
