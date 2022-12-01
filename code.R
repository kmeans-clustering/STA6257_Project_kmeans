library('readxl')
library('papeR')
library('Hmisc')
library('reshape2')
library('plotly')
library('factoextra')
library('scatterplot3d')
library(dplyr)
library(tidyverse)
library(knitr)
library(ggthemes)
library(ggrepel)
library(dslabs)
library(xtable)
library(kableExtra)
library(Hmisc)
library(qwraps2)
library(cowplot)
library(cluster)


filename <- "online_retail_II.xlsx"
df = read_excel(filename) 

names(df)[names(df) == 'Customer ID'] <- 'CustomerID'

head(df) %>%
  kbl() %>%
  kable_paper("hover", full_width = T)

df=na.omit(df, cols="CustomerID")
data = subset(df, select = -c(Description, Country, StockCode) )

data$Amount = data$Quantity * data$Price

customer_monetary <- as.data.frame(data %>%
                                     group_by(CustomerID) %>%
                                     summarise(Amount = sum(Amount)))


customer_frequency <- as.data.frame(data %>% group_by(CustomerID) %>% summarise(Invoice = n()))

names(customer_frequency)[names(customer_frequency) == "Invoice"] <- "Frequency"

data$LastSeen =as.integer(difftime(max(data$InvoiceDate), data$InvoiceDate, units = "days"))

customer_lastseen <-  as.data.frame(data %>% group_by(CustomerID) %>% summarise(LastSeen = min(LastSeen)))

customer <- merge(customer_monetary, customer_frequency, by = 'CustomerID')
customer <- merge(customer, customer_lastseen, by = 'CustomerID')



head(customer)
summary(customer)


melt(customer[,2:4]) %>% ggplot(aes(x=variable, y=value)) + 
  geom_boxplot(aes(fill=variable), outlier.colour = NULL) + theme(legend.position="none")


# create detect outlier function
detect_outlier <- function(x) {
  
  # calculate first quantile
  Quantile1 <- quantile(x, probs=.25)
  
  # calculate third quantile
  Quantile3 <- quantile(x, probs=.75)
  
  # calculate inter quartile range
  IQR = Quantile3-Quantile1
  
  # return true or false
  x > Quantile3 + (IQR*1.5) | x < Quantile1 - (IQR*1.5)
}

# create remove outlier function
remove_outlier <- function(dataframe,
                           columns=names(dataframe)) {
  
  # for loop to traverse in columns vector
  for (col in columns) {
    
    # remove observation if it satisfies outlier function
    dataframe <- dataframe[!detect_outlier(dataframe[[col]]), ]
  }
  
  return(dataframe)
  # print("Remove outliers")
  # print(dataframe)
}

customer <- remove_outlier(customer, c('Amount','Frequency','LastSeen'))
# summary(customer)
# customer
# nrow(customer_details)
melt(customer[,2:4]) %>% ggplot(aes(x=variable, y=value)) + 
  geom_boxplot(aes(fill=variable), outlier.colour = NULL) + theme(legend.position="none")

customer_details <- customer
customer <-  customer %>% select(-CustomerID)
head(customer)
head(customer_details)

customer_scaled <- customer %>% mutate_at(c("Amount", "Frequency", "LastSeen"), ~(scale(.) %>% as.vector))

nrow(customer)
nrow(customer_details)
nrow(customer_scaled)
head(customer_scaled)
summary(customer_scaled)

fviz_nbclust(customer_scaled, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)+
  labs(subtitle = "Elbow Method")

fviz_nbclust(customer_scaled, kmeans, method = "silhouette")+ theme_classic()+
  labs(subtitle = "Silhouette Method")


# Gap Statistics Method
# set.seed(123)
# gap_stat <- clusGap(customer_scaled, FUN = kmeans, nstart = 25,
#                     K.max = 10, B = 50)
# # Print the result
# #print(gap_stat, method = "firstmax")
# 
# fviz_gap_stat(gap_stat)


set.seed(123)
k = 3
m1.kmean <- customer_scaled %>% kmeans(k, iter.max = 10, nstart = 100)
# m2.kmean <- customer_scaled$Amount %>% kmeans(k, iter.max = 20, nstart = 100)
# m3.kmean <- customer_scaled$Frequency %>% kmeans(k, iter.max = 20, nstart = 100)
# m4.kmean <- customer_scaled$LastSeen %>% kmeans(k, iter.max = 20, nstart = 100)
# m5.kmean <- c(customer_scaled$Amount, customer_scaled$Frequency) %>% kmeans(k, iter.max = 20, nstart = 100)

m1.kmean$cluster
m1.kmean$tot.withinss

# 66%, 3725.543

customer$Cluster <- as.factor(m1.kmean$cluster)
customer_details$Cluster <- as.factor(m1.kmean$cluster)

summary(customer)
clustStats <- customer %>%
  group_by(Cluster) %>%
  summarise_all("mean")


clustStats %>%
  kbl() %>%
  kable_paper("hover", full_width = T)


head(customer_scaled)
fviz_cluster(m1.kmean, data = customer_scaled,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)


fviz_cluster(m1.kmean, data = customer_scaled,choose.vars = c("LastSeen","Amount"),
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

fviz_cluster(m1.kmean, data = customer_scaled,choose.vars = c("Frequency","Amount"),
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

fviz_cluster(m1.kmean, data = customer_scaled,choose.vars = c("LastSeen","Frequency"),
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)         




# head(customer_scaled)
colors <- c("#9f00e6", "#e69f00", "#00e69f")
colors <- colors[as.numeric(m1.kmean$cluster)]
s3d <- scatterplot3d(customer_scaled, pch = 16, color=colors)
legend("top",legend = paste("Cluster", 1:3),
       col =  c("#9f00e6", "#e69f00", "#00e69f"), pch = 16)    
centers <- data.frame(m1.kmean$centers)
s3dp <- s3d$points3d(centers,
             col = c("#000000","#0e0a00","#000e0a"), type = "h", pch = 8)



head(customer)
customer %>% ggplot(aes(x=Cluster, y=Amount, fill=Cluster)) + geom_boxplot()+  theme_classic()  + 
  labs(title = "Clusters based on Amount Spent", subtitle = "K-Means Model m1.kmeans")

customer %>% ggplot(aes(x=Cluster, y=Frequency, fill=Cluster)) + geom_boxplot() +  theme_classic() + 
  labs(title = "Clusters based on Number of transactions", subtitle = "K-Means Model m1.kmeans")

customer %>% ggplot(aes(x=Cluster, y=LastSeen, fill=Cluster)) + geom_boxplot() +  theme_classic() + 
  labs(title = "Clusters based on customer last seen shopping", subtitle = "K-Means Model m1.kmeans")



nrow(customer)

nrow(customer %>% filter(Cluster == 1))
nrow(customer %>% filter(Cluster == 2))
nrow(customer %>% filter(Cluster == 3))

m1.kmean

# https://dl.acm.org/doi/10.5555/1283383.1283494
#https://towardsdatascience.com/try-this-simple-trick-to-improve-your-clustering-b2d5d502039b
my_kmeans <- function(df, k, n_iterations, init = c("kmeans++", "random")) {
  # Check which initialization should be done
  init <- match.arg(init)
  
  # Helper function for euclidean distance
  euclidean_distance <- function(p1, p2) {
    dist <- sqrt(sum((p1-p2)^2))
    return(dist)
  }
  
  # Helper function to calculate distances between all points and all centers
  calculate_distances <- function(df, centers) {
    distances <- matrix(NA, nrow = nrow(df), ncol = nrow(centers))
    for (object_id in 1:nrow(df)) {
      for (center_id in 1:nrow(centers)) {
        distances[object_id, center_id] <- euclidean_distance(df[object_id, ], centers[center_id, ])
      }
    }
    return(distances)
  }
  
  if (init == "random") {
    # Choose all centers randomly
    centers <- df[sample(nrow(df), k, replace = FALSE), ]
  } else if (init == "kmeans++") {
    # Initialize according to kmeans++ algorithm
    
    # Choose first center randomly
    next_center_id <- sample(seq_len(nrow(df)), 1)
    centers <- df[next_center_id, ]
    non_centers <- df[-next_center_id, ]
    
    # Choose next centers with probabilities proportional to their distance to 
    # the closest center. Higher distance equals higher probability.
    while(nrow(centers) < k) {
      distances <- calculate_distances(non_centers, centers)
      distances <- apply(distances, 1, min)
      probabilities <- distances/max(distances)
      next_center_id <- sample(seq_len(nrow(non_centers)), 1, prob = probabilities)
      next_center <- non_centers[next_center_id, ]
      centers <- rbind(centers, next_center)
      non_centers <- non_centers[-next_center_id, ]
    }
  }
  
  # Perform n iterations
  iteration <- 1
  while(iteration < n_iterations) {
    # Calculate distance of each point to each center
    distances <- calculate_distances(df, centers)
    
    # Assign each point to the closest center
    cluster_id <- apply(distances, 1, which.min)
    
    # Calculate new centers
    for (i in seq_len(k)) {
      this_cluster <- df[cluster_id == i,]
      centers[k, ] <- colMeans(this_cluster)
    }
    
    iteration <- iteration + 1
  }
  
  return(cluster_id)
}


m2.kmean <- my_kmeans(customer_scaled, 3, 10, "kmeans++")
m2.kmean
m2 <- m2.kmean
names(m2) <- c(1:length(m2))
m1.kmean$cluster
typeof(m2.kmean)

m2 <- data.frame(cluster = m2.kmean)
str(m1.kmean$cluster)
length(m2)
customer <- customer_details[,1:4]
customer$Cluster <- as.factor(m2.kmean)
customer_details$Cluster <- as.factor(m2.kmean)
c1 <- c(1:3)
names(c1) <- c("a","b", "c")


m3.kmean <- list(cluster = m2.kmean)
m3.kmean$cluster
summary(customer)
clustStats <- customer %>%
  group_by(Cluster) %>%
  summarise_all("mean")


class(m1.kmean)

clustStats %>%
  kbl() %>%
  kable_paper("hover", full_width = T)


diss = dist(m2)
m3.kmean <- pam(diss, 4)
m3.kmean$cluster = m2

head(customer_scaled)
fviz_cluster(m3.kmean, data = customer_scaled,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)


fviz_cluster(m2.kmean, data = customer_scaled,choose.vars = c("LastSeen","Amount"),
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

fviz_cluster(m2.kmean, data = customer_scaled,choose.vars = c("Frequency","Amount"),
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

fviz_cluster(m2.kmean, data = customer_scaled,choose.vars = c("LastSeen","Frequency"),
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)         




# head(customer_scaled)
colors <- c("#9f00e6", "#e69f00", "#00e69f")
colors <- colors[as.numeric(m2.kmean$cluster)]
s3d <- scatterplot3d(customer_scaled, pch = 16, color=colors)
legend("top",legend = paste("Cluster", 1:3),
       col =  c("#9f00e6", "#e69f00", "#00e69f"), pch = 16)    
# centers <- data.frame(m2.kmean$centers)
# s3dp <- s3d$points3d(centers,
#                      col = c("#000000","#0e0a00","#000e0a"), type = "h", pch = 8)


distance <- get_dist(head(customer[,5:5]))
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
