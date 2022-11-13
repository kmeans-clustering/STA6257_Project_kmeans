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
m1.kmean <- customer_scaled %>% kmeans(k, iter.max = 20, nstart = 100)
m2.kmean <- customer_scaled$Amount %>% kmeans(k, iter.max = 20, nstart = 100)
m3.kmean <- customer_scaled$Frequency %>% kmeans(k, iter.max = 20, nstart = 100)
m4.kmean <- customer_scaled$LastSeen %>% kmeans(k, iter.max = 20, nstart = 100)
m5.kmean <- c(customer_scaled$Amount, customer_scaled$Frequency) %>% kmeans(k, iter.max = 20, nstart = 100)
m1.kmean$tot.withinss

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

head(customer_scaled)
colors <- c("#999999", "#E69F00", "#56B4E9")
colors <- colors[as.numeric(m1.kmean$cluster)]
s3d <- scatterplot3d(customer_scaled, pch = 16, color=colors)
legend("top",legend = paste("Cluster", 1:3),
       col =  c("#999999", "#E69F00", "#56B4E9"), pch = 16)    
s3d$points3d(m1.kmean$centers,
             col = "red", type = "h", pch = 8)




head(customer)
customer %>% ggplot(aes(x=Cluster, y=Amount, fill=Cluster)) + geom_boxplot()+  theme_classic()  + 
  labs(title = "Clusters based on Amount Spent", subtitle = "K-Means Model m1.kmeans")

customer %>% ggplot(aes(x=Cluster, y=Frequency, fill=Cluster)) + geom_boxplot() +  theme_classic() + 
  labs(title = "Clusters based on Number of transactions", subtitle = "K-Means Model m1.kmeans")

customer %>% ggplot(aes(x=Cluster, y=LastSeen, fill=Cluster)) + geom_boxplot() +  theme_classic() + 
  labs(title = "Clusters based on customer last seen shopping", subtitle = "K-Means Model m1.kmeans")





m1.kmean
