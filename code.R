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
# head(customer)
summary(customer)


melt(subset(customer, select = -c(CustomerID) )) %>% ggplot(aes(x=variable, y=value)) + 
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
# nrow(customer)
melt(subset(customer, select = -c(CustomerID) )) %>% ggplot(aes(x=variable, y=value)) + 
  geom_boxplot(aes(fill=variable), outlier.colour = NULL) + theme(legend.position="none")


customer_scaled <- customer %>% mutate_at(c("Amount", "Frequency", "LastSeen"), ~(scale(.) %>% as.vector))

customer_scaled = subset(customer_scaled, select = -c(CustomerID) )

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
m1.kmean <- customer_scaled %>% kmeans(k, iter.max = 20, nstart = 25)

customer$Cluster <- as.factor(m1.kmean$cluster)

clustStats <- customer[,2:5] %>%
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

fviz_cluster(m1.kmean, data = customer,choose.vars = c("Frequency","Amount"),
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

fviz_cluster(m1.kmean, data = customer,choose.vars = c("LastSeen","Frequency"),
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)         


colors <- c("#999999", "#E69F00", "#56B4E9")
colors <- colors[as.numeric(customer$Cluster)]
s3d <- scatterplot3d(customer[,2:4], pch = 16, color=colors)
legend("top",legend = paste("Cluster", 1:3),
       col =  c("#999999", "#E69F00", "#56B4E9"), pch = 16)    


centers <- data.frame(c(1:3), m1.kmean$center)

names(centers)[names(centers) == "c.1.3."] <- "Cluster"

head(centers)

colors1 <- c("#999999", "#E69F00", "#56B4E9")
colors1 <- colors1[as.numeric(centers$Cluster)]
colors1
s3d1 <- scatterplot3d(centers[,2:4],  pch = 15, color=colors1)
legend("top",legend = paste("Cluster", 1:3),
       col =  c("#999999", "#E69F00", "#56B4E9"), pch = 16)    