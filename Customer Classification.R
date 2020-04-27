library(tidyverse)
library(factoextra)
library(NbClust)
library(cluster)
library(plotly)
data <- read.csv('C:\\Users\\dell\\Downloads\\Mall_customers.csv')

df <- read.csv('C:\\Users\\dell\\Downloads\\Mall_customers.csv')
df %>% glimpse()
df %>% inspectdf::inspect_na()

df$Gender %>% table()

df$Gender <- as.numeric(df$Gender)
df$Gender <- ifelse(df$Gender>1,1,0)
df$Gender %>% table()
df <- df %>% select(-CustomerID)
df %>% glimpse()

# standardize data
df$Gender <- df$Gender %>% as.matrix()
df$Age <- df$Age %>% scale()
df$Spending.Score..1.100. <- df$Spending.Score..1.100. %>% scale()
df$Annual.Income..k.. <- df$Annual.Income..k.. %>% scale()
df %>% glimpse()


# Optimal Number of Clusters ----

df %>% 
  fviz_nbclust(kmeans, method = "wss") +
  labs(subtitle = "Elbow method")  # 4 cluster

df %>% 
  fviz_nbclust(kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")  # 6 cluster

df %>% 
  fviz_nbclust(kmeans, nstart = 50,  method = "gap_stat")+
  labs(subtitle = "Gap statistic method")

library(forcats)
# Fitting K-Means to the data ----
set.seed(123)
kmeans <- df %>% kmeans(centers = 4)
y_kmeans <- kmeans$cluster %>% as_factor()

df %>% clusplot(y_kmeans,
                shade = TRUE,
                color = TRUE,
                labels = 2,
                plotchar = F,
                main = 'Clusters of customers')


# Visualize the result ----
g <- data %>% 
  ggplot(aes(Spending.Score..1.100.,Annual.Income..k..,
             color = y_kmeans)) +
  geom_point(aes(text = Spending.Score..1.100. ),size = 2) + 
  scale_x_continuous(breaks = seq(0,150,10)) +
  scale_y_continuous(breaks = seq(0,150,10)) +
  labs(x="spendinf score", 
       y="income",
       title="Mall Customers",
       subtitle="4 clusters")

g %>% ggplotly(tooltip = "text")
