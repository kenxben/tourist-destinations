#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# KB 2020
# Clustering by features
# Step 4 : calculate metrics from cities data
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

library(data.table)
library(magrittr)
library(geosphere)
library(ggplot2)
library(factoextra)
library(cluster)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Load data
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

destinations <- read.csv("results/clusters.csv", row.names = 2, encoding = 'latin1')


#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Kmeans clustering
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

# subset relevant columns
df <- destinations[2:9]

set.seed(123)

# metrics for optimal number of clusters
fviz_nbclust(df, kmeans, method = "wss")
fviz_nbclust(df, kmeans, method = "silhouette")

# kmeans with 3 clusters
kmeans_clusters <- kmeans(destinations[2:9], centers = 3)

viz <- fviz_cluster(kmeans_clusters, data = df, main="")
saveRDS(viz, "results/kmeans_viz.rds")
