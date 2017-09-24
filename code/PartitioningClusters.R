library(factoextra)

data("USArrests")
df <- USArrests
str(df)
head(df)
dim(df)
summary(df)

# Remove NAs
df <- na.omit(df)
# Scale
dfScaled <- scale(df)
mode(dfScaled)
class(dfScaled)

# K-Means

# How many clusters?
fviz_nbclust(dfScaled, FUNcluster = kmeans, method = "wss")
# Cluster
set.seed(1949)
km.dfScaled <- kmeans(dfScaled, 4, nstart = 50)
km.dfScaled$size
# Visualize clusters
fviz_cluster(km.dfScaled, data = dfScaled)
fviz_cluster(km.dfScaled, data = dfScaled,
             ellipse.type = "euclid")
fviz_cluster(km.dfScaled, data = dfScaled,
             ellipse.type = "t",
             repel = TRUE,
             start.plot = TRUE,
             ggtheme = theme_minimal())
# star.plot doesn't seem to be working in RStudio for kmeans

# K-Medoids
library(fpc)
library(cluster)

# How many clusters?
fviz_nbclust(dfScaled, FUNcluster = pam, method = "silhouette")
# Cluster
pam.dfScaled <- pam(dfScaled, 2)
# Visualize clusters
fviz_cluster(pam.dfScaled)
fviz_cluster(pam.dfScaled, data = dfScaled,
             ellipse.type = "t")
fviz_cluster(pam.dfScaled, data = dfScaled,
             ellipse.type = "t",
             repel = TRUE)
fviz_cluster(pam.dfScaled, data = dfScaled,
             ellipse.type = "t",
             repel = TRUE,
             star.plot = TRUE)

# So what if we tried 4 clusters? Would it match kmeans?
pam.dfScaled <- pam(dfScaled, 4)
# Visualize clusters
fviz_cluster(pam.dfScaled)
# It did!

# Using pamk (from fpc)
pamk.dfScaled <- pamk(dfScaled)


# CLARA (accepts missing values)
# How many clusters?
fviz_nbclust(dfScaled, FUNcluster = clara, method = "silhouette")
# Cluster
clara.dfScaled <- clara(dfScaled, 2, samples = 50, pamLike = TRUE)
# Visualize clusters
fviz_cluster(clara.dfScaled)
fviz_cluster(clara.dfScaled,
             ellipse.type = "t")
fviz_cluster(clara.dfScaled,
             ellipse.type = "t",
             geom = "point", pointsize = 1)

# Now CLARA with 4 clusters
# Cluster
clara.dfScaled <- clara(dfScaled, 4, samples = 50, pamLike = TRUE)
# Visualize clusters
fviz_cluster(clara.dfScaled)
