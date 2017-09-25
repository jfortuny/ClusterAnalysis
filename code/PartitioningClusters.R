library(factoextra)
library(dendextend)
library(igraph)

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

# Visualizing dendrograms
dd <- dist(scale(USArrests), method = "euclidean")
hc <- hclust(dd, method = "ward.D2")
fviz_dend(hc, cex = 0.5,
          main = "Dendrogram - War D2",
          xlab = "Objects", ylab = "Distance", sub = "")
# horizontally
fviz_dend(hc, cex = 0.5, horiz = TRUE)
# Cutting and coloring
fviz_dend(hc, k = 4, cex = 0.5,
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE,
          rect = TRUE,
          rect_border = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          rect_fill = TRUE)
# Color palettes: RColorBrewer or ggsci
fviz_dend(hc, cex = 0.5, k = 4,
          k_colors = "jco")
fviz_dend(hc, cex = 0.5, k = 4,
          k_colors = "jco",
          rect = TRUE,
          rect_border = "jco",
          rect_fill = TRUE)
# Circular dendrogram
fviz_dend(hc, cex = 0.5, k = 4,
          k_colors = "jco",
          type = "circular")
# Phylogenic tree
fviz_dend(hc, cex = 0.5, k = 4,
          k_colors = "jco",
          type = "phylogenic",
          repel = TRUE)
fviz_dend(hc, cex = 0.5, k = 4,
          k_colors = "jco",
          type = "phylogenic",
          repel = TRUE,
          phylo_layout = "layout.gem")

# Zooming in a dendrogram
# 1) Create the full dendrogram
den_plot <- fviz_dend(hc, cex = 0.5, k = 4,
                      k_colors = "jco")
# 2) Get the Dendrogram's data
den_data <- attr(den_plot, "dendrogram")
# 3) Plot the overall dendrogram
print(den_plot)
# 4) Choose subtree level/cut to print
den_cuts <- cut(den_data, h = 5)
# 5) Print a branch
fviz_dend(den_cuts$lower[[1]], main = "Subtree 1")
fviz_dend(den_cuts$lower[[2]], main = "Subtree 2")
fviz_dend(den_cuts$lower[[3]], main = "Subtree 3")
fviz_dend(den_cuts$lower[[4]], main = "Subtree 4")
