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

# Calculate the sdistance matrix
dist.dfScaled <- dist(dfScaled, method = "euclidean")
mode(dist.dfScaled)
class(dist.dfScaled)
# View a subset of the distance matrix
as.matrix(dist.dfScaled)[1:5,1:5]

# hclust
# Cluster
hc.dfScaled <- hclust(d = dist.dfScaled, method = "ward.D2")
# Visualize clusters in a dendogram
plot(hc.dfScaled)
fviz_dend(hc.dfScaled)
fviz_dend(hc.dfScaled, cex = 0.5)
# Cut the tree to 4 clusters
hc.dfScaled.4c <- cutree(hc.dfScaled, k = 4)
# Visualize these 4 clusters in the dendogram
fviz_dend(hc.dfScaled, cex = 0.5,
          k = 4,
          rect = TRUE)
# Visualize these 4 clusters in a scatterplot
fviz_cluster(list(data = dfScaled, cluster = hc.dfScaled.4c),
             ellipse.type = "convex",
             repel = TRUE,
             show.clust.cent = FALSE,
             ggtheme = theme_minimal())

# Comparing dendorgrams
library(dendextend)
# Cluster
hc1 <- hclust(dist.dfScaled, method = "average")
hc2 <- hclust(dist.dfScaled, method = "ward.D2")
# Create the dendrogram objects
dend1 <- as.dendrogram(hc1)
dend2 <- as.dendrogram(hc2)
# Create a list to hold the dendrograms
dend_list <- dendlist(dend1, dend2)
# Visual comparison with tanglegram
tanglegram(dend1, dend2)
tanglegram(dend1, dend2,
           highlight_distinct_edges = FALSE,
           common_subtrees_color_lines = FALSE,
           common_subtrees_color_branches = TRUE)
# Correlation between dendrograms
cor.dendlist(dend_list, method = "cophenetic")
cor.dendlist(dend_list, method = "baker")
