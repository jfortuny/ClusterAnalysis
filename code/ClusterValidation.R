library(factoextra)
library(NbClust)
library(fpc)

# Prepare the data: use iris
df <- iris[, -5]
df <- scale(df)
# Cluster
# kmeans
km.res <- eclust(df, "kmeans", k=3, nstart=25, graph = FALSE)
fviz_cluster(km.res, geom = "point",
             ellipse.type = "norm",
             palette = "jco",
             ggtheme = theme_minimal())
# hc
hc.res <- eclust(df, "hclust", k=3, 
                 hc_metric = "euclidean",
                 hc_method = "ward.D2",
                 graph = FALSE)
fviz_dend(hc.res, show_labels = FALSE,
          palette = "jco", as.ggplot = TRUE)

# Validate
# Silhouette validation
fviz_silhouette(km.res, palette = "jco", ggtheme = theme_classic())
# Find the misplaces points
sil <- km.res$silinfo$widths[, 1:3]
neg_sil_index <- which(sil[, "sil_width"] < 0)
sil[neg_sil_index, , drop=FALSE]
# Statistical validation (fpc library)
km_stats <- cluster.stats(dist(df), km.res$cluster)
# This has produced lots of statistical measures of fit
# Dunn index:
km_stats$dunn
# all stats
km_stats
