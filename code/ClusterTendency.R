library(factoextra)
library(clustertend)

# Use iris and a sample from iris
str(iris)
df <- iris[, -5]
# This should not have structure
random_df <- apply(df, 2, function(x) {runif(length(x), min(x), max(x))})
random_df <- as.data.frame(random_df)
# Scale
df <- iris.scaled <- scale(df)
random_df <- scale(random_df)

# Visual inspection
# Since the data contains more than 2 variables we use PCA to reduce dimensionality
fviz_pca_ind(prcomp(df),
             title = "PCA iris dataset",
             habillage = iris$Species,
             palette = "jco",
             geom = "point",
             ggtheme = theme_classic(),
             legend = "bottom")
# Clusters naturally exist in the iris dataset
fviz_pca_ind(prcomp(random_df),
             title = "PCA Random dataset",
             palette = "jco",
             geom = "point",
             ggtheme = theme_classic(),
             legend = "bottom")
# Clusters do not exist naturally in the random dataset

# Assessing clustering tendency
# Hopkins statistic (any value lower than 0.5 is clusterable)
set.seed(1949)
hopkins(df, n = nrow(df) - 1)
set.seed(1949)
hopkins(random_df, n = nrow(df) - 1)
# Visual method: the Visual Assessment of Cluster Tendency (VAT)
fviz_dist(dist(df), show_labels = FALSE) + labs(title = "Iris data")
fviz_dist(dist(random_df), show_labels = FALSE) + labs(title = "Random data")
