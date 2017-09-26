library(clValid)
library(factoextra)
library(NbClust)
library(fpc)

# Prepare the data (use iris)
df <- scale(iris[,-5])
# run clValid
# internal measures
clmethods <- c("hierarchical", "kmeans", "pam")
intern <- clValid(df, nClust = 2:6,
                  clMethods = clmethods,
                  validation = "internal")
summary(intern)
# stability measures
clmethods <- c("hierarchical", "kmeans", "pam")
stab <- clValid(df, nClust = 2:6,
                clMethods = clmethods,
                validation = "stability")
optimalScores(stab)
