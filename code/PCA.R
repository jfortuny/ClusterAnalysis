library(FactoMineR)
library(factoextra)

data("decathlon")
head(decathlon)
RES.PCA <- PCA(decathlon, quanti.sup = 11:12, quali.sup = 13)
plot(RES.PCA, habillage = 13)
barplot(RES.PCA$eig[,1], main="Eigenvalues",
         names.arg = paste("Dim", 1:nrow(RES.PCA$eig), sep = ""))
plot(RES.PCA, choix = "var", axes=c(3,4), lim.cos2.var = 0)
dimdesc(RES.PCA, proba=0.2)


# Everitt - A Handbook of Statistical Analyses
data("heptathlon", package = "HSAUR2")
str(heptathlon)
# Recode variables so high values are "Good"
heptathlon$hurdles <- max(heptathlon$hurdles) - heptathlon$hurdles
heptathlon$run200m <- max(heptathlon$run200m) - heptathlon$run200m
heptathlon$run800m <- max(heptathlon$run800m) - heptathlon$run800m
# scatter plot matrix
pairs(~hurdles+highjump+shot+run200m+longjump+javelin+run800m, data=heptathlon)
# examine correlations
round(cor(heptathlon[, -ncol(heptathlon)]),2)
# remove the outlier
heptathlon <- heptathlon[-grep("PNG",rownames(heptathlon)),]
round(cor(heptathlon[, -ncol(heptathlon)]),2)
pairs(~hurdles+highjump+shot+run200m+longjump+javelin+run800m, data=heptathlon)

heptathlon.pca <- prcomp(heptathlon[, -ncol(heptathlon)], scale. = TRUE)
heptathlon.pca
summary(heptathlon.pca)
plot(heptathlon.pca)
