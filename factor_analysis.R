#Exploratory Factor Analysis: 

library(corrplot)
library(gplots)
library(RColorBrewer)
library(nFactors)
library(GPArotation)


brandrattings <- read.csv("branddata.csv", header = T)
head(brandrattings)
tail(brandrattings)

#data structure and quality check:
summary(brandrattings)
str(brandrattings)

#rescale the data:
brandsclaed <- brandrattings
brandsclaed[,1:9] <- scale(brandrattings[,1:9])
summary(brandsclaed)


#correalation plot to inspect bivariate relationship among the variables:
corrplot(cor(brandsclaed[,1:9]), order = "hclust")
#we can observe three general clusters


#business question: what is the average position of the brand on each adjective?
brand.aggr <- aggregate(. ~ brand, data = brandsclaed, mean)
rownames(brand.aggr) <- brand.aggr[,1]
brand.aggr<- brand.aggr[,-1]
brand.aggr

# create a heatmap to visualize aggregate:
brand.heatmap <- heatmap.2(as.matrix(brand.aggr),
                           col = brewer.pal(9,"GnBu"), 
                           trace = "none",
                           key = F,
                           dend = "none",
                           main = "\n\n\n\n\nBrand Atributes")

#Exploratory Factor Analysis: Determine number of factors-->
nScree(brandsclaed[,1:9])
eigen(cor(brandsclaed[,1:9]))$values


#nScree and eigen values suggest that we can model with two or three factors
#we try both and check which one is better
twofactor <- factanal(brandsclaed[,1:9], factors = 2)
twofactor
threefactor <- factanal(brandsclaed[,1:9], factors = 3)
threefactor
#three factor model distinctively loads on three factors


brand.oblimin <- factanal(brandsclaed[,1:9],
                          factors = 3,
                          rotation = "oblimin")
brand.oblimin
brand.oblimin.heatmap <- heatmap.2(brand.oblimin$loadings,
                                   col = brewer.pal(9,"Greens"),
                                   trace = "none", key = F,
                                   dend = "none",
                                   Colv = F, cexCol = 1.2,
                                   main = "\n\n\n\n\nFactor loadings for brand adjectives")


#Factor Scores for brands:
brand.oblimin.score <- factanal(brandsclaed[,1:9], 
                                factors = 3,
                                rotation = "oblimin",
                                scores = "Bartlett")
brand.scores <- data.frame(brand.oblimin.score$scores)
brand.scores$brand <- brandsclaed$brand
head(brand.scores)
brand.fa.aggr <- aggregate(.~ brand, data = brand.scores, mean)
rownames(brand.fa.aggr) <- brand.fa.aggr[,1]
brand.fa.aggr <- brand.fa.aggr[,-1]
names(brand.fa.aggr) <- c("Leader", "Value", "Latest")
brand.fa.aggr
brand.fa.aggr.heatmap <- heatmap.2(as.matrix(brand.fa.aggr),
                                   col = brewer.pal(9,"GnBu"),
                                   trace = "none", key = F,
                                   dend = "none",
                                   cexCol = 1.2,
                                   main = "\n\n\n\n\nFactor Scores Aggregated By Brands")








