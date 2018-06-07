# Replication file for Kaufman, King, and Komisarchik (2018)
# This file uses the "compactness" software package

library(devtools)
devtools::install_github("aaronrkaufman/compactness")
library("compactness")

library(grid)
library(foreign)
library(gdata)
library(mosaic)
library(gbm)
library(randomForest)
library(e1071)
library(glmnet)

## Load training labels
load("D:/GitHub/compactness_software/data/training_labels.RData")

# Read shapefiles
sl = "D:/Dropbox/Compactness Shared/Data/Other Shapefiles/both.shp"
namecol = "NAME"
shp = read_shapefiles(sl, namecol)

# Generate compactness features
features = generate_features(shp)

# Calculate Cross-validation
idx = sample(1:6,nrow(bf2), replace=T)
a = c()
resids = list()
bf2 = merge(bf2, df2[,c("district", "pca")], by.x="NAME", by.y="district")
bf2 = bf2[,-c(1:15,31)]
bf2 = bf2[,-11]


for(i in 1:6){
  train = bf2[idx!= i,]
  test = bf2[idx==i,]

  ols <- lm(pca ~ polsby + boyce + hull + corners + sym_x + sym_y + orig_area + xvar + yvar +
              polsby*hull + polsby*sym_x + polsby*sym_y + sym_x*sym_y + varline + polsby*corners + hull*corners +
              polsby*sym_x*sym_y,
            data = train)
  opreds <- predict(ols, newdata = test)

  boost <- gbm(pca ~ .,
               data =train, interaction.depth = 3, n.trees = 2000)
  bpreds <- predict(boost, newdata = test, n.trees = 1000)

  rfm <- randomForest(x = train, y = train$pca, ntree = 2000)
  rfpreds <- predict(rfm, test)

  sv <- svm(pca ~ . , data =train)
  svpreds <- predict(sv, test)

  ensemble = rowMeans(cbind(opreds, bpreds, rfpreds, svpreds))
  a[i] = cor(ensemble, test$pca)
  resids[[i]] = data.frame(district = test$district, truth = test$pca, preds = ensemble, set = i)

  label1 = paste0("rho == ", round(a[i],digits=2))

  temp = data.frame(xv = rank(ensemble), yv=test$pca)


}


# Modeling
ols = list()
boost = list()
rfm = list()
sv = list()
idx = sample(1:6,nrow(bf2), replace=T)
bf2 = merge(bf2, df2[,c("district", "pca")], by.x="NAME", by.y="district")
bf2 = bf2[,-c(1:15,31)]
bf2 = bf2[,-11]



for(i in 1:6){
  train = bf2[idx!=i,]
  test =  bf2[idx==i,]
  
  ols[[i]] <- lm(pca ~ polsby + boyce + hull + corners + sym_x + sym_y + orig_area + xvar + yvar +
                   polsby*hull + polsby*sym_x + polsby*sym_y + sym_x*sym_y + varline + polsby*corners + hull*corners + 
                   polsby*sym_x*sym_y,
                 data = train)
  boost[[i]] <- gbm(pca ~ .,
                    data =train, interaction.depth = 3, n.trees = 1000)
  rfm[[i]] <- randomForest(x = train[,-which(colnames(train)=="pca")], y = train$pca, ntree = 1000)
  sv[[i]] <- svm(pca ~ . , data =train)
}

models = c(ols, boost, rfm, sv)