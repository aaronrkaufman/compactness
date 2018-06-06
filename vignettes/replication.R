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
features = generate_features(shp)