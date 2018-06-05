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

## Load training data