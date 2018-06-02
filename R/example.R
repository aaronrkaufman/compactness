setwd("D:/GitHub/compactness_software/compactness/data")

## Required libraries
library(sp)
library(sf)
library(rgdal)
library(raster)
library(rgeos)
library(png)
library(pracma)
library(jpeg)
library(imager)
library(geosphere)
library(shotGroups)
library(RcppRoll)
library(gbm)
library(randomForest)
library(e1071)

source("../R/generate_features.R")
source("../R/generate_predictions.R")
source("../R/compactness_wrapper.R")
source("../R/bounding_geometry.R")
source("../R/read_shapefiles.R")
source("../R/harris_variations.R")

# Load models
load("../data/models_5_27_18.RData")

# Load test data
shp = "20110727_q2_congressional_final_draft.shp" # California 2011 Congressional district maps
namecol = "DISTRICT"
shp = read_shapefiles(shp, namecol)

# Altnerate test data
# shp = "CnclDist_July2012.shp"  # LA City Council districts
# shp = read_shapefiles(shp, "DISTRICT")

# Generate features
features = generate_features(shp)

# Generate predictions and SEs
predictions = suppressWarnings(generate_predictions(features, namecol))
ses = predictions- 2 - 0.01 * predictions^2