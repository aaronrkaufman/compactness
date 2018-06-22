#' This is the main function in this package, and the only one you should use normally. 
#' 
#' Estimates the compactness and two standard errors for each polygon in a shp file as per Kaufman, King, and Komisarchik (2018)
#'
#' @param shp The filename of a shp file containing district polygons.
#' @param namecol The name of the variable in the shapefile containing each district's unique identifier
#' @param verbose Default TRUE. 
#' @return A vector of length corresponding to number
#'   of polygons in shp.
#' @export
#' @examples
#' get_compactness("CnclDist_July2012.shp")
#' @import sp
#' @import sf
#' @import rgdal
#' @import rgeos
#' @import pracma
#' @import jpeg
#' @import imager
#' @import geosphere
#' @import shotGroups
#' @import RcppRoll
#' @import gbm
#' @import randomForest
#' @import e1071
#' @import cleangeo


get_compactness = function(shp, namecol, verbose=TRUE){ # what optional arguments do I need?
  files = read_shapefiles(shp, namecol, verbose)
  features = generate_features(files, verbose)
  predictions = suppressWarnings(generate_predictions(features, files[[3]])) # everything should be clean now anyway =P
  predictions$ses = predictions$compactness- 2 - 0.01 * predictions$compactness^2
  return(predictions)
}

