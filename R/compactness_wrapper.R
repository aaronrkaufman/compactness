#' This is the main function in this package, and the only one you should use normally. 
#' 
#' Estimates the compactness and two standard errors for each polygon in a shp file as per Kaufman, King, and Komisarchik (2018)
#'
#' @param shp The filename or filepath of a shp file containing district polygons.
#' @param namecol The name, in quotes, of the variable in the shapefile containing each district's unique identifier
#' @param verbose Default TRUE. 
#' @param new.models Default NULL. Should we generate predictions using the built-in model, or a new user-generated model?
#' @return A data frame where rows correspond to the number of polygons in shp. The columns include the district identifier, the compactness score where high is less compact, and the standard errors.
#' @export
#' @examples
#' get_compactness("CnclDist_July2012.shp", "NAME")
#' @import sp
#' @import sf
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


get_compactness = function(shp, namecol, verbose=TRUE, new.models = NULL){ 
  files = read_shapefiles(shp, namecol, verbose)
  features = generate_features(files, verbose)
  predictions = suppressWarnings(generate_predictions(features = features, namecol = files[[3]], new.models = new.models))
  predictions$ses = predictions$compactness- 2 - 0.01 * predictions$compactness^2 # as per the findings in the appendix
  return(predictions)
}

