#' This is the main function in this package, and the only one you should use normally. 
#' 
#' Estimates the compactness and two standard errors for each polygon in a shp file as per Kaufman, King, and Komisarchik (2018)
#'
#' @param shp The filename of a shp file containing district polygons.
#' @param namecol The name of the variable in the shapefile containing each district's unique identifier
#' @return A vector of length corresponding to number
#'   of polygons in shp.
#'
#' @examples
#' get_compactness("CnclDist_July2012.shp")


get_compactness = function(shp, namecol){ # what optional arguments do I need?
  files = read_shapefiles(shp, namecol)
  features = generate_features(files)
  predictions = suppressWarnings(generate_predictions(features, files[[3]])) # everything should be clean now anyway =P
  ses = predictions- 2 - 0.01 * predictions^2
}

