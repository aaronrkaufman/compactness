## Just a wrapper for the main function

#' Estimates the compactness and two standard errors for each polygon in a shp file as per Kaufman, King, and Komisarchik (2018)
#'
#' @param shp The filename of a shp file containing district polygons.

#' @return A vector of length corresponding to number
#'   of polygons in shp.
#'
#' @examples
#' get_compactness("CnclDist_July2012.shp")


get_compactness = function(shp){ # what optional arguments do I need?
  ##load models
  load("./models/compactness_models.RData")
  files = read_shapefiles(shp)
  features = generate_features(files)
  predictions = suppressWarnings(generate_predictions(features)) # everything should be clean now anyway =P
  ses = predictions- 2 - 0.01 * predictions^2
}

