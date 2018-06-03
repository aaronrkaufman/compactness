#' Runs the main prediction, which is an ensemble as per Kaufman, King, and Komisarchik (2018)
#'
#'
#' @param features A features object produced by the "generate_features" function

#' @return A vector of length corresponding to number
#'   of polygons in the shp.
#'
#' @examples
#' filename = "CnclDist_July2012.shp"
#' namecol = "NAME"
#' shp = read_shapefiles(filename, namecol)
#' features = genreate_features(shp)
#' preds = generate_predictions(features, shp[[3]])

generate_predictions = function(features, namecol){
  olspreds = lapply(1:6, FUN=function(x) predict(models[[x]], features))
  boostpreds = lapply(7:12, FUN=function(x) predict(models[[x]], features, n.trees=100))
  rfpreds = lapply(13:18, FUN=function(x) predict(models[[x]], features))
  svpreds = lapply(19:24, FUN=function(x) predict(models[[x]], features))
  preds = c(olspreds, boostpreds, rfpreds, svpreds)
  preds2 = do.call(cbind, preds)
  preds2 = rowMeans(preds2)
  preds3 = data.frame(district = features[,namecol], compactness = preds2)
  return(preds3)
}
