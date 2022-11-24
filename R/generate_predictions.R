#' Runs the main prediction, which is an ensemble as per Kaufman, King, and Komisarchik (2018)
#'
#'
#' @param features A features object produced by the "generate_features" function
#' @param namecol A string indicating the ncolumn containing the unique shape identifier
#' @param new.models  Default NULL. Should we use the built-in predictive models or new, user-generated ones?

#' @return A vector of length corresponding to number
#'   of polygons in the shp.
#' @export
#' @examples
#' filename = "CnclDist_July2012.shp"
#' namecol = "NAME"
#' shp = read_shapefiles(filename, namecol)
#' features = genreate_features(shp)
#' preds = generate_predictions(features, shp[[3]])

generate_predictions = function(features, namecol, new.models = NULL, all_features = TRUE){
  cols_to_check = (ncol(features)-27):ncol(features)
  idx = complete.cases(features[,cols_to_check])
  features = features[idx,]
  
  if(is.null(new.models)){
    olspreds = lapply(1:6, FUN=function(x) predict(models[[x]], features))
    boostpreds = lapply(7:12, FUN=function(x) predict(models[[x]], features, n.trees=100))
    rfpreds = lapply(13:18, FUN=function(x) predict(models[[x]], features))
    svpreds = lapply(19:24, FUN=function(x) predict(models[[x]], features))
  } else if(!is.null(new.models)){
    olspreds = lapply(1:6, FUN=function(x) predict(new.models[[x]], features))
    boostpreds = lapply(7:12, FUN=function(x) predict(new.models[[x]], features, n.trees=100))
    rfpreds = lapply(13:18, FUN=function(x) predict(new.models[[x]], features))
    svpreds = lapply(19:24, FUN=function(x) predict(new.models[[x]], features))
  }
  preds = c(olspreds, boostpreds, rfpreds, svpreds)
  preds2 = do.call(cbind, preds)
  preds2 = rowMeans(preds2)
  if(all_features == TRUE){
    preds3 = data.frame(district = features, compactness = preds2)
  }else{
    preds3 = data.frame(district = features[,namecol], compactness = preds2)
  }
  preds3$compactness[preds3$compactness > 100] = 100
  preds3$compactness[preds3$compactness < 0] = 0
  return(preds3)
}
