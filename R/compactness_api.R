#' This function queries the compactness API rather than calculating compactness locally.
#'
#' 
#'
#' @param dir The name of a directory holding the shp, prj, shx, and dbf files.
#' @param namecol The ID of the column with the district name in it
#' @return A list of three: the metadata in n x p where n is the number of districts in the shapefile and p is the number of covariates; a list of lists of n where each sublist is a lat-long matrix, and a sublist of length >1 indicates a noncontiguous district; and the name column
#' @export
#' @examples
#' compactness_api(dir = "shapefiles", namecol = "GEOID")


compactness_api = function(dir, namecol){ # namecol specifies the id of the column with the district name in it
  
  #dir = "D:/github/compactness-software/data"
  #namecol = "DISTRICT"
  
  f = list.files(dir)
  shp_loc = f[grepl(".shp$", f)][2]
  shx_loc = f[grepl(".shx$", f)][2]
  prj_loc = f[grepl(".prj$", f)][2]
  dbf_loc = f[grepl(".dbf$", f)]
  
  if(any(is.null(shp_loc, shx_loc, prj_loc, dbf_loc))){
    stop("Can't find all the necessary files. Are you sure there is an shp, shx, prj, and dbf in the indicated directory?")
  }
  
  post_zip <- httr::POST(
    url = "https://compactness.herokuapp.com/api/compact",
    body = list(
      shp = httr::upload_file(paste0(dir,"/",shp_loc)), 
      shx = httr::upload_file(paste0(dir,"/",shx_loc)), 
      dbf = httr::upload_file(paste0(dir,"/",dbf_loc)), 
      prj = httr::upload_file(paste0(dir,"/",prj_loc)),
      namecol = namecol
    )
  )
  
  if(post_zip$status_code != 200){
    stop("Something went wrong with your API request. Please email us (aaronkaufman@nyu.edu) with your shapefiles for help!")
  }
  
  out = httr::content(post_zip)[[1]]
  out = as.data.frame(do.call(rbind, out))
  
  return(out)
}
