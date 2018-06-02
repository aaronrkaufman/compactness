## This script takes the name a .shp file and breaks it apart into its metadata and coordinates.

#' 
#'
#' @param shp The filename of a shp file containing district polygons.
#' @param namecol The ID of the column with the district name in it

#' @return A list of three: the metadata in n x p where n is the number of districts in the shapefile and p is the number of covariates; a list of lists of n where each sublist is a lat-long matrix, and a sublist of length >1 indicates a noncontiguous district; and the name column
#'
#' @examples
#' read_shapefiles("CnclDist_July2012.shp")

get_multi_coord = function(projected, id){
  l = length(projected@polygons[[id]]@Polygons)
  coords = lapply(1:l, FUN=function(x) projected@polygons[[id]]@Polygons[[x]]@coords)
  return(coords)
}

read_shapefiles = function(shp, namecol){ # namecol specifies the id of the column with the district name in it
  metadata <- tryCatch({
    dists = sf::st_read(shp, quiet=T)
    l = nrow(dists)
    metadata = dists
  }, warning = function(war) {
    # warning handler picks up where error was generated
    print(paste("Warning in extracting metadata: ",war))
  }, error = function(err) {
    print(paste("Error in extracting metadata:  ",err))
  }, finally = {
    print(paste("You would like to estimate compactness for ", l, " districts.", sep=""))
  }) 
    
    
  temp <- tryCatch({
    temp = rgdal::readOGR(shp, verbose=F)
    }, warning = function(war) {
      print(paste("Warning in reading coordinates: ",war))
    }, error = function(err) {
      print(paste("Error in reading coordinates:  ",err))
    }, finally = {
      print(paste("Loaded coordinates for ", l, " districts.", sep=""))
    })
      
  proj <- tryCatch({
    proj = proj4string(temp)
    projected =  spTransform(temp, CRS("+proj=longlat +datum=WGS84"))
    }, warning = function(war) {
      print(paste("Warning in projecting coordinates: ",war))
    }, error = function(err) {
      print(paste("Error in projecting coordinates:  ",err))
    }, finally = {
      print(paste("Projected shapefiles for ", l, " districts.", sep=""))
    })    
  
  coords <- tryCatch({ # produces a list of lists
    coords = lapply(1:length(temp), FUN=function(x) get_multi_coord(proj, x))   
    }, warning = function(war) {
      print(paste("Warning in extracting coordinates: ",war))
    }, error = function(err) {
      print(paste("Error in extracting coordinates:  ",err))
    }, finally = {
      print(paste("Successfully extracted coordinates from ", l, " districts.", sep=""))
    })      
  
  return(list(metadata, coords, namecol))
}

