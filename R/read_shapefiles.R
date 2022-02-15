#' This script takes the name a .shp file and breaks it apart into its metadata and coordinates.
#'
#' 
#'
#' @param shp The filename of a shp file containing district polygons.
#' @param namecol The ID of the column with the district name in it
#' @param verbose Default TRUE. Inherited from comapactness_wrapper(). 
#' @return A list of three: the metadata in n x p where n is the number of districts in the shapefile and p is the number of covariates; a list of lists of n where each sublist is a lat-long matrix, and a sublist of length >1 indicates a noncontiguous district; and the name column
#' @export
#' @examples
#' read_shapefiles("CnclDist_July2012.shp")


read_shapefiles = function(shp, namecol, verbose=TRUE){ # namecol specifies the id of the column with the district name in it
  
  try(if(!file.exists(shp)) stop("shp argument must be a filepath!"))
  
  metadata <- tryCatch({
    dists = sf::st_read(shp, quiet=T)
    l = nrow(dists)
    metadata = data.frame(dists)
    metadata = as.data.frame(metadata[,-ncol(metadata)])
    #if(verbose) print(paste("You would like to estimate compactness for ", l, " districts.", sep=""))
  }, warning = function(war) {
    # warning handler picks up where error was generated
    print(paste("Warning in extracting metadata: ",war))
  }, error = function(err) {
    print(paste("Error in extracting metadata:  ",err))
    break
  })
  
  
  namecol_exists <- tryCatch({
    test = metadata[,namecol]
    if(verbose) print("Successfully located the identifier column. ")
  }, warning = function(war) {
    print("Something weird is going on. Check your namecol input!")
  }, error = function(err) {
    print("Your namecol does not exist in the data set.")
    break
  })
    
    
  temp <- tryCatch({
    temp = rgdal::readOGR(shp, verbose=F)
    if(verbose) print(paste("Loaded coordinates for ", l, " districts.", sep=""))
    }, warning = function(war) {
      print(paste("Warning in reading coordinates: ",war))
    }, error = function(err) {
      print(paste("Error in reading coordinates:  ",err))
      break
    })
      
  proj <- tryCatch({
    proj = sp::proj4string(temp)
    projected =  sp::spTransform(temp, sp::CRS("+proj=longlat +datum=WGS84"))
    #if(verbose) print(paste("Projected shapefiles for ", l, " districts.", sep=""))
#    }, warning = function(war) {
#      print(paste("Warning in projecting coordinates: ",war))
    }, error = function(err) {
      print(paste("Error in projecting coordinates:  ",err))
      break
    })
  
  #areas = sapply(slot(proj, "polygons"), slot, "area") * 1000000
  #metadata$district_area = areas
  # I need area calculated in here. And it needs to identify hole polygons.
  
  coords <- tryCatch({ # produces a list of lists
    coords = lapply(1:length(temp), FUN=function(x) get_multi_coord(proj, x))
    #if(verbose) print(paste("Successfully extracted coordinates from ", l, " districts.", sep=""))   
    #}, warning = function(war) {
    #  print(paste("Warning in extracting coordinates: ",war))
    }, error = function(err) {
      print(paste("Error in extracting coordinates:  ",err))
      break
    })      
  
  out = structure(list(metadata, coords, namecol), class="compactnessShapefile")
  
  return(out)
}
