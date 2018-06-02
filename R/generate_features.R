## Wraps the functions which generate subsets of the compactness features

#' Note that this function takes some time to run, especially with larger sets of districts.
#'
#' @param shp A shapefile object produced by "read_shapefiles"

#' @return A data frame, n by p, where n is the number of districts in the shapefile, and p is 17 plus the number of columns in the metadata.
#'
#' @examples
#' filename = "CnclDist_July2012.shp"
#' shp = read_shapefiles(filename)
#' features = genreate_features(shp)


## I Can pull a lot of this from previous work (application_measures.R)

crack_shp = function(coord1){  # a list of polygon coords; list of length 1 in the case of contiguous districts
  if(length(coord1)==1){
    df = data.frame(coord1)
    colnames(df) = c("x", "y")
    df = data.frame(df)
    centroid_x = mean(df$x, na.rm=T)
    centroid_y = mean(df$y, na.rm=T)
    
    lines = sapply(1:nrow(df), FUN=function(x)  dist(rbind(df[x,], df[x-1,])))
    jagged = mean(unlist(lines))/sum(unlist(lines)) # added on 7/17/17
    
    radii = sapply(1:nrow(df), FUN=function(x)  dist(rbind(df[x,],  c(centroid_x, centroid_y))))
    
    boyce = mean(abs(radii - mean(radii))/mean(radii))
    maxx = max(df$x)
    minx = min(df$x)
    maxy = max(df$y)
    miny = min(df$y)
    lenwid = abs(maxx-minx)/abs(maxy - miny)
    #area = polyarea(df$x, df$y)
    return(c(nrow(df), var(df$x), var(df$y), var(df$x)/var(df$y),
             mean(unlist(lines)), var(unlist(lines)), boyce, lenwid, jagged, length(coord1)))
  } else {
    line1 = c()
    for(i in 1:length(coord1)){
      df = coord1[[i]]
      colnames(df) = c("x", "y")
      df = data.frame(df)
      lines = sapply(1:nrow(df), FUN=function(x)  dist(rbind(df[x,], df[x-1,])))
      line1[[i]] = lines
    }
    df = do.call(rbind,coord1)
    colnames(df) = c("x", "y")
    df = data.frame(df)
    centroid_x = mean(df$x, na.rm=T)
    centroid_y = mean(df$y, na.rm=T)
    radii = sapply(1:nrow(df), FUN=function(x)  dist(rbind(df[x,],  c(centroid_x, centroid_y))))
    
    boyce = mean(abs(radii - mean(radii))/mean(radii))
    maxx = max(df$x)
    minx = min(df$x)
    maxy = max(df$y)
    miny = min(df$y)
    lenwid = abs(maxx-minx)/abs(maxy - miny)
    jagged = mean(unlist(lines))/sum(unlist(lines))
    #area = polyarea(df$x, df$y)
    return(c(nrow(df), var(df$x), var(df$y), abs(1-var(df$x)/var(df$y)), # added this last one 5/25/18
             mean(unlist(line1)), var(unlist(line1)), boyce, lenwid, jagged, length(coord1)))
    
  }
}

get_first_features = function(shp){
  metadata = data.frame(shp[[1]])
  xy = shp[[2]]
  namecol = shp[[3]]
  pointsvars_sl = lapply(xy, crack_shp)
  pointsvars_sl = do.call(rbind, pointsvars_sl)
  pointsvars_sl = data.frame(pointsvars_sl)
  colnames(pointsvars_sl) = c("points", "var_xcoord", "var_ycoord", "varcoord_ratio",
                              "avgline", "varline", "boyce", "lenwid", "jagged", "parts")
  pointsvars_sl$district = metadata[,namecol]
  return(pointsvars_sl)
}


## But I'll have to figure out how to do the arc features here
get_all_bound_features = function(shp){
  temp = lapply(1:nrow(shp[[1]]), FUN=function(x) get_one_bound_feature(shp[[2]][[x]]))
  out = do.call(rbind, temp)
  return(out)
}


## One part of this code needs to write JPGs to disk, then call a python script (from R) to generate significant corners
## To do this, it should be easy to plot the shape with no labels or axes
get_corners_features = function(shp){
  temp = lapply(1:nrow(shp[[1]]), FUN=function(x) get_one_corner(shp[[2]][[x]]))
  out = do.call(rbind, temp)
  return(out)
}

## Here I call a function I source from Harris.R
get_one_corner = function(xy){
  full = do.call(rbind, xy)
  width = max(full[,1]) - min(full[,1])
  height = max(full[,2]) - min(full[,2])
  ratio = width/height
  jpeg("temp.jpg", height=1000, width = 1000*ratio)
  plot(0, xlim=c(min(full), max(full[,1])),
       ylim = c(min(full[,2]), max(full[,2])),
       xaxt='n', yaxt='n', xlab=NA, ylab=NA, bty='n')
  for(i in 1:length(xy)){
    polygon(x = xy[[i]][,1], y=xy[[i]][,2], col="grey", border = "grey")
  }
  dev.off()
  # Now how do I do this in R...
  corners_out = harris3(img = "temp.jpg")
  
  ## I need to output the number of corners, the xvar of them, and the yvar of them
  return(c(corners=nrow(corners_out), xvar = var(corners_out[,1]), yvar=var(corners_out[,2])))
}



## And finally, the symmetry features
get_symmetry_features = function(shp){
  coords = shp[[2]]
  temp = lapply(1:length(coords), FUN=function(x) get_one_symmetry(coords[[x]]))
  out = do.call(rbind, temp)
  return(out)
}

get_one_symmetry = function(xy){
  if(length(xy) > 1){
    return(get_one_symmetry_noncontig(xy))
  } else {
    return(get_one_symmetry_contig(xy[[1]]))
  }
}

get_one_symmetry_noncontig = function(xy){
  # get centroid, get area
  full = do.call(rbind, xy)
  centroid = c(mean(full[,1]), mean(full[,2]))
  dist_area = sum(sapply(xy, FUN=function(x) areaPolygon(x)/1000000))
  
  # make regular sp object
  orig = data.frame(full)
  colnames(orig) = c("x", "y")
  orig$poly = unlist(sapply(1:length(xy), FUN=function(x) rep(x, nrow(xy[[x]]))))
  
  # get x-flipped coords, make sp object
  xsym_df = orig
  xsym_df$x =xsym_df$x - 2*(xsym_df$x - centroid[1])
  coordinates(xsym_df) <- ~x+y
  p1 = lapply(1:length(xy), FUN=function(x) Polygon(xsym_df[xsym_df$poly==x,]))
  xsym2 = SpatialPolygons(list(Polygons(p1, ID="x")))
  
  # get y-flipped coords, get area, make sp object
  ysym_df = orig
  ysym_df$y =ysym_df$y - 2*(xsym_df$y - centroid[2])
  coordinates(ysym_df) <- ~x+y
  p2 = lapply(1:length(xy), FUN=function(x) Polygon(ysym_df[ysym_df$poly==x,]))
  ysym2 = SpatialPolygons(list(Polygons(p2, ID="y")))
  
  # finish setup
  coordinates(orig) = ~x + y
  p3 = lapply(1:length(xy), FUN=function(x) Polygon(orig[orig$poly==x,]))
  orig2 = SpatialPolygons(list(Polygons(p3, ID="orig")))
  
  ## Clean up orphaned holes
  for(i in 1:length(xsym2@polygons[[1]]@Polygons)){
    xsym2@polygons[[1]]@Polygons[[i]]@hole = FALSE
    ysym2@polygons[[1]]@Polygons[[i]]@hole = FALSE
  }
  
  # Get the unions of the original and the flipped districts
  xunion = gUnion(orig2, xsym2)
  yunion = gUnion(orig2, ysym2)
  
  # get areas of intersects, calculate ratios
  # Note that these will be lots of islands probably
  x_area = sum(sapply(1:length(xunion@polygons[[1]]@Polygons),
                      FUN=function(x) areaPolygon(xunion@polygons[[1]]@Polygons[[x]]@coords)/1000000))
  y_area = sum(sapply(1:length(yunion@polygons[[1]]@Polygons),
                      FUN=function(x) areaPolygon(yunion@polygons[[1]]@Polygons[[x]]@coords)/1000000))
  sym_x = x_area/dist_area
  sym_y = y_area/dist_area
  
  # out
  return(c(sym_x, sym_y))
}


get_one_symmetry_contig = function(xy){
  # get centroid, get area
  centroid = c(mean(xy[,1]), mean(xy[,2]))
  dist_area = areaPolygon(xy)/1000000
  
  # make regular sp object
  orig = data.frame(xy)
  colnames(orig) = c("x", "y")

  # get x-flipped coords, make sp object
  xsym_df = orig
  xsym_df$x =xsym_df$x - 2*(xsym_df$x - centroid[1])
  coordinates(xsym_df) <- ~x+y
  xsym2 = SpatialPolygons(list(Polygons(list(Polygon(xsym_df)), ID="x")))

  # get y-flipped coords, get area, make sp object
  ysym_df = orig
  ysym_df$y =ysym_df$y - 2*(xsym_df$y - centroid[2])
  coordinates(ysym_df) <- ~x+y
  ysym2 = SpatialPolygons(list(Polygons(list(Polygon(ysym_df)), ID="y")))

  # finish setup
  coordinates(orig) = ~x + y
  orig2 = SpatialPolygons(list(Polygons(list(Polygon(orig)), ID="orig")))
  
  ## Get unions
  xunion = gUnion(orig2, xsym2)
  yunion = gUnion(orig2, ysym2)
  # get areas of intersects, calculate ratios
  # Note that these will be lots of islands probably
  x_area = sum(sapply(1:length(xunion@polygons[[1]]@Polygons),
                   FUN=function(x) areaPolygon(xunion@polygons[[1]]@Polygons[[x]]@coords)/1000000))
  y_area = sum(sapply(1:length(yunion@polygons[[1]]@Polygons),
                      FUN=function(x) areaPolygon(yunion@polygons[[1]]@Polygons[[x]]@coords)/1000000))
  sym_x = x_area/dist_area
  sym_y = y_area/dist_area
  
  # out
  return(c(sym_x, sym_y))
}



### I need a wrapper for the feature generation
generate_features = function(shp){
  print("Generating features...")
  firsts = get_first_features(shp) # debugged, works for noncontig
  print("First features generated")
  syms = get_symmetry_features(shp) # debugged, works for noncontig
  colnames(syms) = c("sym_x", "sym_y")
  print("Symmetry features generated")
  corners = get_corners_features(shp) # debugged, works for noncontig
  print("Corners features generated")
  bounds = get_all_bound_features(shp) # debugged, works for noncontig
  print("Bounding features generated")
  features = cbind(data.frame(shp[[1]]), firsts, bounds, corners, syms)
  print("All features generated!")
  features$parts = 1 # how can I programatically generate this?
  return(features)
}
