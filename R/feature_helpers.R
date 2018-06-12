get_multi_coord = function(projected, id){
  l = length(projected@polygons[[id]]@Polygons)
  coords = lapply(1:l, FUN=function(x) projected@polygons[[id]]@Polygons[[x]]@coords)
  return(coords)
}

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
  file.remove("temp.jpg")
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
  return(c(corners=nrow(corners_out), xvar = var(corners_out[,1]), yvar=var(corners_out[,2]),
           cornervar_ratio = abs(1-(var(corners_out[,1])/var(corners_out[,2])))))
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
  dist_area = sum(sapply(xy, FUN=function(x) geosphere::areaPolygon(x)/1000000))
  
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


## Source:
## http://dwoll.de/rexrepos/posts/diagBounding.html#minimum-bounding-box


## Bounding box
getBBox <- function(xy) {
  stopifnot(is.matrix(xy), is.numeric(xy), nrow(xy) >= 2, ncol(xy) == 2)
  
  ## rotating calipers algorithm using the convex hull
  H    <- chull(xy)      ## hull indices, vertices ordered clockwise
  n    <- length(H)      ## number of hull vertices
  hull <- xy[H, ]        ## hull vertices
  
  ## unit basis vectors for all subspaces spanned by the hull edges
  hDir  <- diff(rbind(hull, hull[1, ])) ## hull vertices are circular
  hLens <- sqrt(rowSums(hDir^2))        ## length of basis vectors
  huDir <- diag(1/hLens) %*% hDir       ## scaled to unit length
  
  ## unit basis vectors for the orthogonal subspaces
  ## rotation by 90 deg -> y' = x, x' = -y
  ouDir <- cbind(-huDir[ , 2], huDir[ , 1])
  
  ## project hull vertices on the subspaces spanned by the hull edges, and on
  ## the subspaces spanned by their orthogonal complements - in subspace coords
  projMat <- rbind(huDir, ouDir) %*% t(hull)
  
  ## range of projections and corresponding width/height of bounding rectangle
  rangeH  <- matrix(numeric(n*2), ncol=2)  ## hull edge
  rangeO  <- matrix(numeric(n*2), ncol=2)  ## orthogonal subspace
  widths  <- numeric(n)
  heights <- numeric(n)
  
  for(i in seq(along=numeric(n))) {
    rangeH[i, ] <- range(projMat[  i, ])
    
    ## the orthogonal subspace is in the 2nd half of the matrix
    rangeO[i, ] <- range(projMat[n+i, ])
    widths[i]   <- abs(diff(rangeH[i, ]))
    heights[i]  <- abs(diff(rangeO[i, ]))
  }
  
  ## extreme projections for min-area rect in subspace coordinates
  ## hull edge leading to minimum-area
  eMin  <- which.min(widths*heights)
  hProj <- rbind(   rangeH[eMin, ], 0)
  oProj <- rbind(0, rangeO[eMin, ])
  
  ## move projections to rectangle corners
  hPts <- sweep(hProj, 1, oProj[ , 1], "+")
  oPts <- sweep(hProj, 1, oProj[ , 2], "+")
  
  ## corners in standard coordinates, rows = x,y, columns = corners
  ## in combined (4x2)-matrix: reverse point order to be usable in polygon()
  ## basis formed by hull edge and orthogonal subspace
  basis <- cbind(huDir[eMin, ], ouDir[eMin, ])
  hCorn <- basis %*% hPts
  oCorn <- basis %*% oPts
  pts   <- t(cbind(hCorn, oCorn[ , c(2, 1)]))
  
  
  ## Convert the bounding box to a shapefile
  area_sqkm = areaPolygon(pts)/1000000
  
  # lenwid
  dists = unique(dist(pts))
  dists = dists[order(dists)]
  lenwid = dists[1]/dists[4] # there will be 2 short sides, 2 long sides, and 2 diagonals. I want short/long, not diagonal
  
  return(c(area_sqkm, lenwid))
}


## Convex Hull

getConvexHull = function(xy){
  hull= xy[chull(xy),]
  area_sqkm = areaPolygon(hull)/1000000
  perim = perimeter(hull)/1000
  return(c(area_sqkm, perim))
}


## Min bounding cirlce
get_circle = function(xy){
  temp = getMinCircle(xy)
  rad = temp$rad * 111.325
  perim = 2 * pi * rad
  area = pi * rad * rad
  return(list(area=area, perim=perim))
}


get_one_bound_feature = function(xy){
  # the key challenge of this script is making sure everything is in the same units
  dist_area = sum(sapply(xy, FUN=function(x) areaPolygon(x)/1000000))
  dist_perim = sum(sapply(xy, FUN=function(x) perimeter(x)/1000))
  
  xy = do.call(rbind, xy)
  
  bbox_area = getBBox(xy)[1]
  lenwid = getBBox(xy)[2]
  hull= getConvexHull(xy)
  hull_area = hull[1]
  hull_perim = hull[2]
  circle_data = get_circle(xy)
  circle_area = circle_data[[1]]
  circle_perim = circle_data[[2]]
  
  adj_r = sqrt(dist_area / pi)
  schwartz = dist_perim / (2 * pi * adj_r)
  polsby = (4 * pi * dist_area)/(dist_perim * dist_perim)
  
  return(c(hull = dist_area/hull_area, bbox = dist_area/bbox_area, 
           reock = dist_area/circle_area, polsby=polsby, schwartzberg = schwartz,
           circle_area = circle_area, circle_perim = circle_perim,
           hull_area = hull_area, hull_perim=hull_perim,
           orig_area = dist_area, district_perim = dist_perim))
}


get_all_bound_features = function(shp){
  temp = lapply(1:length(shp[[2]]), FUN=function(x) get_one_bound_feature(shp[[2]][[x]]))
  out = do.call(rbind, temp)
  return(out)
}

harris3 = function(img = "temp.jpg", window_size = 5, k = 0.01, thresh = 0.9){
  require(imager)
  require(RcppRoll)
  
  # read in image
  image.orig <- load.image(img)
  image <- grayscale(image.orig)
  # take first order calculations
  height = ncol(image)
  width = nrow(image)
  
  # Take the gradient of this N-dimensional array that is a pixel image
  grad = imgradient(image, axes = "xy", scheme = 2)
  dx = as.matrix(grad$x)
  dy = as.matrix(grad$y)
  
  # Set up the loop
  offset = floor(window_size/2)
  
  xy <- expand.grid(x = seq_len(width), y = seq_len(height))
  
  ## Since looping in R is slow, we vectorize as much as possible
  ## and do the windowing in cpp with RcppRoll
  Sxx <- roll_sum(as.vector(dx^2), n = window_size, fill = 0)
  Sxy <- roll_sum(as.vector(dy * dx), n = window_size, fill = 0)
  Syy <- roll_sum(as.vector(dy^2), n = window_size, fill = 0)
  
  #Find determinant and trace, use to get corner response
  ## notice that this whole part is now vectorized (and so fast).
  det <- (Sxx * Syy) - (Sxy^2)
  trace <- Sxx + Syy
  r <- det - k*(trace^2)
  
  xy[r > thresh, ]
}
