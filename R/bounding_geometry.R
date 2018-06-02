## SOurce:
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


## Wrap this so I can pass a coordinate data set and get all the features for 1 district

#xy = la_city[[2]][[1]]

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
    
  return(c(hull = dist_area/hull_area, bbox = dist_area/bbox_area, #lenwid,
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
