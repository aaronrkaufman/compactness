# ## This file contains three versions of the Harris corner detector, based on the
# ## python implementation at https://github.com/hughesj919/HarrisCorner/blob/master/Corners.py
# 
# ## here is a direct port of Corners.py. It is about as fast as the python version.
# harris1 = function(img = "temp.jpg", window_size = 5, k = 0.04, thresh = 0.995){
#   require(imager)
#   # read in image
#   image.orig <- load.image(img)
#   image <- grayscale(image.orig)
#   # take first order calculations
#   height = ncol(image)
#   width = nrow(image)
#   
#   # Take the gradient of this N-dimensional array that is a pixel image
#   grad = imgradient(image, axes = "xy", scheme = 2)
#   dx = as.matrix(grad$x)
#   dy = as.matrix(grad$y)
#   Ixx = dx^2
#   Ixy = dy * dx
#   Iyy = dy^2
#   
#   # Set up the loop
#   offset = window_size/2
#   
#   # set up the R matrix
#   rmat = matrix(0, nrow = width, ncol=height)
#   
#   ## Loop
#   for(x in (offset+1):(width-offset)){
#     for(y in  (offset+1):(height-offset)){
#       #print(x)
#       #Calculate sum of squares
#       windowIxx = Ixx[(x-offset):(x+offset), (y-offset):(y+offset)]
#       windowIxy = Ixy[(x-offset):(x+offset), (y-offset):(y+offset)]
#       windowIyy = Iyy[(x-offset):(x+offset), (y-offset):(y+offset)]
#       Sxx = sum(unlist(windowIxx))
#       Sxy = sum(unlist(windowIxy))
#       Syy = sum(unlist(windowIyy))
#       
#       #Find determinant and trace, use to get corner response
#       det = (Sxx * Syy) - (Sxy^2)
#       trace = Sxx + Syy
#       r = det - k*(trace^2)
#       rmat[x,y] = r
#       
#       #If corner response is over threshold, color the point and add to corner list
#       #if(r > thresh){
#          #  cornerList[length(cornerList) + 1] = list(c(cornerList, c(x,y)))
#          #  print(paste("Found one: ", length(cornerList)))
#          #}
#     }
#   }
#   
#   rm2 <- rmat > thresh
#   rm.image <- as.cimg(rm2)
#   if(dim(image.orig)[4]) rm.image <- add.color(rm.image)
# 
#   out <- list(corners = rm2, image = image.orig + rm.image)
#   out
# }
# 
# 
# ## Here is an improved version that avoids unnecessarily repeating computations.
# ## It is about twice as fast as the python version
# harris2 = function(img = "temp.jpg", window_size = 5, k = 0.04, thresh = 0.995){
#   require(imager)
#   # read in image
#   image.orig <- load.image(img)
#   image <- grayscale(image.orig)
#   # take first order calculations
#   height = ncol(image)
#   width = nrow(image)
#   
#   # Take the gradient of this N-dimensional array that is a pixel image
#   grad = imgradient(image, axes = "xy", scheme = 2)
#   dx = as.matrix(grad$x)
#   dy = as.matrix(grad$y)
#   Ixx = dx^2
#   Ixy = dy * dx
#   Iyy = dy^2
#   
#   # Set up the loop
#   offset = floor(window_size/2)
#   
#   # set up the R matrix
#   rmat = matrix(0, nrow = width, ncol=height)
# 
#   ## Loop
#   for(x in (offset+1):(width-offset)){
#     xpos <- (x-offset):(x+offset)
#     for(y in  (offset+1):(height-offset)){
#       ypos <- (y-offset):(y+offset)
#       #Calculate sum of squares
#       Sxx = sum(Ixx[xpos, ypos])
#       Sxy = sum(Ixy[xpos, ypos])
#       Syy = sum(Iyy[xpos, ypos])
#       
#       #Find determinant and trace, use to get corner response
#       det = (Sxx * Syy) - (Sxy^2)
#       trace = Sxx + Syy
#       r = det - k*(trace^2)
#       rmat[x,y] = r
#       
#       #If corner response is over threshold, color the point and add to corner list
#       #if(r > thresh){
#          #  cornerList[length(cornerList) + 1] = list(c(cornerList, c(x,y)))
#          #  print(paste("Found one: ", length(cornerList)))
#          #}
#     }
#   }
#   
#   rm2 <- rmat > thresh
#   rm.image <- as.cimg(rm2)
#   if(dim(image.orig)[4]) rm.image <- add.color(rm.image)
# 
#   out <- list(corners = rm2, image = image.orig + rm.image)
#   out
# }

## Here is a version that replaces loop-based windowing with RcppRoll.
## It is about 30 times faster than the python version.
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


#system.time(x <- harris1())
#system.time(x <- harris2())
#system.time(x <- harris3())

#plot(image)
#points(x, col = "red")





