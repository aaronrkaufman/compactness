## This script generates the models used in the application
## First it generates features, then it builds models

setwd("D:/GitHub/compactness_software/data")

## Required libraries
library(sp)
library(sf)
library(rgdal)
library(raster)
library(rgeos)
library(png)
library(pracma)
library(jpeg)
library(imager)
library(geosphere)
library(shotGroups)
library(RcppRoll)
library(gbm)
library(randomForest)
library(e1071)

source("../R/generate_features.R")
source("../R/read_shapefiles.R")
source("../R/feature_helpers.R")

load("D:/GitHub/compactness_software/data/training_data.RData")

# Load training labels with old features
df = do.call(rbind, mylist)


# Read in the from shapefiles
sl = "D:/Dropbox/Compactness Shared/Data/Other Shapefiles/both.shp" # is this all I need?
namecol = "NAME"
shp = read_shapefiles(sl, namecol)

# Generate some features
firsts = get_first_features(shp) 
syms = get_symmetry_features(shp) 
colnames(syms) = c("sym_x", "sym_y")
corners = get_corners_features(shp) 
bounds = get_all_bound_features(shp) 
bfeatures = cbind(data.frame(shp[[1]]), firsts, bounds, corners, syms)

# # Next step: merge, load previous features, correlate. See which ones look off.
# # Something's going wrong: when I read in the shapefiles, those with multiple polygons are getting compressed
 head(df)
 head(bfeatures)
 bf2 = bfeatures[bfeatures$NAME %in% df$district,]
 df2 = df[!duplicated(df$district),]
 bf2$NAME = as.character(bf2$NAME)
 bf2 = bf2[order(bf2$NAME),]
 df2 = df2[order(df2$district),]
# 
# cor(bf2$AREA_GEO, df2$orig_area) #1, and all the others until hull
# cor(bf2$hull, df2$hull) # 0.98
# cor(bf2$bbox, df2$grofman) #-0.38 # why did I call that grofman?
# cor(bf2$reock, df2$reock) # 0.97
# cor(bf2$polsby, df2$polsby) #0.999
# cor(bf2$corners, df2$corners) # 0.481 # this puts a max on the next two corrs
# cor(bf2$xvar, df2$xvar) # 0.0
# cor(bf2$yvar, df2$yvar) # 0.47
# cor(bf2$sym_x, df2$sym_x) # 0.67 # these are a little more worrying, but w/e
# cor(bf2$sym_y, df2$sym_y) # 0.64
# 
# ## Do these correlations matter? What if I redo the modeling with these features?
# ## If they turn out equally well, then I'll be happy.
# 
# ### This code should reproduce the plot in the paper
# a = c()
# resids = list()
# for(i in 1:6){
#   train = mylist[setdiff(1:6, i)]
#   train = do.call(rbind, train)
#   test = mylist[[i]]
#   
#   ols <- lm(pca ~ polsby + boyce + hull + corners + sym_x + sym_y + orig_area + xvar + yvar +
#               polsby*hull + polsby*sym_x + polsby*sym_y + sym_x*sym_y + varline + polsby*corners + hull*corners + 
#               polsby*sym_x*sym_y,
#             data = train)
#   opreds <- predict(ols, newdata = test)
#   
#   boost <- gbm(pca ~ .,
#                data =train[,-c(1,2)], interaction.depth = 3, n.trees = 2000)
#   bpreds <- predict(boost, newdata = test, n.trees = 1000)
#   
#   rfm <- randomForest(x = train[,-1:-3], y = train[, 3], ntree = 2000)
#   rfpreds <- predict(rfm, test)
#   
#   sv <- svm(pca ~ . , data =train[,-1:-2])
#   svpreds <- predict(sv, test)
#   
#   ensemble = rowMeans(cbind(opreds, bpreds, rfpreds, svpreds))
#   a[i] = cor(ensemble, test[,3])
#   resids[[i]] = data.frame(district = test$district, truth = test[,3], preds = ensemble, set = i)
#   
#   label1 = paste0("rho == ", round(a[i],digits=2))
#   
#   df = data.frame(xv = rank(ensemble), yv=test[,3])
#   
#   test = 100
#   if(i==2){
#     test = 98
#   }
#   
#   plotx = ggplot(df, aes(x=xv, y=yv)) + ylab(paste0("Test Set: ", i)) + geom_point(size=.5) +
#     xlab(paste("Train Sets:", paste(setdiff(1:6, i), collapse=","))) +
#     geom_abline(intercept=0, slope=1) + coord_fixed() +
#     scale_x_discrete(limit = c(1,25,50,75, 100), labels=c(1,25,50,75,100)) +
#     scale_y_discrete(limit = c(1,25,50,75, test), labels=c(1,25,50,75,100)) +
#     theme(panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           panel.border = element_blank(),
#           panel.background = element_blank(),
#           axis.line.x = element_line(color="black", size = .5),
#           axis.line.y = element_line(color="black", size = .5),
#           axis.ticks.y=element_blank(),
#           axis.text.x = element_text(size=22),
#           axis.title.x = element_text(size=22),
#           axis.text.y = element_text(size=22),
#           axis.title.y = element_text(size=22),
#           legend.position = "none") +
#     ggplot2::annotate("text", label=label1, x = 75, y=25, col="red", parse=T, size=10)
#   assign(paste0("plot", i), plotx)
# }
# 
# multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
#   library(grid)
#   plots <- c(list(...), plotlist)
#   numPlots = length(plots)
#   if (is.null(layout)) {
#     layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
#                      ncol = cols, nrow = ceiling(numPlots/cols))
#   }
#   if (numPlots==1) {
#     print(plots[[1]])
#   } else {
#     grid.newpage()
#     pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
#     for (i in 1:numPlots) {
#       matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
#       print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
#                                       layout.pos.col = matchidx$col))
#     }
#   }
# }
# 
# 
# multiplot(plot1, plot4, plot2, plot5, plot3, plot6, cols=3)
# 
# 
# ## And this code uses the new feature generation for comparison
# idx = sample(1:6,nrow(bf2), replace=T)
# a = c()
# resids = list()
# bf2 = merge(bf2, df2[,c("district", "pca")], by.x="NAME", by.y="district")
# bf2 = bf2[,-c(1:15,31)]
# bf2 = bf2[,-11]
# 
# 
# for(i in 1:6){
#   train = bf2[idx!= i,]
#   test = bf2[idx==i,]
#   
#   ols <- lm(pca ~ polsby + boyce + hull + corners + sym_x + sym_y + orig_area + xvar + yvar +
#               polsby*hull + polsby*sym_x + polsby*sym_y + sym_x*sym_y + varline + polsby*corners + hull*corners + 
#               polsby*sym_x*sym_y,
#             data = train)
#   opreds <- predict(ols, newdata = test)
#   
#   boost <- gbm(pca ~ .,
#                data =train, interaction.depth = 3, n.trees = 2000)
#   bpreds <- predict(boost, newdata = test, n.trees = 1000)
#   
#   rfm <- randomForest(x = train, y = train$pca, ntree = 2000)
#   rfpreds <- predict(rfm, test)
#   
#   sv <- svm(pca ~ . , data =train)
#   svpreds <- predict(sv, test)
#   
#   ensemble = rowMeans(cbind(opreds, bpreds, rfpreds, svpreds))
#   a[i] = cor(ensemble, test$pca)
#   resids[[i]] = data.frame(district = test$district, truth = test$pca, preds = ensemble, set = i)
#   
#   label1 = paste0("rho == ", round(a[i],digits=2))
#   
#   temp = data.frame(xv = rank(ensemble), yv=test$pca)
#   
# 
# }
# 
# 
# ## Yup that looks fine to me


# Generate new features, then model

ols = list()
boost = list()
rfm = list()
sv = list()
idx = sample(1:6,nrow(bf2), replace=T)
bf2 = merge(bf2, df2[,c("district", "pca")], by.x="NAME", by.y="district")
bf2 = bf2[,-c(1:15,31)]
bf2 = bf2[,-11]



for(i in 1:6){
  train = bf2[idx!=i,]
  test =  bf2[idx==i,]
  
  ols[[i]] <- lm(pca ~ polsby + boyce + hull + corners + sym_x + sym_y + orig_area + xvar + yvar +
              polsby*hull + polsby*sym_x + polsby*sym_y + sym_x*sym_y + varline + polsby*corners + hull*corners + 
              polsby*sym_x*sym_y,
            data = train)
  boost[[i]] <- gbm(pca ~ .,
               data =train, interaction.depth = 3, n.trees = 1000)
  rfm[[i]] <- randomForest(x = train[,-which(colnames(train)=="pca")], y = train$pca, ntree = 1000)
  sv[[i]] <- svm(pca ~ . , data =train)
}

models = c(ols, boost, rfm, sv)
save(models, file="D:/GitHub/compactness_software/compactness/R/sysdata.rda")
