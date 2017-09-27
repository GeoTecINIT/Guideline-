library(spatstat.utils)
library(spatstat)
library(maptools)
###############################################################
shptolpp <- function(shpp,shpl){
  p <- readShapePoints(shpp)
  l <- readShapeLines(shpl)
  L <- as.linnet(l)
  p <- as.ppp(p)
  lp <- lpp(unmark(p),L)
  return(unique(lp))
}