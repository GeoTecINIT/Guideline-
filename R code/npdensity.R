npdensity.lpp <- function(X,sigma,rp=10,dimyx=NULL,delta=4){
  
  L <- as.linnet(X)
  
  Llines <- as.psp(L)
  linemask <- as.mask.psp(Llines,dimyx=dimyx)
  lineimage <- as.im(linemask,value=0)
  
  xx <- raster.x(linemask)
  yy <- raster.y(linemask)
  mm <- linemask$m
  xx <- as.vector(xx[mm])
  yy <- as.vector(yy[mm])
  pixelcentres <- ppp(xx, yy, window=as.rectangle(linemask), check=FALSE)
  pixdf <- data.frame(xc=xx, yc=yy)
  
  p2s <- project2segment(pixelcentres, Llines)
  projloc <- as.data.frame(p2s$Xproj)
  projmap <- as.data.frame(p2s[c("mapXY", "tp")])
  projdata <- cbind(pixdf, projloc, projmap)
  
  gridx <- pixelcentres$x
  gridy <- pixelcentres$y
  grid <- lpp(data.frame(gridx,gridy),L)
  
  
  
  
  ox <- X$data$x  # Emerge the x-coordination 
  oy <- X$data$y  #Emerge the y-coordination
  ngrid <- npoints(grid) # Emerge number of points
  nrealization <- npoints(X)
  dlpp <- crossdist.lpp(X,grid,method="C") # Distance matrix 
  
  if (missing(rp)){rp <- 10} 
  
  b <- delta*sigma
  deltat <- b/rp
  t <- seq(0.01,b,by=deltat)  
  t <- c(t,b) 
  
  npintlpp <- rep(0,0) 
  
  for(i in 1:ngrid){
    ldfp <- dlpp[,i]
    densfp <- 0
    
    for(j in 1:nrealization){
      if(ldfp[j] <= b){
        edge <- sum(unlist(lapply(X=1:length(t), function(k){
          dkernel(t[k],sd=sigma)*countends(L,c(ox[j],oy[j]),t[k])*deltat
        })))
        densfp <- densfp+(dkernel(ldfp[j],sd=sigma)/edge)
      }
    }
    npintlpp[i]=densfp
  }
  
  values <- npintlpp 
  Z <- lineimage
  Z[pixelcentres] <- values
  # df <- cbind(projdata, values)
  # density.diggle.lpp <- linim(L, Z, df=df)
  density.diggle.lpp <- linim(L, Z)
  
  return(density.diggle.lpp)
}

