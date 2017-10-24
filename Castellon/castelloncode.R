library(spatstat)
a <- proc.time()
out <- npdensity.lpp(X,sigma = 150,delta=3)
proc.time()-a
plot(out)
plot(out,style = "w",adjust = 0.3)
