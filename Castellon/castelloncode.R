library(spatstat)
a <- proc.time()
out <- npdensity.lpp(X,sigma = 150,delta=3)
proc.time()-a
plot(out)
plot(out,style = "w",adjust = 0.3)


pdf("castellon.pdf")
plot(X,main="",pch=20)
dev.off()

pdf("castellonw.pdf")
plot(out,style = "w",main="",adjust = 0.3)
dev.off()

pdf("castellonp.pdf")
plot(out,main="")
dev.off()