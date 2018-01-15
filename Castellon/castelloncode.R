library(spatstat)
bw=seq(100,800,by=100)
bw=sort(c(bw,450,550))
cv=rep(0,0)
for(j in 1:length(bw)){
  cv[j]=sum(unlist(lapply(X=1:npoints(X),FUN = function(i){
    Y <- X[-i]
    d <- density(Y,bw[j])
    log(d[X[i]])
  })))
}

int <- unlist(lapply(X=1:length(bw),FUN = function(i){
  integral.linim(density.lpp(X,sigma = bw[i]))
}))

cvv <- cv-int
plot(bw,cvv,type = "l",main = "",ylab = "CV",xlab = expression(epsilon),lwd=2)
abline(v=bw[which.max(cvv)])


a <- proc.time()
out <- npdensity.lpp(X,sigma = 150,delta=3)
proc.time()-a
plot(out)
plot(out,style = "w",adjust = 0.3)


pdf("castellon.pdf",height = 8,width = 10)
par(mar=c(0,0,0,0),mai=c(0,0,0,0))
plot(X,main="",pch=20,cex=1.7,cols=2,lwd=2)
dev.off()

pdf("castellonw.pdf")
plot(out,style = "w",main="",adjust = 0.3)
dev.off()

pdf("castellonp.pdf")
plot(out,main="")
dev.off()

d <- density.lpp(X,sigma = 950,positive=T)
pdf("castellonwh.pdf",width = 7,height = 5)
par(mar=c(0,0,0,1))
plot(d,main="")
dev.off()
pdf("castellonwhw.pdf",width = 8,height = 6)
par(mar=c(0,0,0,1))
plot(d,main="",style = "w",scale = 10000)
dev.off()

pdf("k.pdf",width = 8,height = 8)
par(mar=c(5,5,0.2,0.2))
plot(linearKinhom(X,lambda = d[as.ppp(X)]),main="")
dev.off()
pdf("g.pdf",width = 8,height = 8)
par(mar=c(5,5,0.2,0.2))
plot(linearpcfinhom(X,lambda = d[as.ppp(X)]),main="")
dev.off()



