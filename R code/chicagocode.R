library(spatstat)
X <- unmark(chicago)
X
system.time(d <- npdensity.lpp(X,sigma = 100,delta = 3))
# user   system  elapsed 
# 3334.336  231.812 3566.209

pdf("chicago.pdf")
plot(d,main="")
dev.off()

pdf("chicagow.pdf")
plot(d,style = "w",main="")
dev.off()

pdf("chicagop.pdf")
plot(X,main="",pch=20)
dev.off()