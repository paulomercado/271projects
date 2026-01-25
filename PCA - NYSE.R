##########################################################
## Dataset: Stock Prices
##########################################################

#load dataset
data=read.csv("NYSEstocks.csv")
data.pc = data[-1]
names(data.pc)

#perform PCA on scaled data
pc1 = prcomp(data.pc, scale=T)
summary(pc1)
plot(pc1, type="l")
pc1$rotation
pc1$rotation[,1]
pc1$rotation[,2]

#perform PCA on unscaled data
pc1.u = prcomp(data.pc)
summary(pc1.u)
plot(pc1.u, type="l")
pc1.u$rotation[,1]

##########################################################
## Dataset: Stock Returns
##########################################################

#load dataset
ret=read.csv("NYSEreturn.csv")

#perform PCA on scaled data
pc.r = prcomp(ret, scale=T)
summary(pc.r)
plot(pc.r, type="l")
pc.r$rotation[,1]

#perform PCA on unscaled data
pc.r.u = prcomp(ret)
summary(pc.r.u)
plot(pc.r.u, type="l")
pc.r.u$rotation[,1]
pc.r.u$rotation[,2]
