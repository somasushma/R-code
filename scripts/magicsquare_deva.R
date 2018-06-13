#experiments with magic squares
source("R/scripts/permutations.R")
m=matrix(data = c(2,7,6,9,5,1,4,3,8),nrow = 3, ncol = 3, byrow = T)

#magic 3D
y=array(data=0,dim = c(72,3))
l=1
for(k in c(1:3)){
  y[c(l:(l+5)),]=getPerms(m[k,])
  l=l+6
  y[c(l:(l+5)),]=getPerms(m[,k])
  l=l+6
}
y[c(l:(l+5)),]=getPerms(c(m[1,1],m[2,2],m[3,3]))
l=l+6
y[c(l:(l+5)),]=getPerms(c(m[1,3],m[2,2],m[3,1]))
l=l+6
y[c(l:(l+5)),]=getPerms(c(m[1,1],m[2,3],m[3,2]))
l=l+6
y[c(l:(l+5)),]=getPerms(c(m[1,3],m[2,1],m[3,2]))
l=l+6
y[c(l:(l+5)),]=getPerms(c(m[3,1],m[2,3],m[1,2]))
l=l+6
y[c(l:(l+5)),]=getPerms(c(m[3,3],m[2,1],m[1,2]))


# library(rgl)
rgl.clear()
plot3d(y[,1],y[,2],y[,3], col="blue", cex=2, xlab="X", ylab = "Y", zlab = "Z")
rgl.bg( sphere = FALSE, color=c("gray90","white"), back = "lines")
rgl.bbox(color = "lightgreen", alpha=0.8) 
rgl.spheres(y[,1],y[,2],y[,3],radius=.5,color="red", alpha=0.5)

#complete number relationship
#square magic
as.numeric(paste(m[1,],collapse = ""))^2+as.numeric(paste(m[2,],collapse = ""))^2+as.numeric(paste(m[3,],collapse = ""))^2
as.numeric(paste(rev(m[1,]),collapse = ""))^2+as.numeric(paste(rev(m[2,]),collapse = ""))^2+as.numeric(paste(rev(m[3,]),collapse = ""))^2

as.numeric(paste(m[,1],collapse = ""))^2+as.numeric(paste(m[,2],collapse = ""))^2+as.numeric(paste(m[,3],collapse = ""))^2
as.numeric(paste(rev(m[,1]),collapse = ""))^2+as.numeric(paste(rev(m[,2]),collapse = ""))^2+as.numeric(paste(rev(m[,3]),collapse = ""))^2

pm=array(data=0,dim = c(8,3))
pm[1,]=c(276,951,438)
pm[2,]=c(672,159,834)
pm[3,]=c(294,753,618)
pm[4,]=c(492,357,816)
pm[5,]=c(258,714,693)
pm[6,]=c(852,396,417)
pm[7,]=c(654,798,213)
pm[8,]=c(456,312,897)
rgl.clear()
plot3d(pm[,1],pm[,2],pm[,3], col="blue", cex=2, xlab="X", ylab = "Y", zlab = "Z")
rgl.bg( sphere = FALSE, color=c("gray90","white"), back = "lines")
rgl.bbox(color = "lightgreen", alpha=0.8) 
rgl.spheres(pm[,1],pm[,2],pm[,3],radius=15,color="red", alpha=0.5)

# dot product
sum(m[1,]*m[2,])
sum(m[2,]*m[3,])

sum(m[,1]*m[,2])
sum(m[,2]*m[,3])

sum(m[1,]*m[,1])
sum(m[2,]*m[,2])
sum(m[3,]*m[,3])

mp=rbind((m[1,]*m[,1]),(m[2,]*m[,2]),(m[3,]*m[,3]))

#eigenvector
eig=eigen(m)
rgl.clear()
plot3d(eig$vectors[1,],eig$vectors[2,],eig$vectors[3,], col="blue", cex=2, xlab="X", ylab = "Y", zlab = "Z")
rgl.bg( sphere = FALSE, color=c("gray90","white"), back = "lines")
rgl.bbox(color = "lightgreen", alpha=0.8) 
rgl.spheres(eig$vectors[1,],eig$vectors[2,],eig$vectors[3,],radius=.1,color="red", alpha=0.5)

#kaubera-sarvatobhadra
mk=m+19

#as number sums fo kaubera
as.numeric(paste(mk[1,],collapse = ""))^2+as.numeric(paste(mk[2,],collapse = ""))^2+as.numeric(paste(mk[3,],collapse = ""))^2
as.numeric(paste(rev(mk[1,]),collapse = ""))^2+as.numeric(paste(rev(mk[2,]),collapse = ""))^2+as.numeric(paste(rev(mk[3,]),collapse = ""))^2

as.numeric(paste(mk[,1],collapse = ""))^2+as.numeric(paste(mk[,2],collapse = ""))^2+as.numeric(paste(mk[,3],collapse = ""))^2
as.numeric(paste(rev(mk[,1]),collapse = ""))^2+as.numeric(paste(rev(mk[,2]),collapse = ""))^2+as.numeric(paste(rev(mk[,3]),collapse = ""))^2

212625+282420+232227
252621+202428+272223
212823+262422+252027
232821+222426+272025
212427+262023+252822
272421+232026+222825
252423+262827+212022
232425+272826+222021

#general square
a=10
b=2
c=6
mg=matrix(nrow = 3, ncol = 3, byrow = T,
          data = c(a+c, a-b-c, a+b,
                   a+b-c, a , a-b+c,
                   a-b, a+b+c, a-c ))

#magic 3D
y=array(data=0,dim = c(72,3))
l=1
for(k in c(1:3)){
  y[c(l:(l+5)),]=getPerms(mg[k,])
  l=l+6
  y[c(l:(l+5)),]=getPerms(mg[,k])
  l=l+6
}
y[c(l:(l+5)),]=getPerms(c(mg[1,1],mg[2,2],mg[3,3]))
l=l+6
y[c(l:(l+5)),]=getPerms(c(mg[1,3],mg[2,2],mg[3,1]))
l=l+6
y[c(l:(l+5)),]=getPerms(c(mg[1,1],mg[2,3],mg[3,2]))
l=l+6
y[c(l:(l+5)),]=getPerms(c(mg[1,3],mg[2,1],mg[3,2]))
l=l+6
y[c(l:(l+5)),]=getPerms(c(mg[3,1],mg[2,3],mg[1,2]))
l=l+6
y[c(l:(l+5)),]=getPerms(c(mg[3,3],mg[2,1],mg[1,2]))

rgl.clear()
plot3d(y[,1],y[,2],y[,3], col="blue", cex=2, xlab="X", ylab = "Y", zlab = "Z")
rgl.bg( sphere = FALSE, color=c("gray90","white"), back = "lines")
rgl.bbox(color = "lightgreen", alpha=0.8) 
rgl.spheres(y[,1],y[,2],y[,3],radius=.5,color="red", alpha=0.5)

# 4 x 4
#Ramanujan square
mr=matrix(data=c(1,10,15,8,
                 16,7,2,9,
                 6,13,12,3,
                 11,4,5,14),
          nrow=4, ncol = 4, byrow = T)

#chandrAtreya square
mc=matrix(data=c(7,12,1,14,
                 2,13,8,11,
                 16,3,10,5,
                 9,6,15,4),
          nrow=4, ncol = 4, byrow = T)