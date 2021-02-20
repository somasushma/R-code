library("pracma")
library("numbers")

n=25
par(mfrow=c(sqrt(n),sqrt(n)), mar=c(0.2,0.2,0.2,0.2))
for (j in 1:n) {
a1=0.3+runif(1,0,1)
a2=0.3+runif(1,0,1)

b1=0.3+runif(1,0,1)
b2=0.3+runif(1,0,1)

k1=runif(1, -pi/2,pi/2)
k2=runif(1, -pi/2,pi/2)
k3=runif(1, -pi/2,pi/2)
k4=runif(1, -pi/2,pi/2)

c1= round(runif(1, 5,50),0)
c2=c1
d1=runif(1,40,50)
while(!coprime(c1,c2)){
d2=2*d1+1
c2=round(runif(1,d1,d2),0)
}
c3= abs(c1-c2)
#c4=2*c1
c4=c1+c2-c3
  
xt=function(t) a1*cos(c1*t+k1)+ a2*cos(c2*t+k2)
yt=function(t) b1*sin(c3*t+k3)+ b2*sin(c4*t+k4)

t=seq(0, 2*pi, .001)

x=xt(t)
y=yt(t)

plot(x,y, type = "l", lwd=.2, asp=1, col="midnightblue", axes=F, xlab="", ylab="")
points(.95*x,.95*y, type = "l", lwd=.1, asp=1, col="cyan4")
# points(.8*x,.8*y, type = "l", lwd=.2, asp=1, col="darkred")

c(a1,a2,b1,b2,c1,c2,c3,c4,k1,k2,k3,k4)
}

dev.copy(png, file="~/R/Figures/Figures1/tritangs01.png", width=11, height=11, res=300, units="in")
dev.off()