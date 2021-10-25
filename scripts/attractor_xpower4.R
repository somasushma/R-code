windowsFonts(f1 = windowsFont("Constantia"),
             f2 = windowsFont("MS Gothic"),
             f3 = windowsFont("Cambria"))
#e
e=exp(1)

#function: midpoint------------
midpoint= function(x,y) c(sum(range(x))/2, sum(range(y))/2)

#function: distance------------
distance= function(x,y) sqrt((midpoint(x,y)[1]-x)^2+(midpoint(x,y)[2]-y)^2)

f1=function(x) (1+a*(x+x^2+x^3+x^4+x^5))/(1+b*x^4)
f2=function(x) (1+a*(x+x^3+e^(-x^2)))/(1+b*x^2)
f3=function(x) (1+a*(x+x^2+x^3+e^(-x^2)))/(1+b*x^2)
f4=function(x) a*x^2*cosh(x)/(sinh(x))^2-b*x #b=-1:1; a=-10:10
f5=function(x) (1+a*(x+x^2+x^3))/(e^(x^2)+x)-b*x #b=-1:1; a=-3.5:3.5
f6=function(x) (1+x^2)^(-a)+b*x #a=0:10; b=-2:2
f=f6
m=4

#parameter search step
pbox=array(data = NA, dim = c(m^2, 2))
k=1
while( k<= (m^2)){
n=1000
a=runif(n=1, min = -10, max = 10)
b=runif(n=1, min = -1, max = 1)
#if(abs(a)>b) next
pm=array(data=NA, dim = c(n,2))
x=0.1
y=0.1
for (j in 1:n) {
  x1=y+f(x)
  y1=-x+f(x1)
  pm[j,]=c(x1, y1)
  x=x1
  y=y1
}
te=distance(pm[,1],pm[,2])
if(any(abs(pm) > 150)) { next 
  } else if(max(te)-min(te) < .66 || max(abs(pm)) < 2.5){
    next
  } else {
    pbox[k,]=c(a,b)
    k=k+1
  }
}

#plot step------------
par(pty="s", mfrow=c(m,m))

for(k in 1:(m^2)){
n=50000
a=pbox[k,1]
b=pbox[k,2]
pm=array(data=NA, dim = c(n,3))
x=0.1
y=0.1
for (j in 1:n) {
  x1=y+f(x)
  y1=-x+f(x1)
 pm[j,]=c(x1, y1, (round(j/2000)+1))
  x=x1
  y=y1
}

par(mar=c(2,2,2,2), mgp=c(1.1, .4, 0))
plot(pm[,1], pm[,2], pch=16, col=pm[,3], cex=.05, asp=1, main = bquote("a="~.(a)~"; b="~.(b)), xlab="X", ylab = "Y", family="f3")
}

# pic=1
naman=paste("~/R/Figures/Figures1/studentt_attractor_",pic,".png", sep  = "")
pic=pic+1
dev.copy(png, file=naman, width=8, height=8, res=300, units="in")
dev.off()

#rotational symmetry calculation
n=5
cbind(c(1:n), round(cos(2*pi/n*(1:n)), 5))
