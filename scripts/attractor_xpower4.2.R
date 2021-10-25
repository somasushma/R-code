windowsFonts(f1 = windowsFont("Constantia"),
             f2 = windowsFont("Book Antiqua"),
             f3 = windowsFont("Cambria Math"))
#e
e=exp(1)

#function: midpoint------------
midpoint= function(x,y) c(sum(range(x))/2, sum(range(y))/2)

#function: distance------------
distance= function(x,y) sqrt((midpoint(x,y)[1]-x)^2+(midpoint(x,y)[2]-y)^2)

f1=function(x) (1+a*(x+x^2+x^3+x^4+x^5))/(1+b*x^4)
f2=function(x) (1+a*(x+x^3+e^(-x^2)))/(1+b*x^2)
f3=function(x) (1+a*(x+x^2+x^3+e^(-x^2)))/(1+b*x^2)
f4=function(x) (1+a*(x^2+x^4)+c*(x+x^3++x^5))/(1+b*x^4)

f=f4
m=4

#parameter search step
pbox=array(data = NA, dim = c(m^2, 3))
k=1
while( k<= (m^2)){
  n=300
  a=runif(n=1, min = -4, max = 4)
  c=runif(n=1, min = -4, max = 4)
  b=runif(n=1, min = 0, max = 10)
  if(abs(a)>b) next
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
  if(any(abs(pm[,1]) > 150) || any(abs(pm[,2]) > 150)) { next 
  } else if(max(te)-min(te) < .66 || max(abs(pm)) < 2.5){
    next
  } else {
    pbox[k,]=c(a,b,c)
    k=k+1
  }
}

#plot step------------
par(pty="s", mfrow=c(m,m))

for(k in 1:(m^2)){
  n=50000
  a=pbox[k,1]
  c=pbox[k,3]
  b=pbox[k,2]
  pm=array(data=NA, dim = c(n,3))
  x=0.1
  y=0.1
  for (j in 1:n) {
    x1=y+f(x)
    y1=-x+f(x1)
    pm[j,]=c(x1, y1, (round(j/1000)+1))
    x=x1
    y=y1
  }
  
  par(mar=c(2,2,2,2), mgp=c(1.1, .4, 0))
  plot(pm[,1], pm[,2], pch=16, col=pm[,3], cex=.1, asp=1, xlab="X", ylab = "Y", family="f3")
 mtext(do.call(expression, list(bquote("a="~.(a)~"; b="~.(b)),bquote("c="~.(c)))),side=3,line=1:0, cex = .75)
}

# pic=1
naman=paste("~/R/Figures/Figures1/x4.2_attractor_",pic,".png", sep  = "")
pic=pic+1
dev.copy(png, file=naman, width=8, height=8, res=300, units="in")
dev.off()

#rotational symmetry calculation
n=6
cbind(c(1:n), round(cos(2*pi/n*(1:n)), 5))