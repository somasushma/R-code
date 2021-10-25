ellke=function(x,y){
  a=1
  yn=y
  if(x==1) xn=.9999999 else xn=x
  b=sqrt(1-xn^2)
  s=1-xn^2/2
  ss=0
  t=1
  n=8
  for(j in 1:n){
    d=atan((a-b)*tan(yn)/(a+b*(tan(yn))^2))
    yn=2*yn-d
    te=a
    a=(a+b)/2
    c=(te-b)/2
    b=sqrt(te*b)
    s=s-t*c^2
    ss=ss+c*sin(yn)
    t=2*t
  }
  if(x==1 && y==pi/2){ return(c(Inf, 1, Inf,  1)) 
  } else if(x==1){ return(c(Inf, 1, yn/((2^j)*a),  s*yn/((2^j)*a)+ss))
  } else{
    return(c("K(x)"=pi/(2*a), "E(x)"=s*pi/(2*a), "K(x,phi)"=yn/((2^j)*a),  "E(x,phi)"=s*yn/((2^j)*a)+ss))
  }
}

ellja=function(y,x){
  n=8
  a=rep(NA,n+1)
  b=rep(NA,n+1) 
  c=rep(NA,n+1)
  s=rep(NA,n+1)
  a[1]=1
  b[1]=sqrt(1-x^2)
  c[1]=x
  for (j in 1:n) {
    a[j+1]=(a[j]+b[j])/2
    b[j+1]=sqrt(a[j]*b[j])
    c[j+1]=(a[j]-b[j])/2
  }
  s[j+1]=2^n*a[j+1]*y
  for (j in (n+1):2) {
    d=asin(c[j]*sin(s[j])/a[j])
    s[j-1]=(s[j]+d)/2
  }
  sn=sin(s[1]) 
  cn=cos(s[1])
  cd=cos(s[2]-s[1])
  dn=cos(s[1])/cd
  return(c("sn(u,x)"=sn,"cn(u,x)"=cn, "dn(u,x)"=dn))
}

#Gaussian AGM-------
M= function(first_number,second_number,iterations) {
  x=first_number; y=second_number; n=iterations;
  xn=x; yn=y
  for(j in 1:n ) {
    xn1=(xn+yn)/2
    yn1=sqrt(xn*yn)
    xn=xn1; yn=yn1
  }
  return(mean(xn,yn))
}

#modified Gaussian AGM-------
N= function(first_number,second_number,iterations) {
  x=first_number; y=second_number; n=iterations;
  xn=x; yn=y; zn=0
  for(j in 1:n) {
    xn1=(xn+yn)/2
    yn1=zn+sqrt((xn-zn)*(yn-zn))
    zn1=zn-sqrt((xn-zn)*(yn-zn))
    xn=xn1; yn=yn1; zn=zn1
    if(xn==yn) break
  }
  return(mean(xn,yn))
}

#elliptic integral 1------------
K=function(x){
  y=sqrt(1-x^2)
  return(pi/(2*M(1,y,10)))
}

#elliptic integral 2------------
E=function(x){
  y=sqrt(1-x^2)
  return(pi*N(1, 1-x^2, 10)/(2*M(1,y,10)))
}

pee=function(x) {
  y=sqrt(1-x^2)
  2*M(1,y,10)*M(1, x, 10)/(N(1, 1-x^2, 10)+N(1, x^2, 10)-1)
}

#fonts
windowsFonts(f1 = windowsFont("Constantia"),
             f2 = windowsFont("Book Antiqua"),
             f3 = windowsFont("Cambria Math"))

#plotting 
xends=c(0,25)
par(mar=c(2,2,2,2), mgp=c(1.1, .4, 0))
plot(xends, c(-1,1), type = "n", xlab="x", ylab="y", family="f3")
grid(col="gray55")
xbox=seq(xends[1],xends[2],.1)
kbox=seq(0,.99, length.out = 50)
zbox=array(dim = c(length(kbox), length(xbox)))
k=1
for (j in kbox) {
  te=sapply(xbox, function(x) ellja(x,j))
  points(x=xbox, y=te[1,], type="l", col=rainbow(length(kbox))[k], lwd=1.5)

  zbox[k,]=te[1,]
  k=k+1
}

points(x=xbox, y=sin(xbox), type="l", col="black", lwd=2)

zbox=t(zbox)

library(plot3D)
library(plot3Drgl)

par(mar=c(2,2,2,2), mgp=c(1.1, .4, 0))
persp3D(x=xbox, y=kbox, z = zbox, colkey = F, xlab="u", ylab="k", zlab="sn(u,k)", col = jet2.col(200), theta = 34, phi = 55, border = "gray25", resfac = 1)

plotrgl()
snapshot3d("sn2.jpg")

dev.copy(png, file="~/cutting_block/R/figures/elliptic.png", width=8, height=5, res=300, units="in")
dev.off()

