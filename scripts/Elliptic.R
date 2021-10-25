library(MASS)
library(numbers)
library(pracma)
library(xtable)

#fonts
windowsFonts(f1 = windowsFont("Constantia"),
             f2 = windowsFont("Book Antiqua"),
             f3 = windowsFont("Cambria Math"))

#pi approximation circle
n=330
n*sin(pi/n)*cos(pi/n) #Area
2*n*sin(pi/n) #Perimeter
n*sin(pi/n) # pee

62832/20000 #Aryabhata

#YV
a=8.55
4*(1-1/a+1/(a*29)-1/(a*29*6)+1/(a*29*6*8))^2

#ellipse perimeter------------
a=1; d=sqrt(1/2); b=sqrt(1-d^2)

ellipke(1-(b/a)^2)$e*4

pi*2*sqrt(a*b) #Kepler

pi*2*(a+b)/2 #Kepler 2

pi*sqrt(2*(a^2+b^2)) #Euler

2*sqrt(4*a^2+6*b^2) #mahAvIra

pi*((a+b)*3/2-sqrt(a*b))

pi*2*((a^(3/2)+b^(3/2))/2)^(2/3)

#different formulae for different ellipses-------
a=1; x= c(.2,.4, .6, .9)
te=sapply(x, function(b) c(pi*2*sqrt(a*b), pi*2*(a+b)/2, pi*sqrt(2*(a^2+b^2)), 2*sqrt(4*a^2+6*b^2), ellipke(1-(b/a)^2)$e*4))
te=t(te)
rownames(te)=x
colnames(te)=c("Kepler", "Average", "Euler", "Mahavira", "Actual")

ye=xtable(te, digits = 3)
print(ye)

#Error in mahAvIra-----
a=1; x= a*seq(0,1, .001)
te=sapply(x, function(b) 2*sqrt(4*a^2+6*b^2))
ye=sapply(x, function(b) ellipke(1-(b/a)^2)$e*4*a)
te=cbind(te,ye, (ye-te)/ye)

#Euler's original formula--------
a=1; b=1/sqrt(2)
d=a^2/b^2-1
n=20
m=sapply(0:n, function(x) 2*x+1)
xbox=rep(NA, n+1)
for (j in 1:(n+1)) {
  te=prod((m[1:(j-1)])^2)*tail(m[1:j],1)
  ye=(prod(1:j))^2*2^(2*j)
  xbox[j]=((-1)^(j+1))*te/ye*d^(j)
}
2*pi*b*sum(c(1, xbox))

ellipke(1-(b/a)^2)$e*4*a

#Euler's reformulated formula second attempt-----
a=1; b=.5
d=sqrt(1-b^2/a^2)
n=30

2*pi*a*(1-sum(sapply(1:n, function(x) 1/(2*x-1)*(factorial(2*x)/(2^x*factorial(x))^2)^2*d^(2*x))))

ellipke(1-(b/a)^2)$e*4*a

#Gauss formula

Chalf=function(n){
  te=prod(sapply(1:n, function(x) 1/2-x+1))/factorial(n)
  return(te)
}

a=1; b=.3
h=((a-b)/(a+b))^2
n=20

pi*(a+b)*(1+sum(sapply(1:n, function(x) (Chalf(x))^2*h^x)))

ellipke(1-(b/a)^2)$e*4*a

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
K=function(eccentricity_squared){
  x=eccentricity_squared
  y=sqrt(1-x)
  return(pi/(2*M(1,y,10)))
}

#elliptic integral 2------------
E=function(eccentricity_squared){
  x=eccentricity_squared
  y=sqrt(1-x)
  return(pi*N(1, 1-x, 10)/(2*M(1,y,10)))
}

#pi-----------
pee=function(eccentricity_squared) {
  x=eccentricity_squared
  y=sqrt(1-x)
  2*M(1,y,10)*M(1, sqrt(x), 10)/(N(1, 1-x, 10)+N(1, x, 10)-1)
}