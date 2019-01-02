#constants
phi=(sqrt(5)-1)/2
rroot2=1/sqrt(2)
#---------
#defining functions
fun.list= vector(mode = "list", 12)   
fun.list[["f1.1"]]= function(x,y){
  a[1]+ a[2]*x +a[3]*sqrt(abs(x)) + a[4]*x^3 +y
}

fun.list[["f1.2"]]= function(x,y){
  a[1]+ a[2]*x +a[3]*sqrt(abs(x)) + a[4]*x^3 -y
}


fun.list[["f1.3"]]= function(x,y){
  a[1]+ a[2]*x +a[3]*abs(x) + a[4]*x^3 +y
}

fun.list[["f1.4"]]= function(x,y){
  a[1]+ a[2]*x +a[3]*abs(x) + a[4]*x^3 -y
}

fun.list[["f1.5"]]= function(x,y){
  a[1]+ a[2]*x +a[3]*x^2 + a[4]*x^3 + y
}

fun.list[["f1.6"]]= function(x,y){
  a[1]+ a[2]*x +a[3]*x^2 + a[4]*x^3 -y
}

fun.list[["f1.7"]]= function(x,y){
  a[1] +  a[3]*abs(x) +y
}

fun.list[["f1.8"]]= function(x,y){
  a[1]+a[3]*abs(x) -y
}

fun.list[["f1.9"]]= function(x,y){
  a[1]+a[2]*x+ a[3]*abs(x) + y
}

fun.list[["f1.10"]]= function(x,y){
  a[1]+a[2]*x+ a[3]*abs(x) - y
}

fun.list[["f1.11"]]= function(x,y){
  a[1]+ a[3]*abs(x) + a[4]*x^3 +y
}

fun.list[["f1.12"]]= function(x,y){
  a[1]+a[3]*abs(x) + a[4]*x^3 -y
}
fun.list[["f2.1"]]= function(x){
  a[5]+x
}

fun.list[["f2.2"]]= function(x){
  a[5]-x
}


#---------
#loading functions
f1=fun.list$f1.9
f2=fun.list$f2.2

#core search
par(mfrow=c(5,5))
n=2000
m=1000
l=0
maxl=50
amat=vector(mode = "list", maxl)

while(l < maxl) {
  pm=array(data=NA, dim = c(n,2))
  # a=rep(0,12)
  # b=round(runif(n = 1, min = 4, max = 12),0)
  a=runif(6, min=-1.01, max = 1.01)
  
  xn=.3*cos(pi/4)
  yn=.3*sin(pi/4)
  ep=.0000001
  xm=xn+ep
  ym=xn+ep
  dbox=rep(NA,m)
  for(j in 1:m) {
    xn1=f1(xn,yn)
    yn1=f2(xn)
    xm1=f1(xm,ym)
    ym1=f2(xm)
    d=(xn1-xm1)^2+(yn1-ym1)^2
    dbox[j]=d
    xn=xn1
    yn=yn1
    xm=xm1
    ym=ym1
  }
  if(dbox[m]>100 || is.nan(dbox[m])) { next 
  } else if(any(abs(dbox[(m-500):(m-1)]-dbox[m])<.0000001) ){ next
  } else {
    for (j in 1:n) {
      xn1=f1(xn,yn)
      yn1=f2(xn)
      pm[j,]=c(xn1,yn1)
      xn=xn1
      yn=yn1
      
      
    }
    if(any(is.nan(pm)) || any(abs(pm) > 1)){
      next
    } else {
      l=l+1
      amat[[l]]=a
      par(pty="s", mar=c(2,2,2,1), mgp=c(1.1,.5,0))
      plot(pm[,1], pm[,2], pch=16, cex=.2, main=l, col=terrain.colors(10), xlab = "x", ylab = "y")
    }  
  }
}

#blow up selected------------
for (cnt in 1:50) {
a=amat[[cnt]]
n=2000
m=200
# te=as.vector(outer(seq(-.1,.1,.01), seq(-.1,.1,.01)*1i, FUN = "+"))
# xbox=Re(te)
# ybox=Im(te)
t=seq(0,(2*pi-2*pi/m), 2*pi/m)
xbox=.3*cos(t)
ybox=.3*sin(t)

pm=array(data=NA, dim = c(n*m,3))

s=0
col=0
for (k in 1:m) {
  xn=xbox[k]
  yn=ybox[k]
  col=col+1
  for (j in 1:n) {
    xn1=f1(xn,yn)
    yn1=f2(xn)
    if(abs(xn1) > 10|| abs(yn1) > 10|| is.nan(xn1) || is.nan(yn1)){
      break
    } else {
      s=s+1  
      pm[s,]=c(xn1,yn1, col)
    }
    xn=xn1
    yn=yn1
    
  }
}

# col=c("#67001f","#b2182b","#d6604d","#f4a582","#fddbc7", "#ffffff","#e0e0e0", "#bababa","#878787","#4d4d4d", "#1a1a1a") #default red
# col=c("#543005","#8c510a", "#bf812d", "#dfc27d","#f6e8c3","#f5f5f5",  "#c7eae5","#80cdc1","#35978f",  "#01665e", "#003c30") #green blue

col=c("#40004b","#762a83","#9970ab","#c2a5cf","#e7d4e8","#f7f7f7","#d9f0d3","#a6dba0","#5aae61","#1b7837","#00441b") #green blue violet
      
# col=c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99","#e31a1c",  "#fdbf6f","#ff7f00","#cab2d6", "#6a3d9a","#ffff99")
# col=rainbow(11)
png(filename = paste("~/R/Figures3/cubic_abs_ham/Lz_abs_1_",sprintf("%04d", cnt), ".png", sep = ""),width = 1200,height = 1200)
par(mfrow=c(1,1), pty="s", mar=c(2,2,2,1), mgp=c(1.1,.5,0))
plot(x=pm[1:s,1], y=pm[1:s,2], pch=16, cex=.2, main="", xlab = "x", ylab = "y", col=col[(pm[,3]) %% 11])
dev.off()
}