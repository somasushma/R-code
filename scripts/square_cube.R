cbrt=function(x) x^(1/3)
isInt=function(x) round(x, 10)==round(x,1)

n=10000
pm=array(data = 0, dim = c(n, 3))
j=1
for (x in 1:n) {
  for(y in x:n){
    z=cbrt(x^2+y^2)
    if(isInt(z)){
      pm[j,]=c(x,y,z)
      j=j+1
    }
  }
}

pm=pm[which(pm[,1] !=0),]
par(mar=c(2,2,2,1), mgp=c(1.1,.4,0))
plot(pm[,1], pm[,2], pch=16, xlab="x", ylab = "y")
m=100
x=(1:m)^2+1; y=(1:m)*((1:m)^2+1)
points(x, y, pch=16, col="red")
