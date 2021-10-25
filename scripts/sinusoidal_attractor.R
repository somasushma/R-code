a=-3.45
b=-2.42
c1=.1
d=.23
k=3
n=15000
pm=array(data = NA, dim = c(n,2))
xn=1.37
yn=-1.1
for (j in 1:n) {
  xn1=d+a*(cos(yn))^2+b*sin(xn/k)
  yn1=c1-xn
  pm[j,]=c(xn1, yn1)
  xn=xn1
  yn=yn1
}

m=5
t=2*pi/m
pm=pm[50:n,]
fpm=pm
for (j in 1:(m-1)) {
  te=cbind(pm[,1]*cos(t*j)-pm[,2]*sin(t*j), pm[,1]*sin(t*j)+pm[,2]*cos(t*j))
  fpm=rbind(fpm, te)  
}

par(pty="s", mar=c(2,2,2,1), mgp=c(1.1, .3, 0))
plot(fpm, pch=16, col="darkblue", asp = 1, axes = F, xlab = "", ylab = "", main = bquote("a="~.(a)~", b="~.(b)~", c="~.(c1)~", d="~.(d)~", k="~.(k)~", m="~.(m)), cex.main=3)
box()
