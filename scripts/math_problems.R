r=5
m=ceiling(log2(r))
n=15
k=2^m-r
a=100
x0=13
xbox=rep(NA, n)
x=x0
for (j in 1:n) {
  x=(a*x^k)^(1/(2^m))
  xbox[j]=x
}

#-----
n=1000000
f=rep(NA, n)
fx1=function(x) x/(1+x^2+x^4)
fx2=function(x) 1/(1+x+x^3)

for (j in 1:n) {
  f[j]=fx2(j)
}
sum(f)

#euler gamma convergence-----------
n=20
x=sapply(1:n, function(x) -digamma(1)+log(1/x)+digamma(x+1))

# converges to sqrt(2)
((1/2)/(3/4))/((5/6)/(7/8))/(((9/10)/(11/12))/((13/14)/(15/16)))

