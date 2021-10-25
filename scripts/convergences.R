#convergence to special roots
n=100
f=rep(NA,n)
f[1]=1
f[2]=2
for (j in 3:n) {
  f[j]=(f[j-1])^(1/3)+(f[j-2])^(1/6)
}

plot(f[1:(n-1)],f[2:n], type="l")

#convergence to 3/2, 4/3 etc
n=100
f=rep(NA,n)
f[1]=(3/2)^(2/3)
for (j in 2:n) {
  f[j]=f[1]^f[j-1]
}

#sums of 9 powers
library(numbers)
library(pracma)
te = sapply(0:10, function(x) sum(9^(0:x)))