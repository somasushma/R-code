library("pracma")
library("numbers")
library("MASS")
library("gmp")

# arbitary precision divisor function
big.divisors <- function(MyN) {
  myRle <- function (x1) {
    n1 <- length(x1)
    y1 <- x1[-1L] != x1[-n1]
    i <- c(which(y1), n1)
    list(lengths = diff(c(0L, i)), values = x1[i], uni = sum(y1)+1L)
  }
  
  if (MyN==1L) return(MyN)
  else {
    pfacs <- myRle(factorize(MyN))
    unip <- pfacs$values
    pv <- pfacs$lengths
    n <- pfacs$uni
    myf <- unip[1L]^(0L:pv[1L])
    if (n > 1L) {
      for (j in 2L:n) {
        myf <- c(myf, do.call(c,lapply(unip[j]^(1L:pv[j]), function(x) x*myf)))
      }
    }
  }
  myf[order(asNumeric(myf))]  ## 'order' is faster than 'sort.list'
}

# arbitary precision aliquot function
aliquot.big=function(x){
  ali=vector(mode = "list", length = length(x))
  for (j in 1:length(x)) {
    ali[[j]]=sum(head(big.divisors(x[j]), -1))
  }
  return(ali)  
}

#abundance
abu.big=function(x){
  abu= sapply(1:length(x), function(j) as.character(sum(big.divisors(x[j]))))
  return(abu)  
}

#is.abundant
is.abundant=function(x){
  y=sapply(1:length(x), function(j) as.character(sum(big.divisors(x[j]))-2*x[j]))
 y= as.numeric(y)>0
 return(y)
}

#aliquot map
n=1000
ali.b=vector(mode = "list", length = n)
f=rep(NA, n)
for (j in 1:n) {
  m=1
  k=j
  ali.b[[j]][m]=as.character(k)
  repeat{
    k=as.character(as.bigz(unlist(aliquot.big(as.bigz(k)))))
    if(k %in% ali.b[[j]]) break
    m=m+1
    ali.b[[j]][m]=k
    if(as.numeric(k)==0 || as.numeric(k)>10^30) break 
  }
  f[j]=m
  if(as.numeric(k)>10^30) f[j]=Inf
}

#read 840
library(readr)
X840 <- read_table2("840.csv", col_names = FALSE)
f[840]=length(ali.b[[840]])

#plot
par(pty="m", mar=c(2,2,1,1), mgp=c(1,.35,0))
plot(f, type="h", col="darkred", xlab="n",ylab= expression(paste(f,"[n]")), main="aliquot map")
points(x=which(f==Inf), y=rep(0,length(which(f==Inf))), col="blue", pch=16)

#nonconverging
which(f==Inf)
lapply(which(f==Inf), function(x) head(ali.b[[x]]))
f276=c(276, 306, 396, 696)
f552=c(552,888)
f564=c(564,780) 
f660=c(660,828,996)
f966=c(966)

par(pty="m", mar=c(2,2,1,1), mgp=c(1,.35,0))
plot(as.numeric(ali.b[[276]]), type="l", col="darkred", lwd=2, log = "y", xlab="n", ylab="f[n]", main="Lehmer 5")
points(as.numeric(ali.b[[552]]), type="l", col="darkblue", lwd=2)
points(as.numeric(ali.b[[564]]), type="l", col="darkgreen", lwd=2)
points(as.numeric(ali.b[[660]]), type="l", col="hotpink", lwd=2)
points(as.numeric(ali.b[[966]]), type="l", col="cyan4", lwd=2)

#notable values
which(f>100 & f !=Inf)
lapply(which(f>100 & f !=Inf), function(x) head(ali.b[[x]]))
f138=c(138,150, 168, 222, 234, 312, 528, 570, 726, 870, 960)
f702=c(702, 978, 990)
f720=c(720)
f840=c(840)
f858=c(858)
f936=c(936)

par(pty="m", mar=c(2,2,1,1), mgp=c(1,.35,0))
plot(as.numeric(ali.b[[840]]), type="l", col="darkred", lwd=2, log = "y", xlab="n", ylab="f[n]", main="notable values")
points(as.numeric(ali.b[[138]]), type="l", col="cyan4", lwd=2)
points(as.numeric(ali.b[[702]]), type="l", col="darkblue", lwd=2)
points(as.numeric(ali.b[[720]]), type="l", col="darkgreen", lwd=2)
points(as.numeric(ali.b[[858]]), type="l", col="hotpink", lwd=2)
points(as.numeric(ali.b[[936]]), type="l", col="gray35", lwd=2)

#convergence style
cong=unlist(lapply(ali.b, function(x) tail(x,1)))

#abundant overrepresentation
fabu=f[which(is.abundant(c(1:n)))]
fabu=fabu[fabu !=Inf]
fnabu=f[which(!is.abundant(c(1:n)))]
fnabu=fnabu[fabu !=Inf]

boxplot(list(fabu, fnabu), horizontal = T, col=c("lightblue", "pink"), log = "x")

#plot of abundant numbers
l=20000
ab=which(is.abundant(c(1:l)))

den.ab=sapply(10:length(ab), function(x) length(which(ab<ab[x]))/ab[x])

par(mfrow=c(2,1))

par(pty="m", mar=c(2,2,1,1), mgp=c(1,.35,0))
plot(ab, type="n", col="darkgreen", lwd=2, xlab="n", ylab="ab[n]", main="abundant numbers")
abline(v=seq(0,l,1000), h=seq(0,l,1000), col="gray", lty=3)
points(ab, type="l", col="darkgreen", lwd=2, xlab="n", ylab="ab[n]", main="abundant numbers")
abline(a = 0, b=1/0.2045201428, col="darkred")
abline(a = 0, b=4, col="skyblue3")

par(pty="m", mar=c(2,2,1,1), mgp=c(1,.35,0))
plot(den.ab, type="n", col="darkgreen", lwd=2, xlab="n", ylab="density at n", main="density of abundant numbers")
abline(v=seq(0,l,1000), h=seq(0,1,.01), col="gray", lty=3)
abline(h=c(.248,.247), col=c("darkred", "skyblue3"))
points(den.ab, type="l", col="darkgreen", lwd=2, xlab="n", ylab="density at n", main="density of abundant numbers")

#Mersenne primes/abundant numbers
p=primes(100)
Mp=sapply(p, function(x) as.character(pow.bigz(2,x)-1))
te=sapply(Mp, function(x) length(factorize(as.bigz(x))))
Mp=Mp[which(te==1)]
p=p[which(te==1)]
P=sapply(1:length(p), function(x) as.character(as.bigz(Mp[x])*pow.bigz(2,p[x]-1)))