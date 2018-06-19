library("pracma")
library("numbers")
library("MASS")
library("xtable")

n=300
f1=rep(0, n*(n+1)/2)
f2=rep(0, n*(n+1)/2)
l=0
for (j in 1:n) {
  s=(1:j)^2
  f1[(l+1):(l+j)]=s
  f2[(l+1):(l+j)]= (s %% j)
  l=l+j
}

#basic plots
par(mfrow=c(1,1))
par(pty="m", mar=c(2,2,1,1), mgp=c(1,.35,0))
plot(x=c(0:(length(f2)-1)), f2, pch=16, cex=.25, col=c("dodgerblue4","darkred"), xlab="n",ylab= expression(paste(f,"[n]")), main=expression(paste(f, " linearized square modulo triangle")))

curve((-1+sqrt(1+8*x))/2, from = 0, to=length(f2), n=2*length(f2), col="darkgreen", add=T)

abline(v=seq(0,n*(n+1)/2,1000), h=seq(0,n,10), col="gray", lty=3)

#secondary parabola starts
r1=(1:round(sqrt(n/2),0))^2
r2=sapply(1:(length(r1)-1), function(x) (r1[x]+r1[x+1])/2)
r1=sort(c(r1,r2))
y1=2*r1
x1=y1*r1
points(x1,y1, col="darkgreen")

#secondary parabola curve fits
a=ceil((1:length(x1)+2/3)^2)
cee=c(1,25,50,100,100,200,200,400,480, 600, 700, 1000, 1100, 1400, 1550, 2100, 2000, 2700, 2800, 3250, 3250,4200, 3900)

endpoints=(a^2+cee)/1.89

for (j in 1:length(y1)) {
  curve(a[j]-sqrt(1.89*x-cee[j]), from = x1[j], to=endpoints[j], col="lightcoral", add=T, n=2*length(f2))  
}

#square modulus triangle
m=300
sqmod=matrix(data=NA, nrow = m, ncol = m)
sqrs=matrix(data=NA, nrow = m, ncol = m)

l=0
for (j in 1:m) {
  sqmod[j,(1:j)]=f2[(l+1):(l+j)]
  sqrs[j,(1:j)]=f1[(l+1):(l+j)]
  l=l+j
}

#as image
image(t(sqmod), asp=1, axes=F, ylim=c(1,0), col=colors()[c(1:136,233:502)][1:max(sqmod, na.rm = T)]) #as is

image(t(asinh(sqmod)), asp=1, axes=F, ylim=c(1,0), col=colors()[c(1:136,233:502)][1:max(sqmod, na.rm = T)]) #with asinh transform

#print      
print(xtable(sqmod, digits = 0), include.rownames = F, include.colnames = F) #xtable
print(xtable(sqrs, digits = 0), include.rownames = F, include.colnames = F) #xtable

prmatrix(sqmod, na.print = "", collab = rep("",m),rowlab = rep("",m))

#plots of particular rows of matrix
par(mfrow=c(3,3))
par(pty="m", mar=c(2,2,1,1), mgp=c(1,.35,0))
n=293
plot(c(na.omit(sqmod[n,])), type="h", col=c("dodgerblue4","darkred"), xlab="n",ylab=paste("T[",n,", k]"), main=paste("row=", n))

# column descent by 1 start sequence
d1=sapply(0:(m-1), function(x) floor(x^2/2)+x)
# column descent by 2 start sequence
d2=sapply(1:(m), function(x) floor(x*(x+2)/3)-1) #also sapply(1:(m), function(x) floor(x*(x-1)/3)+x-1)
# column descent by 3 start sequence
d3=sapply(0:(m-1), function(x) floor((x*(x+6)/4))) # also d3=sapply(1:(m), function(x) (2*x*(x+6)-3*(1-(-1)^x))/8)

#descent matrix
dmat=matrix(nrow = 12, ncol=9)
dmat[,1]=d1[1:12]
dmat[2:12,2]=d2[1:11]
dmat[3:12,3]=d3[1:10]
dmat[6:12,4]=c(4,9,12,13,16,21,28)
dmat[8:12,5]=c(9,11,15,16,19)
dmat[9:12,6]=c(9,10,13,18)
dmat[10:12,7]=c(9,9,11)
dmat[11:12,8]=c(9,8)
dmat[12,9]=c(9)

#column descent sequence run length
drl=rep(0,m)
k=0
l=1
for (j in 1:length(drl)) {
 if(j==l^2-k){
   k=k+1
   l=l+1
 } 
  drl[j]=j^2-j-k+1
  kbox[j]=k
}

#single line formula for above
drl=sapply(1:m, function(x) x^2-x-round(sqrt(x))+1)

#number of starting square terms per column
n.sqtrm= sapply(1:m, function(x) round(sqrt(x)))

#number of terms before the final run of k^2 square terms in a column
before.sq=sapply(1:m, function(x) x^2-x+1)

#modulo frequency
fr=table(f2)
par(pty="m", mar=c(2,2,1,1), mgp=c(1,.35,0), mfrow=c(1,1))
plot(x=names(fr), y=c(fr), type = "h", col="dodgerblue4", xlab="n", ylab="fr")

#fit for square terms
te=fr[(1:floor(sqrt(n)))^2+1]
ye=as.numeric(names(te))
reg1=lm(te ~ ye)
abline(reg1, col="darkred", lty=3, lwd=2)

#minima near square terms
wi=10
sqs=sapply(4:floor(sqrt(n)), function(x) x^2+1)
te=sapply(sqs, function(x) min(fr[(x-wi):(x+wi)]))
ye=sapply(1:length(sqs), function(x) sqs[x]-wi-1+match(te[x], fr[(sqs[x]-wi):(sqs[x]+wi)]))
points(x=ye, y=te, col="red")

#maxima other than square terms
wi=15
sqs=sapply(0:floor(sqrt(n)), function(x) x^2+1)
ye=sapply(1:n, function(x) ifelse(x %in% sqs, 0, fr[x]))
te=sapply(tail(sqs, -4), function(x) max(ye[(x-wi):(x+wi)]))
ye=sapply(1:length(tail(sqs, -4)), function(x) tail(sqs, -4)[x]-wi-1+match(te[x], fr[(tail(sqs, -4)[x]-wi):(tail(sqs, -4)[x]+wi)]))
points(x=ye, y=te, col="blueviolet")

te=mean(fr[sapply(0:floor(sqrt(n)), function(x) x^2+1)]) #mean freq of square terms
ye=mean(fr[-sapply(0:floor(sqrt(n)), function(x) x^2+1)]) #mean freq of non-square terms

#location of primes
te=fr[primes(300)+2]
ye=primes(300)+1
points(ye, te, col="red")

#attempt at minima
te=unique(unlist(sapply(1:n, function(x) which(fr[1:x]==min(fr[1:x])))))
ye=c(fr[te])
points(te-1, ye, col="red", pch=16)
