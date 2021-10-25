library(pracma)
library(numbers)
n=200
abox=rep(NA,100*100)
bbox=rep(NA,100*100)
cbox=rep(NA,100*100)
l=1
for (j in 1:n) {
  for (k in 1:n) {
    if(coprime(j,k) && j>k){
    abox[l]=j^2-k^2
    bbox[l]=2*j*k
    cbox[l]=j^2+k^2
    l=l+1
    }
  }
}
triples.bkk=as.array(cbind(abox, bbox, cbox))
triples.bkk=triples.bkk[!is.na(triples.bkk[,1]),]
triples.bkk=t(sapply(1:length(triples.bkk[,1]), function(x) triples.bkk[x,]/gcd(triples.bkk[x,1],triples.bkk[x,2])))
triples.bkk=t(sapply(1:length(triples.bkk[,1]), function(x) sort(triples.bkk[x,])))
triples.bkk=unique(triples.bkk,MARGIN = 1)
triples.bkk=triples.bkk[order(triples.bkk[,3]),]

#number of triples below m
m=40100
te=sapply(seq(100, m, 100), function(x) length(which(triples.bkk[,3]< x))/x )

par(pty="m", mar=c(2,2,2,1), mgp=c(1.1, .3, 0))
plot(te, type="l", main=bquote(" fraction of 90 degree triple <"~.(m)), xlab="", ylab="", col="darkgreen", xaxt="n")
axis(1, at=seq(0,m,100), labels = seq(0,m,100)*100)
abline(h=1/(2*pi), col="gray35", lty=2)

#octant of primitive triples
par(pty="s", mar=c(2,2,2,1), mgp=c(1.1, .3, 0))
plot(triples.bkk[1:max(which(triples.bkk[,3]<m)),1], triples.bkk[1:max(which(triples.bkk[,3]<m)),2], pch=16, cex=.5, asp=1, col="darkgreen", xlab = "a", ylab = "b", main = "90 degree triples, c<10000")
curve(sqrt((triples.bkk[max(which(triples.bkk[,3]<m)),3])^2-x^2), from = 0, to=(triples.bkk[max(which(triples.bkk[,3]<m)),3])*cos(pi/4), add = T, col="darkred", lwd=1.5)
segments(0,0,0,triples.bkk[max(which(triples.bkk[,3]<m)),3], col="darkred", lwd=1.5)
segments(0,0,triples.bkk[max(which(triples.bkk[,3]<m)),3]*cos(pi/4),triples.bkk[max(which(triples.bkk[,3]<m)),3]*sin(pi/4), col="darkred", lwd=1.5)

#location of rational points on octant
plot(triples.bkk[,1]/triples.bkk[,3], triples.bkk[,2]/triples.bkk[,3], pch=".", cex=.01, asp=1, col="darkgreen", xlab="a", ylab="b")

#triple tree-------------
M1=matrix(data=c(1,2,2,2,1,2,2,2,3), byrow = T, nrow = 3)
M2=matrix(data=c(1,-2,2,2,-1,2,2,-2,3), byrow = T, nrow = 3)
M3=matrix(data=c(-1,2,2,-2,1,2,-2,2,3), byrow = T, nrow = 3)

P0=matrix(data=c(3,4,5), byrow = F, nrow=3)

n=4
triple_tree=vector(mode="list",length = n)
triple_tree[[1]]=array(P0, dim=c(3,1,1))
for(j in 2:n){
  triple_tree[[j]]=array(NA, dim=c(3,1,3^(j-1)))
  l=0
  s=0
  while(s < length(triple_tree[[j-1]][1,,])){
    s=s+1  
  for(k in 1:3){
    l=l+1
     triple_tree[[j]][,,l]=sort(eval(parse(text=paste("M",k,"%*% triple_tree[[j-1]][,,s]", sep=""))))
  }
  }
}

par(pty="m", mar=c(2,2,2,1), mgp=c(1.1, .3, 0))
plot(triples.bkk[,1]/triples.bkk[,3], triples.bkk[,2]/triples.bkk[,3], pch=16, cex=.5, asp=1, col="gray25", xlab="a/c", ylab="b/c", main="primary b.k.k triples")

col= adjustcolor(c("red", "royalblue3", "orangered", "rosybrown4"),alpha.f = .5)
psize=c(4.5,3.5,2.5,1.5)

for(j in 1:length(triple_tree)){
  for (k in 1:length(triple_tree[[j]][1,,])) {
    points(triple_tree[[j]][1,,k]/triple_tree[[j]][3,,k], triple_tree[[j]][2,,k]/triple_tree[[j]][3,,k], pch=16, col=col[j], cex=psize[j]) 
    
  }
}

#divisibility
te=triples.bkk[order(triples.bkk[,3], triples.bkk[,1]),]
m=40100
te=te[which(triples.bkk[,3]<m),]

d=c(3, 4, 5,7,8)
te=te/d[4]
ye=which(sapply(1:nrow(te), function(x) any(te[x,] %in% round(te[x,],0))))
length(ye)/nrow(te)


# from dhenu saMkhya
f=sapply(1:20, function(x) fibonacci(x))
j=5
f[j]*f[j+3]; 2*f[j+1]*f[j+2]; f[2*j+3]


#integer altitude triples---------
n=200
abox=rep(NA,100*100)
bbox=rep(NA,100*100)
dbox=rep(NA,100*100)
l=1
for (j in 1:n) {
  for (k in 1:n) {
    if(coprime(j,k) && j>k){
      dbox[l]=2*j*k*(j^2-k^2)
      abox[l]=2*j*k*(j^2+k^2)
      bbox[l]=(j^2+k^2)*(j^2-k^2)
      l=l+1
    }
  }
}
triples.rta=as.array(cbind(abox, bbox, dbox))
triples.rta=triples.rta[!is.na(triples.rta[,1]),]
triples.rta=t(sapply(1:length(triples.rta[,1]), function(x) triples.rta[x,]/gcd(gcd(triples.rta[x,1],triples.rta[x,2]),triples.rta[x,3])))
triples.rta=t(sapply(1:length(triples.rta[,1]), function(x) sort(triples.rta[x,], decreasing = T)))
triples.rta=unique(triples.rta,MARGIN = 1)
triples.rta=triples.rta[order(triples.rta[,3]),]

triples.rta=triples.rta[order(triples.rta[,1], triples.rta[,2]),]

#division of hypotenuse by altitude
te=(triples.rta[,1]^2-triples.rta[,3]^2)/(triples.rta[,2]^2-triples.rta[,3]^2)
te=triples.rta[,3]/sqrt(triples.rta[,1]^2+triples.rta[,2]^2)


