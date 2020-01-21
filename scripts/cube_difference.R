cbrt=function(x) x^(1/3)

te=sapply(1:10000, function(x) 3*x^2+3*x+1)
ye=list()
for (j in 1:length(te)) {
  ye[[j]]=te[j]-(1:floor(te[j]^(1/3)))^3
}

cb.diff=which(unlist(lapply(ye, function(x) any(round(x^(1/3)-round(x^(1/3),0), 10)==0))))
te=ye[cb.diff]
cbrts=lapply(1:length(te), function(x) which(round(cbrt(te[[x]])-round(cbrt(te[[x]]),0), 10)==0))

pander(cbind(cb.diff, unlist(lapply(cbrts, function(x) paste(x, collapse = ", ")))), row.names = F, col.names=c("c", "ns"), justify="rr")

cbind(table(unlist(cbrts)))

#hexagonal spiral
e=exp(1)
n=5
rots=vector(mode="list", length = n)
for(j in 1:n){
  te=c(0:(j*6-1))
  rots[[j]]=e^(te*2*pi/length(te)*1i)
  rots[[j]]=c(tail(rots[[j]],(j-1)), head(rots[[j]],length(rots[[j]])-(j-1)))
}
rots=c(0+0i, rots)
rots=unlist(rots)*c(0,seq(.3, 7, length.out = (length(unlist(rots))-1)))
par(mar=c(1,1,2,1))
plot(rots, type="n", asp=1, axes = F, xlab="", ylab="")
lines(rots, lty=2)
text(rots, labels = 1:length(rots), col = "dodgerblue3", font=2)
lines(rots[c(6,17,34, 57, 86)], col="red", lty=3)
