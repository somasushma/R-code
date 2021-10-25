library("numbers")
library("pracma")
n=1000
dlist=vector(mode = "list", length = n)
dlist=lapply(1:n, function(x) divisors(x))

disld=lapply(dlist, function(x) sapply(1:(length(x)-1), function(y) x[y+1]-x[y]))
disld[[1]]=0

isldcnt=lapply(disld, function(x) rle(x))

f=lengths(dlist)-unlist(lapply(1:n, function(x) sum(isldcnt[[x]][[1]][which(isldcnt[[x]][[2]]==1)])))

par(pty="m", mar=c(2.2,2.2,2,1), mgp=c(1.2,.4,.0))
plot(f, type="h", col="darkgreen", xlab = "n", ylab="f[n]")
curve(.85*sqrt(x), from = 1, to=1000, add = T)