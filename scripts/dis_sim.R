# install.packages("poweRlaw")
# library("poweRlaw")

n=10000 #population size
m=25 #number of days
popbox=vector(mode = "list", length = m+1) #population over time
a=7 #infective period before recovery
d=.01 #death rate
days=rep(0,n) #days of infection
pop=rep(0,n) #population
ifct=rep(0,n) #number of transmissions
fst=sample(x=c(1:n),size = 1,replace = F) #first infected person
pop[fst]=1
popbox[[1]]=pop
p=round(rplcon(n=n, xmin=1, alpha=5)) #power law infection transmisson
for (j in 1:m) {
  l=which(pop==1)
  days[l]= days[l]+1
  pop[which(days >= a)]=2
  infcs=sample(x = p,size = length(l),replace = F)
  ifct[l[which(ifct[l]==0)]]=infcs[which(ifct[l]==0)]

  if(runif(1, min=0, max = 1) <= .5) pos=-1 else pos=1
  k=unlist(sapply(l, function(x) x+c((sign(pos)*ifct[x]):pos)))
  
  if(length(k)>0){
    k[which(k>n)]=k[which(k>n)]-n  
    k[which(k<1)]=k[which(k<1)]+n
    pop[k[which(pop[k]==0)]]=1
  } 
  ind=sample(x = 1:n, size = n,replace = F)
  pop=pop[ind]
  days=days[ind]
  ifct=ifct[ind]
  popbox[[j+1]]=pop
  
}
uninfected=unlist(lapply(popbox, function(x) length(which(x==0))))
infected=unlist(lapply(popbox, function(x) length(which(x==1))))
recovered=unlist(lapply(popbox, function(x) length(which(x==2))))

windowsFonts(f1 = windowsFont("Constantia"),
             f2 = windowsFont("MS Gothic"),
             f3 = windowsFont("Cambria"))

par(mfrow=c(1,1), mar=c(2,2,2,1), mgp=c(1.1,.4,0))
plot(uninfected, type="o", pch=16, col="brown", main="disease progression", xlab = "days", ylab = "numbers", family= "f1")
points(infected, type="o", pch=16, col="blue")
points(recovered, type="o", pch=16, col="darkgreen")

te=as.vector(outer(X = c(1:100), Y = c(1:100)*1i, FUN = "+"))
col=c("gray", "red", "cyan")
par(mfrow=c(5,5))
lapply(1:25, function(x)  plot(te, col=col[popbox[[x]]+1], pch=16, asp=1, axes=F, xlab="", ylab="", main=x))
