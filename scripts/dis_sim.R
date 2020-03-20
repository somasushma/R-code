# install.packages("poweRlaw")
# library("poweRlaw")

# install.packages("poweRlaw")
# library("poweRlaw")

n=40000 #population size; give square number for square plot
m=25 #number of days
popbox=vector(mode = "list", length = m+1) #population over time
a=7 #infective period before recovery
d=.01 # effective death rate
dd=c(rep(1,d*n),rep(0, (n-d*n)))
dd=sample(x = dd, size = n, replace = F) #death distribution
days=rep(0,n) #period of infection in integer time units 
pop=rep(0,n) #population
ifct=rep(0,n) #number of transmissions
fst=sample(x=c(1:n),size = 1,replace = F) #first infected person
pop[fst]=1
popbox[[1]]=pop
p=round(rplcon(n=n, xmin=.2, alpha=2.5)) #power law infection transmisson
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
  ii=which(pop==1)
  dyi=sum(sample(x=dd, size = length(ii),replace = F))
  pop[sample(x=ii, size = dyi, replace = F)]=3
    
  ind=sample(x = 1:n, size = n,replace = F)
  pop=pop[ind]
  days=days[ind]
  ifct=ifct[ind]
  popbox[[j+1]]=pop
  
}
uninfected=unlist(lapply(popbox, function(x) length(which(x==0))))
infected=unlist(lapply(popbox, function(x) length(which(x==1))))
recovered=unlist(lapply(popbox, function(x) length(which(x==2))))
dead=unlist(lapply(popbox, function(x) length(which(x==3))))

windowsFonts(f1 = windowsFont("Constantia"),
             f2 = windowsFont("MS Gothic"),
             f3 = windowsFont("Cambria"))

#plot progression
par(mfrow=c(1,1), mar=c(2,2,2,1), mgp=c(1.05,.3,0))
plot(uninfected, type="o", pch=16, col="brown", main="disease progression", xlab = "time units", ylab = "numbers", family= "f1")
points(infected, type="o", pch=16, col="blue")
points(recovered, type="o", pch=16, col="darkgreen")
points(dead, type="o", pch=16, col="black")

#plot population. white uninfected, brown infected, green recovered, black dead.
te=as.vector(outer(X = c(1:sqrt(n)), Y = c(1:sqrt(n))*1i, FUN = "+"))
col=c("#f5f5f5", "#a6611a", "#018571", "black")
par(mfrow=c(5,5))
lapply(1:25, function(x)  plot(te, col=col[popbox[[x]]+1], pch=".", asp=1, axes=F, xlab="", ylab="", main=x))
