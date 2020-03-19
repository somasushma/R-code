# install.packages("poweRlaw")
# library("poweRlaw")

n=10000
m=30
popbox=vector(mode = "list", length = m+1)
a=15
d=.01
days=rep(0,n)
pop=rep(0,n)
fst=sample(x=c(1:n),size = 1,replace = F)
pop[fst]=1
popbox[[1]]=pop
p=round(rplcon(n=10000, xmin=1, alpha=3.5))
for (j in 1:m) {
  days[which(pop==1)]= days[which(pop==1)]+1
  pop[which(days >= a)]=2
  ifct=sample(x = p,size = 1,replace = F)
  
  if(runif(1, min=0, max = 1) <= .5) pos=-1*ifct else pos=1*ifct
  k=as.vector(sapply(which(pop==1), function(x) x+c((sign(pos)*1):pos)))
  
 if(length(k)>0){
  k[which(k>n)]=k[which(k>n)]-n  
  k[which(k<1)]=k[which(k<1)]+n
  pop[k[which(pop[k]==0)]]=1
 } 
  ind=sample(x = 1:n, size = n,replace = F)
  pop=pop[ind]
  days=days[ind]
  popbox[[j+1]]=pop
  
}
uninfected=unlist(lapply(popbox, function(x) length(which(x==0))))
infected=unlist(lapply(popbox, function(x) length(which(x==1))))
recovered=unlist(lapply(popbox, function(x) length(which(x==2))))

windowsFonts(f1 = windowsFont("Constantia"),
             f2 = windowsFont("MS Gothic"),
             f3 = windowsFont("Cambria"))

par(mar=c(2,2,2,1), mgp=c(1.1,.4,0))
plot(uninfected, type="o", pch=16, col="brown", main="disease progression", xlab = "days", ylab = "numbers", family= "f1")
points(infected, type="o", pch=16, col="blue")
points(recovered, type="o", pch=16, col="darkgreen")
