n=10
k=3
f=rep(NA,n)
f[1]=1
f[2]=f[1]+k
for (j in 3:n) {
  f[j]=k*f[j-1]+f[j-2]
}

paste(f, collapse = ", ")
#----------
n=10
k=3
r1=as.character(paste(c(rep(1,k),0),collapse = ""))
r2=as.character(1)
wl=vector(mode = "list", length = n)
wl[[1]]="1"
for (j in 2:n) {
  te=unlist(strsplit(wl[[j-1]],""))
  te=sapply(te, function(x) {if(x==1) gsub("1",r1,x) else gsub("0",r2,x)})
  wl[[j]]=paste(te,collapse = "")
}

lengths(gregexpr("1",wl[[n]]))/lengths(gregexpr("0",wl[[n]]))
paste(sapply(unlist(wl), function(x) sprintf("%s\rightarrow ",x)), collapse = "")
#----------
f= function(x){
  if(x<=1) return(1)
  else return(3*f(x-1) + f(x-2))
}