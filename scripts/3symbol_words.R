library("gtools")

v=c(1,2,3)
n=6
s3w=vector(mode = "list", length = n)
for (j in 1:n) {
  m=permutations(n=3,r=j,v=v, repeats.allowed = T)
  l1=1
  for (k in 1:length(m[,1])) {
  for (l in 1:length(m[k,])) {
    if(m[k,l]==2 && !(1 %in% m[k,c(1:l)])) { 
       break
    } else if(m[k,l]==3 && !all(c(1,2) %in% m[k,c(1:l)])){
      break  
    }
    if(l==j) {
      s3w[[j]][l1]=paste(m[k,], collapse = "")
      l1=l1+1
      }
  }  
  }
}

lengths(s3w)

#obtain terminal word
s3wt=lapply(s3w, function(y) paste(sapply(y, function(x) {te=nchar(x); substr(x, start = te, stop = te)}), collapse = ""))

lengths(gregexpr("1", s3wt[[4]])) #number of 1s in terminal word