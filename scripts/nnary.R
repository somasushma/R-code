nnary=function(x,y){
k=y
b=c()
while(k !=0){
  b=c(b, k %% x)
  k=floor(k/x)
}
return(rev(b))
}

nnary= function(x,y) {
  if(y==0){
    return(0)
  } else {
    k=y
    nnary(x,floor(k/x))
    return(cat(k %% x))
  }
}