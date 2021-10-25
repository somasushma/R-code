#--------
meru <- function(n) {
  if(n <0) {
    return("wrong!")
  } else if(n >= 0 && n < 2){
    return(n)
  } else     return(meru(n-1) + meru(n-2))
}

meru=Vectorize(meru)


te=meru(2:12)
for(k in 1:(length(te)-1)) {
  
  n=300
  f=rep(0,n)
  f[1]=1/te[k]; f[2]=1/te[k+1]
  for(j in 3:n) {
    f[j]=f[j-1]*sqrt(1-(f[j-2])^2)+f[j-2]*sqrt(1-(f[j-1])^2)
    #f[j]=sqrt(1-(f[j-2])^2)*sqrt(1-(f[j-1])^2)
    #f[j]=(1-(f[j-2])^2)^(2/3)*(1-(f[j-1])^2)^(2/3)
  }
  
  sins=sort(tail(f,3), decreasing = T)
  p1=c(0,0)
  p2=c(1,0)
  p3=c(sins[2]/sins[3]*cos(asin(sins[1])),sins[2]/sins[3]*sins[1])
  p=rbind(p1,p2,p3)
  
  par(pty="m", mar=c(1,1,1.5,1), mgp=c(0,0,0))
  plot(p, pch=16, asp=1, col="darkblue", ylim=c(0,p[3,2]+.15), axes = F, xlab = "", ylab="", main= paste0("x1=", fractions(f[1], 15), "; x2=", fractions(f[2],15)))
  polygon(p, col="darkolivegreen1")
  
  text(p, labels = round(asin(sins)*180/pi, 2),pos = c(2,4,3))
  print(paste((asin(sins)*180/pi)[1]-(asin(sins)*180/pi)[2],(asin(sins)*180/pi)[2]-(asin(sins)*180/pi)[3], collapse = " "))
}

# paste0("x1=", fractions(f[1], 15), "; x2=", fractions(f[2],15))
# parse(text = paste0('x1 == 1/sqrt(2)', '~";"~', 'x2==1/sqrt(2)'))

#cols: "darkolivegreen1", "gold"