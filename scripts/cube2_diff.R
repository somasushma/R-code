isInt=function(x) round(x, 10)== round(x, 0)

n=10000
cb.triples=array(0, dim = c(n,3))
j=1
for(x in 1:n){
  for (y in (x+1):n) {
    z=sqrt((x^3+y^3-2)/6)-1
    if(isInt(z)) {
      cb.triples[j,]=c(x,y,z)
      j=j+1
    }
  }
}
cb.triples=cb.triples[which(cb.triples[,1] !=0),]

