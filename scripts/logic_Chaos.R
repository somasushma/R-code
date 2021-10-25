library(Rmpfr)
n=75
prec=1000
den=2000
deno=mpfr(den, prec)
itbox=rep(0,(den+1))

for (k in 0:den) {
xn=mpfr(k,prec)/deno
xbox=rep(0,n)
for (j in 1:n) {
  xbox[j]=as.numeric(formatMpfr(xn))
  xn1=1-abs((1-xn)-xn)
  xn=xn1
if(any(as.numeric(formatMpfr(xn)) == xbox)){
  itbox[k+1]=j
  break
} 
}
}

plot(x=c(0:den)/den, y=itbox, type="l")