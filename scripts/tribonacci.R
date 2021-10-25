n=100
a=rep(NA,n)
a[1]=0
a[2]=1
a[3]=1
for (j in 4:n) {
  a[j]=sum(c(a[j-1],a[j-2],a[j-3]))
}


rt=polyroot(c(-1,-1,-1,1))
tau=Re(rt[3])
tau=1/3*(1+(19-3*sqrt(33))^(1/3)+(19+3*sqrt(33))^(1/3))

#compute high precision root
library(Rmpfr)
prec=500
tau=unirootR(function(x) x^3-x^2-x-1,interval = mpfr(c(1.5,2),prec), tol=10^-50, maxiter=100)$root

#series 1 for pi
n=200
f=rep(NA,n)
m=n/4
for (j in 1:m) {
  k=4*(j-1)+1
  f[k]=1
  f[k+1]=2*((-1)^(j-1))
  f[k+2]=-1
  f[k+3]=0
}

peebox=rep(NA,n)
for (j in 1:n) {
  pee=mpfr(4,prec)*sum(sapplyMpfr(1:j, function(x) mpfr(f[x],prec)*mpfr(1,prec)/(mpfr(x,prec)*mpfr(tau,prec)^mpfr(x,prec))))
  peebox[j]=formatMpfr(pee)
}

par(pty="m", mar=c(2,2,1,1), mgp=c(1.1,.4,.0))
plot(as.numeric(peebox),type="l", ylim=c(3,3.35), ylab="value", col="darkgreen", lwd=3)
abline(h=pi, col="brown", lty=2)

plot(as.numeric(formatMpfr(-round(log10(abs(mpfr(peebox)-Const("pi", prec)))))),type="h", ylab = "# of correct digits")


#mAdhava series-----------
n=500
f=rep(NA,n)
k=-1
for (j in 1:n) {
  k=-1*k
  f[j]=k
}

peebox.m=rep(NA,n)
for (j in 1:n) {
  pee=mpfr(4,prec)*sum(sapplyMpfr(1:j, function(x) mpfr(f[x],prec)/(mpfr(2,prec)*mpfr(x,prec)-mpfr(1,prec))))
  peebox.m[j]=formatMpfr(pee)
}

par(pty="m", mar=c(2,2,1,1), mgp=c(1.1,.4,.0))
plot(as.numeric(peebox.m),type="l", ylim=c(3,3.35), ylab="value", col="darkgreen", lwd=3)
abline(h=pi, col="brown", lty=2)

plot(as.numeric(formatMpfr(-round(log10(abs(mpfr(peebox.m)-Const("pi", prec)))))),type="h", ylab = "# of correct digits")


#Eulerian formula-----------
c1=mpfr(1,prec)/mpfr(2,prec)
c2=mpfr(1,prec)/mpfr(3,prec)
c3=mpfr(1,prec)/mpfr(8,prec)

n=200
f=rep(NA,n)
k=-1
for (j in 1:n) {
  k=-1*k
  f[j]=k
}

peebox.e=rep(NA,n)
for (j in 1:n) {
  te=sum(sapplyMpfr(1:j, function(x) mpfr(f[x],prec)*c1^(mpfr(2,prec)*mpfr(x,prec)-mpfr(1,prec))/(mpfr(2,prec)*mpfr(x,prec)-mpfr(1,prec))))
  ye=sum(sapplyMpfr(1:j, function(x) mpfr(f[x],prec)*c2^(mpfr(2,prec)*mpfr(x,prec)-mpfr(1,prec))/(mpfr(2,prec)*mpfr(x,prec)-mpfr(1,prec)))) 
  #ge=sum(sapplyMpfr(1:j, function(x) mpfr(f[x],prec)*c3^(mpfr(2,prec)*mpfr(x,prec)-mpfr(1,prec))/(mpfr(2,prec)*mpfr(x,prec)-mpfr(1,prec))))
  pee=mpfr(4,prec)*(te+ye)
  peebox.e[j]=formatMpfr(pee)
}

par(pty="m", mar=c(2,2,1,1), mgp=c(1.1,.4,.0))
plot(as.numeric(peebox.e),type="l", ylim=c(3,3.35), ylab="value", col="darkgreen", lwd=3)
abline(h=pi, col="brown", lty=2)

plot(as.numeric(formatMpfr(-round(log10(abs(mpfr(peebox.e)-Const("pi", prec)))))),type="h", ylab = "# of correct digits")


#nampUtiri modest pi series
n=200
4*(3/4+sum(sapply(1:n, function(x) ((-1)^(x+1)/((2*x+1)^3-(2*x+1))) )))
sum(sapply(1:n, function(x) ((-1)^(x+1))*16/((2*x-1)^5+4*(2*x-1))) )

#faster nampUtiri pi series
sum(sapplyMpfr(1:n, function(x) mpfr(-1,prec)^(mpfr(x,prec)-mpfr(1,prec))*mpfr(2,prec)*sqrt(mpfr(3,prec))/(mpfr(3,prec)^(mpfr(x,prec)-mpfr(1,prec))*(mpfr(2,prec)*mpfr(x,prec)-mpfr(1,prec)))))

#arcsin series
prec=1000
n=200
#with phi
phi=(mpfr(1,prec)+sqrt(mpfr(5,prec)))/mpfr(2,prec)

mpfr(10,prec)*sum(sapplyMpfr(0:n, function(x) factorial(mpfr(2,prec)*mpfr(x,prec))/((mpfr(2,prec)*mpfr(x,prec)+mpfr(1,prec))*mpfr(2,prec)^(mpfr(4,prec)*mpfr(x,prec)+mpfr(1,prec))*(factorial(mpfr(x,prec)))^mpfr(2,prec)*phi^(mpfr(2,prec)*mpfr(x,prec)+mpfr(1,prec)))))

#with half
mpfr(6,prec)*sum(sapplyMpfr(0:n, function(x) factorial(mpfr(2,prec)*mpfr(x,prec))/((mpfr(2,prec)*mpfr(x,prec)+mpfr(1,prec))*mpfr(2,prec)^(mpfr(4,prec)*mpfr(x,prec)+mpfr(1,prec))*(factorial(mpfr(x,prec)))^mpfr(2,prec))))



# approximations from peebox-------
4*(3*tau^2+3*tau-1)/(3*tau^2+3*tau+3) #approximate pi formula 
a=1.837319 # substitute in above
e=exp(1)
a=3*(9*e^2+14*e-8)/(58*e)
4*(3*a^2+3*a-1)/(3*a^2+3*a+3)
# 24969675*x^3 - 57183585*x^2 - 97776993*x + 97337956 = 0 # root approximates pi

