library(deSolve)
windowsFonts(f1 = windowsFont("Constantia"),
             f2 = windowsFont("MS Gothic"),
             f3 = windowsFont("Cambria"))

vdP2=function(t, states, params){
  with(as.list(c(states, params)),{
    dx = y
    dy = a*(1-x^2)*y-x+b*cos(c*t)
    list(c(dx, dy))
  }) 
}
f.name= "van der Pol-2"
func=vdP2

den=10
abox=seq(5, 18, .05)
param.list=lapply(abox, function(x) c(a=x, b=1.05, c=2*pi/den))
soln.list=vector(mode = "list", length = length(param.list))
tout=rep(NA,  length(soln.list))

for (j in 1:length(soln.list)) {
#van der Pol2
params= param.list[[j]]
states=c(x=0, y=0)

# solve and plot

times= seq(0, 300, by = 0.01) #set time
soln= ode(y = states, times = times, func = func, parms = params)
soln.list[[j]]=soln
te=which(sapply(2:(nrow(soln)-1), function(x) soln[x+1,2] < soln[x,2] & soln[x-1,2] < soln[x,2]))
te=te[which(round(soln[te,2])>=2)]
ye=tail(soln[te,1], -1)
tout[j]=mean(diff(ye))

}
#mean period verus A
par(mar=c(2,2,2,1), mgp=c(1.1, .4, 0))

plot(x=abox, y=tout, type="n", xlab = "a", ylab=bquote(P[mean]), main = bquote(a==.(head(param.list,1)[[1]][1])~".."~.(tail(param.list,1)[[1]][1])~", "~b==.(params[2])~", "~c==2*pi/.(den)), family="f3")
grid(col = "gray60")
abline(v=5:18, col = "gray60", lty=3)
points(x=abox, y=tout, type="l")

#x only plots
par(mar=c(2,2,2,1), mgp=c(1.1, .4, 0), mfrow=c(6,2))
# jbox=(1:12)*round(length(soln.list)/12)
jbox = sort(sample(1:length(abox), 12))
for (j in jbox) {
  soln=soln.list[[j]]
  params=param.list[[j]]
  plot(x=soln[,1], y=soln[,2],  xlab = "t", ylab = "x", main = bquote(a==.(params[1])~", "~b==.(params[2])~", "~c==2*pi/11), type="n", col="darkblue", lwd=2, family="f3")
  grid(col = "gray60")
  points(x=soln[,1], y=soln[,2], type="l", col="darkblue", lwd=2)
}

#dx/dt vs x
par(mar=c(2,2,2,1), mgp=c(1.1, .4, 0), mfrow=c(3,2))
for (j in (1:6)*round(length(soln.list)/6)) {
  soln=soln.list[[j]]
  params=param.list[[j]]
plot(x=soln[,2], y=soln[,3], pch=".", type = "o", xlab = "x", ylab="y", main = bquote(a==.(params[1])~", "~b==.(params[2])~", "~c==.(params[3])), col="darkblue", family="f3")
}

dev.copy(device = png, file="~/cutting_block/R/figures/vDP2.png", height=6, width=12, units="in", res=300)
dev.off()

#others
par(mar=c(2,2,0,1), mgp=c(1.1, .4, 0), oma=c(0,0,2,0))
par(mfrow=c(2,1))
plot(x=soln[,1], y=soln[,2], ylim=range(soln[,2:ncol(soln)]), xlab = "time", ylab = "x", type="n", col="gray20", lwd=2)
grid(col = "gray60")
points(x=soln[,1], y=soln[,2], type="l", col="gray20", lwd=2)
plot(x=soln[,1], y=soln[,3], ylim=range(soln[,2:ncol(soln)]), xlab = "time", ylab = "y", type="n", col="gray20", lwd=2)
grid(col = "gray60")
points(x=soln[,1], y=soln[,3], type="l", col="gray20", lwd=2)
mtext(bquote(.(f.name)~a==.(params[1])~", "~b==.(params[2])~", "~c==.(params[3])), side = 3 , outer = T, cex = 2)



