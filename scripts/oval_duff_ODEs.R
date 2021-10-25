library(deSolve)

windowsFonts(f1 = windowsFont("Constantia"),
             f2 = windowsFont("MS Gothic"),
             f3 = windowsFont("Cambria"))




#duff
params= c(a=-1, b=1)
states=c(x=1.41, y=0) #(for c(x=0 or x=sqrt(2), y=.0001)-> 2y^2=2x^2-x^4 Gerono lemniscate like; for c(x=1, y=.0001)-> ellipse: a=2*y; x=2*(1/sqrt(2)*y); center at x=1)

f.name= "Duffing"


duff=function(t, states, params){
  with(as.list(c(states, params)),{
    dx = y
    dy = -a*x-b*x^3
    list(c(dx, dy))
  }) 
}

#oval
params= c(a=-1, b=1)
states=c(x=3.333, y=0.0)
f.name= "Oval"


oval=function(t, states, params){
  with(as.list(c(states, params)),{
    dx = y
    dy = -a*x^2-b*x^3
    list(c(dx, dy))
  }) 
}


#elliptic
params= c(k=1/sqrt(2))
states=c(x=.9, y=0)
f.name= "elliptic"


elliptic=function(t, states, params){
  with(as.list(c(states, params)),{
    dx = y
    dy =-2*k^2*x^3-(1-2*k^2)*x
    list(c(dx, dy))
  }) 
}


# solve 
func=elliptic

times= seq(0, 75, by = 0.01) #set time
soln= ode(y = states, times = times, func = func, parms = params)

#plots
par(mar=c(2,2.5,2,1), mgp=c(1.1, .3, 0))
plot(x=soln[,2], y=soln[,3],  type = "n", xlab = "x", ylab=bquote(dot(x)), main = bquote(a==.(params[1])~", "~b==.(params[2])~"; "~x[0]==.(states[1])~", "~y[0]==.(states[2])),  asp=1, family="f3")
grid(col = "gray60")
points(x=soln[,2], y=soln[,3],type = "l", col="#4C00FFFF", lwd=2)

#3D plot
library("rgl")
plot3d(soln[,2], soln[,3], soln[,4], type= "l", col="darkred", xlab="x", ylab="y", zlab = "z")
bg3d("lightgray")
rglwidget()

#x-y-z with time
par(mar=c(2,2.5,0,1), mgp=c(1.1, .4, 0), oma=c(0,0,0,0))
par(mfrow=c(length(states),1))

#x
plot(x=soln[,1], y=soln[,2], ylim=range(soln[,2:ncol(soln)]), xlab = "t", ylab = "x", main=bquote(a==.(params[1])~", "~b==.(params[2])~", "~c==.(params[3])), type="n", col="gray20", lwd=2, family="f3")
grid(col = "gray60")
points(x=soln[,1], y=soln[,2], type="l", col="gray20", lwd=2)

#y
plot(x=soln[,1], y=soln[,3], ylim=range(soln[,2:ncol(soln)]), xlab = "t", ylab = bquote("y|"~dot(x)), type="n", col="gray20", lwd=2, family="f3")
grid(col = "gray60")
points(x=soln[,1], y=soln[,3], type="l", col="gray20", lwd=2)
#z
plot(x=soln[,1], y=soln[,3], ylim=range(soln[,2:ncol(soln)]), xlab = "t", ylab = "y", type="n", col="gray20", lwd=2)
grid(col = "gray60")
points(x=soln[,1], y=soln[,3], type="l", col="gray20", lwd=2)

mtext(bquote(.(f.name)~a==.(params[1])~", "~b==.(params[2])~", "~c==.(params[3])), side = 3 , outer = T, cex = 2)



#x only
par(mar=c(2,2,2,1), mgp=c(1.1, .4, 0), oma=c(0,0,0,0), mfrow=c(1,1))
par(mfrow=c(6,1))
plot(x=soln[,1], y=soln[,2],  xlab = "t", ylab = "x", main = bquote(~a==.(params[1])~", "~b==.(params[2])~", "~c==.(params[3])), type="n", col="gray20", lwd=2)
grid(col = "gray60")
points(x=soln[,1], y=soln[,2], type="l", col="gray20", lwd=2)

dev.copy(device = png, file="~/R/Figures/Figures1/cubTrigX5.png", height=5, width=11, units="in", res=300)
dev.off()