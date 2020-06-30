library(deSolve)

#Lorenz attractor
params= c(a=10, b=8/3, c=28)
states=c(x=1, y=1, z=-1)
lorenz=function(t, states, params){
  with(as.list(c(states, params)),{
      dx = a*(y-x)
      dy =x * (c-z) -y
      dz = x*y  - b*z
        list(c(dx, dy, dz))
      }) 
}


#Brusselator
params= c(a=1, b=3)
states=c(x=1, y=1)
f.name= "Brusellator"

bruss=function(t, states, params){
  with(as.list(c(states, params)),{
    dx = a+x^2*y-b*x-x
    dy =b*x -x^2*y
    list(c(dx, dy))
  }) 
}

#van der Pol1
params= c(a=.1, b=.9, c=.95)
states=c(x=0, y=0)
f.name= "van der Pol-1"

vdP1=function(t, states, params){
  with(as.list(c(states, params)),{
    dx = y
    dy = a*(1-x^2)*y-c*x^3+b*cos(t)
    list(c(dx, dy))
  }) 
}

#van der Pol2
params= c(a=17, b=1, c=2*pi/11)
states=c(x=0, y=0)
f.name= "van der Pol-2"

vdP2=function(t, states, params){
  with(as.list(c(states, params)),{
    dx = y
    dy = a*(1-x^2)*y-x+b*cos(c*t)
    list(c(dx, dy))
  }) 
}

#Sprottian.A
params= c(a=.8, b=2.01, c=1.1)
states=c(x=0.05, y=0.05, z=0.05)
f.name= "Sprottian.A"


spA=function(t, states, params){
  with(as.list(c(states, params)),{
    dx = a*y
    dy = b*y*z-x
    dz = 1-c*y^2
    list(c(dx, dy, dz))
  }) 
}

#oval
params= c(a=-1, b=1)
states=c(x=1.2, y=0)
f.name= "Oval"


oval=function(t, states, params){
  with(as.list(c(states, params)),{
    dx = y
    dy = -a*x^2-b*x^3
    list(c(dx, dy))
  }) 
}


#duff
params= c(a=-1, b=1)
states=c(x=sqrt(5), y=0)
f.name= "Duffing"


duff=function(t, states, params){
  with(as.list(c(states, params)),{
    dx = y
    dy = -a*x-b*x^3
    list(c(dx, dy))
  }) 
}

# solve 
func=duff

times= seq(0, 200, by = 0.01) #set time
soln= ode(y = states, times = times, func = func, parms = params)

#plots
par(mar=c(2,2,2,1), mgp=c(1.1, .4, 0))
plot(x=soln[,2], y=soln[,3], pch=".", type = "o", xlab = "x", ylab="y", main = bquote(a==.(params[1])~", "~b==.(params[2])~", "~c==.(params[3])), col="darkblue", asp=1)

#3D plot
library("rgl")
plot3d(soln[,2], soln[,3], soln[,4], type= "l", col="darkblue", xlab="x", ylab="y", zlab = "z")
rglwidget()


par(mar=c(2,2,0,1), mgp=c(1.1, .4, 0), oma=c(0,0,2,0))
par(mfrow=c(2,1))
plot(x=soln[,1], y=soln[,2], ylim=range(soln[,2]), xlab = "time", ylab = "x", type="n", col="gray20", lwd=2)
grid(col = "gray60")
points(x=soln[,1], y=soln[,2], type="l", col="gray20", lwd=2)
plot(x=soln[,1], y=soln[,3], ylim=range(soln[,2]), xlab = "time", ylab = "y", type="n", col="gray20", lwd=2)
grid(col = "gray60")
points(x=soln[,1], y=soln[,3], type="l", col="gray20", lwd=2)
mtext(bquote(.(f.name)~a==.(params[1])~", "~b==.(params[2])~", "~c==.(params[3])), side = 3 , outer = T, cex = 2)



#x only
par(mar=c(2,2,2,1), mgp=c(1.1, .4, 0), oma=c(0,0,0,0), mfrow=c(1,1))
par(mfrow=c(6,1))
plot(x=soln[,1], y=soln[,2],  xlab = "t", ylab = "x", main = bquote(~a==.(params[1])~", "~b==.(params[2])~", "~c==.(params[3])), type="n", col="gray20", lwd=2)
grid(col = "gray60")
points(x=soln[,1], y=soln[,2], type="l", col="gray20", lwd=2)

dev.copy(device = png, file="~/cutting_block/R/figures/vDP2.png", height=6, width=14, units="in", res=300)
dev.off()