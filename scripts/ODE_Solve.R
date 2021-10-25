library(deSolve)

windowsFonts(f1 = windowsFont("Constantia"),
             f2 = windowsFont("MS Gothic"),
             f3 = windowsFont("Cambria"))

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
params= c(a=1.1, b=1.01, c=1.1)
states=c(x=.1, y=1.5, z=.5)
f.name= "Sprottian.A"


spA=function(t, states, params){
  with(as.list(c(states, params)),{
    dx = a*y
    dy = b*y*z-x
    dz = 1-c*y^2
    list(c(dx, dy, dz))
  }) 
}

#Sprottian.B
params= c(a=1.2, b=1.01, c=1.1)
states=c(x=.01, y=.01, z=.01)
f.name= "Sprottian.B"


spB=function(t, states, params){
  with(as.list(c(states, params)),{
    dx = a*y*z
    dy = b*(x-y)
    dz = c*(1-x^2)
    list(c(dx, dy, dz))
  }) 
}

#Sprottian.D
params= c(a=1.1, b=1.1, c=1.1)
states=c(x=.1, y=.51, z=.01)
f.name= "Sprottian.D"


spD=function(t, states, params){
  with(as.list(c(states, params)),{
    dx = -a*y
    dy = b*(x+z)
    dz = c*x*z+3*y^2
    list(c(dx, dy, dz))
  }) 
}

#Rayleigh-Benard
params= c(a=9, b=12, c=5)
states=c(x=1, y=1, z=1.2)
f.name= "RayBen"


RayBen=function(t, states, params){
  with(as.list(c(states, params)),{
    dx = a*(y-x)
    dy = b*x-y-x*z
    dz = x*y-c*z
    list(c(dx, dy, dz))
  }) 
}

#cubic trig
params= c(a=5, b=3.5, c=2)
states=c(x=0.0, y=0.0)
f.name= "cubic trig"


cubT=function(t, states, params){
  with(as.list(c(states, params)),{
    dx = a*y
    dy = -b*x^3+cos(2*pi*t/c)
    list(c(dx, dy))
  }) 
}

#oval
params= c(a=-1, b=1)
states=c(x=1.3, y=0)
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
states=c(x=0, y=.0001) #(for c(x=0, y=.0001)-> 2y^2=2x^2-x^4 Gerono lemniscate like; for c(x=1, y=.0001)-> ellipse: a=2*y; x=2*(1/sqrt(2)*y); center at x=1)
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

times= seq(0, 75, by = 0.01) #set time
soln= ode(y = states, times = times, func = func, parms = params)

#plots
par(mar=c(2,2.5,2,1), mgp=c(1.2, .3, 0))
plot(x=soln[,2], y=soln[,3],  type = "n", xlab = "x", ylab=bquote(dot(x)), main = bquote(a==.(params[1])~", "~b==.(params[2])~"; "~x[0]==.(states[1])~", "~y[0]==.(states[2])),  asp=1, family="f3")
grid(col = "gray60")
points(x=soln[,2], y=soln[,3],type = "l", col="#4C00FFFF")

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