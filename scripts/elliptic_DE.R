library(deSolve)

windowsFonts(f1 = windowsFont("Constantia"),
             f2 = windowsFont("MS Gothic"),
             f3 = windowsFont("Cambria"))

f.name= "elliptic"


elliptic=function(t, states, params){
  with(as.list(c(states, params)),{
    dx = y
    #dy =-2*k^2*x^3+(2-k^2)*x #dn
    dy=-2*k^2*x^3+(2*k^2-1)*x #cn
    #dy= 2*k^2*x^3-(1+k^2)*x #sn
    list(c(dx, dy))
  }) 
}

kbox=c(.98, .8, .71, .5)
for (k in kbox) {
  params= c(k=k)

  xbox=c(2, -2, 1.5, -1.5, 1,-1, .75, -.75, .6, -.6, .5, -.5, .25, -.25, .15, -.15, 0.005, -0.005)
  m=1
  for (x in xbox) {
  states=c(x=x, y=0)
    # solve 
    func=elliptic
    times= seq(0, 75, by = 0.01) #set time
    soln= ode(y = states, times = times, func = func, parms = params)
    
    #plots
    if(m==1){
      par(mar=c(2,2.5,2,1), mgp=c(1.1, .3, 0))
      plot(x=soln[,2], y=soln[,3],  type = "n", xlim = c(max(soln[,2]), -max(soln[,2])), xlab = "x", ylab=bquote(dot(x)), main = bquote(k==.(params[1])),  asp=1, family="f3")
      grid(col = "gray60")
      
      points(x=soln[,2], y=soln[,3],type = "l", col="#4C00FFFF", lwd=2)  
    } else{
      
      points(x=soln[,2], y=soln[,3],type = "l", col="#4C00FFFF", lwd=2)  
    }

    m=m+1
  }
}


dev.copy(png, file="~/R/Figures/Figures1/Elliptic_cn_ovals.png", width=11, height=8, res=300, units="in")
dev.off()



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