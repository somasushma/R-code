# coords=list()
# params=list()

n1=1.5
n2=1.3
n3=1.4
m=7
a=1.22
b=.1

j=21

params[[j]]=c(n1,n2,n3,m,a,b)
  
t=seq(0, 64*pi, 64*pi/100000)
ft=function(t) (abs(1/a*cos(m/4*t))^n2+abs(1/b*sin(m/4*t))^n3)^(-1/n1)
x=ft(t)*cos(t)
y=ft(t)*sin(t)

coords[[j]]= as.array(cbind(x,y))
coords[[j]]=coords[[j]]/max(coords[[j]])

par(mar=c(2,2,2,1), pty="s", mgp=c(1.2,.4,0))
plot(coords[[j]], pch=16, type="n", axes = F, xlab="", ylab="", asp=1)
lines(coords[[j]], col="blue", lwd=2)


par(pty="s",mar=c(1,1,2,1), mgp=c(1.2,.4,0), mfrow=c(5,5))
for(j in 1:length(coords)){
plot(coords[[j]], pch=16, type="n", axes = F, xlab="", ylab="", main=bquote(.(paste(params[[j]], collapse = ","))), cex.main=2, asp=1)
lines(coords[[j]], col="blue", lwd=2)
}

dev.copy(png, file="~/R/Figures/Figures1/supershapes01.png", width=1500, height=1500)
dev.off()
