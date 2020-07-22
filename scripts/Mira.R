library(readr)

windowsFonts(f1 = windowsFont("Constantia"),
             f2 = windowsFont("Book Antiqua"),
             f3 = windowsFont("Cambria Math"))

Mira <- read_delim("~/R/Dataexperiments/data/astronomy/Mira.tsv", 
                   "\t", escape_double = FALSE, trim_ws = TRUE)
Mira = data.frame(Mira)

par(mar=c(2,2,2,1), mgp=c(1.1,.4,0), mfrow=c(1,1))
hist(Mira$Period, breaks = 30, col = gray.colors(31), main= "Mira periods", xlab = "Mira periods", family="f3")
grid(col="gray30")
abline(v=c(mean(Mira$Period), median(Mira$Period)), col=c("darkgreen", "darkred"), lwd=2, lty=3)
box()

Mira$magDiff = Mira$magMin - Mira$magMax

par(mar=c(2,2,2,1), mgp=c(1.1,.4,0), mfrow=c(1,1))
hist(Mira$magDiff, breaks = 30, col = gray.colors(31), main= "Mira magnitude amplitudes", xlab = "Mira magnitude amplitudes", family="f3")
grid(col="gray30")
box()


#Milky Way objects

MWO <- read_delim("R/Dataexperiments/data/astronomy/MW_star_ass.tsv",  "\t", escape_double = FALSE, col_types = cols( `_DEJ2000` = col_double(), `_RAJ2000` = col_double(),`Diam-a` = col_double(), `Diam-b` = col_double(), Class1 = col_character(),  Class2 = col_character(), Com = col_character(),Name = col_character()), comment = "#", trim_ws = TRUE)

colnames(MWO)=c("RAJ2000", "DEJ2000", "Diam_a", "Diam_b", "Name", "Class1", "Class2", "Com")

#bright stars

bstars <- read_delim("R/Dataexperiments/data/astronomy/bright_Stars.tsv","\t", escape_double = FALSE, col_types = cols(ADS = col_integer(), `B-V` = col_double(), DEJ2000 = col_double(), HD = col_integer(), HR = col_integer(),Name = col_character(), NoteFlag = col_character(), RAJ2000 = col_double(), SpType = col_character(), VarID = col_character(), Vmag = col_double()),  comment = "#", trim_ws = TRUE)

bstars=data.frame(bstars)

#convert to galactic
a0=192.8595*pi/180
d0=27.1284*pi/180
l0=122.9320*pi/180
galc=function(a,d){
  a=a*pi/180
  d=d*pi/180
  y=(cos(d)*sin(a-a0))
  x=(sin(d)*cos(d0)-cos(d)*sin(d0)*cos(a-a0))
  l=l0-atan2(y,x)
  l=l*180/pi
  l[which(l<0)] = l[which(l<0)]+360
  y=sin(d)*sin(d0)+cos(d)*cos(d0)*cos(a-a0)
  b=asin(y)
  b=b*180/pi
  return(cbind(l,b))
}

#Hammer-Aitoff projection

ham.ait=function(l,b){
  l[which(l>180)] = l[which(l>180)]-360
  x=sqrt(8)*cos(b*pi/180)*sin(l*pi/360)/sqrt(1+cos(b*pi/180)*cos(l*pi/360))
  y=sqrt(2)*sin(b*pi/180)/sqrt(1+cos(b*pi/180)*cos(l*pi/360))
  return(cbind(x,y))
}


ham.ait=function(l,b){
  l[which(l>180)] = l[which(l>180)]-360
  x=sqrt(8)*cos(b*pi/180)*sin(l*pi/360)/sqrt(1+cos(b*pi/180)*cos(l*pi/360))
  y=sqrt(2)*sin(b*pi/180)/sqrt(1+cos(b*pi/180)*cos(l*pi/360))
  return(cbind(x,y))
}

#boundary
bdx=c(rep(180.0001,90), rep(180,90))
bdy=c(seq(-90,90,length.out = 90), seq(90,-90,length.out = 90))
te=ham.ait(bdx,bdy)
par(mar=c(1,1,1,1), mgp=c(1.1, .4, 0))
plot(te, type ="l", col="gray30", axes = F, xlab="", ylab="", family="f3", asp = 1000/1001)

#grid longitude
for (j in seq(0, 330, 30)) {
  bdx=rep(j, 180)
  bdy=c(seq(-90,90,length.out = 180))
  te=ham.ait(bdx, bdy)
  points(te, type = "l", lty=3, col="gray30")
}

#grid latitude
for (j in seq(-90, 90, 30)) {
  bdx=c(seq(180.0001, 360, length.out = 90),seq(0,180, length.out = 90))
  bdy=c(rep(j, 180))
  te=ham.ait(bdx, bdy)
  points(te, type = "l", lty=3, col="gray30")
}


#plotting MW objects
te=galc(MWO$RAJ2000, MWO$DEJ2000)
te=ham.ait(te[,1], te[,2])
points(te, pch=16, cex=.1, col="gray55")

#axial labels
l=seq(0,330,30)
b=rep(0, length(l))
te=ham.ait(l, b)
text(x = te[,1], y=te[,2], labels = seq(0, 330, 30), family="f3", col = "gray20", cex = .7)

b= seq(-90,90, 30)
l=rep(0, length(b))
te=ham.ait(l, b)
text(x = te[,1], y=te[,2], labels = seq(-90, 90, 30), family="f3", col = "gray20", cex = .7)

#plotting Mira
te=galc(Mira$X_RAJ2000, Mira$X_DEJ2000)
te=ham.ait(te[,1], te[,2])
points(te, pch=16, cex=.2, col="red")

#plotting stars----------
#mag size scaling
a=min(bstars$Vmag, na.rm = T)
b=max(bstars$Vmag, na.rm = T)
bright=b-bstars$Vmag+.01
bright=((bright)^(1/3))/2

#plotting
te=galc(bstars$RAJ2000, bstars$DEJ2000)
te=ham.ait(te[,1], te[,2])
points(te, pch=16, cex=bright)


dev.copy(png, file="~/R/Dataexperiments/Figs/Mira.png", width=7, height=3.5, unit="in", res=300)
dev.off()
