#Initializing-----------
library(readr)
library(pracma)
library(numbers)

windowsFonts(f1 = windowsFont("Constantia"),
             f2 = windowsFont("Book Antiqua"),
             f3 = windowsFont("Cambria"),
             f4 = windowsFont("Segoe UI Symbol"))

setwd("~/cutting_block/astro/")

#Reading data----------------
#Reading MWO
MWO.MC <- read_delim("MWO.MC.tsv", "\t", escape_double = FALSE, col_types = cols(DEJ2000 = col_double(),  RAJ2000 = col_double(), tag=col_character()), comment = "#", trim_ws = TRUE)

#bright stars
BSTARS <- read_delim("BrightStars.tsv", "\t", escape_double = FALSE, col_types = cols(ADS = col_integer(), `B-V` = col_double(), DEJ2000 = col_double(),  HD = col_integer(), HR = col_integer(), Name = col_character(), RAJ2000 = col_double(), SpType = col_character(), VarID = col_character(), Vmag = col_double()), comment = "#", trim_ws = TRUE)

BSTARS=data.frame(BSTARS)
colnames(BSTARS)=c("RAJ2000", "DEJ2000", "HR", "Name", "HD", "multiples", "VarID", "Vmag", "B.V", "SpType")
#tag  true variables
BSTARS$Var.tag = NA
BSTARS$Var.tag[which(!is.na(BSTARS$VarID) & BSTARS$VarID != "Var?")] = "V" 
#star mag size scaling
a=min(BSTARS$Vmag, na.rm = T)
b=max(BSTARS$Vmag, na.rm = T)
bright=(b-BSTARS$Vmag)/(b-a)+.01
bright=(bright^1.65)*.875

#star color
colr= (1.8-BSTARS$B.V)/(1.8-min(BSTARS$B.V, na.rm = T))*7+1
colr[which(colr<1)]=1
spct = colorRampPalette(c("darkred", "orange", "gold3", "navajowhite2",  "steelblue3", "darkslateblue"))
colr=spct(16)[colr*2]
colr[which(is.na(colr))]="gray40"

#Messiers
MESSIER <- read_delim("MESSIER_etc.tsv", "\t", escape_double = FALSE, col_types = cols(DEJ2000 = col_double(), RAJ2000 = col_double(), alt_name = col_character(), constell = col_character(), dimension = col_character(), name = col_character(), notes = col_character(), object_type = col_character(), vmag = col_double()), trim_ws = TRUE)
MESSIER=data.frame(MESSIER)

size=(MESSIER$dimension)
size[which(size=="null")] = NA
size=strsplit(size, "X")
size=lapply(size, as.numeric)
size=unlist(lapply(size, mean))
MESSIER$size = size
  
#constellation figures (crafted)-----------
cnst = read_delim("Const.tsv", "\t", escape_double = FALSE, col_types = cols(`B-V` = col_double(), DEJ2000 = col_double(), HD = col_integer(),  HR = col_integer(), Name = col_character(), RAJ2000 = col_double(), SpType = col_character(), VarID = col_character(), Vmag = col_double()), comment = "#", trim_ws = TRUE)

cnst=data.frame(cnst)

#functions----------
 #0.1 Insert rows
insertrows = function(df,breaks,newrows){
  xx=1:length(breaks)
  breaks=breaks+xx #To space out the insertion points.
  newmat=matrix(NA,length(breaks)+nrow(df), ncol(df)) #Preallocate memory by creating final dataframe.
  for(j in 1:length(breaks)){newmat[breaks[j],]=newrows[j,]} #Insert added rows into new dataframe
  x=1:nrow(newmat)
  x=x[-(breaks)] #Finding the rows of the new dataframe that will receive old rows
  for(j in 1:nrow(df)){newmat[x[j],]=df[j,]} #x to index the new dataframe for placement of old rows.
  return(newmat)}
 
#0.2 distance
distance=function(x1,y1, x2, y2) sqrt((x2-x1)^2+(y2-y1)^2)

 #1. coordinates--------------
 #1.1 convert RA, DEC to galactic-------
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

 #1.2 ecliptic to RA, DEC
epsi = 23 + 26/60 + 21.448/3600#ecliptic inclination for J2000
epsi = epsi/180*pi
ecliptic=function(l, b){
  l = l*pi/180
  b = b*pi/180
  deno=sin(l)*cos(epsi)-tan(b)*sin(epsi)
  nume=cos(l)
  a = atan2(y=deno, x=nume)
  d = asin(sin(b)*cos(epsi))+cos(b)*sin(epsi)*sin(l)
  return(cbind(a*180/pi,d*180/pi))
}

 #2. projections--------------
  #2.1 Hammer-Aitoff (default)---------------
ham.ait=function(l,b){
  l[which(l>180)] = l[which(l>180)]-360
  x=sqrt(8)*cos(b*pi/180)*sin(l*pi/360)/sqrt(1+cos(b*pi/180)*cos(l*pi/360))
  y=sqrt(2)*sin(b*pi/180)/sqrt(1+cos(b*pi/180)*cos(l*pi/360))
  return(cbind(-x,y))
}

 #2.2 Lambert-------------
 lam=function(l,b){
   l[which(l>180)] = l[which(l>180)]-360
   x=l
   y=90*sin(b*pi/180)
   return(cbind(-x,y))
 }

 #2.3 Equirect----------
equirect=function(l,b){
  l[which(l>180)] = l[which(l>180)]-360
  b0=pi/6
  x=l*pi/180*cos(b0)
  y=b*pi/180-b0
  return(cbind(-x,y))
}

 #2.4 Eckert iV----------
eckert4 = function(l,b){
  l[which(l>180)] = l[which(l>180)]-360
  l=l*pi/180
  b=b*pi/180
  t=sapply(b, function(y) ifelse(test = is.na(y),yes = NA, no = newton(fun = function(x) x + sin(x)*cos(x)+2*sin(x)-(4+pi)*sin(y)/2, x0 = y/2)$root ))
  x=2*l*(1+cos(t))/sqrt(4*pi+pi^2)
  y=2*sin(t)*sqrt(pi/(pi+4))
  return(cbind(-x,y))
}

 #2.5 Winkel Tripel---------
winkel3 = function(l,b){
  l[which(l>180)] = l[which(l>180)]-360
  l=l*pi/180
  b=b*pi/180
  d= acos(cos(b)*cos(l/2))
  Cee=sin(b)/sin(d)
  Cee[which(Cee>1)]=1
  Cee[which(Cee< (-1))]= -1
  x1=sapply(1:length(d), function(y) ifelse(test = (d[y]==0), yes = 0, no = 2*sign(l[y])*d[y]*sqrt(1-Cee[y]^2) ) )
  y1=sapply(1:length(d), function(y) ifelse(test = (d[y]==0), yes = 0, no =  d[y]*Cee[y]) )
  
  b0=50.467/180*pi
  x2=l*cos(b0)
  y2=b-b0
  x=(x1+x2)/2 
  y=(y1+y2)/2 
  
  return(cbind(-x,y))
}

 #2.5 Gott-Mugnolo---------
gott.m=function(l,b){
  l[which(l>180)] = l[which(l>180)]-360
  l=l*pi/180
  b=b*pi/180
b1=asin(cos(b)*sin(l/2))
d=sin(b)/cos(b1)
d[which(d>1)]=1
d[which(d<(-1))]=-1
l1=1/2*asin(d)

t=sapply(b1, function(y) ifelse(test = is.na(y),yes = NA, no = newton(fun = function(x) 2*x + sin(2*x)-pi*sin(y), x0 = y/2)$root ))

x= sqrt(2)*sin(t)
y=1.18*pi/sqrt(8)*l1*cos(t)
  return(cbind(-x,y))
}
 #3. Plotting objects
# provide object as a character vector
# valid values for object are ecli: ecliptic "mwo": Milky Way Objects; messier: Messier; "constf": constellation figures; "bstars": bright stars

sky2map=function(coords =x, projection = y, object=x){
if(missing(coords)) { 
  coords = cbind
  name1 = "RA-DE"} else {
    name1 =coords
    coords=get(coords)
  }
if(missing(projection)) {
  projection = ham.ait
  name2 = "ham.ait"
} else {
  name2= projection 
  projection= get(projection)
}
  
if(missing(object)) object=c("ecli","mwo", "messier", "constf", "bstars")

 #3.1 Sky grid
    #boundary
bdx=c(rep(180.0001,90), rep(180,90))
bdy=c(seq(-90,90,length.out = 90), seq(90,-90,length.out = 90))
te=projection(bdx,bdy)
bd=te

if(name2=="winkel3") asp=.6 else asp=max(te[,2])/max(te[,1])


par(mar=c(.25,.25,.25,.25), mgp=c(0, 0, 0))
plot(te, type ="l", col="gray30", axes = F, xlab="", ylab="", family="f3", asp = 1000/1001)

#grid longitude
for (j in seq(0, 330, 30)) {
  bdx=rep(j, 180)
  bdy=c(seq(-90,90,length.out = 180))
  te=projection(bdx, bdy)
  points(te, type = "l", lty=3, col="gray30")
}

#grid latitude
for (j in seq(-90, 90, 30)) {
  bdx=c(seq(180.0001, 360, length.out = 90),seq(0,180, length.out = 90))
  bdy=c(rep(j, 180))
  te=projection(bdx, bdy)
  points(te, type = "l", lty=3, col="gray30")
}

#axial labels
#longitude
l=seq(0,330,30)
b=rep(0, length(l))
te=projection(l, b)
text(x = te[,1], y=te[,2], labels = seq(0, 330, 30), family="f3", col = "gray20", cex = .5)

#latitude
b= seq(-90,90, 30)
l=rep(0, length(b))
te=projection(l, b)
text(x = te[,1], y=te[,2], labels = seq(-90, 90, 30), family="f3", col = "gray20", cex = .5)

  #3.1 ecliptic
  if(any(object == "ecli")){
    te=ecliptic(seq(0,360,1),rep(0,361))
    te=coords(te[,1],te[,2])
    te=projection(te[,1], te[,2])
    breaks=which(!sapply(1:(nrow(te)-1), function(x) sign(te[x,1]) == sign(te[x+1,1])))
    newrows=matrix(NA, nrow = length(breaks), ncol = 2)
    te=insertrows(te,breaks,newrows)
    lines(te, col="darkseagreen3")
    te=te[seq(0,360, length.out = 12),]
    points(te, pch=124, cex=.5, col="darkseagreen3")
  }
  
  #3.2 Milky way/Magellanic objects
  if( any(object == "mwo")){    
    te=coords(MWO.MC$RAJ2000, MWO.MC$DEJ2000)
    te=projection(te[,1], te[,2])
    
    ye=te[which(MWO.MC$tag=="MW2"),]
    col=adjustcolor("gray70", alpha.f = .2)
    points(ye, pch=16, cex=.3, col=col)
    
    ye=te[which(MWO.MC$tag=="MW1"),]
    col=adjustcolor("gray65", alpha.f = .2)
    points(ye, pch=16, cex=.3, col=col)
    
    ye=te[which(MWO.MC$tag=="MC"),]
    col=adjustcolor("gray75", alpha.f = .2)
    points(ye, pch=16, cex=.2, col=col)
    }

  #3.3 Messiers
  if(any(object == "messier")){ 
    #globular clusters
    ye=MESSIER[which(MESSIER$object_type=="GB"),]
    size=(size-min(size, na.rm = T))/(max(size, na.rm = T)-min(size, na.rm = T))*.15+.2    #size
    size=ye$size
    size=(size-min(size, na.rm = T))/(max(size, na.rm = T)-min(size, na.rm = T))*.15+.2
    
    te=coords(ye$RAJ2000, ye$DEJ2000)
    te=projection(te[,1], te[,2])
    
    points(te, cex=size, pch=-0x2295L, col="mediumpurple4", family="f3")
    
    #planetary nebulae
    ye=MESSIER[which(MESSIER$object_type=="PL"),]
    #size
    size=ye$size
    size=(size-min(size, na.rm = T))/(max(size, na.rm = T)-min(size, na.rm = T))*.2+.3    
    te=coords(ye$RAJ2000, ye$DEJ2000)
    te=projection(te[,1], te[,2])
    
    points(te, cex=size, pch=-0x25CEL, col="darkgreen", family="f3")    
    
    #open clusters
    ye=MESSIER[which(MESSIER$object_type=="OC"),]
    #size
    size=ye$size
    size=(size-min(size, na.rm = T))/(max(size, na.rm = T)-min(size, na.rm = T))*.3+.7    
    te=coords(ye$RAJ2000, ye$DEJ2000)
    te=projection(te[,1], te[,2])
    
    points(te, cex=size, pch=-0x25CB, col="mediumpurple2", family="f3") 
    
    #Diffuse nebulae
    ye=MESSIER[which(MESSIER$object_type=="DI" | MESSIER$object_type=="NB"),]
    #size
    size=ye$size
    size=(size-min(size, na.rm = T))/(max(size, na.rm = T)-min(size, na.rm = T))*.15+.4    
    te=coords(ye$RAJ2000, ye$DEJ2000)
    te=projection(te[,1], te[,2])
    
    points(te, cex=size, pch=-0x25A2, col="olivedrab", family="f3") 
    
    #Galaxies
    ye=MESSIER[which(MESSIER$object_type=="S" | MESSIER$object_type=="E" | MESSIER$object_type=="E?" | MESSIER$object_type=="IR"| MESSIER$object_type=="GX"),]
    size=ye$size
    size=(size-min(size, na.rm = T))/(max(size, na.rm = T)-min(size, na.rm = T))*.15+.35    
    te=coords(ye$RAJ2000, ye$DEJ2000)
    te=projection(te[,1], te[,2])
    points(te, cex=size, pch=-0x2B2D, col="ivory4", family="f4") 
  }
  
  #3.4 constellation figures
  if(any(object=="constf")){
te=coords(cnst$RAJ2000, cnst$DEJ2000)
te=projection(te[,1], te[,2])

breaks=which(sapply(1:(nrow(te)-1), function(x) sign(te[x,1]) != sign(te[x+1,1]) & (abs(te[x+1,1])+abs(te[x,1]))/(2*max(te, na.rm = T)) > 1/6))
ye=te[sapply(breaks, function(x) x:(x+1)),] #gather break points for break point lines

newrows=matrix(NA, nrow = length(breaks), ncol = 2)
te=insertrows(te,breaks,newrows)

lines(te, lwd=.65, col="gray55")

#break point lines
bdmin=bd[sapply(1:nrow(ye), function(x)  which(distance(ye[x,1],ye[x,2], bd[,1], bd[,2])==min(distance(ye[x,1], ye[x,2], bd[,1], bd[,2])))),]
breaks=1:nrow(ye)
ye=insertrows(ye,breaks,bdmin)
breaks=seq(2,(nrow(ye)-2),2)
ye=insertrows(df=ye, breaks = breaks, newrows = matrix(NA, ncol = 2, nrow=length(breaks)))
lines(ye, lwd=.65, lty=3, col="gray55")
}

    #3.5 Bright stars-----
  if(any(object == "bstars")){

#plotting stars
te=coords(BSTARS$RAJ2000, BSTARS$DEJ2000)
te=projection(te[,1], te[,2])

#multiple star tag
points(te[which(!is.na(BSTARS$multiples)),], pch=-0x2014, cex=bright[which(!is.na(BSTARS$multiples))], col=colr[which(!is.na(BSTARS$multiples))], family="f3")

#non-variables
points(te[which(is.na(BSTARS$Var.tag)),], pch=-0x25CF, cex=bright[which(is.na(BSTARS$Var.tag))], col=colr[which(is.na(BSTARS$Var.tag))], family="f3")
##points(te, cex=bright, col="gray50")

#variables
vbright=bright[which(!is.na(BSTARS$Var.tag))]
col=colr[which(!is.na(BSTARS$Var.tag))]
points(te[which(!is.na(BSTARS$Var.tag)),], pch=-0x25C9, cex=vbright, col=col, family="f3")
  }
  
dev.copy(png, file=paste0("sky","_",name1,"_",name2,".png"), width=9, height=9*asp, unit="in", res=750)
dev.off()
}

#e.g. sky2map(projection = "winkel3")


