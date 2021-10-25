#cube difference-----------
library(pander)
library(Ryacas)

cbrt=function(x) x^(1/3) #define cuberoot function

#quadratic approach: superior------
eq=strsplit(tolower(as_r(yac_str("Solve(3*z^2+3*z+1-x^3-y^3, z)"))), "==")
eq=unlist(lapply(eq, function(x) x[2]))

isInt=function(x) round(x, 0) == round(x, 10)
fxy=function(x,y) {}
body(fxy)=parse(text=eq[1])

n=20000
cb.triples=array(0, dim=c(n,3))
l=1
for(j in 1:n){
  for (k in (j+1):n) {
    z=fxy(j,k)
    if(isInt(z)){
      cb.triples[l,]=c(j, k, z)
      l=l+1
    }
  }
}
cb.triples=cb.triples[which(cb.triples[,1] !=0),]

#gathering cb. triples by common z
z= unique(cb.triples[,3])
gat.trips=sapply(z, function(x) which(cb.triples[,3]==x))
gat.trips=lapply(gat.trips, function(x) c(unique(cb.triples[x,3]),cb.triples[x,c(1,2)]))

sink("cube_diffs.txt")
pander(cbind(unlist(lapply(gat.trips, function(x) x[1])), unlist(lapply(gat.trips, function(x) {if(length(x) == 3) paste(x[2:length(x)], collapse = ", ") else paste(x[2], x[4], x[3], x[5], sep = ", ")}))), justify="rr")
sink()

#3D
library(rgl)
plot3d(cb.triples, type = "s",col = "dodgerblue3", size = .5, xlab = "X", ylab = "Y", zlab = "Z")
rgl.snapshot("cube.diff3D.png", fmt = "png", top = TRUE )

#2D plotting
par(pty="m", mar=c(2,2,2,1), mgp=c(1.1, .3, 0))
plot(cb.triples[,c(1,2)], pch=16, col="dodgerblue2", cex=.5, xlab = "x", ylab = "y", main=parse(text=paste0('"Integer solutions of"','~x^3+y^3==(z+1)^3-z^3')))

curve(1*x, from=0,to=20000, add = T, col="darkgreen", lty=3, lwd=1.5) #y=x line

x=1:100; y= 3*x^2+2*x+1 # every number
points(x, y, pch=16, col="red", cex=.5)

x0=(1:100); x=c(3*x0^2, 3*x0^2); y=c(6*x0^2 -3*x0 +1, 6*x0^2 +3*x0 +1) #Japanese points: lie on single parametric parabola.
points(x, y, pch=16, col="darkmagenta", cex=.5) 

#ellipses (total 22 x 2)
ell.pts=list()
#2/3_1
x=c(1,3)
y=c(6,10)
points(x, y, pch=16, col="deeppink1", cex=.5) 
ell.pts$`2/3_1`=cbind(x,y)


#2/3_1
x=c(2,14)
y=c(17,23) 
points(x, y, pch=16, col="goldenrod", cex=.5) 
ell.pts$`2/3_2`=cbind(x,y)

#1_1
x=c(9,15,51,57)
y=c(58,64,82,82)
points(x, y, pch=16, col="deeppink1", cex=.5) 
ell.pts$`1_1`=cbind(x,y)
#1_2
x=c(16,22,58,64)
y=c(51,57,75,75)
points(x, y, pch=16, col="goldenrod", cex=.5) 
ell.pts$`1_2`=cbind(x,y)

text(x = 250, y=75,labels = (parse(text=paste0('u==1'))), col="darkblue", pos = 4)

#sqrt(7/6)_1
x=c(66)
y=c(97)
points(x, y, pch=16, col="deeppink1", cex=.5) 
ell.pts$`sqrt(7/6)_1`=cbind(x,y)
#sqrt(7/6)_2
x=c(58)
y=c(105)
points(x, y, pch=16, col="goldenrod", cex=.5) 
ell.pts$`sqrt(7/6)_2`=cbind(x,y)

#4/3_1
x=c(44, 148)
y=c(173, 225)
points(x, y, pch=16, col="deeppink1", cex=.5) 
ell.pts$`4/3_1`=cbind(x,y)
#4/3_2
x=c(149)
y=c(212)
points(x, y, pch=16, col="goldenrod", cex=.5) 
ell.pts$`4/3_2`=cbind(x,y)

#5/3_1
x=c(146,376,456) 
y=c(395,495,475)
points(x, y, pch=16, col="deeppink1", cex=.5) 
ell.pts$`5/3_1`=cbind(x,y)
#5/3_2
x=c(145, 305)
y=c(426, 506)
points(x, y, pch=16, col="goldenrod", cex=.5) 
ell.pts$`5/3_2`=cbind(x,y)

#sqrt(19/6)_1
x=c(48, 225, 513) 
y=c(355, 538, 634)
points(x, y, pch=16, col="deeppink1", cex=.5) 
ell.pts$`sqrt(19/6)_1`=cbind(x,y)
#sqrt(19/6)_2
x=c(28, 205, 493)
y=c(375, 558, 654)
points(x, y, pch=16, col="goldenrod", cex=.5) 
ell.pts$`sqrt(19/6)_2`=cbind(x,y)

#2_1
x=c(24,72,90,114,264,294,318,390,522,594,618,648,798,822,840,888)
y=c(547,619,643,673,823,847,865,913,979,1003,1009,1015,1015,1009,1003,979)
points(x, y, pch=16, col="deeppink1",cex=.5) 
ell.pts$`2_1`=cbind(x,y)

#2_2
x=c(49,97,115,139,289,319,343,415,547,619,643,673,823,847,865,913)
y=c(522,594,618,648,798,822,840,888,954,978,984,990,990,984,978,954)
points(x, y, pch=16, col="goldenrod", cex=.5) 
ell.pts$`2_2`=cbind(x,y)

text(x = 1035, y=875,labels = (parse(text=paste0('u==2'))), col="darkblue", pos = 4)

#sqrt(25/6)_1
x=c(216) 
y=c(775)
points(x, y, pch=16, col="deeppink1", cex=.5) 
ell.pts$`sqrt(25/6)_1`=cbind(x,y)
#sqrt(25/6)_2
x=c(190) 
y=c(801)  
points(x, y, pch=16, col="goldenrod", cex=.5) 
ell.pts$`sqrt(25/6)_2`=cbind(x,y)

#7/3_1
x=c(82,478,1086,1286) 
y=c(951,1419,1771,1811)
points(x, y, pch=16, col="deeppink1", cex=.5) 
ell.pts$`7/3_1`=cbind(x,y)
#7/3_2
x=c(69,581,1597,1085) 
y=c(1018,1562,1782,1814)  
points(x, y, pch=16, col="goldenrod", cex=.5) 
ell.pts$`7/3_2`=cbind(x,y)

#sqrt(37/6)_1
x=c(45,306,1320,1356,1725,1914) 
y=c(1114,1501,2239,2251,2314,2293)
points(x, y, pch=16, col="deeppink1", cex=.5) 
ell.pts$`sqrt(37/6)_1`=cbind(x,y)
#sqrt(37/6)_2
x=c(7,268,1282,1318,1876,1687) 
y=c(1152,1539,2277,2289,2331,2352)  
points(x, y, pch=16, col="goldenrod", cex=.5) 
ell.pts$`sqrt(37/6)_2`=cbind(x,y)

#8/3_1
x=c(193,1081)   
y=c(1704,2616)
points(x, y, pch=16, col="deeppink1", cex=.5) 
ell.pts$`8/3_1`=cbind(x,y)
#8/3_2
x=c(704,1080,1736,2112)  
y=c(2393,2689,3017,3097)
points(x, y, pch=16, col="goldenrod", cex=.5) 
ell.pts$`8/3_2`=cbind(x,y)

#sqrt(73)/3_1
x=c(403,489,711,947,1739,1911,2703,2939,3161,3247)
y=c(2544,2650,2896,3122,3680,3766,4000,4010,3986,3966)
points(x, y, pch=16, col="deeppink1", cex=.5) 
ell.pts$`sqrt(73)/3_1`=cbind(x,y)

#sqrt(73)/3_2
x=c(230,416,622,1360,1470,3710,2006,2176,3490,2694,2946)
y=c(2183,2447,2697,3363,3439,3689,3737,3807,3843,3943,3961)
points(x, y, pch=16, col="goldenrod", cex=.5) 
ell.pts$`sqrt(73)/3_2`=cbind(x,y)

text(x = 3960, y=3550,labels = (parse(text=paste0('u==sqrt(73)/3'))), col="darkblue", pos = 4)

#3_1
x=c(243,279,1719,1773,2709,2763,4203,4239)
y=c(2764,2818,4258,4294,4762,4780,4780,4762)
points(x, y, pch=16, col="deeppink1", cex=.5) 
ell.pts$`3_1`=cbind(x,y)
#3_2
x=c(298,334,1774,1828,2764,4294,2818,4258)
y=c(2709,2763,4203,4239,4707,4707,4725,4725)
points(x, y, pch=16, col="goldenrod", cex=.5) 
ell.pts$`3_2`=cbind(x,y)

text(x = 4680, y=4540,labels = (parse(text=paste0('u==3'))), col="darkblue", pos = 4)

#sqrt(61/6)_1
x=c(139,220,2740,5245,2464) 
y=c(3228,3363,5739,6114,5583)
points(x, y, pch=16, col="deeppink1", cex=.5) 
ell.pts$`sqrt(61/6)_1`=cbind(x,y)
#sqrt(61/6)_2
x=c(201,282,2526,2802,5307) 
y=c(3166,3301,5521,5677,6052) 
points(x, y, pch=16, col="goldenrod", cex=.5) 
ell.pts$`sqrt(61/6)_2`=cbind(x,y)

#10/3_1
x=c(4111) 
y=c(7110)
points(x, y, pch=16, col="deeppink1", cex=.5) 
ell.pts$`10/3_1`=cbind(x,y)
#10/3_2
x=c(2690,4110) 
y=c(6491,7201) 
points(x, y, pch=16, col="goldenrod", cex=.5) 
ell.pts$`10/3_2`=cbind(x,y)

#sqrt(211/18)_1
x=c(1253,1976,3502,4333) 
y=c(5714,6431,7521,7914)
points(x, y, pch=16, col="deeppink1", cex=.5) 
ell.pts$`sqrt(211/18)_1`=cbind(x,y)
#sqrt(211/18)_2
x=c(46,876,1486)
y=c(3801,5107,5817) 
points(x, y, pch=16, col="goldenrod", cex=.5) 
ell.pts$`sqrt(211/18)_2`=cbind(x,y)

#sqrt(12)_1
x=c(132,606,1302,1362,1956,2022,2892,3642,4278,7788,5028,7314,5898,6618,5964,6558)        
y=c(4351,5101,5971,6037,6631,6691,7387,7861,8179,8179,8455,8455,8629,8629,8635,8635) 
points(x, y, pch=16, col="deeppink1", cex=.5) 
ell.pts$`sqrt(12)_2`=cbind(x,y)
#sqrt(12)_2
x=c(205,679,1375,1435,2029,2095,2965,3715,4351,5101,5971,6037,6691,7387,7861,6631)
y=c(4278,5028,5898,5964,6558,6618,7314,7788,8106,8382,8556,8562,8556,8382,8106,8562)
points(x, y, pch=16, col="goldenrod", cex=.5) 
ell.pts$`sqrt(12)_1`=cbind(x,y)

text(x = 8110, y=7850,labels = (parse(text=paste0('u==sqrt(12)'))), col="darkblue", pos = 4)

#sqrt(79/6)_1
x=c(1009,3178,3682,6175,8200,9334)
y=c(6498,8649,9009,10188,10323,9837)    
points(x, y, pch=16, col="deeppink1", cex=.5) 
ell.pts$`sqrt(79/6)_1`=cbind(x,y)
#sqrt(79/6)_2
x=c(1089,3258,3762,9414,6255,8280)
y=c(6418,8569,8929,9757,10108,10243)
points(x, y, pch=16, col="goldenrod", cex=.5) 
ell.pts$`sqrt(79/6)_2`=cbind(x,y)

text(x = 10000, y=9660,labels = (parse(text=paste0('u==sqrt(79/6)'))), col="darkblue", pos = 4)

#11/3_1
x=c(4104,5710)
y=c(9427,10263)
points(x, y, pch=16, col="deeppink1", cex=.5) 
ell.pts$`11/3_1`=cbind(x,y)
#11/3_2
x=c(2563,4103,5819,7359) 
y=c(8394,9560,10418,10792) 
points(x, y, pch=16, col="goldenrod", cex=.5) 
ell.pts$`11/3_2`=cbind(x,y)

#sqrt(91/6)_1
x=c(636,2361,3192,4113,5163,6114,7107,8439,9372,11814,11097)
y=c(7351,9586,10411,11194,11944,12505,12976,13420,13591,13285,13522)
points(x, y, pch=16, col="deeppink1", cex=.5) 
ell.pts$`sqrt(91/6)_1`=cbind(x,y)

#sqrt(91/6)_2
x=c(544,2269,3100,4021,5071,6022,7015,8347,9280,11005,11722)
y=c(7443,9678,10503,11286,12036,12597,13068,13512,13683,13614,13377)
points(x, y, pch=16, col="goldenrod", cex=.5) 
ell.pts$`sqrt(91/6)_2`=cbind(x,y)

text(x = 12780, y=12470,labels = (parse(text=paste0('u==sqrt(91/6)'))), col="darkblue", pos = 4)

#4_1
x=c(576,636,2892,2976,5796,5892,8124,8220,13440,13380,11040,11124)
y=c(8221,8317,11137,11221,13477,13537,14653,14689,14653,14689,15253,15253)
points(x, y, pch=16, col="deeppink1", cex=.5) 
ell.pts$`4_1`=cbind(x,y)
#4_2
x=c(673,733,2989,3073,5893,5989,8221,8317,11137,11221,13477,13537)
y=c(8124,8220,11040,11124,13380,13440,14556,14592,15156,15156,14592,14556)
points(x, y, pch=16, col="goldenrod", cex=.5) 
ell.pts$`4_2`=cbind(x,y)

text(x = 14340, y=13990,labels = (parse(text=paste0('u==4'))), col="darkblue", pos = 4)

#sqrt(148)/3_1
x=c(395,2521,2681,3625,5455,6615,9641,10971,12691,13547,14275)
y=c(8048,10986,11162,12114,13614,14368,15686,15952,15936,15722,15378)
points(x, y, pch=16, col="deeppink1", cex=.5) 
ell.pts$`sqrt(148)/3_1`=cbind(x,y)
#sqrt(148)/3_2
x=c(352,1168,2802,4818,6778,8022,14448,13632,9982,11998)
y=c(8247,9519,11497,13315,14619,15241,15295,15751,15897,16095)
points(x, y, pch=16, col="goldenrod", cex=.5) 
ell.pts$`sqrt(148)/3_2`=cbind(x,y)

text(x = 15200, y=14970,labels = (parse(text=paste0('u==sqrt(148)/3'))), col="darkblue", pos = 4)

#remaining tetrad set 1
x=c(37,136,69,381,201,2097,3029,8021,1102,3325,2841,3064)
y=c(174, 141,1018,1000,3166,2824,12038,10790,14421,14364,16672,16665)
points(x, y, pch=16, col="gray2", cex=.5) 

#tetrad set 2 # associated with n-curve
x=c(16,502,46,1810)
y=c(801,729,6441,6393)
points(x, y, pch=16, col="red", cex=.5) 

#densities at ellipses---------
dens=unlist(lapply(0:(length(ell.pts)/2-1), function(x) nrow(ell.pts[[x*2+1]])+nrow(ell.pts[[x*2+1]])))
te=strsplit(names(ell.pts), "_")
te=unique(unlist(lapply(te, function(x) x[[1]])))

ye=sapply(te, function(x) eval(parse(text = x)))
par(pty="m", mar=c(4,2,2,1), mgp=c(1.1, .4, 0))
barplot(dens, names.arg = parse(text=paste(te, sep = ",")), las=2, col = "cyan3", main= "# of elliptical solutions corresponding to first few u")
box()

#ellipse work out----------
#intercepts
n=c(2/3, 1, sqrt(7/6), 4/3, 5/3, sqrt(19/6), 2, sqrt(25/6), 7/3, sqrt(37/6), 8/3, sqrt(73)/3, 3, sqrt(61/6), 10/3, sqrt(211/18), sqrt(12), sqrt(79/6), 11/3, sqrt(91/6), 4, sqrt(148)/3)

te="2/3, 1, sqrt(7/6), 4/3, 5/3, sqrt(19/6), 2, sqrt(25/6), 7/3, sqrt(37/6), 8/3, sqrt(73)/3, 3, sqrt(61/6), 10/3, sqrt(211/18), sqrt(12), sqrt(79/6), 11/3, sqrt(91/6), 4, sqrt(148)/3"
te=gsub(" ", "", te)
te=unlist(strsplit(te,","))
names(n)= te

g0=3*n^2+1
g=6*g0^2-6*g0+1
g=54*n^4+18*n^2+1 #direct

h0=6*n^2+1
h1=3*h0^2+1
h=k^2-h1
h=3*(972*n^8+648*n^6+108*n^4-1) #direct

#coeffs
c=(g+1)/2
c=27*n^4+9*n^2+1 #direct
a=c+(9*n^2+1)
b=c-(9*n^2+2)

#curves for ellipse
library(Ryacas)

ell.list=list()
l=1
for (j in 1:4) {
  ye=as_r(yac_str(paste0("Solve(", "x^2-x*y+y^2-", a[j],"*x-", b[j], "*y+", c[j], ",y)"))) #solve with yacas
  ye=tolower(ye)
  ye=strsplit(ye, "==")
  ye=unlist(lapply(ye, function(x) x[2]))
  ell=function(x) {}
  body(ell)=parse(text = ye[1]) #get curve from above solution
  ell.list[[l]]=ell
  l=l+1
  ell=function(x) {}
  body(ell)=parse(text = ye[2]) #get curve from above solution
  ell.list[[l]]=ell
  l=l+1
  
  ye=as_r(yac_str(paste0("Solve(", "x^2-x*y+y^2-", b[j],"*x-", a[j], "*y+", c[j], ",y)"))) #solve with yacas
  ye=tolower(ye)
  ye=strsplit(ye, "==")
  ye=unlist(lapply(ye, function(x) x[2]))
  ell=function(x) {}
  body(ell)=parse(text = ye[1]) #get curve from above solution
  ell.list[[l]]=ell
  l=l+1
  ell=function(x) {}
  body(ell)=parse(text = ye[2]) #get curve from above solution
  ell.list[[l]]=ell
  l=l+1
}

#obtain range for plotting
rng.list=list()
for (j in 0:3) {
  fl=T
for (k in 1:4) {
  rng.list[[j*4+k]]=unlist(suppressWarnings(optimize(ell.list[[j*4+k]],interval = c(-g[j+1], 2*g[j+1]), maximum = fl)))
  fl=!fl
}
}

#plotting ellipses
par(pty="m", mar=c(2,2,2.5,1), mgp=c(1.1, .3, 0))
for (j in 0:3) {
plot(0,0, type="n", xlim = c(rng.list[[j*4+2]][2],rng.list[[j*4+3]][2]), ylim = c(rng.list[[j*4+2]][2],rng.list[[j*4+3]][2]),  xlab="x", ylab="y", asp=1)
  mtext(c(parse(text=paste0('x^2-x*y+y^2-', a[j+1],'*x-', b[j+1], '*y+', c[j+1], '==0')),parse(text=paste0('x^2-x*y+y^2-', b[j+1],'*x-', a[j+1], '*y+', c[j+1], '==0'))),side=3, line = c(0,1))
  for (k in 1:4) {
  suppressWarnings(curve(ell.list[[j*4+k]](x), from = rng.list[[j*4+2]][2], to=rng.list[[j*4+3]][2], n=100000, lty=2, lwd=1.5, col="darkgreen", add=T))
  }
  points(ell.pts[[2*j+1]], pch=20,col="deeppink1")
  points(ell.pts[[2*j+2]], pch=20,col="goldenrod")
}


#brute force search: inferior---------
te=sapply(1:300000, function(x) 3*x^2+3*x+1)
ye=list()
for (j in 1:length(te)) {
  ye[[j]]=te[j]-(1:floor(te[j]^(1/3)))^3
}

cb.diff=which(unlist(lapply(ye, function(x) any(round(x^(1/3)-round(x^(1/3),0), 10)==0))))
te=ye[cb.diff]
cbrts=lapply(1:length(te), function(x) which(round(cbrt(te[[x]])-round(cbrt(te[[x]]),0), 10)==0))

pander(cbind(cb.diff, unlist(lapply(cbrts, function(x) paste(x, collapse = ", ")))), row.names = F, col.names=c("c", "ns"), justify="rr") #table of cuberoots in relationship

cbind(table(unlist(cbrts))) #numbers of appearances of each

#prepare pairs for plotting
te=list()
l=1
for(j in 1:length(cbrts)){
  if(length(cbrts[[j]])>2){
    te[[l]]=c(cbrts[[j]][1], cbrts[[j]][4])
    l=l+1
    te[[l]]=c(cbrts[[j]][2], cbrts[[j]][3])
    l=l+1
  } else {
    te[[l]]=c(cbrts[[j]][1], cbrts[[j]][2])
    l=l+1
  }
}
te=do.call("rbind", te)


#isolate tetrads
ye=cbrts[which(lengths(cbrts)==4)]
l=1
tetrad=array(data=0, dim = c(length(ye)*2,2))
for (j in 1:length(ye)) {
  tetrad[l,]=c(ye[[j]][1],ye[[j]][4])
  l=l+1
  tetrad[l,]=c(ye[[j]][2],ye[[j]][3])
  l=l+1
}

points(tetrad, pch=16, col="orange")

#roots and ratios
pander(cbind(te, sprintf("%.3f", te[,2]/te[,1])), row.names = F, col.names=c("a", "b", "b/a"), justify="rrr") 


#hexagonal spiral--------
e=exp(1)
n=5
rots=vector(mode="list", length = n)
for(j in 1:n){
  te=c(0:(j*6-1))
  rots[[j]]=e^(te*2*pi/length(te)*1i)
  rots[[j]]=c(tail(rots[[j]],(j-1)), head(rots[[j]],length(rots[[j]])-(j-1)))
}
rots=c(0+0i, rots)
rots=unlist(rots)*c(0,seq(.3, 7, length.out = (length(unlist(rots))-1)))
par(mar=c(1,1,2,1))
plot(rots, type="n", asp=1, axes = F, xlab="", ylab="")
lines(rots, lty=2)
text(rots, labels = 1:length(rots), col = "dodgerblue3", font=2)
lines(rots[c(1, 6,17,34, 57, 86)], col="red", lty=3)

#cube2 difference-----
n=10000
cb.2.triples=array(0, dim = c(n,3))
j=1
for(x in 1:n){
  for (y in (x+1):n) {
    z=sqrt((x^3+y^3-2)/6)-1
    if(isInt(z)) {
      cb.2.triples[j,]=c(x,y,z)
      j=j+1
    }
  }
}
cb.2.triples=cb.2.triples[which(cb.2.triples[,1] !=0),]

#2D plots
par(pty="m", mar=c(2,2,2,1), mgp=c(1.1, .3, 0))
plot(cb.2.triples[,c(1,2)], pch=16, col="dodgerblue2", cex=.5, xlab = "x", ylab = "y", main=parse(text=paste0('"Integer solutions of"','~x^3+y^3==(z+2)^3-z^3')))

#ellipse parameter workout cb.2
n=c(2/3, 1, sqrt(7/6), 4/3, 5/3, sqrt(19/6), 2, sqrt(25/6), 7/3, sqrt(37/6), 8/3, sqrt(73)/3, 3, sqrt(61/6), 10/3, sqrt(211/18), sqrt(12), sqrt(79/6), 11/3, sqrt(91/6), 4, sqrt(148)/3)

c0=27*n^4+9*n^2+1 #direct
a=(c+(9*n^2+1))*2
b=(c-(9*n^2+2))*2
c=a+b+2