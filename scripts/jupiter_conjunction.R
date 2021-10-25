jupiter=11.86
saturn=29.457
j1y=360/jupiter
s1y=360/saturn
g.conj=360/(j1y-s1y)
trigon=((1:47)*360/(j1y-s1y)*j1y) %% 360
tbox=(1:360)*pi/180
plot(x=cos(tbox), y=sin(tbox), asp = 1, type = "l")

points(x=cos(trigon*pi/180), y=sin(trigon*pi/180), type = "o", pch=16)