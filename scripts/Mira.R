library(readr)

windowsFonts(f1 = windowsFont("Constantia"),
             f2 = windowsFont("Book Antiqua"),
             f3 = windowsFont("Cambria Math"))

Mira <- read_delim("R/Dataexperiments/data/astronomy/Mira.tsv", 
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

#convert to galactic
a0=192.8595*pi/180
d0=27.1284*pi/180
l0=122.9320*pi/180
galc=function(a,d){
a=a*pi/180
d=d*pi/180
x=(cos(d)*sin(a-a0))/(sin(d)*cos(d0)-cos(d)*sin(d0)*cos(a-a0))
l=l0-atan(x)

y=sin(d)*sin(d0)+cos(d)*cos(d0)*cos(a-a0)
b=asin(y)
return(cbind(l*180/pi, b*180/pi))
}