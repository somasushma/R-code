library(MASS)
library(numbers)
library(pracma)

#fonts
windowsFonts(f1 = windowsFont("Constantia"),
             f2 = windowsFont("Book Antiqua"),
             f3 = windowsFont("Cambria Math"))

n=300
m=100000
pbox=Primes(m)
pbox=tail(pbox, -1)
te= sapply(1:n, function(x) pbox[x]/(pbox[x]+(-1)^((pbox[x]-1)/2)))
pia=sapply(1:n, function(x) 2*prod(te[1:x]))

par(mar=c(2,2,2,2), mgp=c(1.1, .4, 0))
plot(pia, type="o", pch=16, col="darkblue", cex=.5, xlab="n", ylab = "prod", family="f3", main = bquote(prod(p[n]/(p[n]+(-1)^((p[n]-1)/2)))))
grid(col="gray55")
abline(h=pi, col="darkred", lwd=2, lty=3)

dev.copy(png, file="~/R/Figures/Figures1/Euler_pi_product.png", width=9, height=7, res=300, units="in")
dev.off()