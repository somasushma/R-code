fx1=function(x) log(x)/(1+x^2)
fx2=function(x) (sinh(x)-x)/(x*(cosh(x))^2)
fx3=function(x) (sinh(x)-x)/(x^3*(cosh(x))^2)
fx4=function(x) (sinh(x)-x)/(x^3*(cosh(x/2))^2)
fx5=function(x) (x*sinh(x))/cosh(x)^2
fx6=function(x) asinh(sin(x))
fx7=function(x) x/sin(x)
K=-integral(fx1, xmin = 0, xmax = 1, method = "Kronrod", reltol = 1e-14)
#K is alternatively slow converging
-integral(fx1, xmin = 1, xmax = 1000000, method = "Kronrod", reltol = 1e-14)

#K is alternatively
1/2*integral(fx6, xmin = 0, xmax = pi, method = "Kronrod", reltol = 1e-14)

#K is alternatively
1/2*integral(fx7, xmin = 0, xmax = pi/2, method = "Kronrod", reltol = 1e-14)

#alternative for K
n=10000
sum(sapply(0:n, function(x) 1/(4*x+1)^2-1/(4*x+3)^2))

4*K/pi-1
integral(fx2, xmin = 0, xmax = 400, method = "Kronrod", reltol = 1e-14)
integral(fx3, xmin = 0, xmax = 710, method = "Kronrod", reltol = 1e-14)

