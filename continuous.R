fx = function(x) {(630/56)*(x^4)*(1-x^4)} 
# 0 < x < 1
a = 0; b = 1;
x = seq(a, b, by=0.01) 
plot(x,fx(x),type='l', main="(630/56)*(x^4)*(1-x^4)")


#Mean: Ex = integral of x*P(x) on [a,b]: 
fxx = function(x) {x* (630/56)*(x^4)*(1-x^4)} 
Ex = integrate(fxx, lower=a, upper=b)$value


#Variance: Vx = E(x^2) - [E(x)]^2
fx3 = function(x) {x^2* (630/56)*(x^4)*(1-x^4)}
v = integrate(fx3, lower=a, upper=b)$value
Vx = v - Ex^2


# F(x) = integrate(fx, lower=-Inf, upper = t)
# we want P(c<x<d)
c = 0.2; d = 0.8;
# this is = P(0<x<1) - P(0.8<x<1) - P(0<x<.2)
P = integrate(fx, lower=a, upper=b)$value - integrate(fx, lower=a, upper=c)$value - integrate(fx, lower=d, upper=b)$value