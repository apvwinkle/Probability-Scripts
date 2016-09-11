##### 331 R Scripts
####################################################################################

##### Introduction to R Lecture
### 1. Introductory Comments
# a. I prefer to type into a .R document and copy into the command window
# b. The number symbol "#" is the comment character
# c. R will do arithmetic, common math functions, advanced mathematical and statistical functions,
# professional quality graphics, simulations, user-writable functions, any combination thereof,
# and more.

### 2. Basics
# The basic class of objects is a vector ("numeric")
x = c(1, 2, 3, 6, 11)
class(x)
x
2*x
x+x
x^2

#Watch out for the recycling rule
y = c(4,5)
x+y

#The order of operations is obeyed
(x-1)*2+1

### 3. Counting
factorial(7)			#Factorial function
choose(7,3)				#Combination
factorial(7)/factorial(3)/factorial(4)  #Check combination formula
prod(7:4)				#No factorial function; use product function with sequence

library(gregmisc)			#Install package from "Packages" drop-down, then load library
permutations(4,3)			#Lists permutations			
combinations(4,3)			#Lists combinations

### 4. Sequences
1:7					#Colon operator automatically steps by 1
seq(5, 14, by=2)			#Start at 5, go to 14 by 2's
rep(2, 5)				#Repeat 2 five times
rep(c(2,3), 5)				#Repeat (2,3) five times

### 5. Some "set" operations
union(1:7,5:12)				#The union of two sets
intersect(1:7,5:12)			#The intersection of two sets
setdiff(1:7,5:12)			#Set A, less set B
length(1:7)				#Number of elements in set

### 6. Look at the workspace
ls()					#Variables in workspace
class(x)				#Kind of variable x is
rm(x)					#Remove x from workspace
ls()					
rm(list=ls())				#Remove all variables

### 7. Plotting
# "plot" is the basic function
x = seq(0,2*pi,by=1)			#Set x-values to plot
y = sin(x)				#Obtain y-values from x
plot(x,y)				#Plot points
plot(x,y,type="l")			#Plot line between points

x = seq(0,2*pi,by=0.01)			#Obtain finer partition
y = sin(x)
plot(x,y)
plot(x,y,type="l",col="blue")		#Make the line blue


##### Problem 1.6
#(a)
community = c("Galicia","Asturias","Cantabria","Pais Vasco",
	"Navarra","La Rioja","Aragon","Cataluna","Islas Baleares",
	"Castilla y Leon","Madrid", "Castilla-La Mancha", "C. Valenciana",
	"Region de Murcia","Extremadura", "Andalucia", "Islas Canarias")
wheat.surface = c(18817,65,440,25143,66326,34214,311479,74206,7203,
	619858,13118,263424,6111,9500,143250,558292,100)
wheat.spain = data.frame(community, wheat.surface)
rm(community, wheat.surface)

#(b)
attach(wheat.spain)  #Makes the variables within the data.frame available for use by column name
c(max(wheat.surface), min(wheat.surface), diff(range(wheat.surface)))

#(c)
a = which(wheat.surface==max(wheat.surface))
wheat.spain[a,]

#(d)
partd = wheat.spain[order(wheat.surface),]
head(partd)

#(e)
parte = wheat.spain[order(wheat.surface, decreasing=TRUE),]
head(parte)

#(f) #Three different ways
wheat.c1 = wheat.spain[community!="Asturias",]  #'!=' is the boolean 'not equals'
head(wheat.c1)
wheat.c2 = wheat.spain[c(1,3:17),]			#Call exactly which rows desired
head(wheat.c2)
wheat.c3 = wheat.spain[-2,]				#Call which rows to remove
head(wheat.c3)

#(g)
wheat.c = rbind(wheat.c1, c("Asturias", 65))
tail(wheat.c)
wheat.c = wheat.spain #Resore exactly

#(h)
wheat.c = cbind(wheat.c, acre = wheat.surface/0.40468564224)
head(wheat.c)

# (i) The total surface area is 2151546 hectares or 5316586 acres.
attach(wheat.c)
c(sum(wheat.surface), sum(acre))

# (j)
newwheat = wheat.c[,2:3]
row.names(newwheat) = wheat.c[,1]
wheat.c = newwheat
head(wheat.c)
rm(newwheat)

# (k) 29.41%
sum(wheat.surface > mean(wheat.surface))/length(wheat.surface)*100  #Take this line apart

# (l)
partl = wheat.c[order(row.names(wheat.c)), ]
head(partl)

# (m) The total harvested area is 36537.0 and 90284.9 (hectares and acres), respectively.
lessthan40k = wheat.c[acre < 40000,]
lessthan40k
detach(wheat.c)
attach(lessthan40k)
less = c(sum(wheat.surface), sum(acre))

# (n)
detach(lessthan40k)
attach(wheat.c)
stepone = wheat.c[acre >= 40000,]
wheat.sum = rbind(stepone,less)
row.names(wheat.sum)[11] = c("less than 40,000")
wheat.sum

# (o)
dump("wheat.c",file="wheat.txt")
ls()
rm(wheat.c)
ls() #Notice wheat.c is gone
source("wheat.txt")
ls() #wheat.c is back

# (p) There are different values stored in each of wheat.txt and wheat.dat. Specifically,
#the values from part (m) are collapsed into one category "less than 40,000" in
#wheat.dat, whereas wheat.txt has all of the values.
#Structurally, the dump() created a file with the R commands to recreate the wheat.c data.frame
#whereas the read.table() created a tab-separated file
write.table(wheat.sum, "wheat.dat")

# (q)
read.table("wheat.dat")


##### Chapter 1 #12a
convert <- function(km, hect) {
	mile = km/0.6214
	acre = hect/2.471
c(mile,acre)
} #end function

convert(10.2, 22.4)	#Call your new function


##############################################################################
########## Chapter 3 #########################################################
##############################################################################
##### 3.3 Ex 3.9 (p. 82) Birthday Problem, Monte Carlo
birthday.match <- function(m) {
#BIRTHDAY.MATCH Simulates one group of m people and whether there is a match
  days = round(364*runif(m))+1
  unique.days = unique(days)
  out=0
  if (length(unique.days)!=m) out=1
  return(out)
}

birthday <- function(m,MC) {
#BIRTHDAY is a Monte Carlo method for the birthday problem, using birthday.match()
  matches = integer(length=MC)
  for (i in 1:MC) {
    matches[i] = birthday.match(m)
  } #end for
  out = sum(matches)/MC
  return(out)
} #end function



##### 3.4 Gas Pump Life Example
fx = function(x){0.6*exp(-0.6*x)}
k = integrate(fx,0,2)$value
k

par(mfrow=c(1,2), pty="s")
x <- seq(0,5,0.01)
y <- fx(x)
plot(x, y, xlim=c(0,10), ylim=c(0,1), type="l", xlab="x",
ylab="f(x)")
segments(-2,0,-1,0)
segments(1,0,2,0)
title("PDF for X")
y <- -x^3/4 +3*x/4+1/2
plot(x, y, xlim=c(-2,2), ylim=c(0,1), type="l", xlab="x",
ylab="F(x)")
segments(-2,0,-1,0)
segments(1,1,2,1)
title("CDF for X")
par(mfrow=c(1,1))
par(pty="m")
#
# Section 3.4.5.1 Numerical Integration with S
fx <- function(x){3/4-3/4*x^2}
integrate(fx, lower=-0.5, upper=1)


### 3.4.7 Weak Law of Large Numbers
#Play with samples for a little while
u.br=seq(0.5,6.5,1)
n=12 #n = number of die rolls 
x=ceiling(runif(n, min=0, max=6)); x; hist(x,breaks=u.br,freq=FALSE); mean(x)


### 3.4.7.extra: Monte Carlo Integration
x = runif(1000)
f = function(x) {(1/sqrt(2*pi))*exp(-x^2/2)}
sum(f(x))/length(x)


### 3.4.8 Skewness Coefficients
# T = total number of pumps
T = 0:10
pT = c(1,2,3,4,5,5,5,4,3,2,1)/35
ET = sum(T*pT)
sigmaT = sqrt(sum(T^2*pT) - ET^2)
(T-ET)^3
T.star = (T-ET)^3/sigmaT^3
skew.T = sum(T.star*pT)
skew.T

#X = lifetime of pump, f(x)= 0.6e^(-0.6x)
fx3 = function(x){(x-5/3)^3*0.6*exp(-0.6*x)}
integrate(fx3,0,Inf)$value
integrate(fx3,0,10)$value

#For data, 'x', use
#library(moments)
#skewness(x)


##############################################################################
########## Chapter 4 #########################################################
##############################################################################
##### Discrete PMF and CDF demonstration of examples from handout

### Binomial
# X = number of correct guesses out of n=20 5-response multiple choice Q's
p = 0.20 			#P(success) for one trial
n = 20   			#Number of trials
x=0:n				#Vector of trial indices
p.x = dbinom(x, size=n, prob=p)	#PMF
c.x = pbinom(x, size=n, prob=p) #CDF
par(mfrow=c(2,1))
barplot(p.x, names.arg=x, col="blue", xlab="x", ylab="probability") #Plot PMF
title("Binomial PMF")
barplot(c.x, names.arg=x, col="green", xlab="x", ylab="probability") #Plot PMF
title("Binomial CDF")


### Geometric
# Aloib is a stock with a good rate of return, but has a p=0.01
# probability of a bad news report causing it to lose 1/2 of its value
# X = number days before the day the stock loses its value
p = 0.01 			#P(success) for one trial
n = 150   		#Number of trials to plot, not a parameter of distribution
x=0:(n-1)			#Vector of trial indices, reparametrized to match R's definition
par(mfrow=c(2,1))
p.x = dgeom(x, prob=p)		#PMF
barplot(p.x, names.arg=x, col="blue", xlab="x", ylab="probability") #Plot PMF
title("Geometric PMF")
c.x = pgeom(x, prob=p)  	#PMF
barplot(c.x, names.arg=x, col="green", xlab="x", ylab="probability") #Plot PMF
title("Geometric CDF")


### Negative Binomial
# Same as geometric, except it takes r=2 bad news reports to drop the stock
# X = number of days before the 2nd bad news report comes
p = 0.01 			#P(success) for one trial
n = 150   			#Number of trials
r = 2				#Number of successes
x=0:(n-1)			#Vector of trial indices
par(mfrow=c(2,1))
p.x = dnbinom(x, size=r, prob=p)  #PMF
barplot(p.x, names.arg=x, col="blue", xlab="x", ylab="probability") #Plot PMF
title("Negative Binomial PMF")
c.x = pnbinom(x, size=r, prob=p)  #PMF
barplot(c.x, names.arg=x, col="green", xlab="x", ylab="probability") #Plot PMF
title("Negative Binomial CDF")


### Poisson
#A particular flash drive product has lambda=0.2 bad sectors per flash drive
#X = number of bad sectors / drive
lambda = 0.2			#Poisson parameter, mean missing pulses per disk = 0.2
x = 0:5				#First few elements of sample space
p.x = dpois(x, lambda=lambda)	#Truncated PMF
barplot(p.x, names.arg=x, col="blue", xlab="x", ylab="probability") #Plot PMF
title("Poisson PMF, lambda=0.2")
round(p.x, 2)   #Nicely display P(X)

#X = number of bad sectors in a case of 10 drives
lambda = 2  		#Poisson parameter, mean missing pulses per disk = 0.2
x = 0:10				#First few elements of sample space
p.x = dpois(x, lambda=lambda)	#Truncated PMF
barplot(p.x, names.arg=x, col="blue", xlab="x", ylab="probability") #Plot PMF
title("Poisson PMF, lambda=2.0")
round(p.x, 2)


### Hypergeometric
# X = number of ways to select x good and k-x bad items
# with a sample of size k from a population of m good and n bad items
n = 100 #Number of items, total
m = 50		#Number of defective items
r = 4		#Number of items selected
x=0:r		#Vector of trial indices
p.x = dhyper(x, m=m, n=n, k=r)  #PMF
barplot(p.x, names.arg=x, col="blue", xlab="x", ylab="probability") #Plot PMF
title("Hypergeometric PMF")
round(p.x, 2)


##### Continuous PDF examples
### Exponential
x=seq(0,5,length.out=200)
f.x=dexp(x,0.5)
f.y=dexp(x,2)
f.z=dexp(x,5)
plot(x,f.y,type="l",lwd=3,lty=1,col="blue",xlab="X",ylab="Probability",main="Exponential Dist, varying lambda")
lines(x,f.x,type="l",lwd=3,lty=2,col="red")
lines(x,f.z,type="l",lwd=3,lty=3,col="green")
legend("topright",legend=c(2,0.5,5),col=c("blue","red","green"),
      lwd=3,lty=1:3)

### Gamma
x=seq(0,5,length.out=200)
f.x=dgamma(x,shape=2,rate=0.5)
f.y=dgamma(x,shape=2,rate=2)
f.z=dgamma(x,shape=2,rate=5)
plot(x,f.z,type="l",lwd=3,lty=1,col="blue",xlab="X",ylab="Probability",main="Gamma Dist, alpha=2, varying lambda")
lines(x,f.y,type="l",lwd=3,lty=2,col="red")
lines(x,f.x,type="l",lwd=3,lty=3,col="green")
legend("topright",legend=c(5,2,0.5),col=c("blue","red","green"),
       lwd=3,lty=1:3)

x=seq(0,5,length.out=200)
f.x=dgamma(x,shape=5,rate=2)
f.y=dgamma(x,shape=2,rate=2)
f.z=dgamma(x,shape=0.5,rate=2)
plot(x,f.y,type="l",lwd=3,lty=1,col="blue",xlab="X",ylab="Probability",main="Gamma Dist, varying alpha, lambda=2")
lines(x,f.x,type="l",lwd=3,lty=2,col="red")
lines(x,f.z,type="l",lwd=3,lty=3,col="green")
legend("topright",legend=c(2,5,0.5),col=c("blue","red","green"),
       lwd=3,lty=1:3)

### Weibull
x=seq(0,5,length.out=200)
f.x=dweibull(x,shape=0.5,scale=2)
f.y=dweibull(x,shape=1,scale=2)
f.z=dweibull(x,shape=3,scale=2)
plot(x,f.x,type="l",lwd=3,lty=1,col="blue",xlab="X",ylab="Probability",main="Weibull Dist, varying alpha, beta=2")
lines(x,f.y,type="l",lwd=3,lty=2,col="red")
lines(x,f.z,type="l",lwd=3,lty=3,col="green")
legend("topright",legend=c(0.5,1,3),col=c("blue","red","green"),
       lwd=3,lty=1:3)

### Beta
x=seq(0,1,length.out=200)
f.x=dbeta(x,shape1=2,shape2=2)
f.y=dbeta(x,shape1=1,shape2=1)
f.z=dbeta(x,shape1=0.5,shape2=0.5)
plot(x,f.x,type="l",lwd=3,lty=1,col="blue",xlab="X",ylab="Probability",main="Beta Distribution")
lines(x,f.y,type="l",lwd=3,lty=2,col="red")
lines(x,f.z,type="l",lwd=3,lty=3,col="green")
legend("bottom",legend=c("(2,2)","(1,1)","(.25,.25)"),col=c("blue","red","green"),
       lwd=3,lty=1:3)

x=seq(0,1,length.out=200)
f.x=dbeta(x,shape1=.25,shape2=2)
f.y=dbeta(x,shape1=2,shape2=.25)
f.z=dbeta(x,shape1=3,shape2=3)
plot(x,f.z,type="l",lwd=3,lty=1,col="blue",xlab="X",ylab="Probability",main="Beta Distribution")
lines(x,f.y,type="l",lwd=3,lty=2,col="red")
lines(x,f.x,type="l",lwd=3,lty=3,col="green")
legend("center",legend=c("(3,3)","(2,.25)","(.25,2)"),col=c("blue","red","green"),
       lwd=3,lty=1:3)

### Normal
x=seq(0,1,length.out=200); mu=0.5; SD=0.75
y = dnorm(x,mu,SD)
plot(x,y,type="l",lwd=3,col="blue",lty=2, xlab="x", ylab="Probability",main="Three Normal Distributions, (mu, sigma)")
mu=0; SD=1
x = seq(-4,4,by=0.01)
y = dnorm(x,mu,SD)
lines(x,y,type="l",lwd=3,col="red",lty=1)
mu=-.5; SD=1.5
y = dnorm(x,mu,SD)
lines(x,y,type="l",lwd=3,col="green",lty=3)
legend("topright",legend=c("(.5, .75)","(0,1)","(-.5,1.5)"),col=c("red","blue","green"),
       lwd=3,lty=c(1,2,3))


##### Demonstration of R's probability distribution functions
# p. 69, #53.  We have X~N(mean=5, sigma=10); R sets sigma=sd
# All of the probability functions have the following structure:
# Prefix + Suffix
# Prefixes: d, p, q, r
#   d gives the PMF (discrete) or PDF (continuous)
#   p gives the CDF
#   q gives the quantile/percentile
#   r generates random numbers from the distribution
# Suffixes: binom, pois, norm, gamma, etc.
#   The suffix gives the name of the distribution

#(a) P(X>10)
1-pnorm(10,5,10)
#(b) P(-20 < X < 15)
pnorm(15,mean=5,sd=10)-pnorm(-20,mean=5,sd=10)
#(c) x such that P(X>x) = 0.05
#The functions always give the left tail, so we need 1-0.05=0.95:
qnorm(0.95,mean=5,sd=10)
#(d) Generate 8 random normal deviates
rnorm(8,mean=5,sd=10)
#(e) Generate 1000 random normal deviates, histogram, with curve
out = rnorm(1000,mean=5,sd=10)
hist(out,freq=FALSE)  #Need density plot, not frequencies!
x=seq(-25,35,by=0.1)
y=dnorm(x,mean=5,sd=10)
lines(x,y,type="l",col="blue")



##############################################################################
##### Chapter 5 Multivariate Probability Distributions
##############################################################################
##### 5.6 Multinomial
### Ex 5.17, p. 186
x = c(4,4,2); n=sum(x); PI=c(0.5,0.3,0.2)
dmultinom(x, size=n, prob=PI)


##### 5.7 Bivariate Normal
### p. 187
#Note: These scripts are an improvement upon those given in the text, which only plots
#the JPDF, contour map, and heat map, without allowing for changing rho.  This
#improvement provides a good example of passing a parameter into a function (RHO) which
#is, in turn, used by other functions within the main function (rho)

function1.draw <- function(f, low = -1, hi = 1, n = 50, RHO=0.5){
#Draws 3D Graph of function f
  r <- seq(low, hi, length = n)
  z <- outer(r, r, f, rho=RHO)
  persp(r, r, z, axes=FALSE, box=TRUE)
  title(paste("rho=",RHO))
} #end function1.draw

bivnorm <- function(x,y,rho=0.5){
#bivariate normal JPDF
  exp( (x^2-2*rho*x*y+y^2) / (-2*(1-rho^2)) )/
  (2*pi*sqrt(1-rho^2))
} #end bivnorm
 
tri.plot <- function(f,min=-3,max=3,n=50,RHO=0.5) {
#Plot 3D JPDF, contour plot, and heat map
  function1.draw(f,min,max,n,RHO) #Plot 3D graph
  x = seq(min,max,length=100)     #Set-up for contour + heat maps     
  y = x
  contour(x,y,outer(x,y,f,rho=RHO),nlevels=10,xlab="x",ylab="y") #Plot contour graph
  image(x,y,outer(x,y,f,rho=RHO),zlim=range(outer(x,y,f,rho=RHO)),add = FALSE) #Plot color contour
} #end tri.plot

par(mfcol=c(3,4)) #Set up plot
tri.plot(bivnorm,RHO=0.0)
tri.plot(bivnorm,RHO=0.3)
tri.plot(bivnorm,RHO=0.6)
tri.plot(bivnorm,RHO=0.9)

par(mfrow=c(1,1))

##############################################################################
##### Chapter 6 Central Limit Theorem Demonstration
##############################################################################
##########################################################################
##### Demonstrate the LLN and CLT
### Uniform Assumption
#Function for demonstration
CLT.unif <- function(samples, sample.size, min=0, max=6) {
#CLT.unif produces a plot of discrete uniform data samples,
#and a plot of their means, in order to do a side-by-side
#demonstration of the LLN and the CLT
#samples = simulation sample size
#sample.size = sample size of a single sample, n
#min = the minimum value (0 for a single die roll, using ceiling function)
#max = the maximum value (6 for a single die roll)
###############################################################
#Generate the data
n=sample.size
x.bar=0; xs=NULL
for (i in 1:samples) {
  x = ceiling(runif(n, min=min, max=max)) #sample
  xs = c(xs,x)                            #collect all data
  x.bar[i] = mean(x)                      #vector of means
} #end for
Emean=3.5; sigma=5*(5+2)/12; SE=sigma/sqrt(n)
mean.xbar=mean(x.bar); SE.xbar=sd(x.bar)
means=c(Emean, mean.xbar)
SEs=c(SE, SE.xbar)
unif.out=data.frame(means, SEs, row.names=c("Theory", "Sample"))

#Create histograms
par(mfrow=c(1,2))
u.br=seq((min+0.5),(max+0.5),1)
hist(xs,breaks=u.br)
hist(x.bar)
return(unif.out)
} #end function

#Run for different combinations of samples and n
n=10
CLT.unif(1000, n) #Try (1000, n) for n=10, 100, 1000

###############################################################
### Binomial Assumption
#Function for demonstration
CLT.binom <- function(samples, sample.size, size=20, p=0.25) {
#CLT.binom produces a plot of binomial data samples,
#and a plot of their means, in order to do a side-by-side
#demonstration of the LLN and the CLT
#samples = simulation sample size
#sample.size = sample size of a single sample, n
#min = the minimum value (0 for a single die roll, using ceiling function)
#max = the maximum value (6 for a single die roll)
###############################################################
#Generate the data
n=sample.size
x.bar=0; xs=NULL
for (i in 1:samples) {
  x = rbinom(n, size, p)
  xs = c(xs,x)
  x.bar[i] = mean(x)
} #end for
Emean = size*p; 
sigma=sqrt(size*p*(1-p)); SE=sigma/sqrt(n)
mean.xbar=mean(x.bar); SE.xbar=sd(x.bar)
means=c(Emean, mean.xbar)
SEs=c(SE, SE.xbar)
binom.out=data.frame(means, SEs, row.names=c("Theory", "Sample"))

#Create Histograms
par(mfrow=c(1,2))
b.br=seq(-0.5,(size+0.5),1)
hist(xs,breaks=b.br)
hist(x.bar)
return(binom.out)
} #end function

#Run for different combinations of samples and n (=sample size)
n=10
CLT.binom(1000, n) #Try (1000, n) for n=10, 100, 1000



##############################################################################
#Old Material
### Chapter 2.3
par(mfrow=c(2,2)) #Create 2x2 frame for graphs

# The PDF for an exponential RV with parameter lambda = 0.6 has the following graph
x1 = seq(0, 10, by=0.01)
f.x = dexp(x1, rate=0.6) #lambda = rate
plot(x1, f.x, type="l")
title("Exact plot of PDF")

# A histogram of a sample of n=10000 exponential deviates
n=10000
x2 = rexp(n, rate=0.6)
hist(x2, breaks=50)    #"breaks" gives the number of bins

# A histogram of n=10000 transformed uniform deviates
# The inverse CDF is xinv = -ln(1-x)/lambda
x3 = runif(n)
xinv = -log(1-x3)/0.6
hist(xinv, breaks=50)

#What is the point?
#1) Graph 1 is the "exact" graph
#2) Graph 2 is near the exact, created from random EXPONENTIAL numbers
#3) Graph 3 is near the exact, but created from random UNIFORM numbers
#This illustrates how to generate non-uniform random numbers using only
#uniform random numbers 
