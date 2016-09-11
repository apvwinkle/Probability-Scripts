## Chapter 4
## November 16, 2007
## Alan T. Arnholt
#
library(PASWR)
# Example 4.1
Watts <- c(40,60,75,100,120)
meanWatts <- (1/5)*sum(Watts)
varWatts<- (1/5)*sum((Watts-meanWatts)^2)
ans <- c(meanWatts, varWatts)
ans
#
# Section 4.2.2
# Figure 4.1
par(mfrow=c(1,2), pty="s")
plot(0:8, dbinom(0:8,8,0.3), type="h", xlab="x",ylab="P(X=x)",
xlim=c(-1,9))
title("PDF for X~Bin(8, 0.3)")
plot(0:8, pbinom(0:8,8,0.3), type="n", xlab="x", ylab="F(x)",
xlim=c(-1,9), ylim=c(0,1))
segments(-1,0,0,0)
segments(0:8, pbinom(0:8,8,.3), 1:9, pbinom(0:8,8,.3))
lines(0:7, pbinom(0:7,8,.3), type="p", pch=16)
segments(-1,1,9,1, lty=2)
title("CDF for X~Bin(8, 0.3)")
par(mfrow=c(1,1))
#
# Example 4.2
set.seed(31)
bino.gen(1000, 5, 0.5)
#
set.seed(31)
x <- rbinom(1000, 5, .5)
table(x)/1000 # Empirical distribution
#
# Example 4.3
sum(dbinom(6:10,10,0.33))
1 - pbinom(5,10,0.33)
1 - sum(dbinom(5:0,10,0.33))
#
# Example 4.4
library(PASWR); attach(Soccer); data(Soccer)
L1 <- Goals[1:228]
L2 <- Goals[2:229]
L3 <- Goals[3:230]
L4 <- Goals[4:231]
L5 <- Goals[5:232]
LAG <- cbind(L1,L2,L3,L4,L5)
round(cor(LAG),3)
# or more succinctly
LAG <- sapply( 1:5, function(x){Goals[x:(x+227)]} )
round(cor(LAG),3)
#

### Inter-arrival times
IRT = CGT[2:575]-CGT[1:574]
table(IRT) #Notice there are no zeros, implying no 2 goals scored during the same min.,
#supporting the notion of Poisson Process

#
table(Goals)
#
mean(Goals, na.rm=TRUE)
var(Goals, na.rm=TRUE) # na.method="omit" for S-PLUS
#
OBS <- table(Goals)
Empir <- round(OBS/sum(OBS), 3)
TheoP <- round(dpois(0:(length(OBS)-1), mean(Goals, na.rm=TRUE)), 3)
EXP <- round(TheoP*232, 0)
ANS <- cbind(OBS, EXP, Empir, TheoP)
ANS
detach(Soccer)
#
# Figure 4.3 (minus the psfrag stuff)
par(mfrow=c(1,2), pty="s")
plot(0:8, dpois(0:8,1), type="h", xlab="x", ylab="P",xlim=c(-1,9),
main="PDF")
plot(0:8, ppois(0:8,1), type="n", xlab="x", ylab="F",xlim=c(-1,9),
ylim=c(0,1), main="CDF")
segments(-1,0,0,0)
segments(0:8, ppois(0:8,1), 1:9, ppois(0:8,1))
lines(0:7, ppois(0:7,1), type="p", pch=16)
segments(-1,1,9,1, lty=2)
par(mfrow=c(1,1))
par(pty="m")
#
# Example 4.6
1 - ppois(7,4)
ppois(3,8)
#
# Example 4.7
dpois(0,2)
ppois(4,2)
ppois(5,120)
#
# Example 4.8
r <- seq(0,8,1)
round(dbinom(r,100,0.04), 3)
round(dpois(r,4), 3)
#
# Example 4.9
dgeom(3, 0.2)
#
# Example 4.10
dnbinom(2, 4, 0.9)
#
# Example 4.11
dhyper(25, 147, 3, 25)
#
# Example 4.13
(pi/6)*mean(runif(1000,3,5)^3)
#
# Example 4.14
set.seed(33)
U <- runif(1000)
X <- sqrt((2-sqrt(4-3*U)))
mean(X)
var(X)
# Using Numerical Integration
f <- function(x){(4/3)*x*(2-x^2)}
ex <- function(x){x*f(x)}
ex2 <- function(x){x^2*f(x)}
EX <- integrate(ex,0,1)
EX2 <- integrate(ex2,0,1)
VX <- EX2$value - EX$value^2
c(EX$value,EX2$value,VX)
#
# Example 4.17
# part a.
round(pexp(12,1/8) - pexp(3,1/8),4)
# Or
f1 <- function(x){(1/8)*exp(-x/8)}
integrate(f1,3,12) # For R
#
# For S-PLUS
# f1 <- function(x){(1/8)*exp(-x/8)}
# integrate(f1,3,12)$integral
#
# part b.
qexp(0.95, 1/8)
# part c.
1-pexp(15, 1/8)
#
# Example 4.18
# part a.
attach(Soccer)
inter.times <- CGT[2:575] - CGT[1:574]
mean(inter.times)
sd(inter.times)
# part b.
rate <- 1/(90/(575/232))
ntot <- length(inter.times)
OBS <- table(cut(inter.times, breaks=c(seq(0,130,10), 330)))
EmpiP <- round(OBS/ntot,3)
TheoP <- round(c((pexp(seq(10,130,10),rate) - pexp(seq(0,120,10),rate)),
(1 - pexp(130,rate))), 3)
EXP <-round(TheoP*ntot, 0)
ANS <-cbind(OBS, EXP, EmpiP, TheoP)
ANS
# part c.
hist(inter.times,breaks=seq(0,310,10),col=13,xlim=c(0,125),prob=TRUE,
xlab="Time Between Goals")
xt <- seq(0,140,0.01)
ft <- dexp(xt, rate)
lines(xt, ft, type="l")
detach(Soccer)
#
# Example 4.20
# part a.
1 - ppois(4,6)
# part b.
1 - pgamma(1,2,3)
gam23<-function(x){9*x*exp(-3*x)}
integrate(gam23,1,Inf) # R
# For S-PLUS
# gam23<-function(x){9*x*exp(-3*x)}
# integrate(gam23,1,Inf)$integral 
#
# Example 4.21
# part a.
g <- function(x){(2000 - 0.1*exp(-2*x))}
k1a <- integrate(g,0,5)$value
k1a
# Or
k1 <- (10000 +0.05*exp(-10) -0.05)
k1
f <- function(x){1/k1*(2000 - 0.1*exp(-2*x))}
integrate(f,0,5)
# part b.
integrate(f,0,1)
# part c.
et <- function(x){x*f(x)}
ET <- integrate(et,0,5)$value
ET
et2 <- function(x){x^2*f(x)}
ET2 <- integrate(et2,0,5)$value
VX <- ET2 - ET^2
SX <- sqrt(VX)
SX
# part d.
dbinom(0,5,0.1999967)
# part e.
fr <- function(x){2000*x+0.05*exp(-2*x)-0.05*k1 -0.05}
uniroot(fr,c(0,5))$root
#
days <- floor(uniroot(fr,c(0,5))$root*365)
days
# part f.
# Figure 4.9
year <- seq(0,5,length=500)
CDF <- function(x){1/k1*(2000*x + 0.05*exp(-2*x)-0.05)}
plot(year, f(year)/(1-CDF(year)),type="l",ylab="h(year)")
#
# Example 4.22
1 - pweibull(8,2,8)
#
8*gamma(3/2)
#
# Example 4.23
1 - pbeta(0.8,4,2)
# or
b42 <- function(x){(gamma(6)/(gamma(4)*gamma(2)))*x^3*(1-x)}
integrate(b42,0.8,1) # R
# For S-PLUS
# b42 <- function(x){(gamma(6)/(gamma(4)*gamma(2)))*x^3*(1-x)}
# integrate(b42,0.8,1)$integral # S-PLUS
#
# Example 4.24
GB <- function(x)
{(1/8)*(gamma(5)/(gamma(2)*gamma(3)))*((x-8)/8)*((16-x)/8)^2}
integrate(GB, 8, 10) # R ... for S-PLUS integrate(GB, 8, 10)$integral
# Solving with pbeta()
A <- 8
B <- 16
x <- 10
pbeta((x-A)/(B-A),2,3)
#
# Example 4.25
# part a.
pnorm(115,100,10) - pnorm(90,100,10)
# part b.
qnorm(.90,100,10)
# part c.
qnorm(.10 + pnorm(105,100,10),100,10)
#
# Example 4.26
# part b.
p <- pnorm(0.7,0.5,0.1)
p
# part c.
sum(dbinom(19:20,20,p))
#
# Quantile-QUantile Plots for Normal Distributions
attach(Score)
par(pty="s")
X <- (1:20-1/2)/20
Xs <- qnorm(X)
Ys <- sort(scores)
plot(Xs,Ys)
quantile(Xs,c(0.25, 0.75))
quantile(Ys,c(0.25, 0.75))
# Figure 4.15
qqnorm(scores)
qqline(scores)
# Figure 4.17 (with different random numbers...)
ntester(scores)
# Figure 4.18
EDA(scores)
detach(Score)
#
################################################################################
# Code to help with problem 44
# Data Frame with CGT is Soccer
attach(Soccer)
# Note:  enter different values for per.length (90,45,30,15,5, and 1)
per.length <- 90  #period length must be a factor of 90
n.per <- 232*90/per.length  #number of periods
per.cor <-2*(90/per.length)  #period correction to allow for doubling in indices
n.cnt <- n.per+per.cor  #length of A and B

B <- matrix(c(1:n.cnt,rep(0,n.cnt)),byrow=F,ncol=2)    
# cnt = number of periods + 2, Matrix of 1, 2, 3 in 1st col and 0s in 2nd col

A <- c(ceiling(CGT/per.length),rep(0,per.cor))
#add per.cor extra zeros to make counter work b/c of two j + 1 s in the code...

ngoals <-length(A) -per.cor
#true number of goals

j<-1
#start j counter

while(j<=(ngoals)){
    i<-0   #reset number of goals per game
    while(A[j]==A[j+1])   #two goals in the same game
    {i<-i+1  #increase number of goals per game
    j<-j+1}  #increase index of A
    B[A[j],2]<-i+1  #store in B the number of goals scored in game A[j]
    j<-j+1  #increase index of A
}
B <- B[1:n.per,]  #truncate B to the right length
table(B[,2])
sum(B[,2])
Mean <- mean(B[,2])
Var <- var(B[,2])
E.MEAN <- ngoals/n.per
E.VAR <- E.MEAN
VAL <-c(E.MEAN, Mean, E.VAR, Var)
names(VAL)<-c("Expected Mean", "Observed Mean","Expected Variance", 
"Observed Variance")
OBS <- table(B[,2])
OBS
Empir <- round(OBS/length(B[,2]),3)
TheoP <- round(dpois((0:(length(OBS)-1)),E.MEAN),3)
EXP <-round(TheoP*n.per,0)
ANS <-cbind(OBS,EXP,Empir,TheoP)
ANS
VAL
OBS <- as.vector(OBS)
PL<-barplot(OBS,names=as.character(dimnames(ANS)[[1]][1:length(OBS)]), 
col="pink")
lines(PL,EXP,type="h")
title(main=paste("Distribution of Goals \n for", per.length, 
"Minute Periods"), 
xlab="Bars Represent Observed Goals ----- Vertical Lines Represent Expected Goals", 
ylab="Number of Games")
detach(Soccer)
################################################################################








