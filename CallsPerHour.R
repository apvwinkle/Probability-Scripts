# X = the number of calls per hour
x = 0:10; lambda = 2; #I'm guessing a max. of 10 calls/hour, mean is 2 calls/hour
# X~Pois(lambda = 2)
# for a Poisson distribution, X is the number of outcomes in a given time
# interval or space region, t. The paramater lambda is the mean number of 
# outcomes in a fixed amount of t. Here, lambda = 2 calls per hour, so t
# represents hours.
P = dpois(x,lambda)
E = lambda;
V = lambda;
barplot(P, names.arg=x, main="PMF of the Number of Calls Per Hour", xlab="X", ylab="Probability, P(x)")

#a)
  Pa = ppois(1,lambda) #max 1 call in an hour, mean of 2 calls per hour
#b)
  lambda2 = 10*(2/60) #probability of calls in a 10-minute period
  Pb = 1 - ppois(0,lambda2) #prob that phone will ring at least once
#c)
  lambda3 = 2/60 #probability of calls per minute. This means x = # minutes
  Pc = qpois(.5, lambda3) #The quantile is right continuous: qpois(p, lambda) is 
    #the smallest integer x such that P(X ??? x) ??? p.