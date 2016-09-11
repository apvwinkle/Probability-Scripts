# x = the number of hit that aren't home runs before the 3rd home run.
x = 0:9; r = 3; pb = 0.10; #need 3 successes, prob of succes is 10%
# x~NB(r = 3, pi = 0.10) #pi = pb
# for negative binomial, x is the number of Bernoulli trials that
# result in failure prior to the rth success. r = 3 home runs and
# pi = 0.10, the probability of hitting a home run for each at bat.
P = dnbinom(x, size=r, prob=pb)
E = r * (1 - pb) / pb;
V = r * (1 - pb) / pb^2;
barplot(P, names.arg=x, main="PMF of Hits Before 3rd Home Run", xlab="X", ylab="Probability, P(x)")

#a)
  Pa = dnbinom(8, 3, 0.10) #8 failures first, need 3 successes, 10% chance of success
#b)
  Pb = sum(P)              #add of the probability from trials 0 to 9
  #the total isn't one because we would need many more chances to get
  #home runs (making x=0:90 gives the total probability as 0.99)