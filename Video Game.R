# X = the number of attempts and failures before passing the level
x = 0:10; pb = 0.20; #I'm guessing a max. of 10 attempts, 20% chance of success
# X~Geo(pi = 0.20) #pi = pb
# for a Geometric distribution, X is the number of attempts before the
# first success. The probability of success is 0.20 for this case
P = dgeom(x,prob=pb)
E = (1 - pb)/pb
V = (1 - pb)/pb^2
barplot(P, names.arg=x, main="PMF of the Number of Attempts Before Passing", xlab="X", ylab="Probability, P(x)")

#a)
  Pa = dgeom(2,prob=pb)
#b)
  Pb = pgeom(2,prob=pb)
#c)
  Pc = 1 - Pb