# X = the number of tagged deer
x = 0:20; m = 10; n = 190; k = 20 #10 tagged, 190 untagged, sample size of 20
# X~hypergeo(m = 10, n = 190, k = 20)
# for a hypergeometric distribution, X is the number of outcomes of a  certain 
# type that you get from a sample of size k. The total sample space has m objects
# of the same type as x, and n that are a different type. The sample is taken 
# without replacement from a finite population.
x = 1:20
P = dhyper(x, m, n, k)
E = m*k/(m+n);
V = m*n*k*(m+n-k)/((m+n)^2*(m+n-1));
barplot(P, names.arg=x, main="PMF of the Number of Untagged Deer Captured", xlab="X", ylab="Probability, P(x)")

#a)
  Pa = phyper(q=1, m, n, k)
#b)
  untagged = 25:100;
  Pb = dhyper(x=3, m=10, n=untagged, k=20)
  barplot(Pb, names.arg=untagged, main="PMF of the Number of Untagged Deer", xlab="X", ylab="Probability, P(x)")
  