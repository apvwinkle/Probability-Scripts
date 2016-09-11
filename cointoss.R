n = 100;
set.seed(39) #set the seed, for reproducible results
while ((percent[1] > 2) || (percent[2]>2) || (percent[3]>2) || (percent[4]>2)) {
  x = sample(0:1, size = 3*n, replace=TRUE) #Let 1 = Heads, 0 = Tails
  X = matrix(x, nrow= n, ncol=3) # 1 row = 1 three-coin-toss
  X;
  heads = apply(X, MARGIN=1, FUN=sum) # take the sum of each row
  heads;
  t = table(heads) #makes the table
  t;
  p = t/n;
  p
  theo = c(3,2,2,1,2,1,1,0)
  tt = table(theo)
  theop = tt/8
  percent = (abs(p-theop)/theop)*100
  n = n + 1
}