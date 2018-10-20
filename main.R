source("functions.R")

# 1.a. --------------------------------------------------------------------

# array containing k values for each matrix (5, 50, 100, 500)
ks <- c(5, 50, 100, 500)
# initalizing 'results vector' (it will contain the results of the proportion)
res.a <- vector()
# inizializing 'execution times vector' (execution time of one-step algo for each matrix)
iter.times <- vector(length=4)

# now, for every type of k, do one-step algorithm (m-step with m=1)
# and concatenate the results in res.a (True if the matrices are different)

for( i in 1:length(ks)){
  
  start.time <- Sys.time()
  # m_steps takes in input the actual k and m=1 (one-step algo)
  # and gives as output the result
  res.a <- c(res.a, m_steps(k=ks[i], m=1))
  end.time <- Sys.time()
  iter.times[i] <- end.time - start.time
}

# plotting execution times results
names(iter.times) <- ks
plot(x = names(iter.times), y = iter.times, xlab = 'matrix dimension', col = "dodgerblue3",
     ylab = 'exec time (log scale)', main='one step algorithm', xaxt='n', type='s')
axis(side = 1, at = ks)

# describing the theoretical function y= ak^2 approximating the unknown constant 'a' 
# with the empirical value of execution time for the k = 500 matrix (a ~ time_k_500 / 500^2)
theor.time.comp <- function(x) (iter.times[4]/500^2)*x^2 
curve(theor.time.comp, add=T, col='chartreuse3')

# 1.b ---------------------------------------------------------------------
ks = c(5, 50, 100, 500)
m = 100
prop = vector()

for( i in 1:length(ks)){
  res.b = c(m_steps(k=ks[i], m=m))
  t = table(res.b) / length(res.b)
  prop = c(prop, t['TRUE'])
}


# 1.c ---------------------------------------------------------------------

ks = c(5, 50, 100, 500)
ms = c(1000, 10000)
prop = vector()

seq.start.time <- Sys.time()
for( i in 1:length(ks)){
  for( j in 1:length(ms)){
    res.b = c(m_steps(k=ks[i], m=ms[j]))
    t = table(res.b) / length(res.b)
    prop = c(prop, t['TRUE'])  
  }
}
seq.end.time <- Sys.time()
seq.tot.time <- seq.end.time - seq.start.time

# 1.c.parallel ------------------------------------------------------------
library(foreach)
library(doParallel)

ks = c(5, 50, 100, 500)
ms = c(1000, 10000)

par.start.time <- Sys.time()
cl <- parallel::makeForkCluster(nnodes=8)
doParallel::registerDoParallel(cl)

a <- foreach(i = 1:length(ks), .combine = 'c') %:% foreach(j = 1:length(ms)) %dopar% {
  res.b = c(m_steps(k=ks[i], m=ms[j]))
  t = table(res.b) / length(res.b)
  t['TRUE']
}

parallel::stopCluster(cl)

par.end.time <- Sys.time()
par.tot.time <- par.end.time - par.start.time



# 2.b P(E) estimation ---------------------------------------------------------

matrix_arr = list()
n_matrices = 500

for (i in 1:n_matrices){
  temp = list()
  
  for (j in 1:3){
    temp[[j]] <- generate_random_matrix(k=5)
  }
  
  matrix_arr[[i]] = temp
}


# 2.c ---------------------------------------------------------------------
# p-step with p = 100, for each triple in the list
# p(E) estimation <- useless

p = 100
res = c()

for (i in 1:n_matrices){
  equality = m_steps(k=5, m = p, U = matrix_arr[[i]][[1]], 
                     V = matrix_arr[[i]][[2]],
                     W = matrix_arr[[i]][[3]])
  if(length(equality[equality == 'FALSE']) > 0){
    res = c(res, FALSE) # not equal
  }
  else {
    res = c(res, TRUE)
  }
}

# iterative bayes for p from 1 to 50
p <- 50
p.error <- .5 # less than .5

results <- vector(mode="numeric", length = p)
results[1] <- .5 # personal probabilistic assumption

for(i in 1:p){
  p.e.given.b <- (results[i]) / (results[i] + p.error*(1 - results[i]))
  if( i != p )
    results[i + 1] <- p.e.given.b
}

# 2.d ---------------------------------------------------------------------

library(manipulate)
manipulate(plot(results, 
  xlim=c(0,x.max)),  
  x.max=slider(1,p))
