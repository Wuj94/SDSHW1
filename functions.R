# Random matrices generation ----------------------------------------------

# Generates a random square matrix which elements are samples from a 
# Bernoulli(.5) distribution
# - input args:
# k is the number of rows / columns
# - output: 
# k*k matrix

generate_random_matrix <- function(k=5) {
  M <- matrix(rbinom(k*k, 1,.5), nrow=k)
  return(M)
}

# Verifies the identity UV != W
# - input:
# for a given k:
# U is a k*k binary matrix
# V is a k*k binary matrix
# W is a k*k binary matrix
# - output:
# true if the UV != W, false otherwise.

check_triplet <- function(U,V,W) {
  stopifnot(dim(U) == dim(V) && dim(V) == dim(W))
  equal <- T
  if(identical((U %*% V) %% 2, W)){
      equal <- F
  }
  return(equal)
}


# Algorithms ------------------------------------------------------

# Implements the one step algorithm given in the homework text.
# - input:
# for a given k:
# U is a k*k binary matrix
# V is a k*k binary matrix
# W is a k*k binary matrix
# - output:
# false if UVz == Wz, true otherwise.

one_step <- function(U, V, W) {
  stopifnot(dim(U) == dim(V) && dim(V) == dim(W))
  z <- array(rbinom(dim(U)[1], 1, .5))
  
  res <- (U %*% ((V %*% z) %% 2)) %% 2
  if( identical
      (res, (W %*% z) %% 2) ){
    return(F)
  } else {
    return(T)
  }
}

# Implements the m-step algorithm given in the homework text.
# - input:
# k is the number of columns / rows of the matrices U,V,W
# m is the number of trials
# U is a k*k binary matrix
# V is a k*k binary matrix
# W is a k*k binary matrix
# - output:
# an array containing the results obtained for each trial. Namely, 
# the array entries are: false if UVz == Wz, true otherwise.

m_steps <- function(k = 5, m = 100, U = generate_random_matrix(k), 
                    V = generate_random_matrix(k),
                    W = generate_random_matrix(k)) {
  stopifnot(k > 0 && m > 0)
  stopifnot(dim(U) == dim(V) && dim(V) == dim(W))
  
  a = c(one_step(U,V,W))
  if(m > 1) {
    for( i in 2:m ) {
      a = c(a, one_step(U,V,W))
    }
  }
  return(a)
}
