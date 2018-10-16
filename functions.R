
# Random matrices generation ----------------------------------------------


generate_random_matrix <- function(k=5) {
  M <- matrix(rbinom(k*k, 1,.5), nrow=k)
  return(M)
}

check_triplet <- function(U,V,W) {
  equal <- T
  if(identical((U %*% V) %% 2, W)){
      equal <- F
  }
  return(equal)
}


# Algorithms ------------------------------------------------------

one_step <- function(U, V, W) {
  stopifnot(dim(U) == dim(V) && dim(V) == dim(W))
  z <- array(rbinom(dim(U)[1], 1, .5))
  
  res <- (U %*% ((V %*% z) %% 2)) %% 2
  if( identical(res, (W %*% z) %% 2) ){
    return(F)
  } else {
    return(T)
  }
}

m_steps <- function(k = 5, m = 100) {
  stopifnot(k > 0 && m > 0)
  
  U <- generate_random_matrix(k)
  V <- generate_random_matrix(k)
  W <- generate_random_matrix(k)
  
  a = c(one_step(U,V,W))
  if(m > 1) {
    for( i in 2:m ) {
      a = c(a, one_step(U,V,W))
    }
  }
  return(a)
}

