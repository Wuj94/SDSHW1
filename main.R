source("functions.R")


# 1.a. --------------------------------------------------------------------
ks = c(5, 50, 100, 500)
res.a = vector()

for( i in 1:length(ks)){
  res.a = c(res.a, m_steps(k=ks[i], m=1))
}


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

for( i in 1:length(ks)){
  for( j in 1:length(ms)){
    res.b = c(m_steps(k=ks[i], m=ms[j]))
    t = table(res.b) / length(res.b)
    prop = c(prop, t['TRUE'])  
  }
}


# 1.c.parallel ------------------------------------------------------------
library(foreach)
library(doParallel)

ks = c(5, 50, 100, 500)
ms = c(1000, 10000)

cl <- parallel::makeForkCluster()
doParallel::registerDoParallel(cl)

a <- foreach(i = 1:length(ks), .combine = 'c') %:% foreach(j = 1:length(ms)) %dopar% {
  res.b = c(m_steps(k=ks[i], m=ms[j]))
  t = table(res.b) / length(res.b)
  t['TRUE']  
}

parallel::stopCluster(cl)
