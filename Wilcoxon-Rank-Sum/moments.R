library(tidyverse)

Wilcoxon <- function(m, n, N) {
  as.numeric((1:N) %*% sample(c(rep(0, m), rep(1, n))))
}

m <- 21
n <- 5
N <- m + n

n_sample <- 100000

W <- data.frame(value = sapply(1:n_sample, function(i) Wilcoxon(m, n, N)))

# moment 1
#mean(W$value)
#n*(N+1)/2

# moment 2
mean(W$value**2)/(  (N*(N+1)/2)^2*choose(N-2,n-2)/choose(N,n) + N*(N+1)*(2*N+1)/6*choose(N-2,n-1)/choose(N,n) )


mean(W$value**2)/(  n*(N+1)*(n*(3*N+2)+N)/12 )


# n*(N+1)*(n*(3*N+2)+N)/12 - ( n(N+1)/2 )^2 = 1/12*n*m*(N+1)
var(W$value)/(  1/12*n*m*(N+1) )

