##################################################
#
# Simulation of trivariate mixture distribution     
#
##################################################

#01/02/2017

m <- 2; r <- 3; n <- 300; S <- 100
lambda <- c(0.4, 0.6)
mu <- matrix(c(0, 0, 0, 3, 4, 5), m, r, byrow = TRUE)
sigma <- matrix(rep(1, 6), m, r, byrow = TRUE)
#########
centers <- matrix(c(0, 0, 0, 4, 4, 4), 2, 3, byrow = TRUE)
ISE <- matrix(0, m, r, dimnames = list(Components = 1:m, Blocks = 1:r))
nblabsw <- 0
#########
set.seed(1000)
for (mc in 1:S) {
  x <- rmvnormmix(n, lambda, mu, sigma)
  a <- npEM(x, centers, verb = FALSE, samebw = FALSE)
  if (a$lambda[1] > a$lambda[2]) nblabsw <- nblabsw + 1
  
  for (j in 1:m) {
    for (k in 1:r) {
      ISE[j, k] <- ISE[j, k] + ise.npEM(a, j, k, dnorm, lower = mu[j, k] - 5, upper = mu[j, k] + 5, plots = FALSE, mean = mu[j, k], sd = sigma[j, k])$value 
    }
  }
}
########
MISE <- ISE/S
print(sqMISE <- sqrt(MISE))
summary(a)
plot(a)
