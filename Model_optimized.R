
model {
  pi <- 3.141593
  omega <- 2 * pi
  
  ## Pre-index parameters
  for (o in 1:nObs) {
    alpha_o[o] <- alpha[id[o]]
    gamma_o[o] <- gamma[id[o]]
    lambda_o[o] <- lambda[id[o]]
    A_o[o] <- A[id[o]]
  }
  
  ## Likelihood
  for (o in 1:nObs) {
    ndvi[o] ~ dnorm(mu[o], tau)
    
    mu[o] <- alpha_o[o] +
      gamma_o[o] * (1 - exp(-age[o] / lambda_o[o])) +
      sin(phi + fire_angle[o] + omega_age[o]) * A_o[o]
  }
  
  ## Cell-level model
  for (i in 1:nGrid) {
    
    gamma.mu[i] <- inprod(env[i,], gamma.beta[])
    lambda.mu[i] <- inprod(env[i,], lambda.beta[])
    A.mu[i] <- inprod(env[i,], A.beta[])
    
    alpha[i] ~ dlnorm(alpha.mu, alpha.tau)
    gamma[i] ~ dlnorm(gamma.mu[i], gamma.tau)
    lambda[i] ~ dlnorm(lambda.mu[i], lambda.tau)
    A[i] ~ dlnorm(A.mu[i], A.tau)
  }
  
  ## Priors
  phi ~ dunif(-pi, pi)
  
  alpha.mu ~ dnorm(0.15, 10)
  
  for (l in 1:nBeta) {
    gamma.beta[l] ~ dnorm(0, 0.1)
    lambda.beta[l] ~ dnorm(0, 0.1)
    A.beta[l] ~ dnorm(0, 0.1)
  }
  
  ## Hyperpriors
  gamma.tau ~ dgamma(1,1)
  alpha.tau ~ dgamma(1,1)
  lambda.tau ~ dgamma(1,1)
  A.tau ~ dgamma(1,1)
  tau ~ dgamma(1,1)
  
  ## SDs
  sigma <- 1/sqrt(tau)
  gamma.sigma <- 1/sqrt(gamma.tau)
  alpha.sigma <- 1/sqrt(alpha.tau)
  lambda.sigma <- 1/sqrt(lambda.tau)
  A.sigma <- 1/sqrt(A.tau)
}