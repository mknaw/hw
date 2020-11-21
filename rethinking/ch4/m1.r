# 4M1. For the model definition below, simulate observed y values from
# the prior (not the posterior).

# yi ∼ Normal(μ, σ)
# μ ∼ Normal(0, 10)
# σ ∼ Exponential(1)

N = 1e4

mus <- rnorm( N, 0, 10 )
sigmas <- rexp( N, 1 )
ys <- rnorm( N, mus, sigmas )

library(rethinking)
dens(ys)


# 4M2. Translate the model above into a quap formula.
alist(
  Y ~ dnorm( mu, sigma ),
  mu ~ dnorm( 0, 10 ),
  sigma ~ dexp( 1 ))
