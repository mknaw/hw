# 3M1. Suppose the globe tossing data had turned out to be 8 water in
# 15 tosses. Construct the posterior distribution, using grid approximation.
# Use the same flat prior as before.

p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep( 1 , 1000 )
likelihood <- dbinom( 8 , size=15 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
plot( p_grid, posterior, type="l" )


# 3M2. Draw 10,000 samples from the grid approximation from above.
# Then use the samples to calculate the 90% HPDI for p.

samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )
library(rethinking)
print( HPDI( samples , prob=0.9 ) )


# 3M3. Construct a posterior predictive check for this model and data.
# This means simulate the distribution of samples, averaging over the
# posterior uncertainty in p.
# What is the probability of observing 8 water in 15 tosses?

w <- rbinom( 1e4 , size=15 , prob=samples )
print( length( w[ w == 8 ] ) / length( w ) )


# 3M4. Using the posterior distribution constructed from the new (8/15)
# data, now calculate the probability of observing 6 water in 9 tosses.


w <- rbinom( 1e4 , size=9 , prob=samples )
print( length( w[ w == 6 ] ) / length( w ) )


# 3M5. Start over at 3M1, but now use a prior that is zero below 0.5 and
# a constant above p = 0.5. This corresponds to prior information that a
# majority of the Earth’s surface is water. Repeat each problem above and
# compare the inferences. What difference does the better prior make?
# If it helps, compare inferences (using both priors) to the true value p = 0.7.

p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- c( rep( 0 , 500 ), rep( 2 , 500 ) )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
plot( p_grid, posterior, type="l" )

samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )
library(rethinking)
print( HPDI( samples , prob=0.9 ) )

w <- rbinom( 1e4 , size=15 , prob=samples )
hist( w )
print( length( w[ w == 8 ] ) / length( w ) )

w <- rbinom( 1e4 , size=9 , prob=samples )
print( length( w[ w == 6 ] ) / length( w ) )

# Interestingly, no huge effect on the upper bound of the HDPI, just a
# higher lower bound. I would expect a narrower HDPI with a "more opinionated"
# prior. A slightly higher chance in the simulations of drawing
# exactly 8/15 and 6/9 - this is because the minimum sampled probability is
# much higher.


# 3M6. Suppose you want to estimate the Earth’s proportion of water very precisely.
# Specifically, you want the 99% percentile interval of the posterior distribution
# of p to be only 0.05 wide. This means the distance between the upper and lower
# bound of the interval should be 0.05.
# How many times will you have to toss the globe to do this?

check_interval <- function( n_tosses ) {
    p_grid <- seq( from=0 , to=1 , length.out=1000 )
    prior <- rep( 1 , 1000 )
    likelihood <- dbinom( round( 0.7 * n_tosses ) , size=n_tosses , prob=p_grid )
    posterior <- likelihood * prior
    posterior <- posterior / sum(posterior)
    samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )
    interval <- unname( PI( samples, 0.95 ) )
    print( diff( interval ) )
}

check_interval(1e1)
check_interval(1e2)
check_interval(1e3)
check_interval(1.28e3)

# Requires close to 1,300 tosses!
