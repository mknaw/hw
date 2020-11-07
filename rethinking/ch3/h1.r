birth1 <- c(1,0,0,0,1,1,0,1,0,1,0,0,1,1,0,1,1,0,0,0,1,0,0,0,1,0,            0,0,0,1,1,1,0,1,0,1,1,1,0,1,0,1,1,0,1,0,0,1,1,0,1,0,
            0,0,0,0,0,0,1,1,0,1,0,0,1,0,0,0,1,0,0,1,1,1,1,0,1,0,
            1,1,1,1,1,0,0,1,0,1,1,0,1,0,1,1,1,0,1,1,1,1)
birth2 <- c(0,1,0,1,0,1,1,1,0,0,1,1,1,1,1,0,0,1,1,1,0,0,1,1,1,0,
            1,1,1,0,1,1,1,0,1,0,0,1,1,1,1,0,0,1,0,1,1,1,1,1,1,1,
            1,1,1,1,1,1,1,1,1,0,1,1,0,1,1,0,1,1,1,0,0,0,0,0,0,1,
            0,0,0,1,1,0,0,1,0,0,1,1,0,0,0,1,1,1,0,0,0,0)


# 3H1. Using grid approximation, compute the posterior distribution
# for the probability of a birth being a boy. Assume a uniform prior
# probability. Which parameter value maximizes the posterior probability?

births <- c( birth1, birth2 )
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep( 1 , 1000 )
likelihood <- dbinom( sum(births), size=length(births), p=p_grid )
posterior <- prior * likelihood
posterior <- posterior / sum(posterior)
plot( p_grid, posterior, type="l" )

max_posterior <- p_grid[ which.max(posterior) ]
print( max_posterior )


# 3H2. Using the sample function, draw 10,000 random parameter values
# from the posterior distribution you calculated above. Use these samples
# to estimate the 50%, 89%, and 97% highest posterior density intervals.

samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )
library(rethinking)
print( HPDI( samples , prob=c( 0.5, 0.89, 0.97 ) ) )


# 3H3. Use rbinom to simulate 10,000 replicates of 200 births. You should
# end up with 10,000 numbers, each one a count of boys out of 200 births.
# Compare the distribution of predicted numbers of boys to the actual count
# in the data (111 boys out of 200 births). There are many good ways to
# visualize the simulations, but the dens command (part of the rethinking
# package) is probably the easiest way in this case. Does it look like the
# model fits the data well? That is, does the distribution of predictions
# include the actual observation as a central, likely outcome?

replicates <- rbinom( 1e4, size=200, prob=samples )
dens( replicates )

# The histogram does "include the actual observation as a central, likely
# outcome," but I wonder if this method of generating replicates isn't a
# bit circular...


# 3H4. Now compare 10,000 counts of boys from 100 simulated first borns only
# to the number of boys in the first births, birth1.
# How does the model look in this light?

likelihood <- dbinom( sum(birth1), size=length(birth1), p=p_grid )
posterior <- prior * likelihood
posterior <- posterior / sum(posterior)
samples1 <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )
print( HPDI( samples1 , prob=c( 0.5, 0.89, 0.97 ) ) )
replicates1 <- rbinom( 1e4, size=100, prob=samples1 )
dens( replicates1 )

# Decidedly appears to be a smaller probability of boy in the first data
# array than the in the cumulative data from both arrays.
# Interestingly, this time the 50% HPDI actually contains 0.5, which is
# closer to the true ratio one would expect.


# 3H5. The model assumes that sex of first and second births are independent.
# To check this assump- tion, focus now on second births that followed female
# first borns. Compare 10,000 simulated counts of boys to only those second
# births that followed girls. To do this correctly, you need to count the
# number of first borns who were girls and simulate that many births, 10,000
# times. Compare the counts of boys in your simulations to the actual observed
# count of boys following girls. How does the model look in this light?
# Any guesses what is going on in these data?

births_after_f = birth2[ birth1 == 0 ]
likelihood <- dbinom( sum(births_after_f), size=length(births_after_f), p=p_grid )
posterior <- prior * likelihood
posterior <- posterior / sum(posterior)
samples_after_f <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )
print( HPDI( samples_after_f , prob=c( 0.5, 0.89, 0.97 ) ) )
replicates_after_f <- rbinom( 1e4, size=100, prob=samples_after_f )
dens( replicates_after_f )

# Filtering `birth2` on only those entries that followed a girl in `birth1`
# show a much higher posterior probability of the second child being a boy.
# This can indicate a preference for male offspring spurring parents who
# had a firstborn girl to "try again."
