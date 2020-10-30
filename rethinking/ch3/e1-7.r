p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep( 1 , 1000 )
likelihood <- dbinom( 6 , size=9 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
set.seed(100)
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )

# 3E1. How much posterior probability lies below p = 0.2?
sum( samples <= 0.2 ) / 1e4

# 3E2. How much posterior probability lies above p = 0.8?
sum( samples >= 0.8 ) / 1e4

# 3E3. How much posterior probability lies between p = 0.2 and p = 0.8?
sum( samples[ samples >= 0.2 & samples <= 0.8 ] ) / 1e4

# 3E4. 20% of the posterior probability lies below which value of p?
quantile( samples, probs = 0.2 )

# 3E5. 20% of the posterior probability lies above which value of p?
quantile( samples, probs = 0.8 )

# 3E6. Which values of p contain the narrowest interval equal to 66% of
#      the posterior probability?
library(rethinking)
HPDI( samples , prob=0.66 )

# 3E7. Which values of p contain 66% of the posterior probability,
#      assuming equal posterior probability both below and above the interval?
quantile( samples, probs = c( 0.33/2 , 1 - 0.33/2 ) )

