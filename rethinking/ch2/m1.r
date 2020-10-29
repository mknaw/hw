# 2M1. Recall the globe tossing model from the chapter.
# Compute and plot the grid approximate posterior distribution
# for each of the following sets of observations.
# In each case, assume a uniform prior for p.
# (1) W, W, W
# (2) W, W, W, L
# (1) L, W, W, L, W, W, W

p_of_water <- function( n_Ws, N, grid_size ) {
  p_grid <- seq( from=0, to=1, length.out=grid_size )
  likelihood <- dbinom( n_Ws, size=N, prob=p_grid )
  posterior <- likelihood / sum(likelihood)
  plot( p_grid, posterior, type="l" )
}

GRID_SIZE <- 50

par( mfrow=c( 1, 3 ) )
p_of_water( 3, 3, GRID_SIZE )
p_of_water( 3, 4, GRID_SIZE )
p_of_water( 5, 7, GRID_SIZE )

