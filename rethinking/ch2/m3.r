# 2M2. Now assume a prior for p that is equal to zero when
# p < 0.5 and is a positive constant when p ≥ 0.5.
# Again compute and plot the grid approximate posterior
# distribution for each of the sets of observations in the
# problem just above.

p_of_water <- function( n_Ws, N, grid_size ) {
  p_grid <- seq( from=0, to=1, length.out=grid_size )
  prior <- ifelse( p_grid < 0.5 , 0, 1 )
  likelihood <- dbinom( n_Ws, size=N, prob=p_grid )
  posterior <- prior * likelihood
  posterior <- posterior / sum(posterior)
  plot( p_grid, posterior, type="l" )
}

GRID_SIZE <- 50

par( mfrow=c( 1, 3 ) )
p_of_water( 3, 3, GRID_SIZE )
p_of_water( 3, 4, GRID_SIZE )
p_of_water( 5, 7, GRID_SIZE )

