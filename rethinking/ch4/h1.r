# 4H1. The weights listed below were recorded in the !Kung census, but
# heights were not recorded for these individuals. Provide predicted heights
# and 89% intervals for each of these individuals. That is, fill in the table
# below, using model-based predictions.

library(rethinking)
data(Howell1)
d <- Howell1
d <- d[ d$age >= 18, ]
model <- quap(
  alist(
    height ~ dnorm( mu, sigma ),
    mu <- a + b * ( weight - mean(weight) ),
    a ~ dnorm( 140, 20 ),
    b ~ dlnorm( 0, 1 ),
    sigma ~ dunif( 0, 50 )
  ), data=d)

post <- extract.samples( model )
for (weight in c(46.95, 43.72, 64.78, 32.59, 54.63)) {
  marginal_mu <- post$a + post$b * ( weight - mean(d$weight) )
  heights <- rnorm( 1e4, marginal_mu, post$sigma )
  print( sprintf("Mean height for weight %s: %.2f", weight, mean(heights)) )
  interval <- unname( PI( heights, 0.89 ) )
  print( sprintf( "Confidence interval of %.2f, %.2f", interval[1], interval[2] ) )
}

# 4H2. Select out all the rows in the Howell1 data with ages below 18 years
# of age. If you do it right, you should end up with a new data frame with
# 192 rows in it.

d <- Howell1
d <- d[ d$age < 18, ]
print( sprintf( "Have %s rows", dim(d)[1] ) )

# (a) Fit a linear regression to these data, using quap. Present and interpret
#     the estimates. For every 10 units of increase in weight, how much taller
#     does the model predict a child gets?

model <- quap(
  alist(
    height ~ dnorm( mu, sigma ),
    mu <- a + b * ( weight - mean(weight) ),
    a ~ dnorm( 140, 20 ),
    b ~ dlnorm( 0, 1 ),
    sigma ~ dunif( 0, 50 )
  ), data=d)
post <- extract.samples( model )
mean_a <- mean( post$a )
mean_b <- mean( post$b )
print( sprintf(
  "have a = %.2f, b = %.2f, implying a %.2f cm increase for each 10 lbs gained" ,
  mean_a, mean_b, 10 * mean_b ) )
# b seems rather high.

# (b) Plot the raw data, with height on the vertical axis and weight on the
#     horizontal axis. Superimpose the MAP regression line and 89% interval
#     for the mean. Also superimpose the 89% interval for predicted heights.

weight.seq <- seq( from=round( min( d$weight ) ) , to=round( max( d$weight ) ) , by=1 )
pred_data <- data.frame( weight=weight.seq )
mu <- link( model, data=pred_data )
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI , prob=0.89 )
sim.height <- sim( model , data=pred_data )
height.PI <- apply( sim.height , 2 , PI , prob=0.89 )

plot( height ~ weight, d, col=col.alpha(rangi2, 0.5) )
lines( weight.seq, mu.mean )
shade( mu.PI , weight.seq )
shade( height.PI , weight.seq )

# (c) What aspects of the model fit concern you? Describe the kinds of assumptions
#     you would change, if any, to improve the model. You don’t have to write
#     any new code. Just explain what the model appears to be doing a bad job of,
#     and what you hypothesize would be a better model.

# Doesn't look like a good fit at all, most of the actual data points are outside
# the simulated 89% interval; the relationship does not appear linear.


# 4H3 Suppose a colleague of yours, who works on allometry, glances at the practice
# problems just above. Your colleague exclaims, “That’s silly. Everyone knows that
# it’s only the logarithm of body weight that scales with height!” Let’s take your
# colleague’s advice and see what happens.

# (a) Model the relationship between height (cm) and the natural logarithm of weight
#     (log-kg). Use the entire Howell1 data frame, all 544 rows, adults and non-adults.

d <- Howell1
model <- quap(
  alist(
    height ~ dnorm( mu, sigma ),
    mu <- a + b * log( weight ),
    a ~ dnorm( 178, 20 ),
    b ~ dlnorm( 0, 1 ),
    sigma ~ dunif( 0, 50 )
  ), data=d)
post <- extract.samples( model )

# (b) Plot: (1) the predicted mean height as a function of weight, (2) the
#     97% interval for the mean, and (3) the 97% interval for predicted heights.

weight.seq <- seq( from=round( min( d$weight ) ) , to=round( max( d$weight ) ) , by=1 )
pred_data <- data.frame( weight=weight.seq )
mu <- link( model, data=pred_data )
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI , prob=0.97 )
sim.height <- sim( model , data=pred_data )
height.PI <- apply( sim.height , 2 , PI , prob=0.97 )

plot( height ~ weight, d, col=col.alpha(rangi2, 0.5) )
lines( weight.seq, mu.mean )
shade( mu.PI , weight.seq )
shade( height.PI , weight.seq ) 


# 4H4. Plot the prior predictive distribution for the polynomial regression
# model in the chapter. You can modify the code that plots the linear
# regression prior predictive distribution. Can you modify the prior
# distributions of α, β1, and β2 so that the prior predictions stay within
# the biologically reasonable outcome space? That is to say: Do not try to
# fit the data by hand. But do try to keep the curves consistent with what
# you know about height and weight, before seeing these exact data.

N <- 2e2
a <- rnorm( N, 178, 20 )
b_1 <- rlnorm( N, 0, 1 )
b_2 <- rnorm( N, 0, 10 )
b_3 <- rnorm( N, 0, 10 )

plot(
  NULL ,
  xlim=c( -2.5, 2.5 ) ,
  ylim=c( -100, 400 ) ,
  xlab="normalized weight" ,
  ylab="height" )

abline( h=0 , lty=2 )
abline( h=272 , lty=1 , lwd=0.5 )

for ( i in 1:N ) {
  curve(
    a[i] + b_1[i]*x + b_2[i]*x^2 + b_3[i]*x^3 ,
    from=-2.5 ,
    to=2.5 ,
    add=TRUE ,
    col=col.alpha( "black", 0.15 )
  )
}

# The polynomial curves are a little bit ridiculous as they tend to ±∞
# for values in the tail of the weight distribution. With the polynomial
# model we will inherently have this issue... We can try to somewhat mitigate
# it by lowering the prior β magnitudes so we have more "run" before hitting
# the tails:

a <- rnorm( N, 178, 20 )
b_1 <- rlnorm( N, 0, 1 )
b_2 <- rnorm( N, 0, 1 )
b_3 <- rnorm( N, 0, 1 )

plot(
  NULL ,
  xlim=c( -2.5, 2.5 ) ,
  ylim=c( -100, 400 ) ,
  xlab="normalized weight" ,
  ylab="height" )

abline( h=0 , lty=2 )
abline( h=272 , lty=1 , lwd=0.5 )

for ( i in 1:N ) {
  curve(
    a[i] + b_1[i]*x + b_2[i]*x^2 + b_3[i]*x^3 ,
    from=-2.5 ,
    to=2.5 ,
    add=TRUE ,
    col=col.alpha( "black", 0.15 )
  )
}

# Dunno if the plots make a compelling case for using polynomial here, though.

