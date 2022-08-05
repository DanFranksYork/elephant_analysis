library(tidyverse)
library(cmdstanr)

setwd("/Volumes/GoogleDrive/My Drive/code/Elephant_Alert_dyadic_regression")
# Read elephant observation and attribute data
dyads <- read_csv("sim_elephant_dyads.csv")

### create data list for use in STAN -- can contain no NA values in any column, even if column is not specified in model
dyads_ls <- list(
  n_dyads  = nrow(dyads),         # total number of times one or other of the dyad was observed
  together = dyads$event_count, # count number of sightings seen together 
  apart    = dyads$apart,       # count number of sightings seen apart 
  age_diff_mean_centred = dyads$age_diff - mean(dyads$age_diff), # Mean centering
  node_1 = dyads$node_1,
  node_2 = dyads$node_2)      # age difference between individuals

# Prior predictive check (no random effects)
N <- 100 # 200 sample regression lines
alpha <- rnorm(N,-1.5,2)
beta <- rnorm(N,0,0.5) # 0.5
plot( NULL , xlim=range(1,6) , ylim=c(0,1) ,
      xlab="Age difference" , ylab="Predicted weight" ) + abline( h=0 , lty=2 )
# Generate example regression lines and plot to see if there is a good mixture of possible trends
for ( i in 1:N ) {
  curve( plogis(alpha[i] + beta[i]*(x-mean(x))), # Mean centering x
         from=0, to=6,
         col="steelblue", add=TRUE)
}

# Uniform priors (to show their insanity)
alpha <- runif(N,-10000,10000)
beta <- runif(N,-10000,10000)
plot( NULL , xlim=range(1,6) , ylim=c(0,1) ,
      xlab="Age difference" , ylab="Predicted weight" ) + abline( h=0 , lty=2 )
# Generate example regression lines and plot to see if there is a good mixture of possible trends
for ( i in 1:N ) {
  curve( plogis(alpha[i] + beta[i]*(x-mean(x))), # Mean centering x
         from=0, to=6,
         col="steelblue", add=TRUE)
}

# Read Stan model
dyadic_regression_model <- cmdstan_model("elephant_dyadic_regression.stan")

### Fit model with cmndstanr
dyadic_regression_fit <- dyadic_regression_model$sample(
  data = dyads_ls, 
  chains = 4, 
  parallel_chains = 4
)

library(bayesplot)
# Parameters on logit scale
hist(dyadic_regression_fit$draws("alpha"))
hist(dyadic_regression_fit$draws("beta"))

