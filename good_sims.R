
## Load Packages
install.packages("pgirmess")
library(dplyr)
library(pgirmess)
library(broom)

## Set Hyperparameters
set.seed(712)
nsim <- 10000


good_results <- data.frame(p_t = rep(NA, times = nsim),
                           p_perm = rep(NA, times = nsim),
                           slope = rep(NA, times = nsim))


## Simulations with Good Data
for(i in 1:nsim){
  x_good <- runif(100, min = 1, max = 10)
  y <- 2*x_good + 1 + rnorm(n = 100,mean = 0,sd = 1)
  mod <- lm(y ~ x_good)
  tidy_mod <- data.frame(mod |> tidy())
  good_results$p_t[i] <- tidy_mod[2,5]
  good_results$p_perm[i] <- PermTest(mod)$resultats$p.value
  good_results$slope[i] <- mod_t[2,2]
  if(i %% 100 == 0){ print(paste("finished round", i))}
}

write.csv(good_results, "good_results.csv")

