library(dplyr)
library(pgirmess)
library(broom)

## Set Hyperparameters
set.seed(712)
nsim <- 10000


bad_results1 <- data.frame(p_t = rep(NA, times = nsim),
                           p_perm = rep(NA, times = nsim),
                           slope = rep(NA, times = nsim))


## Simulations with Bad Data
## Situation 1: Poisson Regression on Linear Data with Binomial Noise
for(i in 1:nsim){
  x <- sample(1:100, 20, replace = TRUE)
  y <- 2*x + 1 + rbinom(20, 5, 0.5)
  mod <- glm(y ~ x, family = "poisson")
  tidy_mod <- data.frame(mod |> tidy())
  bad_results1$p_t[i] <- tidy_mod[2,5]
  bad_results1$p_perm[i] <- PermTest(mod)$resultats$p.value
  bad_results1$slope[i] <- tidy_mod[2,2]
  if(i %% 100 == 0){ print(paste("finished round", i))}
}

write.csv(bad_results1, "bad_results1.csv")

bad_results2 <- data.frame(p_t = rep(NA, times = nsim),
                           p_perm = rep(NA, times = nsim),
                           slope = rep(NA, times = nsim))


## Simulations with Bad Data
## Situation 2: Heteroskedasticity in Linear Regression
for(i in 1:nsim){
  x <- sample(1:100, 20, replace = TRUE)
  y <- rep(NA, times = 20)
  for(j in 1:20){
    y[j] <- ifelse(x[j] + rbinom(1, 1, 0.5) %% 2 == 0, #random condition
                   2*x[j] + 1 + rnorm(1, mean = 0, sd = 1), #draw from N(0,1)
                   2*x[j] + 1 + rnorm(1, mean = 0, sd = 5)) #draw from N(0,5)
  }
  mod <- lm(y ~ x)
  tidy_mod <- data.frame(mod |> tidy())
  bad_results2$p_t[i] <- tidy_mod[2,5]
  bad_results2$p_perm[i] <- PermTest(mod)$resultats$p.value
  bad_results2$slope[i] <- tidy_mod[2,2]
  if(i %% 100 == 0){ print(paste("finished round", i))}
}

write.csv(bad_results2, "bad_results2.csv")
