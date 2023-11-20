
## Load Packages
library(tidyverse)
library(latex2exp)
library(lmPerm)
library(pgirmess)
library(broom)

## Basic Plot
#good_results |>
#  ggplot(aes(x = x, y = y)) +
#  geom_point() +
#  theme_bw() +
#  geom_abline(slope = 2, intercept = 7, 
#              color = "magenta") +
#  geom_smooth(method = "lm", 
#              color = "blue") +
#  annotate("text", label = "Truth: y = 2x + 7", 
#           color = "magenta",
#           x = 3, y = 50) +
#  annotate("text", label = "Estimate: y = 2.002x + 7.181", 
#           color = "blue",
#           x = 3, y = 45) +
#  labs(title = "Basic OLS Regression")

## Keep Simulating
#slopes <- rep(NA, times = 1000)
#intercepts <- rep(NA, times = 1000)
#for(i in 1:1000){
#  x <- rnorm(n = 100, mean = 12, sd = 7)
#  y <- 2*x + 7 + rnorm(n = 100,mean = 0,sd = 1)
#  df <- data.frame(x,y)
#  lm <- lm(data = df, y ~ x)
#  slopes[i] <- lm$coefficients[2]
#  intercepts[i] <- lm$coefficients[1]
#}
#sim2 <- data.frame(slopes, intercepts)

## Plot Larger Sim
#sim2 |>
#  ggplot(aes(x = slopes)) +
#  geom_histogram(fill = "blue", binwidth = 0.01) +
#  theme_bw() +
#  theme(panel.grid = element_line(color = "white")) +
#  labs(title = "Empirical Slope Distribution",
#       subtitle = TeX(r'(Truth: $\beta_1 = 2$)'),
#       x = TeX(r'($\hat{\beta_1}$)')) +
#  annotate("text", label = "Original CI: [1.97, 2.03]",
#           color = "red", fontface = "bold", size = 6,
#           x = 2, y = 15) +
#  geom_vline(xintercept = 1.97, color = "red", linewidth = 1.2) +
#  geom_vline(xintercept = 2.03, color = "red", linewidth = 1.2)

#sim2 |>
#  ggplot(aes(x = intercepts)) +
#  geom_histogram(fill = "blue", binwidth = 0.1) +
#  theme_bw() +
#  theme(panel.grid = element_line(color = "white")) +
#  labs(title = "Empirical Intercept Distribution",
#       subtitle = TeX(r'(Truth: $\beta_0 = 7$)'),
#       x = TeX(r'($\hat{\beta_0}$)')) +
#  annotate("text", label = "Original CI: [6.83, 7.53]",
#           color = "red", fontface = "bold", size = 5,
#           x = 7.13, y = 10) +
#  geom_vline(xintercept = 6.83, color = "red", linewidth = 1.2) +
#  geom_vline(xintercept = 7.53, color = "red", linewidth = 1.2)

## Break Independence of the Xs
#x_dep <- rep(NA, times = 100)
#for(i in 1:100){
#  x_dep[i] <- 2*i + rnorm(1,0,1)
#}
#sim3 <- data.frame(x = x_dep,
                  # y = 2*x_dep + 7 + rnorm(100,0,1))
#check if this works and go from here

#to do: break either dependence, missing data, or sample size
#box plot sector and y
#check the friedman paper that romano cites


# #Oct 20: 
# set.seed(712)
# nsim <- 10000
# 
# #Sim Good Data
# beta1 <- rep(NA, times = nsim)
# pval_t <- rep(NA, times = nsim)
# pval_perm <- rep(NA, times = nsim)
# results_good <- data.frame(beta1, pval_t, pval_perm)
# 
# #run loop
# for(i in 1:nsim) {
#   x1 <- runif(100, min = 1, max = 10)
#   y1 <- 2*x1 + 1 + rnorm(100, mean = 0, sd = 1)
#   
#   lm1 <- lm(y1 ~ x1)
#   tidy_lm1 <- lm1 |> tidy()
#   results_good$beta1[i] <- tidy_lm1[2,2]
#   results_good$pval_t[i] <- tidy_lm1[2,5]
#   lmp1 <- lmp(y1 ~ x1)
#   tidy_lmp1 <- lmp1 |> summary()
#   results_good$pval_perm[i] <- tidy_lmp1$coefficients[2,3]
# }
# 
# results_good |> head()
# 
# #Sim Bad Data
# beta1 <- rep(NA, times = nsim)
# pval_t <- rep(NA, times = nsim)
# pval_perm <- rep(NA, times = nsim)
# results_bad <- data.frame(beta1, pval_t, pval_perm)
# 
# #run loop
# for(i in 1:nsim) {
#   x2 <- runif(25, min = 1, max = 10)
#   y2 <- 2*x2 + 1 + rchisq(25, df = 3)
#   
#   lm2 <- lm(y2 ~ x2)
#   tidy_lm2 <- lm2 |> tidy() |> as.data.frame()
#   results_bad$beta1[i] <- tidy_lm2[2,2]
#   results_bad$pval_t[i] <- tidy_lm2[2,5]
#   lmp2 <- lmp(y2 ~ x2)
#   tidy_lmp2 <- lmp2 |> summary()
#   results_bad$pval_perm[i] <- tidy_lmp2$coefficients[2,3]
# }
# 
# results_bad |>
#   ggplot(aes(x = pval_t)) +
#   geom_histogram(b)
# 
# sort(results_bad$pval_t)
# 
# tidy_lm2

#Week 12
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
  mod <- lm(y ~ x)
  tidy_mod <- data.frame(mod |> tidy())
  good_results$p_t[i] <- tidy_mod[2,5]
  good_results$p_perm[i] <- PermTest(mod)$resultats$p.value
  good_results$slope[i] <- mod_t[2,2]
  if(i %% 100 == 0){ print(paste("finished round", i))}
}

write.csv(good_results, "good_results.csv")

