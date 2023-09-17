
## Load Packages
library(tidyverse)
library(latex2exp)
library(broom)

## Set Seed
set.seed(712)

## Simulate Data
x <- rnorm(n = 100, mean = 12, sd = 7)
y <- 2*x + 7 + rnorm(n = 100,mean = 0,sd = 1)
sim1 <- data.frame(x,y)
lm1 <- lm(y ~ x, data = sim1)

## Basic Plot
sim1 |>
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  theme_bw() +
  geom_abline(slope = 2, intercept = 7, 
              color = "magenta") +
  geom_smooth(method = "lm", 
              color = "blue") +
  annotate("text", label = "Truth: y = 2x + 7", 
           color = "magenta",
           x = 3, y = 50) +
  annotate("text", label = "Estimate: y = 2.002x + 7.181", 
           color = "blue",
           x = 3, y = 45) +
  labs(title = "Basic OLS Regression")

## Keep Simulating
slopes <- rep(NA, times = 1000)
intercepts <- rep(NA, times = 1000)
for(i in 1:1000){
  x <- rnorm(n = 100, mean = 12, sd = 7)
  y <- 2*x + 7 + rnorm(n = 100,mean = 0,sd = 1)
  df <- data.frame(x,y)
  lm <- lm(data = df, y ~ x)
  slopes[i] <- lm$coefficients[2]
  intercepts[i] <- lm$coefficients[1]
}
sim2 <- data.frame(slopes, intercepts)

## Plot Larger Sim
sim2 |>
  ggplot(aes(x = slopes)) +
  geom_histogram(fill = "blue", binwidth = 0.01) +
  theme_bw() +
  theme(panel.grid = element_line(color = "white")) +
  labs(title = "Empirical Slope Distribution",
       subtitle = TeX(r'(Truth: $\beta_1 = 2$)'),
       x = TeX(r'($\hat{\beta_1}$)')) +
  annotate("text", label = "Original CI: [1.97, 2.03]",
           color = "red", fontface = "bold", size = 6,
           x = 2, y = 15) +
  geom_vline(xintercept = 1.97, color = "red", linewidth = 1.2) +
  geom_vline(xintercept = 2.03, color = "red", linewidth = 1.2)

sim2 |>
  ggplot(aes(x = intercepts)) +
  geom_histogram(fill = "blue", binwidth = 0.1) +
  theme_bw() +
  theme(panel.grid = element_line(color = "white")) +
  labs(title = "Empirical Intercept Distribution",
       subtitle = TeX(r'(Truth: $\beta_0 = 7$)'),
       x = TeX(r'($\hat{\beta_0}$)')) +
  annotate("text", label = "Original CI: [6.83, 7.53]",
           color = "red", fontface = "bold", size = 5,
           x = 7.13, y = 10) +
  geom_vline(xintercept = 6.83, color = "red", linewidth = 1.2) +
  geom_vline(xintercept = 7.53, color = "red", linewidth = 1.2)

## Break Independence of the Xs
x_dep <- rep(NA, times = 100)
for(i in 1:100){
  x_dep[i] <- 2*i + rnorm(1,0,1)
}
sim3 <- data.frame(x = x_dep,
                   y = 2*x_dep + 7 + rnorm(100,0,1))
#check if this works and go from here
  
