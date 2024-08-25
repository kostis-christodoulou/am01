library(tidyverse)
library(patchwork)

my_alpha = 0.68

normal <- ggplot() +
  stat_function(fun = dnorm,
                geom = "area",
                fill = "steelblue",
                alpha = my_alpha,
                xlim = c(-4,4))+
  labs(title = "Standard Normal distribution", y = "dnorm(0,1)")

uniform <- ggplot() +
  stat_function(fun = dunif,
                geom = "area",
                fill = "steelblue",
                alpha = my_alpha)+
  labs(title = "Uniform distribution", y = "dunif(0,1)")

lognormal <- ggplot() +
  stat_function(fun = dlnorm,
                geom = "area",
                fill = "steelblue",
                alpha = my_alpha,
                xlim = c(0,4))+
  labs(title = "Log-Normal distribution", y = "dlnorm(0,1)")

exponential <- ggplot() +
  stat_function(fun = dexp,
                geom = "area",
                fill = "steelblue",
                alpha = my_alpha,
                xlim = c(0,4))+
  labs(title = "Exponential distribution", y = "dexp(rate = 1)")

binom1<- ggplot() +
  stat_function(fun = dbinom,
                args = list(p = 0.5, size = 50),
                geom = "col",
                fill = "steelblue",
                alpha = my_alpha,
                xlim = c(0,50))+
  labs(title = "Binomial, p = 0.5, size = 50",
       x= "",
       y = "")

binom1

binom2 <- ggplot() +
  stat_function(fun = dbinom,
                args = list(p = 0.1, size = 50),
                geom = "col",
                fill = "steelblue",
                alpha = my_alpha,
                xlim = c(0,50))+
  labs(title = "Binomial, p = 0.1, size = 50",
       x= "",
       y = "")

binom2

# skewness of the gamma distribution only depends on its shape parameter, k, and it is equal to 
# {\displaystyle 2/{\sqrt {k}}.}{\displaystyle 2/{\sqrt {k}}.}

gamma1 <- ggplot() +
  stat_function(fun = dgamma,
                args = list(shape=4, scale=2),
                geom = "area",
                fill = "steelblue",
                alpha = my_alpha,
                xlim = c(0,30))+
  labs(title = "Gamma, shape=4, scale=2",
       x= "",
       y = "")

gamma1

gamma2 <- ggplot() +
  stat_function(fun = dgamma,
                args = list(shape=1.5, scale=3),
                geom = "area",
                fill = "steelblue",
                alpha = my_alpha,
                xlim = c(0,30))+
  labs(title = "Gamma, shape=1.5, scale=3",
       x= "",
       y = "")

gamma2


(normal + uniform + binom1 + binom2) / ( gamma2 + gamma1 + exponential+lognormal)

