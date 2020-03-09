library(ggplot2)


population_a_mean <- 0.2
population_a_sd <- 0.1
population_b_mean <- 0.75
population_b_sd <- 0.1
population_c_mean <- 1.25
population_c_sd <- 0.1
population_d_mean <- 1.75
population_d_sd <- 0.1

overall_mean <- mean(population_a_mean,
                     population_b_mean,
                     population_c_mean,
                     population_d_mean)
overall_sd <- sum(population_a_sd,
                  population_b_sd,
                  population_c_sd,
                  population_d_sd)

p9_3 <- ggplot(data.frame(x = c(0, 2)), aes(x = x)) +
  stat_function(fun = dnorm, args = list(population_a_mean, population_a_sd)) +
  stat_function(fun = dnorm, args = list(population_b_mean, population_b_sd)) +
  stat_function(fun = dnorm, args = list(population_c_mean, population_c_sd)) +
  stat_function(fun = dnorm, args = list(population_d_mean, population_d_sd)) + 
  stat_function(fun = dnorm, args = list(overall_mean, overall_sd))
p9_3
