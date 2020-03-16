library(ggplot2)


population_a_mean <- 10
population_a_sd <- 3.3
population_b_mean <- 12
population_b_sd <- 3
population_c_mean <- 15
population_c_sd <- 2.5
population_d_mean <- 20
population_d_sd <- 3.3

overall_mean <- mean(population_a_mean,
                     population_b_mean,
                     population_c_mean,
                     population_d_mean)
# pull this from real (fake for now) data
overall_sd <- mean(population_a_sd,
                   population_b_sd,
                   population_c_sd,
                   population_d_sd)

p9_3 <- ggplot(data.frame(x = c(0, 25)), aes(x = x)) +
  stat_function(fun = dnorm, args = list(population_a_mean, population_a_sd)) +
  stat_function(fun = dnorm, args = list(population_b_mean, population_b_sd)) +
  stat_function(fun = dnorm, args = list(population_c_mean, population_c_sd)) +
  stat_function(fun = dnorm, args = list(population_d_mean, population_d_sd)) + 
  stat_function(fun = dnorm, args = list(overall_mean, overall_sd), colour = "red", linetype="dashed") + 
  xlab("Temperature") +
  ylab("Growth")
p9_3

# print ggplot2 data visualization cheat sheet
# stat_function is useful for overlaying functions
set.seed(1492)
ggplot(data.frame(x = rnorm(100)), aes(x)) +
  geom_density() +
  stat_function(fun = dnorm, colour = "red")


ggplot(diamonds, aes(carat)) +
  geom_density()

