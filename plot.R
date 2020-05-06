library(ggthemes)
library(data.table)
library(ggplot2)
library(tidyverse)

a <- seq(0, 20, by = .1)
b <- seq(20, 40, by = .1)
c <- seq(40, 60, by = .1)
d <- seq(60, 80, by = .1)

overall_mean <- mean(c(a, b, c, d))
# pull this from real (fake for now) data
overall_sd <- sd(c(a, b, c, d))

p9_3 <- ggplot(data.frame(x = c(-10, 90)), aes(x = x)) +
  stat_function(fun = dnorm, args = list(mean(a), sd(a))) +
  stat_function(fun = dnorm, args = list(mean(b), sd(b))) +
  stat_function(fun = dnorm, args = list(mean(c), sd(c))) +
  stat_function(fun = dnorm, args = list(mean(d), sd(d))) +
  stat_function(fun = dnorm, args = list(overall_mean, overall_sd), colour = "red", linetype="dashed") +
  xlab("Temperature") +
  ylab("Growth Rate")
p9_3

# print ggplot2 data visualization cheat sheet
# stat_function is useful for overlaying functions
set.seed(1492)
ggplot(data.frame(x = rnorm(100)), aes(x)) +
  geom_density() +
  stat_function(fun = dnorm, colour = "red")




# dnorm - this function gives height of the probability distribution at each point for a given mean and standard deviation
x <- seq(-10, 10, by = .1)
y <- dnorm(x, mean = 2.5, sd = 0.5)
plot(x, y)

# pnorm - this function gives the probability of a normally distributed random number to be less than the value of a given number. It is also called "Cumulative Distribution Function"
x <- seq(-10, 10, by = 0.2)
y <- pnorm(x, mean = 2.5, sd = 2)
plot(x, y)


# qnorm - this function takes the probability value and gives a number whose cumulative value matches the probability value
x <- seq(0, 1, by = 0.02)
y <- qnorm(x, mean = 2, sd = 1)
plot(x, y)


# rnorm - this function is used to generate random numbers whose distribution is normal. It takes the sample size as input and generates that many random numbers. We draw a histogram to show the distribution of the generated numbers

# create a sample of 50 numbers which are normally distributed
y <- rnorm(50)

hist(y, main = "Normal Distribution")


x1 <- rnorm (3500, mean=65, sd=4.58)
hist(x1)


set.seed(1) # sets a seed of reproducible results. used to produce the same sample again and again
my_hist <- data.frame( # my_hist is a data frame with two columns, group as well as value. It has 700 rows
  group = c(rep("A", 300), rep("B",150), rep("C",150), rep("D",100), rep("E",100)),
  value = c(rnorm(300, 30, 10), # rnorm has n, number of observations, mean, and standard deviation. 200, 20, 5
            # standard deviation is a measure of how spread out numbers are
            # is the square root of the variance
            # the variance is the average of the squared differences from the mean
            # https://www.mathsisfun.com/data/standard-deviation.html
            rnorm(150,5,10),
            rnorm(150,15,10),
            rnorm(100,50,10),
            rnorm(100,75,10)))

# Set desired binwidth and number of non-missing obs
bw = 2
n_obs = sum(!is.na(my_hist$value))

x <- seq(-25,100,0.1)
mu <- 30
sdev <- 15
y <- dnorm(x, mu, sdev)

# overall is the name of the group
# xx is how many times that should be repeated
# xx is length(x)


data.frame(group = rep(overall,xx), value = x, y = y)
# make a data frame
# rbind that to the my_hist data frame

my_hist %>%
  group_by(group) %>%
  nest(value) %>% # nesting creates a list-column of data frames, unnesting flattens it back out into regular columns
  # map() apply a function to each element of a vector. map() always returns a list
  # tilde is used to separate the left- and right-hand sides in a model formula. Our formula here describes the dnorm being multiplied by the bin_width and the sum of all the non_missing observations
  # dnorm takes a vector (.$value in this case), a mean, and standard deviation
  mutate(y = map(data, ~ dnorm(
    .$value, mean = mean(.$value), sd = sd(.$value)
  ) * bw * sum(!is.na(.$value)))) %>%
  unnest(data,y) %>%
  ungroup() %>%
  as.data.table() %>%
  mutate(group = as.factor(group)) %>%
  ggplot(aes(x = value, y = y, group = group, colour = group)) +
  geom_line() +
  xlab("Growing Degree Days") +
  ylab("Growth Rate (mm)") +
  ggtitle("Growth Rate of Species and Select Populations by Growing Degree Days") +
  theme_few()

ggsave("growth_rate.png")