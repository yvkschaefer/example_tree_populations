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
            rnorm(100,75,10)),
  y = NA)

# Set desired binwidth and number of non-missing obs
bw = 2
n_obs = sum(!is.na(my_hist$value))

overall_data <- seq(-25, 100,0.1)
overall_mean <- 30
overall_sdev <- 15
overall_y <- dnorm(overall_data, overall_mean, overall_sdev)

# overall is the name of the group
# length(overall_data) is how many times that should be repeated

df_temp <- data.frame(group = rep("overall",length(overall_data)),
                      value = overall_data,
                      y = overall_y
                      * bw * length(overall_data)
                      )
# df_temp <- data.frame(group = rep("overall", length(overall_data)), value = overall_data)
# make a data frame
# rbind that to the my_hist data frame
my_hist <- rbind(my_hist, df_temp)

my_hist %>%
  group_by(group) %>%
  nest(value) %>% # nesting creates a list-column of data frames, unnesting flattens it back out into regular columns
  # map() apply a function to each element of a vector. map() always returns a list
  # tilde is used to separate the left- and right-hand sides in a model formula. Our formula here describes the dnorm being multiplied by the bin_width and the sum of all the non_missing observations
  # dnorm takes a vector (.$value in this case), a mean, and standard deviation
  mutate(
    y = ifelse(
      group == "overall",
      y,
      map(data, ~ dnorm(
        .$value, mean = mean(.$value), sd = sd(.$value)
      ) * bw * sum(!is.na(.$value)))
    )) %>%
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
