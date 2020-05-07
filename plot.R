library(ggthemes)
library(data.table)
library(ggplot2)
library(tidyverse)

set.seed(1) # sets a seed of reproducible results, produces the same sample again and again
df_populations <- data.frame(
  group = c(
    rep("A", 300),
    rep("B",150),
    rep("C",150),
    rep("D",100),
    rep("E",100)
  ),
  value = c(
    rnorm(300, 30, 10), # rnorm has n, number of observations, mean, and standard deviation. 200, 20, 5
    # standard deviation is a measure of how spread out numbers are
    # is the square root of the variance
    # the variance is the average of the squared differences from the mean
    # https://www.mathsisfun.com/data/standard-deviation.html
    rnorm(150,5,10),
    rnorm(150,15,10),
    rnorm(100,50,10),
    rnorm(100,75,10)),
  y = NA)

# Set desired binwidth and number of non-missing observations
bw = 2
n_obs = sum(!is.na(df_populations$value))

overall_data <- seq(-25, 100,0.1)
overall_mean <- 30
overall_sdev <- 15
overall_y <- dnorm(overall_data, overall_mean, overall_sdev)

df_overall <- data.frame(
  group = rep("Overall", length(overall_data)),
  value = overall_data,
  y = overall_y * n_obs
)

df_populations <- df_populations %>%
  group_by(group) %>%
  nest(value) %>% # nesting creates a list-column of data frames, unnesting flattens it back out into regular columns
  # map() apply a function to each element of a vector. map() always returns a list
  # tilde is used to separate the left- and right-hand sides in a model formula. Our formula here describes the dnorm being multiplied by the bin_width and the sum of all the non_missing observations
  # dnorm takes a vector (.$value in this case), a mean, and standard deviation
  mutate(
    y = map(data, ~ dnorm(
      .$value, mean = mean(.$value), sd = sd(.$value)
    ) * bw * sum(!is.na(.$value)))
  ) %>%
  unnest(data,y) %>%
  ungroup() %>%
  as.data.table() %>%
  mutate(group = as.factor(group))

df_all <- rbind(df_populations, df_overall)

ggplot(df_all, aes(x = value, y = y, colour = group)) +
  geom_line() +
  xlab("Growing Degree Days") +
  ylab("Growth Rate (mm)") +
  ggtitle("Growth Rate of Species and Select Populations by Growing Degree Days") +
  theme_few()

ggsave("growth_rate.png")
