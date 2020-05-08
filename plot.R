library(ggthemes)
library(data.table)
library(ggplot2)
library(tidyverse)

set.seed(1) # sets a seed of reproducible results, produces the same sample again and again
df_populations <- data.frame(
  Seedlot = c(
    rep("A", 50),
    rep("B",100),
    rep("C",160),
    rep("D",100),
    rep("E",50)
  ),
  value = c(
    rnorm(50, 700, 200), # rnorm has n, number of observations, mean, and standard deviation.
    # standard deviation is a measure of how spread out numbers are
    # is the square root of the variance
    # the variance is the average of the squared differences from the mean
    # https://www.mathsisfun.com/data/standard-deviation.html
    rnorm(100,1100,200),
    rnorm(160,1500,185),
    rnorm(100,1950,180),
    rnorm(50,2300,190)),
  y = NA)

# Set desired binwidth and number of non-missing observations
bw = 80
n_obs = sum(!is.na(df_populations$value))

overall_data <- seq(0, 3000, 0.1)
overall_mean <- 1500
overall_sdev <- 450
overall_y <- dnorm(overall_data, overall_mean, overall_sdev)

df_overall <- data.frame(
  Seedlot = rep("Overall", length(overall_data)),
  value = overall_data,
  y = overall_y * length(overall_data)
)

df_populations <- df_populations %>%
  group_by(Seedlot) %>%
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
  mutate(Seedlot = as.factor(Seedlot))

df_all <- rbind(df_populations, df_overall)

ggplot(df_all, aes(x = value, y = y, colour = Seedlot)) +
  geom_line() +
  xlab("Growing Degree Days") +
  ylab("Growth Rate (mm)") +
  ggtitle("Growth Rate of Species and Select Populations \n by Growing Degree Days") +
  theme_few()
