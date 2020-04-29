library(ggplot2)

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


# calculate the means
iris.mean <- aggregate(x = iris, by = list(iris$Species), FUN = mean)
# create a simple barplot
barplot(iris.mean$Sepal.Length)


beaver.mean <- aggregate(x = beaver1, by = list(beaver1$day), FUN = mean)
beaver_2.mean <- aggregate(x = beaver2, by = list(beaver2$day), FUN = mean)

tree <- rbind(beaver.mean, beaver_2.mean)
names(tree) <- c("Population", "del", "Growth", "Temperature", "Activity")
tree$del <- NULL
barplot(tree$Population)



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
