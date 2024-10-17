library(tidyverse)

# x is precip

# y is tree height

precip_in <- runif(1e4, min = 10, max = 100)

hist(precip_in)

beta0 <- 8 
beta1 <- 0.75
sigma <- 1 # standard deviation of uncertainity

u <- rnorm(1e4, mean = 0, sd = sigma)

tree_height <- beta0 + beta1 * precip_in + u

plot(precip_in, tree_height)

tree_pop <- tibble(precip_in, tree_height)
ggplot(tree_pop, aes(precip_in, tree_height)) +
  geom_point(alpha = 0.1) +
  geom_abline(slope = beta1, 
              intercept = beta0,
              color = "firebrick",
              linewidth = 2)

pop_lm <- lm(tree_height ~ precip_in, tree_pop)
summary(pop_lm)

tree_sample <- sample_n(tree_pop, 10)

ggplot(tree_pop, aes(precip_in, tree_height)) +
  geom_point(alpha = 0.05) +
  geom_abline(slope = beta1,
              intercept = beta0,
              color = "firebrick",
              linewidth = 2) +
  geom_point(data = tree_sample,
             color = "cornflowerblue")

sample_lm <- lm(tree_height ~ precip_in, tree_sample)
summary(sample_lm)

ggplot(tree_pop, aes(precip_in, tree_height)) +
  geom_point(alpha = 0.05) +
  geom_abline(slope = beta1,
              intercept = beta0,
              color = "firebrick",
              linewidth = 2) +
  geom_point(data = tree_sample,
             color = "cornflowerblue") +
  geom_abline(slope = 0.76573,
              intercept = 7.69342,
              color = "cornflowerblue",
              linewidth = 1.5)

summary(sample_lm)$coef["precip_in", "Estimate"]
