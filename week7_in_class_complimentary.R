# Question 3.1
water_data <- tibble(
  pctHisp = seq(0, 0.9, by = 0.15),
  healthViolation = c(1, 0, 0, 1, 1, 0 , 1)
)

# Want to draw best fit line through this plot
ggplot(water_data, aes(pctHisp, healthViolation)) +
  geom_point()

# Question 3.2 
logit <- function(p) log(p / (1 - p))
inv_logit <- function(x) exp(x) / (1 + exp(x))

water_data %>% 
  mutate(logit_p = 0 + 1 * pctHisp,
         p = inv_logit(logit_p)) %>% 
  ggplot(aes(pctHisp, healthViolation)) +
  geom_point() +
  geom_line(aes(y = p), color = "red")

# Question 3.3
# Beta0 = -2, beta1 = 0.5

water_data %>% 
  mutate(logit_p = -2 + 0.5 * pctHisp,
         p = inv_logit(logit_p)) %>% 
  ggplot(aes(pctHisp, healthViolation)) +
  geom_point() +
  geom_line(aes(y = p), color = "red")


