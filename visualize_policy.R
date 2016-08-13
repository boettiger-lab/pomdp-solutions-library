library("appl")
library("ggplot2")
library("tidyr")
library("dplyr")

df <- compute_policy(alpha, m$transition, m$observation, m$reward)
ggplot(df, aes(state, state - policy)) + geom_line() + geom_point()

sim <- sim_pomdp(m$transition, m$observation, m$reward, discount, x0 = 5, Tmax = 20, alpha = alpha)

sim$df %>% select(-value) %>% gather(variable, state, -time) %>%
ggplot(aes(time, state, color = variable)) + geom_line() + geom_point() 

Tmax <- length(sim$df$time)
data.frame(time = 0:Tmax, sim$state_posterior) %>%
  gather(state, belief, -time, factor_key = TRUE) %>%
  mutate(state = as.numeric(state)) %>%
  ggplot(aes(state, belief, group = time, alpha = time)) + geom_line() 

