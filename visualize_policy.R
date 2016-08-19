library("appl")
library("ggplot2")
library("tidyr")
library("dplyr")

meta_from_log(data.frame(model = "ricker", K = 40))

## Import a model solution from the library
meta <- meta_from_log(data.frame(model = "ricker", r = .1, K = 40))[1,]
meta
alpha <- alphas_from_log(meta)[[1]]
f <- f_from_log(meta)[[1]]
m <- models_from_log(meta)[[1]]



S_star <- round( optimize(function(x) -f(x,0) + x / discount, c(min(states),max(states)) )$minimum)
det_policy <- function(x) if(x <= S_star) 1 else x - S_star # adjusted for index values, starting at 1

## if we believe the prior value was certainly s, we are then more conservative with anything above f(S), and less conservative with anything below f(S):


s <- S_star
a0 <- 0 # action we took 
certain_prior <- numeric(length(m$observation[,1,1]))
certain_prior[s+1] <- 1

df <- compute_policy(alpha, m$transition, m$observation, m$reward, certain_prior, a0+1) # action as index



df %>% rowwise() %>% mutate(det = det_policy(state)) %>%
ggplot(aes(state, state - policy)) + geom_line() + geom_point() + geom_line(aes(state, state - det)) + geom_vline(xintercept = f(s,a0))




set.seed(1234)
sim <- sim_pomdp(m$transition, m$observation, m$reward, discount, x0 = 5, Tmax = 20, alpha = alpha)

sim$df %>% select(-value) %>% gather(variable, state, -time) %>%
ggplot(aes(time, state, color = variable)) + geom_line() + geom_point() 

Tmax <- length(sim$df$time)
data.frame(time = 0:Tmax, sim$state_posterior) %>%
  gather(state, belief, -time, factor_key = TRUE) %>%
  mutate(state = as.numeric(state)) %>%
  ggplot(aes(state, belief, group = time, alpha = time)) + geom_line() 

