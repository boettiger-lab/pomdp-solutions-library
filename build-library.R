library("appl")

states <- 0:60
actions <- states
obs <- states
reward_fn <- function(x,h) pmin(x,h)
discount <- 0.99
precision <- 1

sigma_g <- sqrt(log(1 + 0.1 / 6)) # Scale the log-standard-deviation to result in similar variance to a uniform distribution of width 0.5
sigma_m <- sigma_g

models = data.frame(C = seq(0,20, 5))

for(i in 1:dim(models)[1]) {
  f <- function(x, h, r = 0.5, K = 50, C = models[i, 1]){
    s <- pmax(x - h, 0)
    s * exp(r * (1 - s / K) * (s - C) / K)
  }
  m <- fisheries_matrices(states, actions, obs, reward_fn, f, sigma_g, sigma_m)
  log_data <- data.frame(model = "allen", r = 0.5, K = 40, C = models[i,1], sigma_g = sigma_g, sigma_m = sigma_m)
  alpha <- sarsop(m$transition, m$observation, m$reward, discount, precision = precision,
                  log_dir = ".", log_data = log_data)

}