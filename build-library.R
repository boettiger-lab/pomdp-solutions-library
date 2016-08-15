library("appl")

states <- 0:50
actions <- states
obs <- states
reward_fn <- function(x,h) pmin(x,h)
discount <- 0.95
precision <- .1

sigma_g <- sqrt(log(1 + 0.5 / 6)) # Scale the log-standard-deviation to result in similar variance to a uniform distribution of width 0.5
sigma_m <- sigma_g

models = rbind(
  data.frame(r = seq(0.1, 1.6, length = 4), K = 40),
  data.frame(r = 0.6, K = seq(10, 40, length = 4))
)

for(i in 1:dim(models)[1]) {
  f <- function(x, h, r = models[i,1], K = models[i,2]){
    s <- pmax(x - h, 0)
    s * exp(r * (1 - s / K) )
  }
  m <- fisheries_matrices(states, actions, obs, reward_fn, f, sigma_g, sigma_m)
  log_data <- data.frame(model = "ricker", r = models[i,1], K = models[i,2], C = NA, sigma_g = sigma_g, sigma_m = sigma_m)
  alpha <- sarsop(m$transition, m$observation, m$reward, discount, precision = precision, memory = 15000,
                  log_dir = ".", log_data = log_data)

}