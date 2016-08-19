library("appl")

states <- 0:50
actions <- states
obs <- states


models = expand.grid(C = seq(0, 15, 5), sigma_m = c(0.02, 0.05))
models <- data.frame(model = "allen", r = 1, K = 40, sigma_g = 0.02, discount = 0.99, precision = 0.01, models)

memory <- round(0.95 * as.numeric(gsub(".* (\\d+) .*", "\\1", system("cat /proc/meminfo", intern=TRUE)[1])) / 1000)


reward_fn <- function(x,h) pmin(x,h)

for(i in 1:dim(models)[1]) {
  
  f <- switch(models[i, "model"], 
    allen = appl:::allen(models[i, "r"], models[i, "K"], models[i, "C"]),
    ricker = appl:::allen(models[i, "r"], models[i, "K"])
  )
    

    
  m <- fisheries_matrices(states, actions, obs, reward_fn, f = f, sigma_g = models[i, "sigma_g"], sigma_m  = models[i, "sigma_m"])
  log_data <- data.frame(model = models[i, "model"], r = models[i, "r"], K  = models[i, "K"], C = models[i, "C"], sigma_g = models[i, "sigma_g"], sigma_m = models[i, "sigma_m"])
  
  
  alpha <- sarsop(m$transition, m$observation, m$reward, discount = models[i, "discount"], precision = models[i, "precision"], memory = 15000,
                  log_dir = ".", log_data = log_data)

}