library("appl")

### SETUP ###

log_dir = "."

## choose the state/action/obs space
states <- seq(0,1.2, length=50) # Vector of all possible states
actions <- seq(0,.8, length=50)   # Vector of actions: harvest
obs <- states

K = 0.9903371
r = 0.05699246
sigma_g = 0.01720091

vars <- expand.grid(r = seq(0.05, 0.3, by =0.05), sigma_m = c(0.1, 0.3, 0.6))

## Bind this to a data.frame listing eahc of the fixed parameters across all runs
fixed <- data.frame(model = "ricker", sigma_g = sigma_g, discount = 0.99, precision = 0.0000001, K = K, C = NA, 
                    max_state = max(states), max_obs = max(obs), max_action = max(actions), min_state = min(states), min_obs = min(obs), min_action = min(actions))
models <- data.frame(vars, fixed)

## Preview the model list
models

## Usual assumption at the moment for reward fn
reward_fn <- function(x,h) pmin(x,h)

### RUN ### 

## Detect available memory (linux servers only)
memory <- round(0.95 * as.numeric(gsub(".* (\\d+) .*", "\\1", system("cat /proc/meminfo", intern=TRUE)[1])) / 1000)


## Compute alphas for the above examples
for(i in 1:dim(models)[1]) {

## Select the model
  f <- switch(models[i, "model"], 
    allen = appl:::allen(models[i, "r"], models[i, "K"], models[i, "C"]),
    ricker = appl:::ricker(models[i, "r"], models[i, "K"])
  )

## Determine the matrices
  m <- fisheries_matrices(states, actions, obs, reward_fn, f = f, 
                          sigma_g = models[i, "sigma_g"], sigma_m  = models[i, "sigma_m"])

## record data for the log
  log_data <- data.frame(model = models[i, "model"], r = models[i, "r"], K  = models[i, "K"], 
                         C = models[i, "C"], sigma_g = models[i, "sigma_g"], sigma_m = models[i, "sigma_m"],
                         memory = memory)
  
## run sarsop
  alpha <- sarsop(m$transition, m$observation, m$reward, 
                    discount = models[i, "discount"], 
                    precision = models[i, "precision"], memory = memory,
                    log_dir = log_dir, log_data = log_data)

}
