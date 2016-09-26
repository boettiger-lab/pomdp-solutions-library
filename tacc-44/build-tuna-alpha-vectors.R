library("appl")
library("pomdpplus")

mc.cores = 4

### SETUP ###
log_dir = "."
p <- 2
states <- seq(0, 1.2^(1/p), len=15)^p # Vector of all possible states
actions <- states  # Vector of actions: harvest
obs <- states

K = 0.9903371
r = 0.05699246
sigma_g = 0.01720091


vars <- expand.grid(r = rev(seq(0.025, 0.2, by =0.025)), sigma_m = c(0.2, 0.4, 0.6))

## Detect available memory (linux servers only)
memory <- round(0.95 * as.numeric(gsub(".* (\\d+) .*", "\\1", system("cat /proc/meminfo", intern=TRUE)[1])))
## Bind this to a data.frame listing each of the fixed parameters across all runs
fixed <- data.frame( K = K, C = NA, sigma_g = sigma_g, discount = 0.99, model = "ricker", 
                     precision = 0.0000001, memory = memory / 12, timeout = 20000, timeInterval = 100,
                     max_state = max(states), max_obs = max(obs), max_action = max(actions), 
                     min_state = min(states), min_obs = min(obs), min_action = min(actions),
                     states_code = "seq(0, 1.2^(1/p), len=100)^p")
pars <- data.frame(vars, fixed)

## Usual assumption at the moment for reward fn
reward_fn <- function(x,h) pmin(x,h)

### RUN ### 

## Compute alphas for the above examples
models <- lapply(1:dim(pars)[1], function(i){
  
  ## Select the model
  f <- switch(pars[i, "model"], 
              allen = appl:::allen(pars[i, "r"], pars[i, "K"], pars[i, "C"]),
              ricker = appl:::ricker(pars[i, "r"], pars[i, "K"])
  )
  ## Compute matrices
  fisheries_matrices(states, actions, obs, 
                     reward_fn, f = f, 
                     sigma_g = pars[i, "sigma_g"], 
                     sigma_m  = pars[i, "sigma_m"])
})
## run sarsop
alphas <- sarsop_plus(models, 
                      discount = pars[1, "discount"], 
                      precision = pars[1, "precision"], 
                      #memory = pars[1, "memory"],
                      #timeout = pars[1, "timeout"],
                      timeout = 1,
                      timeInterval = pars[1, "timeInterval"],
                      log_dir = log_dir, 
                      log_data = pars,
                      mc.cores = mc.cores)


