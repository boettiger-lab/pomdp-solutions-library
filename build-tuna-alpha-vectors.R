library("appl")

### SETUP ###

log_dir = "."

## choose the state/action/obs space
states <- seq(0,1.2, length=200) # Vector of all possible states
actions <- states  # Vector of actions: harvest
obs <- states

K = 0.9903371
r = 0.05699246
sigma_g = 0.01720091

## Detect available memory (linux servers only)
memory <- round(0.95 * as.numeric(gsub(".* (\\d+) .*", "\\1", system("cat /proc/meminfo", intern=TRUE)[1])) / 1000)

vars <- expand.grid(r = rev(seq(0.025, 0.2, by =0.025)), sigma_m = c(0.6, 0.3, 0.1))

## Bind this to a data.frame listing eahc of the fixed parameters across all runs
fixed <- data.frame(model = "ricker", sigma_g = sigma_g, discount = 0.99, precision = 0.0000001, K = K, C = NA, memory = memory, improvementConstant=0.01,
                    max_state = max(states), max_obs = max(obs), max_action = max(actions), min_state = min(states), min_obs = min(obs), min_action = min(actions))
models <- data.frame(vars, fixed)

## Preview the model list
models

## Usual assumption at the moment for reward fn
reward_fn <- function(x,h) pmin(x,h)

### RUN ### 
i = 1

## Compute alphas for the above examples
#for(i in 1:dim(models)[1]) {

## Select the model
  f <- switch(models[i, "model"], 
    allen = appl:::allen(models[i, "r"], models[i, "K"], models[i, "C"]),
    ricker = appl:::ricker(models[i, "r"], models[i, "K"])
  )

## Determine the matrices
  m <- fisheries_matrices(states, actions, obs, reward_fn, f = f, 
                          sigma_g = models[i, "sigma_g"], sigma_m  = models[i, "sigma_m"])

## record data for the log
  log_data <- models[i,]
  
## run sarsop
  alpha <- sarsop(m$transition, m$observation, m$reward, 
                    discount = models[i, "discount"], 
                    precision = models[i, "precision"], 
                    memory = models[i, "memory"],
                    improvementConstant = models[i,"improvementConstant"],
                    log_dir = log_dir, 
                    log_data = log_data)

#}  
  
  ## 34b4a892-3e8f-4255-9516-210557de93f8,29.95,16764.56,16869.2,0.698911,NA,200,200,200,0.99,2016-09-22 02:00:33,0.2,0.6,ricker,0.01720091,0.99,1e-07,0.9903371,NA,117589,0.01,1.2,1.2,1.2,0,0,0
  
