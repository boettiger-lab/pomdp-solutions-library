library("appl")

### SETUP ###

log_dir = "."

## choose the state/action/obs space
states <- 0:20
actions <- states
obs <- states

## Create a data frame with 1 row for each parameter set for which you want to generate alpha vectors:
Ks <- data.frame(K = 10:15, r = 1)
rs <- data.frame(K=15, r = seq(0.6, 1.6, by =0.2))

## Bind this to a data.frame listing eahc of the fixed parameters across all runs
fixed <- data.frame(model = "ricker", sigma_g = 0.1, sigma_m = 0.1, discount = 0.95, precision = 0.1, C = NA)
models <- data.frame(rbind(Ks, rs), fixed)

## Preview the model list
models


## Usual assumption at the moment for reward fn
reward_fn <- function(x,h) pmin(x,h)



### RUN ### 

## Detect available memory (linux servers only)
memory <- round(0.95 * as.numeric(gsub(".* (\\d+) .*", "\\1", system("cat /proc/meminfo", intern=TRUE)[1])) / 1000)


## Compute alphas for the above examples
for(i in 1:dim(models)[1]) {
  f <- switch(models[i, "model"], 
    allen = appl:::allen(models[i, "r"], models[i, "K"], models[i, "C"]),
    ricker = appl:::ricker(models[i, "r"], models[i, "K"])
  )

  m <- fisheries_matrices(states, actions, obs, reward_fn, f = f, 
                          sigma_g = models[i, "sigma_g"], sigma_m  = models[i, "sigma_m"])
  log_data <- data.frame(model = models[i, "model"], r = models[i, "r"], K  = models[i, "K"], 
                         C = models[i, "C"], sigma_g = models[i, "sigma_g"], sigma_m = models[i, "sigma_m"],
                         memory = memory)
  
    alpha <- sarsop(m$transition, m$observation, m$reward, 
                    discount = models[i, "discount"], 
                    precision = models[i, "precision"], memory = memory,
                    log_dir = log_dir, log_data = log_data)

}