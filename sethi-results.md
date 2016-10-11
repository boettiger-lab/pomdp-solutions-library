

```r
library(purrr)
library(ggplot2)
library(dplyr)
library(appl)
library(printr)
options(knitr.table.format = 'markdown')
```



```r
log_dir = "sethi-50-states"

# alphas <- original_alphas
# models <- original_models

meta <- meta_from_log(data.frame(cost = "none"), log_dir = log_dir)

alphas <- alphas_from_log(meta, log_dir = log_dir)
models <- models_from_log(meta)

states <- seq(meta[1,]$min_state, meta[1,]$max_state, len=meta[1,]$n_states)
actions <- seq(meta[1,]$min_action, meta[1,]$max_action, len=meta[1,]$n_action)
discount <- meta[1,]$discount

meta
```



|   |id          | load_time_sec| init_time_sec| run_time_sec| final_precision|end_condition          | n_states| n_obs| n_actions| discount|date                |   r| sigma_m| sigma_g|noise     |    K|    C|model  | precision| timeout| timeInterval| max_state| max_obs| max_action| min_state| min_obs| min_action|cost | beta|
|:--|:-----------|-------------:|-------------:|------------:|---------------:|:----------------------|--------:|-----:|---------:|--------:|:-------------------|---:|-------:|-------:|:---------|----:|----:|:------|---------:|-------:|------------:|---------:|-------:|----------:|---------:|-------:|----------:|:----|----:|
|1  |c673cac26e8 |          0.63|         16.46|      20038.0|       0.0000345|Preset timeout reached |       50|    50|        50|     0.95|2016-10-10 09:05:42 | 0.5|     0.1|     0.1|uniform   | 0.66|   NA|ricker |     1e-07|   20000|         5000|         1|       1|          1|         0|       0|          0|none |   NA|
|2  |c683cac26e8 |          0.50|         16.49|      20046.8|       0.0024503|Preset timeout reached |       50|    50|        50|     0.95|2016-10-10 09:05:50 | 1.0|     0.1|     0.1|uniform   | 0.66|   NA|ricker |     1e-07|   20000|         5000|         1|       1|          1|         0|       0|          0|none |   NA|
|3  |c6b3cac26e8 |          0.58|         33.19|      20072.9|       0.0000081|Preset timeout reached |       50|    50|        50|     0.95|2016-10-10 09:06:09 | 0.5|     0.1|     0.5|uniform   | 0.66|   NA|ricker |     1e-07|   20000|         5000|         1|       1|          1|         0|       0|          0|none |   NA|
|4  |c693cac26e8 |          0.78|         17.42|      20114.8|       0.0400375|Preset timeout reached |       50|    50|        50|     0.95|2016-10-10 09:06:55 | 0.5|     0.5|     0.1|uniform   | 0.66|   NA|ricker |     1e-07|   20000|         5000|         1|       1|          1|         0|       0|          0|none |   NA|
|5  |c733cac26e8 |          0.65|         27.04|      20126.0|       0.0014686|Preset timeout reached |       50|    50|        50|     0.95|2016-10-10 09:07:04 | 1.0|     0.1|     0.1|lognormal | 0.66| 0.05|bh     |     1e-07|   20000|         5000|         1|       1|          1|         0|       0|          0|none |   NA|
|6  |c6a3cac26e8 |          0.56|         21.20|      20143.7|       0.1518690|Preset timeout reached |       50|    50|        50|     0.95|2016-10-10 09:07:21 | 1.0|     0.5|     0.1|uniform   | 0.66|   NA|ricker |     1e-07|   20000|         5000|         1|       1|          1|         0|       0|          0|none |   NA|
|7  |c6f3cac26e8 |          0.72|         30.92|      20141.8|       0.0034428|Preset timeout reached |       50|    50|        50|     0.95|2016-10-10 09:07:23 | 1.0|     0.1|     0.1|lognormal | 0.66| 0.05|allen  |     1e-07|   20000|         5000|         1|       1|          1|         0|       0|          0|none |   NA|
|8  |c6d3cac26e8 |          0.92|         36.68|      20144.8|       0.0230153|Preset timeout reached |       50|    50|        50|     0.95|2016-10-10 09:07:28 | 0.5|     0.5|     0.5|uniform   | 0.66|   NA|ricker |     1e-07|   20000|         5000|         1|       1|          1|         0|       0|          0|none |   NA|
|9  |c6c3cac26e8 |          1.00|         46.69|      20154.0|       0.0004433|Preset timeout reached |       50|    50|        50|     0.95|2016-10-10 09:07:33 | 1.0|     0.1|     0.5|uniform   | 0.66|   NA|ricker |     1e-07|   20000|         5000|         1|       1|          1|         0|       0|          0|none |   NA|
|10 |c5f3cac26e8 |          0.94|         26.60|      20176.0|       0.0013190|Preset timeout reached |       50|    50|        50|     0.95|2016-10-10 09:08:02 | 0.5|     0.1|     0.1|lognormal | 0.66|   NA|ricker |     1e-07|   20000|         5000|         1|       1|          1|         0|       0|          0|none |   NA|
|11 |c753cac26e8 |          0.62|         60.08|      20204.8|       0.0000470|Preset timeout reached |       50|    50|        50|     0.95|2016-10-10 09:08:21 | 1.0|     0.1|     0.5|lognormal | 0.66| 0.05|bh     |     1e-07|   20000|         5000|         1|       1|          1|         0|       0|          0|none |   NA|
|12 |c6e3cac26e8 |          0.55|         51.25|      20214.9|       0.0736340|Preset timeout reached |       50|    50|        50|     0.95|2016-10-10 09:08:35 | 1.0|     0.5|     0.5|uniform   | 0.66|   NA|ricker |     1e-07|   20000|         5000|         1|       1|          1|         0|       0|          0|none |   NA|
|13 |c603cac26e8 |          0.50|         30.92|      20213.7|       0.0046996|Preset timeout reached |       50|    50|        50|     0.95|2016-10-10 09:08:36 | 1.0|     0.1|     0.1|lognormal | 0.66|   NA|ricker |     1e-07|   20000|         5000|         1|       1|          1|         0|       0|          0|none |   NA|
|14 |c633cac26e8 |          0.60|         80.13|      20322.1|       0.0000327|Preset timeout reached |       50|    50|        50|     0.95|2016-10-10 09:10:19 | 0.5|     0.1|     0.5|lognormal | 0.66|   NA|ricker |     1e-07|   20000|         5000|         1|       1|          1|         0|       0|          0|none |   NA|
|15 |c643cac26e8 |          0.58|         74.87|      20370.2|       0.0002617|Preset timeout reached |       50|    50|        50|     0.95|2016-10-10 09:11:10 | 1.0|     0.1|     0.5|lognormal | 0.66|   NA|ricker |     1e-07|   20000|         5000|         1|       1|          1|         0|       0|          0|none |   NA|
|16 |c713cac26e8 |          1.07|         71.57|      20399.2|       0.0001182|Preset timeout reached |       50|    50|        50|     0.95|2016-10-10 09:11:44 | 1.0|     0.1|     0.5|lognormal | 0.66| 0.05|allen  |     1e-07|   20000|         5000|         1|       1|          1|         0|       0|          0|none |   NA|
|17 |c623cac26e8 |          0.53|         34.87|      20407.4|       0.2403810|Preset timeout reached |       50|    50|        50|     0.95|2016-10-10 09:11:52 | 1.0|     0.5|     0.1|lognormal | 0.66|   NA|ricker |     1e-07|   20000|         5000|         1|       1|          1|         0|       0|          0|none |   NA|
|18 |c723cac26e8 |          0.96|         72.57|      20419.9|       0.0411789|Preset timeout reached |       50|    50|        50|     0.95|2016-10-10 09:11:56 | 1.0|     0.5|     0.5|lognormal | 0.66| 0.05|allen  |     1e-07|   20000|         5000|         1|       1|          1|         0|       0|          0|none |   NA|
|19 |c613cac26e8 |          0.66|         27.90|      20611.2|       0.0730887|Preset timeout reached |       50|    50|        50|     0.95|2016-10-10 09:15:13 | 0.5|     0.5|     0.1|lognormal | 0.66|   NA|ricker |     1e-07|   20000|         5000|         1|       1|          1|         0|       0|          0|none |   NA|
|20 |c663cac26e8 |          1.09|         74.88|      20642.7|       0.2498110|Preset timeout reached |       50|    50|        50|     0.95|2016-10-10 09:15:47 | 1.0|     0.5|     0.5|lognormal | 0.66|   NA|ricker |     1e-07|   20000|         5000|         1|       1|          1|         0|       0|          0|none |   NA|
|21 |c743cac26e8 |          0.66|         29.11|      21092.4|       0.0987828|Preset timeout reached |       50|    50|        50|     0.95|2016-10-10 09:23:16 | 1.0|     0.5|     0.1|lognormal | 0.66| 0.05|bh     |     1e-07|   20000|         5000|         1|       1|          1|         0|       0|          0|none |   NA|
|23 |c703cac26e8 |          0.81|         31.39|      21318.7|       0.1288730|Preset timeout reached |       50|    50|        50|     0.95|2016-10-10 09:27:01 | 1.0|     0.5|     0.1|lognormal | 0.66| 0.05|allen  |     1e-07|   20000|         5000|         1|       1|          1|         0|       0|          0|none |   NA|
|24 |c763cac26e8 |          1.11|         77.37|      21373.7|       0.1625740|Preset timeout reached |       50|    50|        50|     0.95|2016-10-10 09:27:58 | 1.0|     0.5|     0.5|lognormal | 0.66| 0.05|bh     |     1e-07|   20000|         5000|         1|       1|          1|         0|       0|          0|none |   NA|
|25 |c653cac26e8 |          0.61|         76.06|      21394.2|       0.1135120|Preset timeout reached |       50|    50|        50|     0.95|2016-10-10 09:28:19 | 0.5|     0.5|     0.5|lognormal | 0.66|   NA|ricker |     1e-07|   20000|         5000|         1|       1|          1|         0|       0|          0|none |   NA|


## Det policy


```r
fs <- f_from_log(meta)

dets <- purrr::map_df(1:dim(meta)[[1]], function(i){ 
  f <- fs[[i]]
  S_star <- optimize(function(x) x / discount - f(x,0), c(min(states),max(states)))$minimum
  h <- pmax(states - S_star,  0)
  policy <- sapply(h, function(h) which.min((abs(h - actions))))
  det <- data.frame(policy, value = 1:length(states), state = 1:length(states))
}, .id = "model_id"
)
```


## Convergence testing


```r
inter <- appl:::intermediates_from_log(meta, log_dir = log_dir)

df1 <- 
purrr::map_df(1:length(models), function(j){
  alphas <- inter[[j]]
  m <- models[[j]]
  purrr::map_df(1:length(alphas), function(i)
    compute_policy(alphas[[i]], m$transition, m$observation, m$reward),
    .id = "intermediate") 
}, .id = "model_id")

df1 %>% 
  ggplot(aes(states[state], states[state] - actions[policy], col=intermediate)) + 
  geom_line() + 
  facet_wrap(~model_id, scales = "free") + 
  coord_cartesian(ylim = c(0,0.5))
```

![](sethi-results_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

## Explore POMDP policy



```r
df <- purrr::map_df(1:length(models), function(i)
  compute_policy(alphas[[i]], models[[i]]$transition, models[[i]]$observation, models[[i]]$reward),
  .id = "model_id")

## Join to metadata table
meta$model_id <- as.character(1:length(models))

## Join to deterministic case
df2 <- bind_rows(
  left_join(df, meta, by = "model_id"),
  left_join(dets, meta, by = "model_id") %>% mutate(sigma_m = 0))
```


## Lognormal noise



```r
df2 %>% filter(r == 1, noise == "lognormal") %>% 
  ggplot(aes(states[state], states[state] - actions[policy], col = as.character(sigma_m))) +
  geom_line() +
  facet_grid(model ~ sigma_g, scales = "free") +
  coord_cartesian(ylim = c(0,0.8))
```

![](sethi-results_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


## Uniform


```r
df2 %>% filter(r == 1, noise == "uniform") %>% 
  ggplot(aes(states[state], states[state] - actions[policy], col = as.character(sigma_m))) +
  geom_line() +
  facet_grid(model ~ sigma_g, scales = "free") +
  coord_cartesian(ylim = c(0,0.8))
```

![](sethi-results_files/figure-html/unnamed-chunk-7-1.png)<!-- -->



## Simulation


```r
x0 <- which.min(abs(0.33 - states))


get_det <- function(meta){
  f <- f_from_log(meta)[[1]]
  S_star <- optimize(function(x) x / discount - f(x,0), c(min(states),max(states)))$minimum
  h <- pmax(states - S_star,  0)
  sapply(h, function(h) which.min((abs(h - actions))))
}
```


```r
s1 <-  meta %>% filter(r == 1, sigma_m == 0.5, sigma_g == 0.5, model == "ricker", noise == "lognormal")
alpha <- alphas_from_log(s1, log_dir)[[1]]
m <- models_from_log(s1)[[1]]
det_policy <- get_det(meta)
```

Simulation in which belief over states is updated in each time step, used to determine a new policy, which is then used to choose the best action given the most recent observation.


```r
sim <- purrr::map_df(1:50, function(i) appl::sim_pomdp(m$transition, m$observation, m$reward, discount, x0 = x0, Tmax = 100, alpha = alpha)$df, .id = "rep")
```


Simulation in which the policy is not updated based on the most recent belief over states, but remains the policy given a uniform prior over belief states. 


```r
s <- compute_policy(alpha,m$transition, m$observation, m$reward)
sim2 <- purrr::map_df(1:50, function(i) mdplearning::mdp_planning(m$transition, m$reward, discount, x0 = x0, Tmax = 100, observation = m$observation, policy = s$policy), .id = "rep")
```




```r
sim3 <- purrr::map_df(1:50, function(i) mdplearning::mdp_planning(m$transition, m$reward, discount, x0 = x0, Tmax = 100, observation = m$observation, policy = det_policy), .id = "rep")
```



```r
df <- bind_rows(pomdp = sim, static = sim2, det = sim3, .id = "scenario")
df %>% group_by(scenario, rep) %>% summarise(value = sum(value)) %>%  group_by(scenario) %>% summarise(mean(value))
```



|scenario | mean(value)|
|:--------|-----------:|
|det      |    1.488961|
|pomdp    |    2.810192|
|static   |    2.666848|

```r
df %>% group_by(scenario, time) %>% summarise(value = mean(value), state = mean(state)) %>%
ggplot(aes(time, states[state], col = scenario)) + geom_line()
```

![](sethi-results_files/figure-html/unnamed-chunk-13-1.png)<!-- -->


## Small growth noise



```r
s1 <-  meta %>% filter(r == 1, sigma_m == 0.5, sigma_g == 0.1, model == "ricker", noise == "lognormal")
alpha <- alphas_from_log(s1, log_dir)[[1]]
m <- models_from_log(s1)[[1]]
det_policy <- get_det(meta)


sim <- purrr::map_df(1:50, function(i) appl::sim_pomdp(m$transition, m$observation, m$reward, discount, x0 = x0, Tmax = 100, alpha = alpha)$df, .id = "rep")
s <- compute_policy(alpha,m$transition, m$observation, m$reward)
sim2 <- purrr::map_df(1:50, function(i) mdplearning::mdp_planning(m$transition, m$reward, discount, x0 = x0, Tmax = 100, observation = m$observation, policy = s$policy), .id = "rep")
sim3 <- purrr::map_df(1:50, function(i) mdplearning::mdp_planning(m$transition, m$reward, discount, x0 = x0, Tmax = 100, observation = m$observation, policy = det_policy), .id = "rep")

df <- bind_rows(pomdp = sim, static = sim2, det = sim3, .id = "scenario")
df %>% group_by(scenario, rep) %>% summarise(value = sum(value)) %>%  group_by(scenario) %>% summarise(mean(value))
```



|scenario | mean(value)|
|:--------|-----------:|
|det      |    1.385433|
|pomdp    |    3.662175|
|static   |    3.488639|

```r
df %>% group_by(scenario, time) %>% summarise(value = mean(value), state = mean(state)) %>%
ggplot(aes(time, states[state], col = scenario)) + geom_line()
```

![](sethi-results_files/figure-html/unnamed-chunk-14-1.png)<!-- -->




## Small both



```r
s1 <-  meta %>% filter(r == 1, sigma_m == 0.1, sigma_g == 0.1, model == "ricker", noise == "lognormal")
alpha <- alphas_from_log(s1, log_dir)[[1]]
m <- models_from_log(s1)[[1]]
det_policy <- get_det(meta)

sim <- purrr::map_df(1:50, function(i) appl::sim_pomdp(m$transition, m$observation, m$reward, discount, x0 = x0, Tmax = 100, alpha = alpha)$df, .id = "rep")
s <- compute_policy(alpha,m$transition, m$observation, m$reward)
sim2 <- purrr::map_df(1:50, function(i) mdplearning::mdp_planning(m$transition, m$reward, discount, x0 = x0, Tmax = 100, observation = m$observation, policy = s$policy), .id = "rep")
sim3 <- purrr::map_df(1:50, function(i) mdplearning::mdp_planning(m$transition, m$reward, discount, x0 = x0, Tmax = 100, observation = m$observation, policy = det_policy), .id = "rep")

df <- bind_rows(pomdp = sim, static = sim2, det = sim3, .id = "scenario")
df %>% group_by(scenario, rep) %>% summarise(value = sum(value)) %>%  group_by(scenario) %>% summarise(mean(value))
```



|scenario | mean(value)|
|:--------|-----------:|
|det      |    3.886553|
|pomdp    |    3.882983|
|static   |    3.877829|

```r
df %>% group_by(scenario, time) %>% summarise(value = mean(value), state = mean(state)) %>%
ggplot(aes(time, states[state], col = scenario)) + geom_line()
```

![](sethi-results_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

