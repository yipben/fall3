library(dplyr)
library(purrr)
library(tictoc)
library(ggplot2)

source("hw2_sim_only.R")

run_full_sim <- function(n) {
  
  # generate no. of wells
  n_wells <- rerun(n, rdunif(1, a = 10, b = 30))
  
  # prob. hydrocarbons
  hydrocarbons <- map(n_wells, ~ rtruncnorm(.x, a = 0, b = 1, mean = .99, sd = .05))
  
  # prob. reservoir
  reservoir <- map(n_wells, ~ rtruncnorm(.x, a = 0, b = 1, mean = .8, sd = .1))
  
  # prob. wet
  prob_wet <- map2(hydrocarbons, reservoir, ~ .x * .y)
  
  # no. producing wells
  is_wet <- map(prob_wet, ~ rbinom(length(.x), size = 1, prob = .x))
  
  # wet well count
  wet_count <- map_int(is_wet, sum)
  
  # wet well npv
  print("Calculating npv...")
  wet_npv <- map_dbl(wet_count, sim_wet_npv)
  
  # dry well count
  dry_count <- map_int(is_wet, ~ length(.x) - sum(.x))
  
  # drywell costs
  print("Calculating drywell costs...")
  dry_cost <- map_dbl(dry_count, sim_dry_cost)
  
  # overall return
  npv_all <- wet_npv - dry_cost
  
  # returning results
  list(
    n_wells = unlist(n_wells),
    is_wet = unlist(is_wet),
    wet_count = wet_count,
    wet_npv = wet_npv,
    dry_count = dry_count,
    dry_cost = dry_cost,
    npv = npv_all
  )
  
}


# RUN FULL SIMULATION =========================================================
# 10,000 takes ~00:01:48 | 100,000 takes ~00:18:10 | 1,000,000 takes ~04:30:00

# tic()
# set.seed(8)
# print(paste("start:", Sys.time()))
# full_sim_results <- run_full_sim(1000000)
# print(paste("end:", Sys.time()))
# toc()

# saveRDS(full_sim_results, file = "one_mil_sim_results.rds")
one_mil_sim_results <- readRDS("one_mil_sim_results.rds")

# =============================================================================


# PLOT SIM RESULTS ------------------------------------------------------------

ggplot() + 
  geom_histogram(
    aes(one_mil_sim_results$npv/1000000), 
    bins = 50,
    colour = "black", 
    fill = "sky blue",
    alpha = .5
  ) +
  labs(
    x = "Net Present Value (millions)",
    y = "Frequency",
    title = "One Million Simulations of Net Present Value (NPV)"
  ) +
  theme(text = element_text(size = 18))

# mean(one_mil_sim_results$npv, na.rm = T)
# sum(is.na(one_mil_sim_results$npv))
# summary(one_mil_sim_results$npv)


# BOOTSTRAPPING FOR CI's ======================================================

boot <- function(stat = "var") {
  
  s <- sample(one_mil_sim_results$npv, size = 1000000, replace = T)
  
  if (stat == "var") {
    
    quantile(s, .05, na.rm = T)
    
  } else if (stat == "es") {
    
    s_sort <- sort(s)
    
    mean(s_sort[1:(1000000 * .05)])
    
  }
}

# 1,000 takes ~00:01:00 | 10,000 takes ~ 00:10:30 |
tic()
set.seed(8)
VaR_estimates <- unlist(rerun(10000, boot()))
toc()

# 1,000 takes ~00:02:21 | 10,000 takes ~ 00:10:30 |
tic()
set.seed(8)
es_estimates <- unlist(rerun(1000, boot(stat = "es")))
toc()
# =============================================================================


# PLOT BOOTSTRAP RESULTS ------------------------------------------------------

# value at risk
ggplot() + 
  geom_histogram(
    aes(VaR_estimates/1000000), 
    bins = 50,
    colour = "black", 
    fill = "sky blue",
    alpha = .5
  ) +
  labs(
    x = "Value at Risk (millions)",
    y = "Frequency",
    title = "Bootstrap results for Value at Rist (VaR)"
  ) +
  theme(
    text = element_text(size = 18)
  ) +
  geom_vline(
    xintercept = quantile(VaR_estimates/1000000, .05),
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = quantile(VaR_estimates/1000000, .95),
    linetype = "dashed"
  )

# expected shortfall
ggplot() + 
  geom_histogram(
    aes(es_estimates/1000000), 
    bins = 50,
    colour = "black", 
    fill = "sky blue",
    alpha = .5
  ) +
  labs(
    x = "Expected Shortfall (millions)",
    y = "Frequency",
    title = "Bootstrap results for Expected Shortfall (ES)"
  ) +
  theme(
    text = element_text(size = 18)
  ) +
  geom_vline(
    xintercept = quantile(es_estimates/1000000, .05, na.rm = T),
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = quantile(es_estimates/1000000, .95, na.rm = T),
    linetype = "dashed"
  )

