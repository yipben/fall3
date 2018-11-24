library(dplyr)
library(purrr)
library(tictoc)
library(ggplot2)

# load sim code from hw2
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
  theme(
    text = element_text(size = 18)
  )

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

# 1,000 takes ~00:01:00 | 10,000 takes ~00:10:30 | 100,000 takes ~01:57:00
# tic()
# set.seed(8)
# VaR_estimates <- unlist(rerun(100000, boot()))
# toc()

# saveRDS(VaR_estimates, file = "VaR_estimates.rds")
VaR_estimates <- readRDS("VaR_estimates.rds")

# 1,000 takes ~00:02:21 | 10,000 takes ~ 00:10:30 | 100,000 takes ~04:00:00
# tic()
# set.seed(8)
# es_estimates <- unlist(rerun(100000, boot(stat = "es")))
# toc()
# 
# saveRDS(es_estimates, file = "es_estimates.rds")
es_estimates <- readRDS("es_estimates.rds")

# =============================================================================


# PLOT BOOTSTRAP RESULTS ------------------------------------------------------

# quantiles
VaR_q_05 <- quantile(VaR_estimates, .05)/1000000
VaR_q_95 <- quantile(VaR_estimates, .95)/1000000
es_q_05 <- quantile(es_estimates, .05, na.rm = T)/1000000
es_q_95 <- quantile(es_estimates, .95, na.rm = T)/1000000

# value at risk
ggplot() + 
  geom_area(
    aes(x = c(VaR_q_05, VaR_q_95), y = 9500), # y = 9500 so area extends to top
    fill = "red",
    alpha = .1
  ) +
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
    xintercept = mean(VaR_estimates)/1000000,
    linetype = "dashed",
    color = "red",
    size = .75
  ) +
  coord_cartesian(
    ylim = c(0, 8900)
  )

# expected shortfall
ggplot() + 
  geom_area(
    aes(x = c(es_q_05, es_q_95), y = 8000), # y = 9500 so area extends to top
    fill = "orange",
    alpha = .15
  ) +
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
    xintercept = mean(es_estimates)/1000000,
    linetype = "dotted",
    color = "orange",
    size = .75
  ) +
  coord_cartesian(
    ylim = c(0, 7000)
  )



# NPV w/ VaR, CVaR ------------------------------------------------------------

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
  theme(
    text = element_text(size = 18)
  ) +
  geom_vline(
    xintercept = mean(VaR_estimates)/1000000,
    linetype = "dashed",
    color = "red",
    size = .75
  ) +
  geom_vline(
    xintercept = mean(es_estimates)/1000000,
    linetype = "dotted",
    color = "orange",
    size = .75
  ) 
