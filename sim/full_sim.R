library(dplyr)
library(purrr)
library(tictoc)

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
    wet_count = unlist(wet_count),
    wet_npv = unlist(wet_npv),
    dry_count = unlist(dry_count),
    dry_cost = unlist(dry_cost),
    npv = unlist(npv_all)
  )
  
}


# RUN FULL SIMULATION =========================================================
# 10,000 takes ~00:01:48 | 100,000 takes ~00:18:10

tic()
full_sim_results <- run_full_sim(100000)
toc()

# =============================================================================


hist(full_sim_results)


