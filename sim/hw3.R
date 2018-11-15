library(purrr)
library(tictoc)
library(truncnorm)

run_wetwell_sim <- function(n) {
  
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
  
  # percent producing
  percent_wet <- map_dbl(is_wet, mean)
  
  # returning results
  list(percent_wet = percent_wet,
       hc_prob = unlist(hydrocarbons),
       res_prob = unlist(reservoir),
       n_wells = unlist(n_wells))
}

# RUN SIMULATION ==============================================================

# tic("simulating") # tic() toc() times the code | 1 mil. takes around 5 min.
sim_results <- run_wetwell_sim(1000000)
# toc()

# =============================================================================


# PLOTS -----------------------------------------------------------------------
ggplot() +
  geom_histogram(aes(sim_results$percent_wet), bins = 50, 
                 colour = "black", fill = "sky blue", alpha = .5) +
  labs(x = "Proportion of Wet Wells",
       title = "Histogram of Wet Well Proportion")

# summary(sim_results$percent_wet)
# mean(sim_results$percent_wet)
# sd(sim_results$percent_wet)

ggplot() +
  geom_histogram(aes(sim_results$hc_prob), bins = 100,
                 colour = "black", fill = "sky blue", alpha = .5) +
  labs(x = "Hydrocarbon Probability",
       title = "Histogram of Hydrocarbon Probability")

# summary(sim_results$hc_prob)
# mean(sim_results$hc_prob)
# sd(sim_results$hc_prob)

ggplot() +
  geom_histogram(aes(sim_results$res_prob), bins = 100,
                 colour = "black", fill = "sky blue", bins = 30, alpha = .5) +
  labs(x = "Reservoir Probability",
       title = "Histogram of Reservoir Probability")

# summary(sim_results$res_prob)
# mean(sim_results$res_prob)
# sd(sim_results$res_prob)
