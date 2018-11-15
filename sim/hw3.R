library(purrr)
library(tictoc)
library(ggplot2)
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

tic("simulating") # tic() toc() times the code | 1 mil. takes around 5 min.
set.seed(3)
sim_results <- run_wetwell_sim(1000000)
toc()

# =============================================================================


# PLOTS -----------------------------------------------------------------------

five_percent_VaR <- quantile(sim_results$percent_wet, .05)
es <- -cvar::ES(sim_results$percent_wet)

# summary(sim_results$percent_wet)
# mean(sim_results$percent_wet)
# sd(sim_results$percent_wet)

ggplot() +
  geom_histogram(aes(sim_results$percent_wet), bins = 50, 
                 colour = "black", fill = "sky blue", alpha = .5) +
  labs(x = "Proportion of Wet Wells",
       title = "Histogram of Wet Well Proportion") +
  geom_vline(xintercept = five_percent_VaR,
             color = "red", linetype = "dashed") +
  geom_vline(xintercept = es,
             color = "orange", linetype = "dotted", size = .7) +
  geom_text(aes(x = .48, y = 60000), 
            label = paste0("5% CVaR\n", round(es, 2)),
            size = 6, color = "orange") +
  geom_text(aes(x = .638, y = 74000), 
            label = paste0("5% VaR\n", round(five_percent_VaR, 2)),
            size = 6, color = "red") +
  theme(text = element_text(size = 18))
  

# summary(sim_results$hc_prob)
# sd(sim_results$hc_prob)
hc_mean <- mean(sim_results$hc_prob)

tic("plotting hc prob")
ggplot() +
  geom_histogram(aes(sim_results$hc_prob), bins = 100,
                 colour = "black", fill = "sky blue", alpha = .5) +
  labs(x = "Hydrocarbon Probability",
       title = "Histogram of Hydrocarbon Probability") +
  geom_vline(xintercept = hc_mean, linetype = "dashed") +
  geom_text(aes(x = .94, y = 800000), 
            label = paste0("Mean\n", round(hc_mean, 2)),
            size = 6) +
  theme(text = element_text(size = 18))
toc()


# summary(sim_results$res_prob)
# sd(sim_results$res_prob)
res_mean <- mean(sim_results$res_prob)

tic("plotting resevoir prob")
ggplot() +
  geom_histogram(aes(sim_results$res_prob), bins = 100,
                 colour = "black", fill = "sky blue", alpha = .5) +
  labs(x = "Reservoir Probability",
       title = "Histogram of Reservoir Probability") + 
  geom_vline(xintercept = res_mean, linetype = "dashed") +
  geom_text(aes(x = res_mean - .03, y = 625000), 
            label = paste0("Mean\n", round(res_mean, 2)),
            size = 6) +
  theme(text = element_text(size = 18))
toc()

