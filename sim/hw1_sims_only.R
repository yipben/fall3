library(ks)
library(readr)
library(purrr)
library(dplyr)
library(triangle)

make_hw1_sim <- function(kernel = F) {
  
  # reading in data
  data <- read_csv("data/hw1.csv")
  
  # combining all returns
  hist_returns <- c(data$oil_return, data$gas_return, data$drywell_return)
  
  # calculating return stats (assuming oil, gas, drywell, all have same dist.)
  xbar <- mean(hist_returns)
  sig_hat <- sd(hist_returns)
  
  # calculating initial cost
  cost06 <- data %>%
    filter(year(date) == 2006) %>%
    select(oil_cost, gas_cost, drywell_cost) %>%
    rowMeans()
  
  # creating kernel density estimate (conditionally)
  if (kernel) {
    bw <- density(hist_returns, bw = "SJ-ste")$bw
    kde <- kde(hist_returns, h = bw)
  }
  
  # returning simulation function
  function(n = 10) {
    
    # initialzing cost to 2006 amount
    c <- cost06
    
    # accumulating cost
    cum_cost <- function(r) {
      c <<- c * (1 + r)
    }
    
    # generating returns for each period
    if (kernel) {
      returns06_11 <- rerun(6, rkde(n, fhat = kde))
    } else {
      returns06_11 <- rerun(6, rnorm(n, mean = xbar, sd = sig_hat))
    }
    returns12_14 <- rerun(3, rtriangle(n, -0.22, -0.07, -0.0917))
    returns15_18 <- rerun(4, rtriangle(n, 0.02, 0.06, 0.05))
    
    # calculating costs
    cost07_12 <- map_dfc(returns06_11, cum_cost)
    cost13_15 <- map_dfc(returns12_14, cum_cost)
    cost16_19 <- map_dfc(returns15_18, cum_cost)
    
    # returning final cost
    c
  }
}

run_sim1 <- make_hw1_sim()
run_sim2 <- make_hw1_sim(kernel = T)

# set.seed(18)
# sim1_results <- run_sim1(10000)
# sim2_results <- run_sim2(10000)
# summary(sim1_results)
# summary(sim2_results)
# sd(sim1_results)
# sd(sim2_results)

