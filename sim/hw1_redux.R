library(ks)
library(dplyr)
library(readr)
library(purrr)
library(readxl)
library(ggplot2)
library(triangle)
library(lubridate)

excel_sheets("data/Analysis_Data.xlsx")
dc <- read_excel("data/Analysis_Data.xlsx", sheet = "Drilling Cost", skip = 2)

# str(dc)
dc91_06 <- dc %>%
  rename(date = 1, oil_cost = 2, gas_cost = 3, drywell_cost = 4,
         oil_return = 5, gas_return = 6, drywell_return = 7) %>%
  filter(date < ymd("2007-01-01"), date >= ymd("1991-01-01")) %>%  # filter date range
  mutate(oil_return = parse_double(oil_return),                    # convert character to numeric
         gas_return = parse_double(gas_return),
         drywell_return = parse_double(drywell_return))

hist_returns <- c(dc91_06$oil_return, dc91_06$gas_return, dc91_06$drywell_return)

ggplot() + 
  geom_histogram(aes(hist_returns), bins = 10)

qplot(sample = hist_returns) +
  labs(x = "Theoretical Quantile",
       y = "Return",
       title = "QQ-plot of returns") 

# Calculate values for normal Dist
xbar <- mean(hist_returns)
sig_hat <- sd(hist_returns)
cost06 <- dc91_06 %>%
  filter(year(date) == 2006) %>%
  select(oil_cost, gas_cost, drywell_cost) %>%
  rowMeans()


# Example function ============================================================
ex <- function(n = 3) {
  c <- 1
  cum_cost <- function(r) {
    c <<- c * (1 + r)
  }
  returns06_11 <- rerun(n, c(.05, .25))
  map_dfc(returns06_11, cum_cost)
}
# =============================================================================


# SIMULATION 1 ----------------------------------------------------------------

run_sim1 <- function(n = 1000) {
  
  # initialze cost to 2006 amount
  c <- cost06
  
  # accumulate cost
  cum_cost <- function(r) {
    c <<- c * (1 + r)
  }
  
  # generate returns for each period
  returns06_11 <- rerun(6, rnorm(n, mean = xbar, sd = sig_hat))
  returns12_14 <- rerun(3, rtriangle(n, -0.22, -0.07, -0.0917))
  returns15_18 <- rerun(5, rtriangle(n, 0.02, 0.06, 0.05))
  
  # calculate costs
  cost07_12 <- map_dfc(returns06_11, cum_cost)
  cost13_15 <- map_dfc(returns12_14, cum_cost)
  cost16_19 <- map_dfc(returns15_18, cum_cost)
  
  # return final costs
  c
}

sim1_results <- run_sim1(1000000)

ggplot() + 
  geom_histogram(aes(sim1_results), bins = 100)


# SIMULATION 2 ----------------------------------------------------------------

# Calculate KDE
sim2_bw <- density(hist_returns, bw = "SJ-ste")$bw
sim2_kde <- kde(hist_returns, h = sim2_bw)


run_sim2 <- function(n = 1000) {
  
  # initialze cost to 2006 amount
  c <- cost06
  
  # accumulate cost
  cum_cost <- function(r) {
    c <<- c * (1 + r)
  }
  
  # generate returns for each period
  returns06_11 <- rerun(6, rkde(n, fhat = sim2_kde))
  returns12_14 <- rerun(3, rtriangle(n, -0.22, -0.07, -0.0917))
  returns15_18 <- rerun(5, rtriangle(n, 0.02, 0.06, 0.05))
  
  # calculate costs
  cost07_12 <- map_dfc(returns06_11, cum_cost)
  cost13_15 <- map_dfc(returns12_14, cum_cost)
  cost16_19 <- map_dfc(returns15_18, cum_cost)
  
  # return final costs
  c
}

sim2_results <- run_sim2(1000000)

ggplot() +
  geom_histogram(aes(sim2_results), bins = 100)
