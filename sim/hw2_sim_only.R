library(dplyr)
library(purrr)
library(readxl)
library(triangle)

source("hw1_sims_only.R")

# Helper functions (from Dr. LaBarr) ------------------------------------------



# -----------------------------------------------------------------------------


sim_base_cost <- function(n_wells) {
  
  acres_cost <- sum(rnorm(n_wells, mean = 600, sd = 50) * 960)
  
  seis_sect_cost <- sum(rnorm(n_wells, mean = 3, sd = .35) * 43000)
  
  drill_cost <- sum(run_sim1(n_wells) * 1000)
  
  acres_cost + seis_sect_cost + drill_cost
}

sim_dry_cost <- function(n_wells = 10) {
  
  if (n_wells < 1) {
    return(0)
  }

  prof_cost <- sum(rtriangle(n_wells, a = 172000, b = 279500, c = 215000))
  
  sim_base_cost(n_wells) + prof_cost
}

make_npv_sim <- function(end_yr = 2033, tax = .046) {
  
  time_range <- 2019:end_yr
  n_years <- length(time_range)
  price_data <- read_excel("data/Analysis_Data.xlsx", sheet = "Price Projections", skip = 2)
  
  stdz <- function(x) {
     x_std = (x - mean(x)) / sd(x)
     x_std
  }

  dstdz <- function(x_std, x) {
     x_old = (x_std * sd(x)) + mean(x)
     x_old
  }
  
  init_prod_dr <- function(x) {
    
    # drawing initial production and decline rate for each well
    init_prod <- rlnorm(x, meanlog = 6, sdlog = .28)
    dec_rate <- runif(x, min = .15, max = .32)
    
    # incorporating correlation structure
    cor_m <- matrix(c(1, .64, .64, 1), nrow = 2)
    U <- t(chol(cor_m))
    ip_drate_cor <- U %*% t(cbind(stdz(dec_rate), stdz(init_prod)))
    
    # extracting correlated data
    dec_rate_cor <- dstdz(ip_drate_cor[1, ], dec_rate)
    init_prod_cor <- dstdz(ip_drate_cor[2, ], init_prod)
    
    # returning initial prod. and (1 - decline rate)
    list(initial_prod = init_prod_cor,
         decline_prop = 1 - dec_rate_cor)
  }
  
  get_yearly_vol <- function(x) {
    
    # calculating initial production and decline rate
    prod_dr <- init_prod_dr(x)
    
    # initial production (barrels per day)
    init_prod <- prod_dr$initial_prod
    
    # decline rate x number of years
    decline_19_33 <- rerun(n_years, prod_dr$decline_prop)
    
    calc_yearly_vol <- function(decline_prop) {
      
      # calculating daily volume
      daily_vol <- (init_prod + (decline_prop * init_prod))/2
      
      # updating initial production amount
      init_prod <<- init_prod * decline_prop
      
      daily_vol * 365
    }
    
    # returning yearly vol. (by well)
    map(decline_19_33, calc_yearly_vol)
    
  }
  
  get_prices <- function(yr) {
    
    yr_prices <- price_data %>% filter(Year == yr)
    
    rtriangle(
      1, 
      a = yr_prices$`Low Oil Price`, 
      b = yr_prices$`High Oil Price`, 
      c = yr_prices$`AEO2018 Reference`)
  }
  
  function(n_wells) {
    
    # year zero costs
    comp_cost <- sum(rnorm(n_wells, mean = 390000, sd = 50000))
    prof_cost <- sum(rtriangle(n_wells, a = 172000, b = 279500, c = 215000) * n_years)
    total_y0_cost <- sim_base_cost(n_wells) + comp_cost + prof_cost
    
    # generating nri [net revenue interest] (by well)
    nri <- rnorm(n_wells, mean = .75, sd = .02)
  
    # generating operation costs (by year)
    op_cost_by_year <- rerun(n_years, rnorm(1, mean = 2.25, sd = .3))
    
    # generating yearly oil prices
    prices <- map(as.list(time_range), get_prices)
    
    # generating yearly volume (by well)
    volume <- get_yearly_vol(n_wells)
    
    # calculating yearly revenue (by well)
    revenue <- map2(prices, volume, ~ .x * .y)
    
    # adjusting revenue with nri
    rev_nri <- map(revenue, ~ .x * nri)

    # calculating yearly revenue and adjusting for taxes
    yearly_rev_nri_tax <- map_dbl(rev_nri, sum) * (1 - tax)
    
    # calculating yearly operational costs
    yearly_op_cost <- map2_dbl(volume, op_cost_by_year, ~ sum(.x * .y))
    
    # calculating yearly fnr
    yearly_fnr <- yearly_rev_nri_tax - yearly_op_cost
    
    # generating 
    wacc <- 1.1 ^ as.numeric(1:n_years)
    
    # returning npv
    sum(yearly_fnr / wacc) - total_y0_cost
  }
  
}

sim_wet_npv <- make_npv_sim()

















