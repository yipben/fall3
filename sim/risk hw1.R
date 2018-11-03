
rm(list=ls())
install.packages("readxl")
install.packages("tidyverse")
library(readxl)
library(tidyverse)
setwd("C:\\Users\\molly\\OneDrive\\Documents\\_MSA 2018 COURSEWORK\\Risk and Simulation")


# importing the Excel file - 2 sheets, historical costs and cost projections

wbpath <- "C:\\Users\\molly\\OneDrive\\Documents\\_MSA 2018 COURSEWORK\\Risk and Simulation\\Analysis_Data.xlsx"

projections <- read_excel(wbpath, sheet=1)
costs <- read_excel(wbpath, sheet=2) 

# clean up cost data
costs %>% 
  rename(
    year =`Drilling Costs`,
    oil_cost = X__1,
    nat_cost = X__2,
    dry_cost =X__3,
    oil_arith = X__4,
    nat_arith = X__5,
    dry_arith  = X__6
  )

costs <- costs[-c(1,2), ]

View(costs)
