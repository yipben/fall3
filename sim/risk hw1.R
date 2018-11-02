
rm(list=ls())
install.packages("readxl")
library(readxl)


setwd("C:\\Users\\molly\\OneDrive\\Documents\\_MSA 2018 COURSEWORK\\Risk and Simulation")
# importing the Excel file
wbpath <- "C:\\Users\\molly\\OneDrive\\Documents\\_MSA 2018 COURSEWORK\\Risk and Simulation\\Analysis_Data.xlsx"

projections <- read_excel(wbpath, sheet=1)
costs <- read_excel(wbpath, sheet=2) 


