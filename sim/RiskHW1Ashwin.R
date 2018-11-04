library(data.table)
library(readxl)
library(dplyr)


# Read in Files -----------------------------------------------------------
OilPrice = read_xlsx("C:/Users/thebi/OneDrive/Documents/GitHub/fall3/sim/data/Analysis_Data.xlsx")
DrillingCost = read_xlsx("C:/Users/thebi/OneDrive/Documents/GitHub/fall3/sim/data/Analysis_Data.xlsx", sheet=2)


# Clean Up Data -----------------------------------------------------------

DrillingCost =
  DrillingCost %>%
  rename(Year = 'Drilling Costs',
         Oil_Cost = X__1,
         Gas_Cost = X__2,
         DryWell_Cost = X__3,
         Oil_Return = X__4,
         Gas_Return = X__5,
         DryWell_Return = X__6)
DrillingCost = DrillingCost[-nrow(DrillingCost),] #take out 2007

