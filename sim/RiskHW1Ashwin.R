library(data.table)
library(readxl)
library(dplyr)
library(sqldf)


# Read in Files -----------------------------------------------------------
OilPrice = read_xlsx("C:/Users/thebi/OneDrive/Documents/GitHub/fall3/sim/data/Analysis_Data.xlsx")
DrillingCost = read_excel("C:/Users/thebi/OneDrive/Documents/GitHub/fall3/sim/data/Analysis_Data.xlsx", sheet=2, skip = 2)
DrillingCost = DrillingCost[-nrow(DrillingCost),] #take out 2007

# Clean Up Data -----------------------------------------------------------

DrillingCost =
  DrillingCost %>%
  rename(Oil_Cost = 'U.S. Nominal Cost per Crude Oil Well Drilled (Thousand Dollars per Well)',
         Gas_Cost = 'U.S. Nominal Cost per Natural Gas Well Drilled (Thousand Dollars per Well)',
         DryWell_Cost = 'U.S. Nominal Cost per Dry Well Drilled (Thousand Dollars per Well)',
         Oil_Return = 'Arithmetic Return - Crude Oil',
         Gas_Return = 'Arithmetic Return - Natural Gas',
         DryWell_Return = 'Arithmetic Return - Dry Well')
DrillingCost$Date = substr(DrillingCost$Date,1,4)

DrillingCost$AvgCost = (as.numeric(DrillingCost$Oil_Cost) + as.numeric(DrillingCost$Gas_Cost) + 
           as.numeric(DrillingCost$DryWell_Cost))/3
head(DrillingCost)

drill = sqldf("select Date, AvgCost, Oil_Return, Gas_Return, DryWell_Return  
      from DrillingCost")
