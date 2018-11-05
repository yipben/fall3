library(data.table)
library(readxl)
library(dplyr)
library(sqldf)
library(ks)


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
drill = drill[31:47,]

mean(AvgCost)
oilMean = mean(as.numeric(drill$Oil_Return[2:47]))
mean(as.numeric(drill$Gas_Return[2:47]))
mean(as.numeric(drill$DryWell_Return[2:47]))
sd(AvgCost)
Oilsd = sd(as.numeric(drill$Oil_Return[2:47]))
sd(as.numeric(drill$Gas_Return[2:47]))
sd(as.numeric(drill$DryWell_Return[2:47]))


# Kernel Density Estimation -----------------------------------------------

set.seed(112358)
r <- rnorm(n=10000, mean=oilMean, sd=Oilsd)


mean(P1)
sd(P1)

hist(P1, breaks=50, main='One Year Value Distribution', xlab='Final Value')
abline(v = 1000, col="red", lwd=2)
mtext("Initial Inv.", at=1000, col="red")

# Distribution Selection - Kernel Estimation #
Density.P1 <- density(P1, bw="SJ-ste")
Density.P1

Est.P1 <- rkde(fhat=kde(P1, h=25.42), n=1000)
hist(Est.P1, breaks=50, main='Estimated One Year Value Distribution', xlab='Final Value')

