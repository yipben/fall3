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
DrillingCost$Return
drill = sqldf("select Date, AvgCost, Oil_Return, Gas_Return, DryWell_Return  
      from DrillingCost")
drill = drill[32:47,]


# distribution ------------------------------------------------------------

returns = c(drill$Oil_Return, drill$Gas_Return, drill$DryWell_Return)
drillMean = mean(as.numeric(returns))
drillSD = sd(as.numeric(returns))

# Kernel Density Estimation -----------------------------------------------

set.seed(112358)
r <- rnorm(n=10000, mean=drillMean, sd=drillSD)
P0 <- 2279.80
P1 <- P0*(1+r)
rm(drillmeans)
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

P30 <- rep(0,10000)
for(i in 1:10000){
  P0 <- 1000
  r <- rnorm(n=1, mean=0.0879, sd=0.1475)
  
  Pt <- P0*(1 + r)
  
  for(j in 1:29){
    r <- rnorm(n=1, mean=0.0879, sd=0.1475)
    Pt <- Pt*(1+r)
  }
  P30[i] <- Pt
}