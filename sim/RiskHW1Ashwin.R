library(data.table)
library(readxl)
library(dplyr)
library(sqldf)
library(ks)
library(triangle)
library(ggplot2)


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
drill = drill[32:47,]



# distribution ------------------------------------------------------------

returns = as.numeric(c(drill$Oil_Return, drill$Gas_Return, drill$DryWell_Return))
drillMean = mean(returns)
drillSD = sd(returns)
hist(returns)

ggplot(as.data.table(returns), aes(x=(returns))) + 
  geom_histogram(colour="black", fill="sky blue", bins=10, alpha=.7) + 
  ylim(c(0,15)) +
  theme_minimal()


# Simulation based on NORMALITY -------------------------------------------

set.seed(12345)
P2019 <- rep(0,100000)
for(i in 1:1000000){
  P0 <- 2279.80
  r <- rnorm(n=1, mean=drillMean, sd=drillSD)
  
  Pt <- P0*(1 + r)
  
  for(j in 1:5){
    r <- rnorm(n=1, mean=drillMean, sd=drillSD)
    Pt <- Pt*(1+r)
  }
  
  for(k in 1:3){
    r <- rtriangle(a=-.22, b=-.07, c=-.0917)
    Pt <- Pt*(1+r)
  }  
  
  for(k in 1:4){
    r <- rtriangle(a=.02, b=.06, c=.05)
    Pt <- Pt*(1+r)
  }
  P2019[i] <- Pt
}

mean(P2019) #3768.79
sd(P2019)   #3268.98
summary(P2019) #Median 7604

hist(P2019, breaks=50, main='2019 Value Distribution (Normality Assumption)', xlab='Final Value')
abline(v = P0, col="red", lwd=2)
mtext("Initial Value", at=P0, col="red")

ggplot(as.data.table(P2019), aes(x=P2019)) + 
  geom_histogram(colour="black", fill="sky blue", bins=30, alpha=.5) + 
  xlim(c(0,25000))+
  labs(title="2019 Simulated Value Distribution (Normality Assumed)", x="Dollars", y="Count")+
  theme_minimal()+
  geom_vline(xintercept=P0, color="red", linetype="dashed", size=1)+
  geom_text(aes(x=700), label="Value at 2006", y=85000)+
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"),
        plot.title = element_text(hjust = 0.5, size=16))
    

# Distribution Selection - Kernel Estimation #

Density.P1 <- density(as.numeric(returns), bw="SJ-ste")
Density.P1  #bandwidth = 0.07935

# Multiple Input Probability Distributions #
P2019KDE <- rep(0,100000)
for(i in 1:100000){
  P0 <- 2279.80  #value of 2006
  r <- rkde(fhat=kde(returns, h=0.07935), n=1)   #not sure what to use for n=???
  Pt <- P0*(1 + r)
  
  for(j in 1:5){
    r <- rkde(fhat=kde(returns, h=0.07935), n=1)
    Pt <- Pt*(1+r)
  }
  
  for(k in 1:3){
    r <- rtriangle(a=-.22, b=-.07, c=-.0917)     
    Pt <- Pt*(1+r)
  }
  
  for(k in 1:4){
    r <- rtriangle(a=.02, b=.06, c=.05)
    Pt <- Pt*(1+r)
  }
  P2019KDE[i] <- Pt
}

sd(P2019KDE)   
summary(P2019KDE) 

hist(P2019KDE, breaks=50, main='2019 Value Distribution (KDE Assumption)', xlab='Final Value')
abline(v = P0, col="red", lwd=2)
mtext("Initial Value", at=P0, col="red")
