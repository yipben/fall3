library(data.table)
library(readxl)
library(dplyr)
library(sqldf)
library(ks)
library(triangle)


# Read in Files -----------------------------------------------------------
OilPrice = read_xlsx("C:\\Users\\sonja\\Documents\\Courses\\502 (LA TS Logistic SQL R Python Visualization DataMining)\\Simulations\\Assignments\\HW1\\Analysis_Data.xlsx")
DrillingCost = read_excel("C:\\Users\\sonja\\Documents\\Courses\\502 (LA TS Logistic SQL R Python Visualization DataMining)\\Simulations\\Assignments\\HW1\\Analysis_Data.xlsx", sheet=2, skip = 2)
DrillingCost = DrillingCost[-nrow(DrillingCost),] #take out 2007

# Clean Up Data -----------------------------------------------------------

DrillingCost =
  DrillingCost %>%
  rename(Oil_Cost = "U.S. Nominal Cost per Crude Oil Well Drilled (Thousand Dollars per Well)",
         Gas_Cost = "U.S. Nominal Cost per Natural Gas Well Drilled (Thousand Dollars per Well)",
         DryWell_Cost = "U.S. Nominal Cost per Dry Well Drilled (Thousand Dollars per Well)",
         Oil_Return = "Arithmetic Return - Crude Oil",
         Gas_Return = "Arithmetic Return - Natural Gas",
         DryWell_Return = "Arithmetic Return - Dry Well")
#DrillingCost$Date = substr(DrillingCost$Date,1,4)
#DrillingCost$AvgCost = (as.numeric(DrillingCost$Oil_Cost) + as.numeric(DrillingCost$Gas_Cost) + 
                          #as.numeric(DrillingCost$DryWell_Cost))/3
#head(DrillingCost)
#drill = sqldf("select Date, AvgCost, Oil_Return, Gas_Return, DryWell_Return
              #from DrillingCost")
#drill = drill[32:47,]

DrillingCost = DrillingCost[32:47,]
All_Return=c(DrillingCost$Oil_Return,DrillingCost$Gas_Return,DrillingCost$DryWell_Return)

#Histogram of actual returns
hist(as.numeric(All_Return))

#Normal parameters for SIMULATION1
allmean=mean(as.numeric(All_Return))
allsd=sd(as.numeric(All_Return))


# Kernel Density Estimation for SIMULATION2
Density.Pt <- density(as.numeric(All_Return), bw="SJ-ste")
Density.Pt
kdemodel <- rkde(fhat=kde(as.numeric(All_Return), h=0.07935), n=1000)
#Histogram of kde of actual returns
hist(kdemodel, breaks=50, main='Estimated One Year Value Distribution', xlab='Final Value')



#SIMULATION1 - normal, triangle, triangle
results <- rep(0,10000)

for(i in 1:10000){
  
  P1=2279.80 #2006
  r0=rnorm(n=1, mean=allmean, sd=allsd)
  P2 <- P1*(1+r0)
  #P3=P2
  
  for(j in 1:5){
    r <- rnorm(n=1, mean=allmean, sd=allsd) #rkde(fhat=kde(P1, h=68.2), n=1000)
    P2 <- P2*(1+r)
    #P3 <-c(P2,P3)
  }
  
  for(j in 1:3){
    #P3=tail(P3, n=1)
    r <- rtriangle(n=1, -0.22, -0.07, -0.0917)
    P2 <- P2*(1+r)
    #P3 <-c(P2,P3)
  }
  
  for(j in 1:4){
    #P3=tail(P3, n=1)
    r <- rtriangle(n=1, 0.02, 0.06, 0.05)
    P2 <- P2*(1+r)
    #P2=c(P2,P3)
  }
  results[i] <- P2
}
results #10000 runs of 2019
hist(results)
mean(results)
sd(results)


#SIMULATION2 - kernaldensity, triangle, triangle
results <- rep(0,10000)

for(i in 1:10000){
  #set.seed(12345)
  P1=2279.80 #2006
  r0=rkde(fhat=kde(as.numeric(All_Return), h=0.07935), n=1)
  P2 <- P1*(1+r0)
  #P3=P2
  #a=a
  
  for(j in 1:5){
    r <- rkde(fhat=kde(as.numeric(All_Return), h=0.07935), n=1)
    P2 <- P2*(1+r)
    #P3 <-c(P2,P3)
  }
  
  for(j in 1:3){
    #P3=tail(P3, n=1)
    r <- rtriangle(n=1, -0.22, -0.07, -0.0917)
    P2 <- P2*(1+r)
    #P3 <-c(P2,P3)
  }
  
  for(j in 1:4){
    #P3=tail(P3, n=1)
    r <- rtriangle(n=1, 0.02, 0.06, 0.05)
    P2 <- P2*(1+r)
    #P3=c(P2,P3)
  }
  results[i] <- P2
}

results #10000 runs of 2019
View(results)
hist(results)
mean(results)
sd(results)
