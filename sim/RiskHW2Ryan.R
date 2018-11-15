# Ryan hw2 simulation and risk

library(data.table)
library(readxl)
library(dplyr)
library(sqldf)
library(ks)
library(triangle)
library(ExtDist)


# Read in Files -----------------------------------------------------------
OilPrice = read_xlsx('data/Analysis_data.xlsx')
DrillingCost = read_excel("data/Analysis_Data.xlsx", sheet=2, skip = 2)
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

DrillingCost = DrillingCost[32:47,] # 91 - 2006
All_Return=c(DrillingCost$Oil_Return,DrillingCost$Gas_Return,DrillingCost$DryWell_Return)

#Histogram of actual returns
hist(as.numeric(All_Return), breaks = 50)

#Normal parameters for SIMULATION1
allmean=mean(as.numeric(All_Return))
allsd=sd(as.numeric(All_Return))


#Histogram of kde of actual returns
Density.Pt <- density(as.numeric(All_Return), bw="SJ-ste")
Density.Pt
kdemodel <- rkde(fhat=kde(as.numeric(All_Return), h=0.07935), n=1000)
hist(kdemodel, breaks=50, main='Estimated One Year Value Distribution', xlab='Final Value')


#SIMULATION1 - normal, triangle, triangle
numberOfIterations = 1000000
results <- rep(0,numberOfIterations)

for(i in 1:numberOfIterations){
  P1=2279.80 * 1000#2006
  r0=rnorm(n=1, mean=allmean, sd=allsd)
  P2 <- P1*(1+r0)
  for(j in 2:6){
    r <- rnorm(n=1, mean=allmean, sd=allsd) #rkde(fhat=kde(P1, h=68.2), n=1000)
    P2 <- P2*(1+r)
  }

  for(j in 7:9){
    r <- rtriangle(n=1, -0.22, -0.07, -0.0917)
    P2 <- P2*(1+r)
  }

  for(j in 10:13){
    r <- rtriangle(n=1, 0.02, 0.06, 0.05)
    P2 <- P2*(1+r)
  }
  results[i] <- P2
}
results #10000 runs of 2019
hist(results, breaks = 200)
mean(results)
sd(results)


#SIMULATION2 - kernaldensity, triangle, triangle
# results <- rep(0,numberOfIterations)
# 
# for(i in 1:numberOfIterations){
#   #set.seed(12345)
#   P1=2279.80 * 1000#2006
#   r0=rkde(fhat=kde(as.numeric(All_Return), h=0.07935), n=1)
#   P2 <- P1*(1+r0)
#   #P3=P2
#   #a=a
# 
#   for(j in 1:5){
#     r <- rkde(fhat=kde(as.numeric(All_Return), h=0.07935), n=1)
#     P2 <- P2*(1+r)
#     #P3 <-c(P2,P3)
#   }
# 
#   for(j in 1:3){
#     #P3=tail(P3, n=1)
#     r <- rtriangle(n=1, -0.22, -0.07, -0.0917)
#     P2 <- P2*(1+r)
#     #P3 <-c(P2,P3)
#   }
# 
#   for(j in 1:4){
#     #P3=tail(P3, n=1)
#     r <- rtriangle(n=1, 0.02, 0.06, 0.05)
#     P2 <- P2*(1+r)
#     #P3=c(P2,P3)
#   }
#   results[i] <- P2
# }
# 
# results #10000 runs of 2019
# hist(results, breaks = 200)
# mean(results)
# sd(results)

########################################################################
#                                                                      #
# Begin                                                                #
#       HW2                                                            #
#                                                                      #
########################################################################
years_ahead = 15

### year 0 expenses ###

# seismic and lease costs
leasedAcresPerWell_m = 600 # normal
leasedAcresPerWell_std = 50
pricePerAcre = 960
seismicSectionsPerWell_m = 3 # normal
seismicSectionsPerWell_std = 0.35
pricePerSeismicSection = 43000

# completion costs
pricePerWellPrep_m = 390000
pricePerWellPrep_std = 50000

# professional overhead costs - triangular distribution
# include these for year 0 and each additional year
profMostLikelyAvg = 215000
profMostLikelyMin = 172000
profMostLikelyMax = 279500
### end year 0 expenses ###

### production risk ###

# follow lognormal distribution with underlying  normal disttribution
# mean = 6, std = 0.28
initProd_m = 420 # barrels of oil per day
initProd_std = 120
initProd_declineRate_corr = 0.64
rateOfDecline_a = 0.15
rateOfDecline_b = 0.32
### end production risk ###

### revenue risk ###

# load csv for price projections
# used to build distributions for the next 15 years
# (2019, 2020, 2021, ...) use triangle distribution
priceProjections_df = read_excel("data/Analysis_Data.xlsx", sheet=1, skip = 2) %>%
  rename(oil_high = "High Oil Price",
         oil_low = "Low Oil Price",
         oil_expected = "AEO2018 Reference")
revenueInterestRate_m = 0.75
revenueInterestRate_std = 0.02 # per well for life of well
# oil price * annual production * revenueInterestRate * taxExpense= annual revenue
### end revenue risk ###

### operating (production) expenses ### 

operatingCPB_m = 2.25 # these should be the same for each well in a given year,
operatingCPB_std = 0.30 # but change year to year according to normal distribution
taxExpense = 0.046
### end operating (production) expenses ###

wacc = 1.10 # weighted average cost of capital 

### cost of a single dry well ### 


resultsDryWell <- rep(0,numberOfIterations)
for (i in 1:numberOfIterations){
  leasedAcresPerWell = rnorm(n = 1, mean = leasedAcresPerWell_m, sd = leasedAcresPerWell_std)
  seismicSectionsPerWell = rnorm(n = 1, mean = seismicSectionsPerWell_m, 
                          sd = seismicSectionsPerWell_std)
  professionalCost = rtriangle(n=1, profMostLikelyMin, profMostLikelyMax, 
                               profMostLikelyAvg)
  drillingCosts = results[i]
  costOfDryWell = (pricePerAcre * leasedAcresPerWell) + 
                  (pricePerSeismicSection * seismicSectionsPerWell) + 
                   professionalCost + drillingCosts
  resultsDryWell[i] = costOfDryWell
}
dryHist = hist(resultsDryWell/1000000, breaks = 200)
ApproxQuantile(dryHist, c(0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99)) #quantiles
range(resultsDryWell)

library(ggplot2)
library(data.table)
ggplot(as.data.table(resultsDryWell), aes(x=resultsDryWell/1000000)) + 
  geom_histogram(colour="black", fill="slateblue3", alpha=.5) + 
  # xlim(c(0,18000))+
  labs(title="Simulated Cost of a Single Dry Well\n", x="\nCost($Millions)", y="Frequency\n")+
  theme_minimal()+
  theme(axis.text=element_text(size=13), axis.title=element_text(size=14,face="bold"),
        plot.title = element_text(hjust = 0.5, size=16))


### net present value of a single wet well ###


# fancy stuff to make lognormal draws work
location <- log(initProd_m^2 / sqrt(initProd_std^2 + initProd_m^2))
shape <- sqrt(log(1 + (initProd_std^2 / initProd_m^2)))
print(paste("location:", location))
print(paste("shape:", shape))

R <- matrix(data=cbind(1, 0.64, 0.64, 1), nrow=2)
U <- t(chol(R)) # choleski decomposition the matrix i need to multiply my data by to get the correlated output, t is transpose
#Perc.B <- 0.7 # percent in bonds
#Perc.S <- 0.3 # % in stocks
#Initial <- 1000 # initial investment

# correlation matrix assumes var = 1, so we must standardize
standardize <- function(x){
  x.std = (x - mean(x))/sd(x)
  return(x.std)
}
# so that the answer is not in standard dollars, we want real $ 
destandardize <- function(x.std, x){
  x.old = (x.std * sd(x)) + mean(x)
  return(x.old)
}

initProductionBOPD <- rlnorm(n = numberOfIterations, location, shape) # lognormal draw # how does this work with using only one well
rateOfDecline = runif(n = numberOfIterations, min = rateOfDecline_a, max = rateOfDecline_b)
initProd_decRate_standard = cbind(standardize(rateOfDecline), standardize(initProductionBOPD))
corrStruct <- U %*% t(initProd_decRate_standard) # multiplying, altering data to bring in the correlation structure, %*% : matrix multiplication
corrStruct <- t(corrStruct) # we now have a correlated matrix :)

initProd_declineRate_final <- cbind(destandardize(corrStruct[,1], rateOfDecline), destandardize(corrStruct[,2], initProductionBOPD))

netPresentValue = rep(0, numberOfIterations)
priceProjections_df = priceProjections_df[1:years_ahead, ]
for(j in 1:numberOfIterations){
  yearEndRate <- rep(0, years_ahead)
  priceProjections <- rep(0, years_ahead)
  revenueInterestRate <- rep(0, years_ahead)
  yearlyProduction = rep(0, years_ahead)
  
  drillingCosts = results[j]
  completionCost = rnorm(n = 1, mean = pricePerWellPrep_m, sd = pricePerWellPrep_std)
  operatingCPB = rnorm(n = years_ahead, mean = operatingCPB_m, sd = operatingCPB_std)
  leasedAcresPerWell = rnorm(n = 1, mean = leasedAcresPerWell_m, sd = leasedAcresPerWell_std)
  seismicSectionsPerWell = rnorm(n = 1, mean = seismicSectionsPerWell_m, 
                                 sd = seismicSectionsPerWell_std)
  revenueInterestRate = rnorm(n = 1, mean = revenueInterestRate_m,
                                 sd = revenueInterestRate_std)
  professionalCost = rtriangle(n=1, profMostLikelyMin, profMostLikelyMax, 
                               profMostLikelyAvg)
  
  for(i in 1:years_ahead){  
    if(i == 1){
      yearBegin = initProd_declineRate_final[j,2] #Production Rate
      declineRate = initProd_declineRate_final[j,1] #Decline Rate
      yearEndRate = (1 - declineRate) * yearBegin  #Year End Rate
      yearlyProduction[i] = 365 * ((yearBegin + yearEndRate) / 2) #Yearly production Volumes in barrels of oil
      
    }
    else{
      yearBegin = yearEndRate
      yearEndRate = (1 - declineRate) * yearBegin
      yearlyProduction[i] = 365 * ((yearBegin + yearEndRate) / 2)
    }
    
    priceProjections[i] = rtriangle(n = 1, as.numeric(priceProjections_df[i,3]), #Price Projections
                                  as.numeric(priceProjections_df[i,2]), 
                                  as.numeric(priceProjections_df[i,4]))
  }
  operatingCosts = operatingCPB * yearlyProduction
  acresCosts = leasedAcresPerWell * pricePerAcre
  seismicSectionsCosts = seismicSectionsPerWell * pricePerSeismicSection
  
  annualRevenues =  (1 - taxExpense) * revenueInterestRate * (priceProjections * yearlyProduction)
  netSales = annualRevenues - operatingCosts - professionalCost
  
  initialCosts = seismicSectionsCosts + acresCosts + completionCost + professionalCost +
                                       drillingCosts
  result1 = rep(0,years_ahead)
  for(i in 1:years_ahead){
    result1[i] = netSales[i]/wacc^i
  }
  netPresentValue[j] = -initialCosts + sum(result1)
}
npvHist = hist(netPresentValue/1000000, breaks = 200) # net present value in millions of dollars

ggplot(as.data.table(netPresentValue), aes(x=netPresentValue/1000000)) + 
  geom_histogram(colour="black", fill="slateblue3", alpha=.5) + 
  # xlim(c(0,18000))+
  labs(title="NPV of a Single Wet Well\n", x="\nNPV($Millions)", y="Frequency\n")+
  theme_minimal()+
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"),
        plot.title = element_text(hjust = 0.5, size=16, face="bold"))

library(HistogramTools)
# conclude 95% of wet wells have a future value greater than $4.5 million 
ApproxQuantile(npvHist, c(0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99)) #quantiles
summary(netPresentValue)



















