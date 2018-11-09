library(data.table)
library(readxl)
library(dplyr)
library(sqldf)
library(ks)
library(triangle)
library(sjstats)

# Read in Files -----------------------------------------------------------
OilPrice = read_xlsx("C:\\Users\\sonja\\Documents\\Courses\\502 (LA TS Logistic SQL R Python Visualization DataMining)\\Simulation and Risk\\Assignments\\HW1\\Analysis_Data.xlsx")
DrillingCost = read_excel("C:\\Users\\sonja\\Documents\\Courses\\502 (LA TS Logistic SQL R Python Visualization DataMining)\\Simulation and Risk\\Assignments\\HW1\\Analysis_Data.xlsx", sheet=2, skip = 2)
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

#Histogram of historical rates of change (ie. return)
hist(as.numeric(All_Return))

ggplot(as.data.table(All_Return), aes(x=as.numeric(All_Return))) + 
  geom_histogram(colour="black", fill="sky blue", alpha=.7, bins=10) + 
  theme_minimal()+
  labs(title="Distribution of Rates of Change\n", x="\n%Change", y="Count\n")+
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"),
        plot.title = element_text(hjust = 0.5, size=16))

#QQ plot
qplot(sample=as.numeric(All_Return), data=as.data.table(All_Return))+
  theme_minimal()+
  stat_qq_line(color="blue")+
  labs(title="Q-Q Plot\n", x="\nTheoretical Quantiles", y="Sample Quantiles\n")+
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"),
        plot.title = element_text(hjust = 0.5, size=16))


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
P2019norm <- rep(0,100000)

for(i in 1:100000){
  
  P0=2279.80 #2006
  r0=rnorm(n=1, mean=allmean, sd=allsd)
  P1 <- P0*(1+r0)
  
  for(j in 1:5){
    r <- rnorm(n=1, mean=allmean, sd=allsd) #rkde(fhat=kde(P1, h=68.2), n=1000)
    P1 <- P1*(1+r)
  }
  
  for(j in 1:3){
    r <- rtriangle(n=1, -0.22, -0.07, -0.0917)
    P1 <- P1*(1+r)
  }
  
  for(j in 1:4){
    r <- rtriangle(n=1, 0.02, 0.06, 0.05)
    P1 <- P1*(1+r)
  }
  P2019norm[i] <- P1
}
P2019norm #10000 runs of 2019
hist(P2019norm)
mean(P2019norm)
median(P2019norm)
sd(P2019norm)
cv(P2019norm)
min(P2019norm)
max(P2019norm)

#Histogram for P2019norm
hist(P2019, breaks=50, main='2019 Value Distribution (Normality Assumption)', xlab='Final Value')
abline(v = P0, col="red", lwd=2)
mtext("Initial Value", at=P0, col="red")

ggplot(as.data.table(P2019norm), aes(x=P2019norm)) + 
  geom_histogram(colour="black", fill="sky blue", bins=30, alpha=.5) + 
  xlim(c(0,15000))+
  labs(title="2019 Simulated Value Distribution (Normality Assumed)\n", x="\nDrilling Cost ($)", y="Count\n")+
  theme_minimal()+
  geom_vline(xintercept=P0, color="red", linetype="dashed", size=1)+
  geom_text(aes(x=700), label="Value at 2006", y=13000)+
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"),
        plot.title = element_text(hjust = 0.5, size=16))

#SIMULATION2 - kernaldensity, triangle, triangle
P2019KDE <- rep(0,100000)

for(i in 1:100000){
  #set.seed(12345)
  P0=2279.80 #2006
  r0=rkde(fhat=kde(as.numeric(All_Return), h=0.07935), n=1)
  P1 <- P0*(1+r0)
  
  for(j in 1:5){
    r <- rkde(fhat=kde(as.numeric(All_Return), h=0.07935), n=1)
    P1 <- P1*(1+r)
  }
  
  for(j in 1:3){
    r <- rtriangle(n=1, -0.22, -0.07, -0.0917)
    P1 <- P1*(1+r)
  }
  
  for(j in 1:4){
    r <- rtriangle(n=1, 0.02, 0.06, 0.05)
    P1 <- P1*(1+r)
  }
  P2019KDE[i] <- P1
}

P2019KDE #10000 runs of 2019
#View(P2019KDE)
hist(P2019KDE)
mean(P2019KDE)
median(P2019KDE)
sd(P2019KDE)
cv(P2019KDE)
min(P2019KDE)
max(P2019KDE)

#Histogram for P2019KDE
hist(P2019KDE, breaks=50, main='2019 Value Distribution (KDE Assumption)', xlab='Final Value')
abline(v = P0, col="red", lwd=2)
mtext("Initial Value", at=P0, col="red")

ggplot(as.data.table(P2019KDE), aes(x=P2019KDE)) + 
  geom_histogram(colour="black", fill="sky blue", bins=30, alpha=.5) + 
  xlim(c(0,18000))+
  labs(title="2019 Simulated Value Distribution (KDE)\n", x="\nDrilling Cost (thousands dollars)", y="Count\n")+
  theme_minimal()+
  geom_vline(xintercept=P0, color="red", linetype="dashed", size=1)+
  geom_text(aes(x=700), label="Value at 2006", y=13000)+
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"),
        plot.title = element_text(hjust = 0.5, size=16))
  geom_vline(xintercept=P0, color="red", linetype="dashed", size=1)+
  geom_text(aes(x=700), label="Value at 2006", y=13000)+
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"),
        plot.title = element_text(hjust = 0.5, size=16))

#Overlapping P2019norm P2019KDE
hist(P2019norm, col=rgb(0.1,0.1,0.1,0.5),xlim=c(0,18750), ylim=c(0,3000), main="Overlapping Histogram")
hist(P2019KDE, col=rgb(0.8,0.8,0.8,0.5), add=T) #Blue: col=rgb(1,0,0,0.5) Red: col=rgb(0,0,1,0.5)
box()
  
#Probabilities
length(which(P2019KDE<4000))/length(P2019KDE) #20% chance that cost > 5 million
  

  

