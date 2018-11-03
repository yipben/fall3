# install.packages('muhaz')
# install.packages('survminer')
library(survival)
library(survminer)
library(muhaz)

pump <- read.csv("C:\\Users\\Ryan Carr\\Documents\\MSA1 PC\\survival\\survivalcsv\\katrina.csv", header = TRUE)
summary(pump)
str(pump)

pump_fit <- survfit(Surv(time = hour,event = survive == 0) ~ 1, data = pump)
pump_fit

# summary statistics for each type of failure
# what % of pumps survived the hurricane?
summary(pump) # 41.04% survived the hurricane

# what percentage of pumps in each failure?
# total of 770-316 = 454 = 58.96% failed
fail_none = pump[pump$reason==0,] # 316/770 = 41.04% did not fail, 100% of those that failed
fail_flood = pump[pump$reason==1,] # 115/770 = 14.94% failed, 115/454 = 25.33% of those that failed
fail_motor = pump[pump$reason==2,] # 112/770 = 14.55% failed, 112/454 = 24.67% of those that failed
fail_surge = pump[pump$reason==3,] # 111/770 = 14.42% failed, 111/454 = 24.45% of those that failed
fail_jam = pump[pump$reason==4,] # 116/770 = 15.06% failed, 116/454 = 25.55% of those that failed

# what are the median survival times?
# overall median survival time 
pump_fit <- survfit(Surv(time = hour,event = survive == 0) ~ 1, data = pump)
pump_fit # median survival time: 45 hours 
         # the time at which the survival rate dropped below 50% is 45 hours

# Plot the survival curves for all pumps and the stratified curves (by reason).
# Discuss anything interesting that you find.

# Plot the survival curves for all pumps
ggsurvplot(pump_fit, data = pump, conf.int = FALSE, palette = "Set1")

# group by reason and plot
pump_fit_wreason <- survfit(Surv(time = hour,event = survive == 0) ~ reason, data = pump[pump$reason != 0,])
reason_labels = c('Flood','Motor','Surge','Jammed')
ggsurvplot(pump_fit_wreason, conf.int = TRUE, palette = "Set1",legend='top',
           legend.labs = reason_labels,legend.title = 'Reason for Failure: ',xlab='Time (hours)')


