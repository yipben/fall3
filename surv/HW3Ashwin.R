library(readr)
library(dplyr)
library(muhaz)
library(ggplot2)
library(survival)
library(survminer)
library(ggfortify)
library(flexsurv)

kat <- read_csv("data/katrina.csv")

# create new variables for later on
kat <- kat %>%
  mutate(reason3 = if_else(reason == 0, "survived",
                           if_else(reason == 1, "flood", 
                                   if_else(reason == 2, "motor", 
                                           if_else(reason == 3, "surge", "jammed")))),
         failed = if_else(survive == 1, 0, 1),
         hour2 = if_else(survive == 1, as.integer(49), hour))

# filter datasets
survive <- kat %>%
  filter(reason == 0)
failed <- kat %>%
  filter(survive == 0)

flood <- kat %>%
  filter(reason == 1)
motor <- kat %>%
  filter(reason == 2)
surge <- kat %>%
  filter(reason == 3)
jammed  <- kat %>%
  filter(reason == 4)


# percentages
mean(kat$survive)
nrow(flood)/nrow(kat)
nrow(motor)/nrow(kat)
nrow(surge)/nrow(kat)
nrow(jammed)/nrow(kat)


# median survival times
median(kat$hour)
median(flood$hour)
median(motor$hour)
median(surge$hour)
median(jammed$hour)


# fit survival curves
all_fit <- survfit(Surv(hour, survive == 0) ~ 1, data = kat)
autoplot(all_fit)
ggsurvplot(all_fit, conf.int = TRUE, palette = "Set1",legend='top',
           legend.title = 'Reason for Failure: ',
           xlab='Time (hours)') # Figure 2

reason_fit <- survfit(Surv(hour, survive == 0) ~ reason3, data = failed)
autoplot(reason_fit)


# log-rank tests
pairwise_survdiff(Surv(hour, survive == 0) ~ reason3, data = failed, rho = 0) # Appendix 1

# hazard function
haz <- kphaz.fit(kat$hour2, kat$failed)
kphaz.plot(haz, main = "Overall Hazard") # Figure 3

ggsurvplot(reason_fit, fun = "cumhaz")
ggsurvplot(all_fit, fun = "cumhaz")

# fitting of AFT models with different distributions

# exponential
flood_fit_exponential <- survreg(Surv(time = hour, event = reason == 1) ~  backup +
                                   bridgecrane + servo + trashrack + elevation +
                                   slope + age, data = kat, dist = "exponential")
summary(flood_fit_exponential)

# weibull
flood_fit_weibull <- survreg(Surv(time = hour, event = reason == 1) ~  backup +
                               bridgecrane + servo + trashrack + elevation +
                               slope + age, data = kat, dist = "weibull")
summary(flood_fit_weibull)

# loglogistic
flood_fit_loglogist <- survreg(Surv(time = hour, event = reason == 1) ~  backup +
                                 bridgecrane + servo + trashrack + elevation +
                                 slope + age, data = kat, dist = "loglogistic")
summary(flood_fit_loglogist)

# lognormal
flood_fit_lognormal <- survreg(Surv(time = hour, event = reason == 1) ~  backup +
                                 bridgecrane + servo + trashrack + elevation +
                                 slope + age, data = kat, dist = "lognormal")
summary(flood_fit_lognormal)



# checking distributions visually for each type of distribution
flood_flex_exponential <- flexsurvreg(Surv(time = hour, event = reason == 1) ~ 
                                        backup + bridgecrane + servo + trashrack + 
                                        elevation + slope + age, data = kat, 
                                      dist = "exponential")
plot(flood_flex_exponential, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1,
     bty = "n", xlab = "hour", ylab = "cumulative hazard",
     main = "exponential distribution")

flood_flex_weibull <- flexsurvreg(Surv(time = hour, event = reason == 1) ~ 
                                    backup + bridgecrane + servo + trashrack + 
                                    elevation + slope + age, data = kat, 
                                  dist = "weibull")
plot(flood_flex_weibull, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1,
     bty = "n", xlab = "hour", ylab = "cumulative hazard",
     main = "weibull distribution")

flood_flex_loglogist <- flexsurvreg(Surv(time = hour, event = reason == 1) ~ 
                                      backup + bridgecrane + servo + trashrack + 
                                      elevation + slope + age, data = kat, 
                                    dist = "llogis")
plot(flood_flex_loglogist, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1,
     bty = "n", xlab = "hour", ylab = "cumulative hazard",
     main = "loglogistic distribution")

### lognormal looks to be the best fit ###
flood_flex_lognormal <- flexsurvreg(Surv(time = hour, event = reason == 1) ~ 
                                      backup + bridgecrane + servo + trashrack + 
                                      elevation + slope + age, data = kat, 
                                    dist = "lognormal")
plot(flood_flex_lognormal, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1,
     bty = "n", xlab = "Hour", ylab = "Cumulative Hazard",
     main = "Lognormal Distribution",cex.lab=1.25)

###                                   ###
# get coefficient estimates
summary(flood_fit_lognormal)
exp(coef(flood_fit_lognormal))

# so now for the stations who didn't get backup pump, we're going to predict
# the mean time to pump failure if they HAD gotten it
# to do this, we're assuming that they'll still have the event at the same
# estimated survival probability as they did previously
kat_nobackup <- kat %>%
  # first, we need to get the linear predictor from the old model
  mutate(old_lp = predict(flood_fit_lognormal, type = "lp"),
         # add ID variable so we know which subjects they are
         ID = row_number()) %>%
  # next, we're only interested in those who had the event AND no backup pump
  dplyr::filter(reason == 1, backup == 0) %>%
  # now i'm doing two things:
  # 1. find the survival prob at the time of their event
  # 2. pretending they did have financial aid (change backup from 0 to 1)
  mutate(old_time = hour,
         surv_prob = 1 - psurvreg(old_time,
                                  mean = old_lp,
                                  scale = flood_fit_lognormal$scale,
                                  distribution = flood_fit_lognormal$dist),
         old_pump = backup,
         backup = old_pump + 1)

# now with that dataset, i need to find their new time
results_backup <- kat_nobackup %>%
  # estimate their new linear predictor value if they had backup pump
  mutate(new_lp = predict(flood_fit_lognormal, newdata = kat_nobackup, type = "lp"),
         # now, keeping the same survival probability as before, get the time
         # corresponding to the new_lp value
         new_time = qsurvreg(1 - surv_prob,
                             mean = new_lp,
                             scale = flood_fit_lognormal$scale,
                             distribution = flood_fit_lognormal$dist),
         # and compute the difference
         pred_time_diff = new_time - old_time) %>%
  select(ID, surv_prob, old_time, new_time, pred_time_diff)
results_backup = arrange(results_backup, desc(pred_time_diff))

########################################################################
kat_nobridgecrane <- kat %>%
  mutate(old_lp = predict(flood_fit_lognormal, type = "lp"),
         ID = row_number()) %>%
  dplyr::filter(reason == 1, bridgecrane == 0) %>%
  mutate(old_time = hour,
         surv_prob = 1 - psurvreg(old_time,
                                  mean = old_lp,
                                  scale = flood_fit_lognormal$scale,
                                  distribution = flood_fit_lognormal$dist),
         old_pump = bridgecrane,
         bridgecrane = old_pump + 1)

results_bridgecrane <- kat_nobridgecrane %>%
  mutate(new_lp = predict(flood_fit_lognormal, newdata = kat_nobridgecrane, type = "lp"),
         new_time = qsurvreg(1 - surv_prob,
                             mean = new_lp,
                             scale = flood_fit_lognormal$scale,
                             distribution = flood_fit_lognormal$dist),
         pred_time_diff = new_time - old_time) %>%
  select(ID, surv_prob, old_time, new_time, pred_time_diff)
results_bridgecrane = arrange(results_bridgecrane, desc(pred_time_diff))

########################################################################
kat_noservo <- kat %>%
  mutate(old_lp = predict(flood_fit_lognormal, type = "lp"),
         ID = row_number()) %>%
  dplyr::filter(reason == 1, servo == 0) %>%
  mutate(old_time = hour,
         surv_prob = 1 - psurvreg(old_time,
                                  mean = old_lp,
                                  scale = flood_fit_lognormal$scale,
                                  distribution = flood_fit_lognormal$dist),
         old_pump = servo,
         servo = old_pump + 1)

results_servo <- kat_noservo %>%
  mutate(new_lp = predict(flood_fit_lognormal, newdata = kat_noservo, type = "lp"),
         new_time = qsurvreg(1 - surv_prob,
                             mean = new_lp,
                             scale = flood_fit_lognormal$scale,
                             distribution = flood_fit_lognormal$dist),
         pred_time_diff = new_time - old_time) %>%
  select(ID, surv_prob, old_time, new_time, pred_time_diff)
results_servo = arrange(results_servo, desc(pred_time_diff))

########################################################################
kat_notrashrack <- kat %>%
  mutate(old_lp = predict(flood_fit_lognormal, type = "lp"),
         ID = row_number()) %>%
  dplyr::filter(reason == 1, trashrack == 0) %>%
  mutate(old_time = hour,
         surv_prob = 1 - psurvreg(old_time,
                                  mean = old_lp,
                                  scale = flood_fit_lognormal$scale,
                                  distribution = flood_fit_lognormal$dist),
         old_pump = trashrack,
         trashrack = old_pump + 1)

results_trashrack <- kat_notrashrack %>%
  mutate(new_lp = predict(flood_fit_lognormal, newdata = kat_notrashrack, type = "lp"),
         new_time = qsurvreg(1 - surv_prob,
                             mean = new_lp,
                             scale = flood_fit_lognormal$scale,
                             distribution = flood_fit_lognormal$dist),
         pred_time_diff = new_time - old_time) %>%
  select(ID, surv_prob, old_time, new_time, pred_time_diff)
results_trashrack = arrange(results_trashrack, desc(pred_time_diff))

# upgrade wells with highest pred_time_diff & new_time below 48
results_backup[22:37, ]
results_bridgecrane[1:15, ] # all negative, useless
results_servo[27:52, ]
results_trashrack[1:15, ] # all negative, useless

# the pumps whos predicted survival time is less than 48 hours
# no use in upgrading pump to survive past 48 hours
results_backup_sub = results_backup[22:67, ]
results_servo_sub = results_servo[27:63, ]

# these are the IDs of the pumps we should upgrade
upgrade_backup_id = c(364, 427, 322, 325, 376, 367)
upgrade_servo_id = c(319, 321, 408, 404, 399, 403, 377, 368, 416, 346, 
                     353, 384, 412, 406)
upgr_pmp_id = c(upgrade_backup_id, upgrade_servo_id)

upgr_backup = results_backup_sub %>% filter(ID == upgrade_backup_id)
upgr_servo = results_servo_sub %>% filter(ID == upgrade_servo_id)
upgr_these_20 = rbind(upgr_backup, upgr_servo)
mean(upgr_these_20$pred_time_diff) # upgrading these pumps gives an average increase of 12.9 hours

upgr_these_20 %>% arrange(ID)

######################## HW3 ######################## 

# Provide a follow-up to your last report and a set of recommendations summarizing
# the findings from your analysis. In this assignment, you will model motor and surge
# failures together and treat all other failure reasons as censored.
# In R, do Surv(time = hour, event = reason %in% c(2, 3)).

# Create both an AFT model and a Cox regression model with the following variables:
# backup, bridgecrane, servo, trashrack, elevation, slope, age. Which of these models 
# do you prefer? Why?

# none of these fit well
aft_fit <- flexsurvreg(Surv(time = hour, event = reason %in% c(2, 3)) ~ 
                         backup + bridgecrane + servo + trashrack + 
                         elevation + slope + age, data = kat, 
                       dist = "lognormal")
plot(aft_fit, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1,
     bty = "n", xlab = "Hour", ylab = "Cumulative Hazard",
     main = "Lognormal Distribution",cex.lab=1.25)

aft_fit1 <- flexsurvreg(Surv(time = hour, event = reason %in% c(2, 3)) ~ 
                          backup + bridgecrane + servo + trashrack + 
                          elevation + slope + age, data = kat, 
                        dist = "llogis")
plot(aft_fit1, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1,
     bty = "n", xlab = "Hour", ylab = "Cumulative Hazard",
     main = "Log logistic Distribution",cex.lab=1.25)

aft_fit2 <- flexsurvreg(Surv(time = hour, event = reason %in% c(2, 3)) ~ 
                          backup + bridgecrane + servo + trashrack + 
                          elevation + slope + age, data = kat, 
                        dist = "weibull")
plot(aft_fit2, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1,
     bty = "n", xlab = "Hour", ylab = "Cumulative Hazard",
     main = "Weibull Distribution",cex.lab=1.25)w

aft_fit3 <- flexsurvreg(Surv(time = hour, event = reason %in% c(2, 3)) ~ 
                          backup + bridgecrane + servo + trashrack + 
                          elevation + slope + age, data = kat, 
                        dist = "exponential")
plot(aft_fit3, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1,
     bty = "n", xlab = "Hour", ylab = "Cumulative Hazard",
     main = "exponential Distribution",cex.lab=1.25)

# fitting with cox
# create ID variable
kat$ID <- 1:nrow(kat)

cox_fit <- coxph(Surv(time = hour, event = reason %in% c(2, 3)) ~ 
                   backup + bridgecrane + servo + trashrack + 
                   elevation + slope + age, data = kat)
summary(cox_fit)

# plot survival curve
ggsurvplot(survfit(cox_fit), data = kat, legend = "none", break.y.by = 0.1,
           xlab = "\nWeek", ylab = "Survival Probability \n", title="Survival Curve Over Time",
           ggtheme=theme_minimal() + theme(plot.title =element_text(hjust=.5, face="bold", size=15), axis.title = element_text(size=13)))

# we want to use the cox model because the survival curve can't be fit with
# any of the distributions like lognormal expontential etc. also, for a cox
# ph, the actual distance between failure times isnt important because we are
# looking at hazard ratios. it looks at RANKS of failure times

# Is there any evidence that any of these effects might not
# be constant over time?

fit_zph <- cox.zph(cox_fit, transform = "km")
fit_zph # looks like age, trashrack, and backup might be correlated with time 
# because of low p values
plot(fit_zph, var = 'age', lwd = 2)
abline(h = 0, col = "red") # reference line at 0
abline(h = cox_fit$coef["age"], col = "purple", lty = 2, lwd = 2)# model estimate
title("Age Proportional Hazard")
legend(46,1, legend=c("Reference Line", "Model Estimate"), col=c("red","purple"),lty=1:2,cex=.8)
# conclude age varies with time
title()
       
plot(fit_zph, var = 'trashrack', lwd = 2)
abline(h = 0, col = "red") # reference line at 0
abline(h = cox_fit$coef["trashrack"], col = "purple", lty = 2, lwd = 2) # model estimate
legend(46,1, legend=c("Reference Line", "Model Estimate"), col=c("red","purple"),lty=1:2,cex=.8)
title("Trashrack Proportional Hazard")
# conclude trashrack varies with time

plot(fit_zph, var = 'backup', lwd = 2)
abline(h = 0, col = "red") # reference line at 0
abline(h = cox_fit$coef["backup"], col = "purple", lty = 2, lwd = 2) # model estimate
title("Proportional Hazard")
# conclude backup varies with time - however, not as obvious

fit_tdc <- coxph(Surv(hour, event = reason %in% c(2, 3)) ~ 
                   backup + bridgecrane + servo + trashrack + 
                   elevation + slope + age, data = kat,
                 tt = function(x, time, ...){x*log(time)})

running_pumps = NULL
kat$running_long = 0

for(i in 1:770){
  if(kat[i, 'failed'] == 1){
    hour_failed = as.integer(kat[i, 'hour']) - 1
    if(hour_failed != 0){
      if(is.na(kat[i, paste0('h', hour_failed)]) == FALSE){
        if(hour_failed - 12 >= 0){
          if(as.integer(kat[i, paste0('h', hour_failed)]) == 1 &&
             as.integer(kat[i, paste0('h', hour_failed - 1)]) == 1 &&
             as.integer(kat[i, paste0('h', hour_failed - 2)]) == 1 &&
             as.integer(kat[i, paste0('h', hour_failed - 3)]) == 1 &&
             as.integer(kat[i, paste0('h', hour_failed - 4)]) == 1 && 
             as.integer(kat[i, paste0('h', hour_failed - 5)]) == 1 &&
             as.integer(kat[i, paste0('h', hour_failed - 6)]) == 1 &&
             as.integer(kat[i, paste0('h', hour_failed - 7)]) == 1 &&
             as.integer(kat[i, paste0('h', hour_failed - 8)]) == 1 &&
             as.integer(kat[i, paste0('h', hour_failed - 9)]) == 1 &&
             as.integer(kat[i, paste0('h', hour_failed - 10)]) == 1 &&
             as.integer(kat[i, paste0('h', hour_failed - 11)]) == 1){
            kat[i, 'running_long'] = 1
            #running_pumps = rbind(running_pumps, kat[i, ])
          }
        }
      }
    }
  }
}


long_fit <- survfit(Surv(hour, reason3 == 'motor') ~ running_long, data = kat[kat$reason != 0,])
reason_labels = c('Motor Failure', 'Motor failure with pump running last 12 hours')
ggsurvplot(long_fit, conf.int = TRUE, palette = "Set1",legend='top',
           legend.title = 'Reason for Failure: ', legend.labs = reason_labels,
           xlab='Time (hours)') # Figure 2













