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
     bty = "n", xlab = "hour", ylab = "cumulative hazard",
     main = "lognormal distribution")
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
