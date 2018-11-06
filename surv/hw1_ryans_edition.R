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
flood_fit_exponential <- survreg(Surv(time = hour, event = reason == 1) ~  backup +
                     bridgecrane + servo + trashrack + elevation +
                     slope + age, data = kat, dist = "exponential")
summary(flood_fit_exponential)
exp(coef(flood_fit_exponential))

flood_fit_weibull <- survreg(Surv(time = hour, event = reason == 1) ~  backup +
                       bridgecrane + servo + trashrack + elevation +
                       slope + age, data = kat, dist = "weibull")
summary(flood_fit_weibull)
exp(coef(flood_fit_weibull))

flood_fit_lognormal <- survreg(Surv(time = hour, event = reason == 1) ~  backup +
                               bridgecrane + servo + trashrack + elevation +
                               slope + age, data = kat, dist = "lognormal")
summary(flood_fit_lognormal)
exp(coef(flood_fit_lognormal))

flood_fit_loglogist <- survreg(Surv(time = hour, event = reason == 1) ~  backup +
                                 bridgecrane + servo + trashrack + elevation +
                                 slope + age, data = kat, dist = "loglogistic")
summary(flood_fit_loglogist)
exp(coef(flood_fit_loglogist))

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
     bty = "n", xlab = "hour", ylab = "weibull hazard",
     main = "weibull distribution")

flood_flex_lognormal <- flexsurvreg(Surv(time = hour, event = reason == 1) ~ 
                                    backup + bridgecrane + servo + trashrack + 
                                    elevation + slope + age, data = kat, 
                                    dist = "lognormal")
plot(flood_flex_lognormal, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1,
     bty = "n", xlab = "hour", ylab = "lognormal hazard",
     main = "lognormal distribution")

flood_flex_loglogist <- flexsurvreg(Surv(time = hour, event = reason == 1) ~ 
                                      backup + bridgecrane + servo + trashrack + 
                                      elevation + slope + age, data = kat, 
                                      dist = "llogis")
plot(flood_flex_loglogist, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1,
     bty = "n", xlab = "hour", ylab = "loglogistic hazard",
     main = "loglogistic distribution")








