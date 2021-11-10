
# install.packages("NobBS")
# library(devtools)
# install_github("sarahhbellum/NobBS")

library(NobBS)
library(lubridate)
library(stringr)
library(tidyverse)
library(rjags)

denguedat <- read.csv("C:/Users/ruu6/OneDrive - CDC/Dengue/Threshold/denguedat_line_week.csv") #312999 obs
head(denguedat)
str(denguedat)

denguedat$onset_week <- as.Date(denguedat$onset_week)
denguedat$report_week <- as.Date(denguedat$report_week)

denguedat$delay <- denguedat$report_week-denguedat$onset_week 
#note some really long, or negative --> 99% is 28
denguedat <- denguedat[which(denguedat$delay>=0),] #lose 997 obs
denguedat <- denguedat[which(denguedat$delay<30),] #lose 2683 obs
# N = 309319

denv2015 <- denguedat[which(year(denguedat$onset_week)==2015 & year(denguedat$report_week)==2015),] #works

denv00_15 <- denguedat[which(year(denguedat$onset_week) %in% c(2000:2015) & year(denguedat$report_week)%in% c(2000:2015)),]

denv_nowcast <- NobBS(data=denv00_15, now = as.Date("2015-12-30"),
                      units = "1 week", onset_date = "onset_week",
                      report_date = "report_week", 
                      moving_window=NULL, max_D=30,cutoff_D=T, 
                      proportion_reported=1, quiet=F,
                      specs=list(
                        dist=c("NB"),
                        alpha1.mean.prior=0,
                        alpha1.prec.prior=0.001,
                        alphat.shape.prior=0.001,
                        alphat.rate.prior=0.001,
                        beta.priors=NULL,
                        param_names=NULL,
                        conf=0.95,
                        dispersion.prior=NULL,
                        nAdapt=3500,
                        nChains=1,
                        nBurnin=1000,
                        nThin=1,
                        nSamp=10000))

# denv_nowcast <- NobBS(data=denguedat, now = as.Date("2016-08-31"),
#                       units = "1 week", onset_date = "onset_week",
#                       report_date = "report_week")

str(denv_nowcast)
tail(denv_nowcast$estimates)
tail(denv_nowcast$estimates.inflated)

library(ggplot2)
#posterior of nowcast
nowcast_posterior <- data.frame(sample_estimate=denv_nowcast$nowcast.post.samps)
ggplot(nowcast_posterior, aes(sample_estimate)) + 
  geom_histogram(binwidth = 10) +
  theme_bw() +
  xlab("Cases (number)") +
  ggtitle("Samples from the posterior distribution of the nowcast for Dec 30, 2015")

#posterior of beta*d, the estimated prob. of a case being reported with no delay
library(dplyr)
beta_0 <- data.frame(prob=exp(denv_nowcast$params.post$`Beta 0`))
ggplot(beta_0, aes(prob)) + 
  geom_histogram() +
  theme_bw() +
  xlab("Probability of delay=0") +
  ggtitle("Posterior distribution of the probability of delay d=0, 
          estimated over the period Jan 1, 2015 to Dec 30, 2015")

#sequence of nowcasts and hindcasts
nowcasts <- data.frame(denv_nowcast$estimates)
ggplot(nowcasts) + 
  geom_line(aes(onset_date, estimate, col="Nowcast estimate"), linetype="longdash") +
  geom_line(aes(onset_date, n.reported, col="Reported to date"), linetype="solid") +
  theme_classic() +
  geom_ribbon(fill="indianred3", aes(x=onset_date, ymin=lower,
                                     ymax=upper), alpha=0.3) +
  xlab("Case onset date") + ylab("Estimated cases") +
  ggtitle("Observed and predicted number of cases \nat the week of nowcast (Dec 2015) and the weeks prior")

