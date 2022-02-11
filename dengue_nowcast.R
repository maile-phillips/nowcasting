
# install.packages("NobBS")
# library(devtools)
# install_github("sarahhbellum/NobBS")

library(NobBS)
library(lubridate)
library(stringr)
library(tidyverse)
library(rjags)

denguedat <- read.csv("/Users/taylorchin/Dropbox (Harvard University)/cdc/dengue_thresholds/denguedat_line_week_createddate.csv") #312999 obs
head(denguedat)
str(denguedat)

# set up data ----
denguedat$onset_week <- as.Date(denguedat$onset_week)
denguedat$report_week <- as.Date(denguedat$report_week)

denguedat$delay <- denguedat$report_week-denguedat$onset_week 

nrow(denguedat) # 83644

#note some really long, or negative --> 99% is 28
denguedat_sub <- denguedat[which(denguedat$delay>=0),] #lose 997 obs
denguedat_sub <- denguedat[which(denguedat$delay<50),] #lose 2683 obs

nrow(denguedat_sub) #73122

# run nowcast ----
#denv2015 <- denguedat[which(year(denguedat$onset_week)==2015 & year(denguedat$report_week)==2015),] #works
#denv00_15 <- denguedat[which(year(denguedat$onset_week) %in% c(2000:2015) & year(denguedat$report_week)%in% c(2000:2015)),]

denv_nowcast <- NobBS(data=denguedat_sub, now = max(denguedat_sub$report_week),
                      units = "1 week", onset_date = "onset_week",
                      report_date = "report_week", 
                      moving_window=104, cutoff_D=T, 
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


# examine df:
denguedat_sub %>%
  count(onset_week) %>%
  mutate(onset_week = as.Date(onset_week)) %>% 
  filter(onset_week >= "2019-12-04") %>% View()

# plot nowcast
nowcasts <- data.frame(denv_nowcast$estimates)

ggplot(nowcasts) + 
  geom_line(aes(onset_date, estimate, col="Nowcast estimate"), linetype="solid") +
  geom_line(aes(onset_date, n.reported, col="Reported to date"), linetype="solid") +
  theme_classic() +
  geom_ribbon(fill="indianred3", aes(x=onset_date, ymin=lower,
                                     ymax=upper), alpha=0.3) +
  xlab("Case onset date") + ylab("Estimated cases") +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "3 months") +
  scale_y_continuous(limits = c(0,60)) +
  ggtitle("Observed and predicted number of cases \nat the week of nowcast (11/24/2021) and the weeks prior")


# Delay distributions ----
(denguedat %>% tally(delay < 0))/nrow(denguedat) # 0.4% 
(denguedat %>% tally(delay > 30))/nrow(denguedat) # 12%

# missing weeks / weeks with 0 cases?
denguedat_sub %>% summarise(min(onset_week), max(onset_week))
all_dates = seq(as.Date(min(denguedat_sub$onset_week)), 
                as.Date(max(denguedat_sub$onset_week)), by = "1 week")
data_dates = denguedat_sub %>% distinct(onset_week) %>% pull(onset_week)
length(all_dates)
length(data_dates)

all_dates[!(all_dates %in% data_dates)]

denguedat_sub %>%
  mutate(delay_week = round(as.numeric(report_week - onset_week)/7)) %>%
  count(onset_week,delay_week) %>%
  summarise(max(delay_week)) #488

denguedat_sub %>%
  mutate(delay_week = round(as.numeric(report_week - onset_week)/7)) %>%
  count(onset_week,delay_week) %>%
  # if delay > 17 weeks, just replace with max of 17 for now
  mutate(delay_week_trunc = ifelse(delay_week > 17, 17, delay_week)) %>%
  group_by(onset_week)%>%
  mutate(prop=n/sum(n)) -> delay_summary 

# plot
colors <- scales::hue_pal()(18) # set the number of colors you need

delay_summary %>%
  filter(year(onset_week) > 2009) %>%
  ggplot(aes(x=onset_week))+
  geom_col(aes(y=prop,group=factor(delay_week_trunc),fill=factor(delay_week_trunc)),
           position = position_stack(reverse = TRUE)) + 
  theme(strip.background = element_blank()) +
  theme_classic() +
  scale_x_date(breaks = "2 year", date_labels = "%Y") +
  scale_fill_manual(values=colors) + 
  guides(fill=guide_legend(ncol=2, reverse=T)) +
  labs(x="Onset week",fill="Delay (weeks) \ntruncated to max 17 weeks",y="proportion") 


# Raw data vs. nobbs dataframe ----
denguedat.new = denguedat
data(denguedat)
denguedat.nobbs = denguedat
denguedat.new %>%
  # group_by_at(vars(onset_week, report_week)) %>% 
  count(report_week) %>%
  mutate(report_week = as.Date(report_week)) %>%
  filter(report_week > "1991-12-23" &  report_week < "2010-11-29") %>%
  mutate(source = "maile") %>%
  bind_rows(denguedat.nobbs %>% count(report_week) %>% mutate(source = "nobbs")) %>%
  ggplot(.) +
  geom_line(aes(x = as.Date(report_week), y = n, colour = source)) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "6 months") +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(x = "Date", y = "Cases")

# test nowcast function using nobbs data ----
test_nowcast <- NobBS(data=denguedat.nobbs, now=as.Date("2010-11-29"),
                      units="1 week",onset_date="onset_week",report_date="report_week", 
                      moving_window = 104)

beta_0 <- data.frame(prob=exp(test_nowcast$params.post$`Beta 0`))
ggplot(beta_0, aes(prob)) + 
  geom_histogram() + 
  theme_bw() + 
  xlab("Probability of delay=0") +
  ggtitle("Posterior distribution of the probability of delay d=0,
         estimated over the period Jan 1, 1990 to Oct 1, 1990")


# Other diagnostic plots ----
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
  geom_line(aes(onset_date, estimate, col="Nowcast estimate"), linetype="solid") +
  geom_line(aes(onset_date, n.reported, col="Reported to date"), linetype="solid") +
  theme_classic() +
  geom_ribbon(fill="indianred3", aes(x=onset_date, ymin=lower,
                                     ymax=upper), alpha=0.3) +
  xlab("Case onset date") + ylab("Estimated cases") +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "3 months") +
  ggtitle("Observed and predicted number of cases \nat the week of nowcast (11/24/2021) and the weeks prior")

