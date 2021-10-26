
# install.packages("NobBS")
# library(devtools)
# install_github("sarahhbellum/NobBS")

library(NobBS)
data("denguedat")
head(denguedat)


test_nowcast <- NobBS(data=denguedat, now = as.Date("1990-10-01"),
                      units = "1 week", onset_date = "onset_week",
                      report_date = "report_week")
str(test_nowcast)
tail(test_nowcast$estimates)
tail(test_nowcast$estimates.inflated)

library(ggplot2)
#posterior of nowcast
nowcast_posterior <- data.frame(sample_estimate=test_nowcast$nowcast.post.samps)
ggplot(nowcast_posterior, aes(sample_estimate)) + 
  geom_histogram(binwidth = 10) +
  theme_bw() +
  xlab("Cases (number)") +
  ggtitle("Samples from the posterior distribution of the nowcast for Oct 1, 1990")

#posterior of beta*d, the estimated prob. of a case being reported with no delay
library(dplyr)
beta_0 <- data.frame(prob=exp(test_nowcast$params.post$`Beta 0`))
ggplot(beta_0, aes(prob)) + 
  geom_histogram() +
  theme_bw() +
  xlab("Probability of delay=0") +
  ggtitle("Posterior distribution of the probability of delay d=0, 
          estimated over the period Jan 1, 1990 to Oct 1, 1990")

#sequence of nowcasts and hindcasts
nowcasts <- data.frame(test_nowcast$estimates)
ggplot(nowcasts) + 
  geom_line(aes(onset_date, estimate, col="Nowcast estimate"), linetype="longdash") +
  geom_line(aes(onset_date, n.reported, col="Reported to date"), linetype="solid") +
  theme_classic() +
  geom_ribbon(fill="indianred3", aes(x=onset_date, ymin=lower,
                                     ymax=upper), alpha=0.3) +
  xlab("Case onset date") + ylab("Estimated cases") +
  ggtitle("Observed and predicted number of cases \nat the week of nowcast (Oct 1990) and the weeks prior")

