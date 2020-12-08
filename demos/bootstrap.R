pacman::p_load(quantmod, lubridate, ggplot2, data.table)

#let's look at SPY (the S&P 500 which is a proxy for the total American market) 
#vs QQQ (a fund that proxies for American tech / biotech) only
num_years = 10

#get the publicly available financial data:
getSymbols("SPY", from = Sys.Date() %m-% years(num_years), to = Sys.Date(), warnings = FALSE, auto.assign = TRUE)
getSymbols("QQQ", from = Sys.Date() %m-% years(num_years), to = Sys.Date(), warnings = FALSE, auto.assign = TRUE)
SPY = data.table(SPY)
QQQ = data.table(QQQ)
#calculate proportional change for trading day
SPY[, prop_change := SPY.Close / shift(SPY.Close) - 1]
QQQ[, prop_change := QQQ.Close / shift(QQQ.Close) - 1]
#pretend the data is iid and get back the "x values" for both populations
SPY_prop_changes = na.omit(c(SPY[, .(prop_change)])[[1]])
QQQ_prop_changes = na.omit(c(QQQ[, .(prop_change)])[[1]])


#inference for the median return
median_SPY_prop_change_est = median(SPY_prop_changes)
median_SPY_prop_change_est

#let's get a CI
B = 50000
median_SPY_prop_change_est_b = array(NA, B)
for (b in 1 : B){
  median_SPY_prop_change_est_b[b] = median(sample(SPY_prop_changes, replace = TRUE))
}

alpha = 0.05
c(quantile(median_SPY_prop_change_est_b, alpha / 2), quantile(median_SPY_prop_change_est_b, 1 - alpha / 2))



#estimate sharpe ratios for both
r_free_rate = 0.008 / 365 #fluctuates daily but we'll call it about 0.8% yearly
SPY_sharpe_est = (mean(SPY_prop_changes) - r_free_rate) / sd(SPY_prop_changes)
QQQ_sharpe_est = (mean(QQQ_prop_changes) - r_free_rate) / sd(QQQ_prop_changes)
SPY_sharpe_est
QQQ_sharpe_est
#they're both positive which is great, but we usually like to look at 
#annualized. There are 252 trading days, so we multiply by 252 / sqrt(252) to scale
#the numerator and denominator correctly. Annualized Sharpes are good if they're greater > 1
SPY_sharpe_est * 252 / sqrt(252)
QQQ_sharpe_est * 252 / sqrt(252)

#let's get CI's for both!!
B = 50000
SPY_sharpe_est_b = array(NA, B)
QQQ_sharpe_est_b = array(NA, B)
for (b in 1 : B){
  SPY_prop_changes_b = sample(SPY_prop_changes, replace = TRUE)
  QQQ_prop_changes_b = sample(QQQ_prop_changes, replace = TRUE)
  SPY_sharpe_est_b[b] = (mean(SPY_prop_changes_b) - r_free_rate) / sd(SPY_prop_changes_b)
  QQQ_sharpe_est_b[b] = (mean(QQQ_prop_changes_b) - r_free_rate) / sd(QQQ_prop_changes_b)
}

#let's say we want to test if these assets offer positive Sharpe i.e. H_0: phi = 0 at alpha = 5%
alpha = 0.05
#First we build CI's
SPY_CI = c(
  quantile(SPY_sharpe_est_b, alpha / 2), 
  quantile(SPY_sharpe_est_b, 1 - alpha / 2)
)
QQQ_CI = c(
  quantile(QQQ_sharpe_est_b, alpha / 2), 
  quantile(QQQ_sharpe_est_b, 1 - alpha / 2)
)
#and then check is 0 is included
SPY_CI * 252
QQQ_CI * 252
#We reject H_0 for both SPY and QQQ; we are reasonably confident that both the 
#S&P500 and QQQ beat the risk free rate as zero is not inside both CI's.

num_bins = 1000
ggplot(data.frame(SPY_sharpe_est_b = SPY_sharpe_est_b, in_ci = SPY_sharpe_est_b >= SPY_CI[1] & SPY_sharpe_est_b <= SPY_CI[2])) +
  geom_histogram(aes(x = SPY_sharpe_est_b, col = in_ci, fill = in_ci), bins = num_bins) +
  geom_vline(xintercept = 0, col = "green")
ggplot(data.frame(QQQ_sharpe_est_b = QQQ_sharpe_est_b, in_ci = QQQ_sharpe_est_b >= QQQ_CI[1] & QQQ_sharpe_est_b <= QQQ_CI[2])) +
  geom_histogram(aes(x = QQQ_sharpe_est_b, col = in_ci, fill = in_ci), bins = num_bins) +
  geom_vline(xintercept = 0, col = "green")

#not covered: paired testing. H_0: QQQ has the same performance as SPY
#all we do is take the paired differences:
QQQ_minus_SPY_sharpe_est_b = QQQ_sharpe_est_b - SPY_sharpe_est_b
QQQ_minus_SSPY_CI = c(
  quantile(QQQ_minus_SPY_sharpe_est_b, alpha / 2), 
  quantile(QQQ_minus_SPY_sharpe_est_b, 1 - alpha / 2)
)
QQQ_minus_SSPY_CI
#we fail to reject!

ggplot(data.frame(QQQ_minus_SPY_sharpe_est_b = QQQ_minus_SPY_sharpe_est_b, in_ci = QQQ_minus_SPY_sharpe_est_b >= QQQ_minus_SSPY_CI[1] & QQQ_minus_SPY_sharpe_est_b <= QQQ_minus_SSPY_CI[2])) +
  geom_histogram(aes(x = QQQ_minus_SPY_sharpe_est_b, col = in_ci, fill = in_ci), bins = num_bins) +
  geom_vline(xintercept = 0, col = "green")

#hw
rm(list = ls())
xs = sort(c(1.09, 2.48, 3.08, 2.57, 1.04, 0.87, 4.18, 2.23, 3.22, 1.33, 2.49, 1.69, 3.18, 1.39, 2.52, 4.8, 2.44, 1.47, 2.64, 3.96, 3.08, 2.71, 2.8, 3.4, 3.86, 2.28, 3.65, 3.28, 1.54, 1.94))
paste(xs, collapse = ", ")

B = 1e5

sample_median_bs = array(NA, B)
for (b in 1 : B){
  sample_median_bs[b] = median(sample(xs, replace = TRUE))
}

alpha = 0.05
sample_median_CI = c(
  quantile(sample_median_bs, alpha / 2), 
  quantile(sample_median_bs, 1 - alpha / 2)
)
sample_median_CI
2.57 - 1/sqrt(30)
2.57 + 1/sqrt(30)


ggplot(data.frame(sample_median_bs = sample_median_bs)) +
  geom_histogram(aes(x = sample_median_bs), bins = B / 100) +
  geom_vline(xintercept = median(xs), col = "green") +
  xlab("boostrap distribution of sample median lifespan")
