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
SPY_CI
QQQ_CI
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