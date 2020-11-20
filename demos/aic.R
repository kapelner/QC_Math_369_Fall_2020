pacman::p_load(fitdistrplus, nimble, VaRES, data.table)
plogistic = function(q, mu, sigma){VaRES::plogistic(q, mu, sigma)}

set.seed(2)
#Look at how much impact the AIC idea has had on science:
#https://scholar.google.com/scholar?hl=en&as_sdt=0%2C39&q=A+new+look+at+the+statistical+model+identification&btnG=

n = 50
x = rt(n, 4)
hist(x, breaks = 20)

#####
#MOD 1: iid N(theta_1, theta_2)
#MOD 2: iid Cauchy(theta_1, theta_2)
#MOD 3: iid Logistic(theta_1, theta_2)
#MOD 4: iid Laplace(theta_1, theta_2)
#MOD 5: iid T(theta_1, theta_2)
#All these models have two parameters: 
#theta_1 represents a center and 
#theta_2 represents a "scale" controlling how fast its tails go to zero
#thus K_m = 2 for all models

fit_normal = fitdist(x, "norm")
fit_cauchy = fitdist(x, "cauchy")
fit_logistic = fitdist(x, "logistic", start = list(mu = mean(x), sigma = 0.1))
fit_laplace = fitdist(x, "dexp", start = list(location = mean(x), scale = 0.1)) #AKA Laplace distribution
fit_t = fitdist(x, "t", start = list(df = 1, ncp = 0))

#here's all the maximum likelihood estimates:
fit_normal
fit_cauchy
fit_logistic
fit_laplace
fit_t

#and all the AIC's
aic_s = c(fit_normal$aic, fit_cauchy$aic, fit_logistic$aic, fit_laplace$aic, fit_t$aic)
names(aic_s) = c("Normal", "Cauchy", "Logistic", "Laplace", "StudentsT")
aic_s_sorted = sort(aic_s)
aic_s_sorted


#now compute the Akaike weights as percentages
best_aic = aic_s_sorted[1]
delta_aics = aic_s_sorted - best_aic
akaike_weights = exp(-delta_aics / 2) / sum(exp(-delta_aics / 2))
round(akaike_weights * 100, 2)

#now compute the AICc's
k = 2 #it's the same for all models
aiccs = aic_s_sorted - 2 * k + 2 * k * n / (n-k-1)
aiccs
sort(aiccs)
aic_s_sorted
#doesn't make any difference in the rankings


#for HW5
set.seed(1)
n = 10
x = rt(n, 4)
round(x, 2)
fit_normal = fitdist(x, "norm")
fit_cauchy = fitdist(x, "cauchy")
fit_logistic = fitdist(x, "logistic", start = list(mu = mean(x), sigma = 0.1))
fit_laplace = fitdist(x, "dexp", start = list(location = mean(x), scale = 0.1)) #AKA Laplace distribution
fit_normal
fit_cauchy
fit_logistic
fit_laplace
c(fit_normal$aic, fit_cauchy$aic, fit_logistic$aic, fit_laplace$aic)

#for midterm2
pacman::p_load(nycflights13, ggplot2, xtable)

X = data.table(nycflights13::weather)
x = as.matrix(X[origin == "JFK", .(max_windspeed = max(wind_speed, na.rm = TRUE)), by = c("month", "day")])[, 3]
ggplot(data.frame(x = x)) + 
  geom_histogram(aes(x = x), bins = 100) +
  xlab("maximum wind speeds by day at JFK airport in 2013")

pgumbel = function(q, mu, sigma){VaRES::pgumbel(q, mu, sigma)}
pfrechet = function(q, mu, sigma){VaRES::pfrechet(q, mu, sigma)}
pgompertz = function(q, mu, sigma){VaRES::pgompertz(q, mu, sigma)}
pgenlogis = function(q, a, mu, sigma){VaRES::pgenlogis(q, a, mu, sigma)}

fit_exponential = fitdist(x, "exp")
fit_normal = fitdist(x, "norm")
fit_weibull = fitdist(x, "weibull")
fit_gamma = fitdist(x, "gamma")
fit_gumbel = fitdist(x, "gumbel", start = list(mu = mean(x), sigma = 0.1))
fit_gompertz = fitdist(x, "gompertz", start = list(b = .1, eta = .1))
fit_frechet = fitdist(x, "frechet", start = list(alpha = .1, sigma = .1))
fit_genlogis = fitdist(x, "genlogis", start = list(a = 1, mu = mean(x), sigma = 0.1))


xtable(round(cbind(
  fit_genlogis$estimate,
  fit_exponential$estimate,
  fit_normal$estimate,
  fit_weibull$estimate,
  fit_gamma$estimate,
  fit_gumbel$estimate,
  fit_gompertz$estimate,
  fit_frechet$estimate
), 2))

xtable(t(as.matrix(round(c(
  fit_exponential$loglik,
  fit_normal$loglik,
  fit_weibull$loglik,
  fit_gamma$loglik,
  fit_gumbel$loglik,
  fit_gompertz$loglik,
  fit_frechet$loglik,
  fit_genlogis$loglik
), 2))))


#for hw6

#sanity check:
x_logis = rlogis(10000, location = 10, scale = 0.1)
fit_logis = fitdist(x_logis, "logis", start = list(location = mean(x_logis), scale = 1))
fit_logis
fit_genlogis = fitdist(x_logis, "genlogis", start = list(a = 1, mu = mean(x_logis), sigma = 0.1))
fit_genlogis


fit_logis = fitdist(x, "logis", start = list(location = mean(x), scale = 1))
fit_logis
fit_genlogis = fitdist(x, "genlogis", start = list(a = 1, mu = mean(x), sigma = 0.1))
fit_genlogis
fit_logis$loglik
fit_genlogis$loglik
