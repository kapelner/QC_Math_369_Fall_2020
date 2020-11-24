# die roll
counts = c(4, 1, 3, 2, 1, 4)
n = sum(counts)
lik_rat_stat = 2 * (n * log(6) + sum(counts * log(counts / n)))
lik_rat_stat
qchisq(.95, 5)



##HW6 4(g) coin flip

n = 100
sum_x = 61
n_min_sum_x = n - sum_x
xbar = sum_x / n

lik_rat_stat = 2 * (sum_x * log(xbar / 0.5) + n_min_sum_x * log((1 - xbar) / 0.5))
lik_rat_stat

##Hw6 clinical trial example
#this will not work on your computer since the data is my private data!
pacman::p_load(data.table, xtable)
X = fread("../Dropbox/private_data/clinical_data_cleaned.csv") 
head(X)
X[, V1 := NULL]
reduced_model = lm(y ~ ., X)
full_model = lm(y ~ . * tx, X)
logLik(reduced_model)
logLik(full_model)
