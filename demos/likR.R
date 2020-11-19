#coin flip

n = 100
sum_x = 61
n_min_sum_x = n - sum_x
xbar = sum_x / n

lik_rat_stat = 2 * (sum_x * log(xbar / 0.5) + n_min_sum_x * log((1 - xbar) / 0.5))
lik_rat_stat

# die roll
counts = c(4, 1, 3, 2, 1, 4)
n = sum(counts)
lik_rat_stat = 2 * (n * log(6) + sum(counts * log(counts / n)))
lik_rat_stat
qchisq(.95, 5)
