x = c(1.85, 4.18, 2.63, 3.41, 4.23, 4.24, 3.17, 3.69, 3.13, 1.04)
n = length(x)

#test H_0: theta = 0. Score score statistic which is a draw from an approximate normal distribution is:
sum(exp(-x) / (1 + exp(-x)))
z = (n - 2 * sum(exp(-x) / (1 + exp(-x)))) / sqrt(n / 3)
z
