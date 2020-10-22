pacman::p_load(extraDistr)

n = 7
theta = 3
theta_0 = 2


x = rgumbel(n, theta, 1)
hist(x, br = n / 10)
thetahathat_mle = log(n / sum(exp(-x)))
thetahathat_mle
se_thetahat_mle = exp(theta_0) / sqrt(n)
se_thetahat_mle
thetahathat_mle_std = (thetahathat_mle - theta_0) / se_thetahat_mle
thetahathat_mle_std

euler_constant = 0.57721

xbar_se = sd(x) / sqrt(n)
xbar_se
(thetahathat_mle - theta_0) / xbar_se