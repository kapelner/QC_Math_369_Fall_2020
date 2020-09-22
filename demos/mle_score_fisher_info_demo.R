if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2, data.table)
set.seed(1984)
RES = 1e-2

theta_real = 0.31415
n = 100
#DGP: iid Bernoulli
xs = rbinom(n, 1, theta_real)
xs

#calculate thetahathat = thetahathat^MLE = xbar
thetahathat = mean(xs)
thetahathat

#Let's plot the likelihood
thetas = seq(0 + RES, 1 - RES, by = RES)
likelihood = thetas^(sum(xs)) * (1 - thetas)^(n - sum(xs))
ggplot(data.frame(theta = thetas, likelihood = likelihood)) +
  geom_line(aes(x = theta, y = likelihood), col = "red")
#now graph a line where the estimate is and where the truth is
ggplot(data.frame(theta = thetas, likelihood = likelihood)) +
  geom_line(aes(x = theta, y = likelihood), col = "red") + 
  geom_vline(xintercept = thetahathat, col = "purple") + 
  geom_vline(xintercept = theta_real, col = "black")
#just missed the real thing which is expected...

#now let's look at log likelihood
log_likelihood = sum(xs) * log(thetas) + (n - sum(xs)) * log(1 - thetas)
demo_frame = data.table(theta = thetas, val = log_likelihood, line = "log_likelihood")
ggplot(demo_frame) +
  geom_line(aes(x = theta, y = val, col = line))

#much flatter ... but that's only because thetas near 0 or 1 take a nosedive
#let's just adjust the y-scale
ggplot(demo_frame) +
  geom_line(aes(x = theta, y = val, col = line)) +
  ylim(-50, -25)

#and of course the MLE stays the same
ggplot(demo_frame) +
  geom_line(aes(x = theta, y = val, col = line)) +
  ylim(-50, -25) + 
  geom_vline(xintercept = thetahathat, col = "purple") + 
  geom_vline(xintercept = theta_real, col = "black")

#Now let's look at the derivative of the log likelihood along with the log likelihood
log_likelihood_prime = sum(xs) / thetas - (n - sum(xs)) / (1 - thetas)
demo_frame = rbind(demo_frame, data.table(theta = thetas, val = log_likelihood_prime, line = "log_likelihood_prime"))
ggplot(demo_frame) +
  geom_line(aes(x = theta, y = val, col = line))

#let's adjust the scale again to see what's going on and let's also plot our estimate
ggplot(demo_frame) +
  geom_line(aes(x = theta, y = val, col = line)) +
  ylim(-100, 100) + 
  geom_vline(xintercept = thetahathat, col = "purple") + 
  geom_vline(xintercept = theta_real, col = "black")

#this is exactly what is expected from calculus 101: the derivative is 0 at 
#the maximum 
#likelihood estimate, positive when theta is less than it and negative
#when it is greater

#now let's look at the second derivative
log_likelihood_prime_prime = -sum(xs) / thetas^2 - (n - sum(xs)) / (1 - thetas)^2
demo_frame = rbind(demo_frame, data.table(theta = thetas, val = log_likelihood_prime_prime, line = "log_likelihood_prime_prime"))
ggplot(demo_frame) +
  geom_line(aes(x = theta, y = val, col = line))

#let's adjust the scale again to see what's going on and let's also plot our estimate
ggplot(demo_frame) +
  geom_line(aes(x = theta, y = val, col = line)) +
  ylim(-500, 200) + 
  geom_vline(xintercept = thetahathat, col = "purple") + 
  geom_vline(xintercept = theta_real, col = "black")

#This is about what we expect: the second derivative is negative at the MLE
#and negative everywhere as there's only one global max in this situation

#since the negative second derivative is more important to our theory, we plot that
demo_frame = demo_frame[line != "log_likelihood_prime_prime"]
demo_frame = rbind(demo_frame, data.table(theta = thetas, val = -log_likelihood_prime_prime, line = "neg_log_likelihood_prime_prime"))

ggplot(demo_frame) +
  geom_line(aes(x = theta, y = val, col = line)) +
  ylim(-200, 500) + 
  geom_vline(xintercept = thetahathat, col = "purple") + 
  geom_vline(xintercept = theta_real, col = "black")

#So far not so interesting... this has basically been a calculus lesson
#about derivatives and second derivatives.

#That's because there's only one sample of data x_1, ..., x_n, so we cannot 
#learn anything about our theory from class since the theory required
#an expectation over the DGP (the average over the x's).

#Let's verify that our estimator is unbiased by looking at many data sets
#for that same theta_real = 0.31415.

Nsim = 100
ggplot_base = ggplot() +
  ylim(-50, -25)

for (nsim in 1 : Nsim){
  xs = rbinom(n, 1, theta_real)
  log_likelihood = sum(xs) * log(thetas) + (n - sum(xs)) * log(1 - thetas)
  ggplot_base = ggplot_base +
    geom_line(data = data.table(theta = thetas, val = log_likelihood, line = "log_likelihood"), aes(x = theta, y = val, col = line)) +
    geom_vline(xintercept = mean(xs), col = "purple")
    
}
plot(ggplot_base) + 
  geom_vline(xintercept = theta_real, col = "black")

#you can see how the average of the purple lines is the black line
#the pin on the theta=0.5 is from the bernoulli PMF. If theta = 0.5,
#the effect of x drops out.

#That's about as far as we can go with one assumed theta. Our theory
#from class was about all possible datasets (like the simulation above) 
# AND all thetas in the parameter space.

#So to verify fact 1b: E[ell'(theta; x)] = 0 for the iid Bern(theta) DGP
#To do so, we generate many datasets for many different thetas

Nsim = 10000

sim_res = matrix(NA, nrow = length(thetas) * Nsim, ncol = 2)
for (i in 1 : length(thetas)){
  for (nsim in 1 : Nsim){
    xs = rbinom(n, 1, thetas[i])
    sim_res[i * (length(thetas) - 1) + nsim, 1] = thetas[i]
    sim_res[i * (length(thetas) - 1) + nsim, 2] = sum(xs) / thetas[i] - (n - sum(xs)) / (1 - thetas[i])
  }
}
sim_res = data.table(theta = as.factor(sim_res[, 1]), log_likelihood_prime = sim_res[, 2])

ggplot(sim_res) +
  geom_boxplot(aes(x = theta, y = log_likelihood_prime)) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  geom_hline(yintercept = 0, col = "green")

#The averages certainly seem to be zero. Let's actually compute all of them

sim_res_means = sim_res[, .(avg_log_likelihood_prime = mean(log_likelihood_prime)), by = theta][, theta := as.numeric(as.character(theta))]
ggplot(sim_res_means) +
  geom_line(aes(x = theta, y = avg_log_likelihood_prime)) + 
  geom_hline(yintercept = 0, col = "green")

#I think we have to be content that the averages are zero

#now let's look at the Fisher Information which is the variance of 
#the score function (divided by n)
sim_res_vars = sim_res[, .(fisher_information = var(log_likelihood_prime) / n), by = theta][, theta := as.numeric(as.character(theta))]
ggplot(sim_res_vars) +
  geom_line(aes(x = theta, y = fisher_information))

#The critical quantity in the variance of the estimator that attains the
#CRLB is the inverse Fisher information 
sim_res_vars = sim_res[, .(inv_fisher_information = n / var(log_likelihood_prime)), by = theta][, theta := as.numeric(as.character(theta))]
ggplot(sim_res_vars) +
  geom_line(aes(x = theta, y = inv_fisher_information))

