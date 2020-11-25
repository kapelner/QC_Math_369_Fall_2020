
res = 1e-3
xmax = 3
jmax = 1e4
js = 1 : jmax
xs = seq(from = 0, to = xmax, by = res)
Fx = array(NA, length(xs))
sqrt_two_pi = sqrt(2 * pi)

for (i in 1 : length(xs)){
  x = xs[i]
  Fx[i] = sqrt_two_pi / x * sum(exp(-(2 * js - 1)^2 * pi^2 / (8 * x^2)))
  if (i %% 100 == 0){
    cat("F(", x, ") =", Fx[i], "\n")
  }
}

pacman::p_load(ggplot2)


fx = array(NA, length(xs))
for (i in 1 : (length(xs) - 1)){
  fx[i] = (Fx[i + 1] - Fx[i]) / res
}

ggplot(data.frame(xs = xs, fx = fx, Fx = Fx)) + 
  geom_line(aes(x = xs, y = fx)) +
  xlab("k") + ylab("f_K(k)")


#critical values
K_95 = xs[min(which(Fx > 0.95))]
K_99 = xs[min(which(Fx > 0.99))]
