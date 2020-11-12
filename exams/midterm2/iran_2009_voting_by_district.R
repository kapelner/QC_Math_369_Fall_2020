#candidate D
x = c(125,  57,  44,  29,  24,  16,  41,  13,  17)
theta = c(.301, .176, .125, .097, .079, .067, .058, .051, .046)
n = sum(x)
exp = n * theta

tab = round(rbind(theta, x, exp, (x-exp)^2/exp), 3)

rowSums(tab)

pacman::p_load(xtable)
xtable(tab)

qchisq(.95, 8)
qchisq(.95, 9)
qchisq(.95, 24)
qchisq(.95, 27)