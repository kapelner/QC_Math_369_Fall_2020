pacman::p_load(datasets)

X = datasets::HairEyeColor[, , "Male"]
X
n = sum(X)
n_i_dots = rowSums(X)
n_dot_js = colSums(X)
theta_i_dots = n_i_dots / n
theta_dot_js = n_dot_js / n
n_i_dots
n_dot_js
theta_i_dots
theta_dot_js

E = n * theta_i_dots %*% t(theta_dot_js)
round(E, 2)
sum(E)
rowSums(E)
(X - E)^2 / E
sum((X - E)^2 / E)


#homogeneity
t(apply(datasets::HairEyeColor, 3, colSums))
