pacman::p_load(combinat, ggplot2, data.table)

#height data from class
xM = c(70,72,73,68,69,70,67,72,71,73)
xF = c(60,59,64,64,64,63)
nM = length(xM)
nF = length(xF)
n = nM + nF
xM
xF


#because there is n = nM + nF = 16 and nM = 10, 
#there are 16-choose-10 possible "permutations" of the data
#if there were more than 1,000,000, we would sample randomly
#but here we actually just take all of the possibilities methodically
all_combinations = combn(1 : n, nM)

B = ncol(all_combinations)
xbar_MF_diff_perms = array(NA, B)
x = c(xM, xF)
for (b in 1 : B){
  xM_perm = x[all_combinations[, b]]
  xF_perm = x[setdiff(1 : n, all_combinations[, b])]
  xbar_MF_diff_perms[b] = mean(xM_perm) - mean(xF_perm)
}

alpha = 0.05
ret_region = c(
  quantile(xbar_MF_diff_perms, alpha / 2), 
  quantile(xbar_MF_diff_perms, 1 - alpha / 2)
)
ret_region
xbar_MF_diff = mean(xM) - mean(xF)
xbar_MF_diff

ggplot(data.frame(xbar_MF_diff_perms = xbar_MF_diff_perms, in_ret = xbar_MF_diff_perms >= ret_region[1] & xbar_MF_diff_perms <= ret_region[2])) +
  geom_histogram(aes(x = xbar_MF_diff_perms, col = in_ret, fill = in_ret), bins = length(unique(xbar_MF_diff_perms))) +
  geom_vline(xintercept = xbar_MF_diff, col = "green")

#pval#pvalue
min(
  ecdf(xbar_MF_diff_perms)(xbar_MF_diff),
  1 - ecdf(xbar_MF_diff_perms)(xbar_MF_diff)
  ) * 2


#another example
rm(list = ls())
data(iris)
iris = data.table(iris)
#famous dataset that Fisher analyzed in 1936 in his paper 
#"The use of multiple measurements in taxonomic problems as an example 
#of linear discriminant analysis". It contains measurements on three 
#types of iris (a flower) species. We will look at two of the species' 
#sepal widths and investigate if they have different distributions.

x_1 = c(iris[Species == "versicolor", .(Sepal.Width)])[[1]]
x_2 = c(iris[Species == "virginica", .(Sepal.Width)])[[1]]
n_1 = length(x_1)
n_2 = length(x_2)
n = n_1 + n_2
summary(x_1)
n_1
summary(x_2)
n_2

#how many permutations?
choose(n, n_1)
#that's a trillion times the number of grains of sand on earth so.... it's 
#impossible to check all of them! Settle for a small proportion, like one million
B = 1e6
xbar_diff_perms = array(NA, B)
x = c(x_1, x_2)
for (b in 1 : B){
  x = sample(x) #random order
  x_1_perm = x[1 : n_1]
  x_2_perm = x[(n_1 + 1) : n]
  
  xbar_diff_perms[b] = mean(x_1_perm) - mean(x_2_perm)
}

alpha = 0.05
ret_region = c(
  quantile(xbar_diff_perms, alpha / 2), 
  quantile(xbar_diff_perms, 1 - alpha / 2)
)
ret_region
xbar_diff = mean(x_1) - mean(x_2)
xbar_diff

ggplot(data.frame(xbar_diff_perms = xbar_diff_perms, in_ret = xbar_diff_perms >= ret_region[1] & xbar_diff_perms <= ret_region[2])) +
  geom_histogram(aes(x = xbar_diff_perms, col = in_ret, fill = in_ret), bins = length(unique(xbar_diff_perms))) +
  geom_vline(xintercept = xbar_diff, col = "green")

#pvalue
min(
  ecdf(xbar_diff_perms)(xbar_diff),
  1 - ecdf(xbar_diff_perms)(xbar_diff)) * 2

#compare to t.test
t.test(x_1, x_2)
#very similar