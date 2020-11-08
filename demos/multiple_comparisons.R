m = 50
#assume all nulls are true => pvals are realizations from iid standard uniform
set.seed(9)
ps = runif(m)
ps
alpha = 0.05
tests_rejected_naively = which(ps < alpha)
ps[tests_rejected_naively]
length(tests_rejected_naively)
#there are four Type I errors / false rejections / false discoveries


#Bonferroni procedure. Set FWER = alpha above and adjust the per-test alpha
bonferroni_threshold = alpha / m
tests_rejected_bonferroni = which(ps < bonferroni_threshold)
length(tests_rejected_bonferroni)
ps_bonferroni_adj = ps * alpha / bonferroni_threshold
sort(ps_bonferroni_adj)

#Sidak procedure. Set FWER = alpha above and adjust the per-test alpha
sidak_threshold = 1 - (1 - alpha)^(1 / m)
tests_rejected_sidak = which(ps < sidak_threshold)
length(tests_rejected_sidak)
ps_sidak_adj = ps * alpha / sidak_threshold
sort(ps_sidak_adj)

#Benjami-Hochberg linear step up procedure. Set FWER = alpha above
sorted_idx = order(ps)
ps_sorted = ps[sorted_idx]
ps_sorted
stepped_up_thresholds = alpha * (1 : m) / m 
stepped_up_thresholds
ps_sorted < stepped_up_thresholds



pacman::p_load(data.table)
#X = fread("https://zenodo.org/record/2396572/files/Yeast_AssembledData3Columns.csv?download=1")
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6792083
#https://zenodo.org/record/2396572#.X6NLm4hKiUk


