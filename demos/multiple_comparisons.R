m = 100
#assume all nulls are true => pvals are realizations from iid standard uniform
set.seed(9)
ps = runif(m)
ps
FWER_0 = 0.05

#Naive procedure. Let alpha = FWER_0.
tests_rejected_naively = which(ps < FWER_0)
ps[tests_rejected_naively]
length(tests_rejected_naively)
#there are four Type I errors / false rejections / false discoveries


#Bonferroni procedure. Set FWER = alpha above and adjust the per-test alpha
alpha_bonferroni = FWER_0 / m
tests_rejected_bonferroni = which(ps < alpha_bonferroni)
length(tests_rejected_bonferroni)
ps_bonferroni_adj = ps * FWER_0 / alpha_bonferroni
sort(ps_bonferroni_adj)

#Sidak procedure. Adjust the per-test alpha
alpha_sidak = 1 - (1 - FWER_0)^(1 / m)
tests_rejected_sidak = which(ps < alpha_sidak)
length(tests_rejected_sidak)
ps_sidak_adj = ps * FWER_0 / alpha_sidak
sort(ps_sidak_adj)

#Simes linear step up procedure.
sorted_idx = order(ps)
ps_sorted = ps[sorted_idx]
ps_sorted
alpha_stepped_up_thresholds = FWER_0 * (1 : m) / m 
alpha_stepped_up_thresholds
which(ps_sorted < alpha_stepped_up_thresholds)
a_star = 0

pacman::p_load(ggplot2)
ggplot(data.frame(test_number = 1 : m, ordered_pvals = ps_sorted, alpha_stepped_up_thresholds = alpha_stepped_up_thresholds)) + 
  geom_point(aes(x = test_number, y = ordered_pvals)) + 
  scale_y_log10() +
  geom_hline(yintercept = FWER_0, col = "red") +
  geom_hline(yintercept = alpha_bonferroni, col = "yellow") +
  geom_hline(yintercept = alpha_sidak, col = "gray") +
  geom_point(aes(x = test_number, y = alpha_stepped_up_thresholds), col = "green")
  


pacman::p_load(data.table, ggplot2)

rm(list = ls())
#https://zenodo.org/record/2396572#.X6NLm4hKiUk
X = fread("IMPC_ProcessedData_Continuous.csv")
m = nrow(X)
X

# ... The first is from the International Mouse Phenotyping Consortium (IMPC). As described in 
# Karp et al. (2017), the IMPC coordinates a large study to functionally annotate every protein 
# coding gene by exploring the impact of the gene knockout on the resulting phenotype for up to 
# 234 traits of interest. Data are uploaded to a public database where phenodeviants are identified 
# using a fixed significance threshold (P???<???0.0001). The dataset and resulting family of hypotheses 
# constantly grow as new knockouts are studied. As part of their analysis, Karp et al. tested both 
# the role of genotype and the role of sex as a modifier of genotype effect. Hence, the analysis 
# resulted in two sets of P-values, one for testing genotype effects and the other for testing 
# sexual dimorphism.
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6792083

#we're going to look only at sexual dimorphism testing for each gene
X = X[, .(pvalue = Sex.Mvko.P.Val)]
ggplot(X) + geom_histogram(aes(x = pvalue), bins = 1000)
X

FWER_0 = 0.05
FDR_0 = 0.05
setorder(X, pvalue)

tests_rejected_naive = which(X$pvalue < FWER_0)
length(tests_rejected_naive)


alpha_bonferroni = FWER_0 / m
tests_rejected_bonferroni = which(X$pvalue < alpha_bonferroni)
length(tests_rejected_bonferroni)

alpha_sidak = 1 - (1 - FWER_0)^(1 / m)
tests_rejected_sidak = which(X$pvalue < alpha_sidak)
length(tests_rejected_sidak)

X[, test_number := 1 : m]
X[, simes_pvalue := test_number / m * FDR_0]
max(which(X$pvalue < X$simes_pvalue))

ggplot(X) + 
  geom_line(aes(x = test_number, y = pvalue)) + 
  xlim(1, 100000) + 
  xlab("test number in pvalue increasing rank order") +
  scale_y_log10(limits = c(1E-15, 1)) +
  geom_hline(yintercept = FWER_0, col = "red") +
  geom_hline(yintercept = alpha_sidak, col = "gray") +
  geom_hline(yintercept = alpha_bonferroni, col = "orange") +
  geom_line(aes(x = test_number, y = simes_pvalue), col = "green")
  # annotate("text", label = "Bonferroni / Sidak thresholds", x = 25000, y = alpha_bonferroni*3, size = 4, colour = "orange") + 
  # annotate("text", label = "Naive threshold", x = 10000, y = FWER_0*3, size = 4, colour = "red") + 
  # annotate("text", label = "Sorted p values", x = 7000, y = 1e-12, size = 4, colour = "black") + 
  # annotate("text", label = "Simes threshold", x = 25000, y = 3e-3, size = 4, colour = "green") 
  
  

#Isn't just doing naive give you FDR control?
X[, naive_rejection := pvalue <= FDR_0]
bins = 1000
ggplot(X) + 
  scale_y_log10() +
  geom_histogram(aes(x = pvalue, col = naive_rejection), bins = bins) +
  geom_hline(yintercept = m / bins, col = "purple")
#no.... what is FDR doing?
#it's essentially cutting out that piece that's expected if all H_0's are true
# X[, simes_rejection := pvalue <= simes_pvalue]
# ggplot(X) + 
#   scale_y_log10() +
#   geom_histogram(aes(x = pvalue, col = simes_rejection), bins = bins) +
#   geom_hline(yintercept = m / bins, col = "purple")
# ggplot(X) + 
#   # scale_y_log10() +
#   geom_histogram(aes(x = pvalue, col = simes_rejection), bins = bins) +
#   geom_hline(yintercept = m / bins * FDR_0, col = "purple") +
#   xlim(0, FDR_0)
#see https://genomicsclass.github.io/book/pages/multiple_testing.html









rm(list = ls())
#https://zenodo.org/record/2396572#.X6NLm4hKiUk
X = fread("Yeast_AssembledData3Columns.csv")
m = nrow(X)
X

# ... Wildenhain et al. (2016), contains phenotypic growth data for 240 diverse yeast gene 
# deletion strains grown in the presence of about 5500 unique compounds. This collection has 
# been generated to investigate how small molecule chemical-genetic fingerprints could be used 
# to predict synergistic chemical-chemical combinations that induce lethal phenotypes.
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6792083


FWER_0 = 0.05
FDR_0 = 0.05
setorder(X, pvalue)

tests_rejected_naive = which(X$pvalue < FWER_0)
length(tests_rejected_naive)


alpha_bonferroni = FWER_0 / m
tests_rejected_bonferroni = which(X$pvalue < alpha_bonferroni)
length(tests_rejected_bonferroni)

alpha_sidak = 1 - (1 - FWER_0)^(1 / m)
tests_rejected_sidak = which(X$pvalue < alpha_sidak)
length(tests_rejected_sidak)

X[, test_number := 1 : m]
X[, simes_pvalue := test_number / m * FDR_0]
max(which(X$pvalue < X$simes_pvalue))

pacman::p_load(ggplot2)
ggplot(X) + 
  geom_line(aes(x = test_number, y = pvalue)) + 
  scale_y_log10(limits = c(1E-15, 1)) +
  geom_hline(yintercept = FWER_0, col = "red") +
  geom_hline(yintercept = alpha_bonferroni, col = "yellow") +
  geom_hline(yintercept = alpha_sidak, col = "gray") +
  geom_line(aes(x = test_number, y = simes_pvalue), col = "green")



