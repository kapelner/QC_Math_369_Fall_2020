ssq1 = 2.07^2
ssq2 = 2.25^2
n1 = 10
n2 = 6

sampling_distr_var = ssq1 / n1 + ssq2 / n2
sampling_distr_se = sqrt(sampling_distr_var)
sampling_distr_se

df_satt_numer = sampling_distr_var^2
df_satt_denom = ssq1^2 / (10^2 * 9) + ssq2^2 / (6^2 * 5)
df_satt = df_satt_numer / df_satt_denom
df_satt

qt(.025, df_satt) #critical cutoff of stdized sampling distribution
# qt(.05, df_satt) #critical cutoff of stdized sampling distribution on homework 3

test_stat = 8.2
test_stat_stdized = test_stat / sampling_distr_se
test_stat_stdized
