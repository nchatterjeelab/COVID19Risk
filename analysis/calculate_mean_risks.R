library(openxlsx)
hr = read.xlsx('data_created/highrisk_all_mixtureN_overall-mean.xlsx')
full.city = readRDS('data_created/full_output_updated.rds')
W1 = full.city$Age_18_39

# -------------- mean city risk
mean.city.risks = W1*exp(hr$mu_under40+0.5*hr$var_under40) + (1-W1)*exp(hr$mu_over40+0.5*hr$var_over40)
full.city$population = as.character(full.city$population)
full.city$population = as.numeric(full.city$population)
sum(mean.city.risks * full.city$population)/sum(full.city$population)
# 6.36555

# -------------- mean risk of NHIS individuals (without SDI)
rs.nhis = readRDS('data_created/individual_rs.rds')
sum(exp(rs.nhis$rs) * rs.nhis$sampling_weights)/sum(rs.nhis$sampling_weights)
# 5.342619

