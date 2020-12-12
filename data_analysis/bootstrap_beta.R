
nhis_imputed = readRDS("data_created/nhis_imputed.rds")

race_distibution = read.xlsx("data_created/race_distribution_NHIS_city.xlsx")
proportion.race = race_distibution[2, 2:6]
rows = 1:nrow(nhis_imputed)
index= rep(rows, round(nhis_imputed$sampling_weights)/100)
nhis_imputed = nhis_imputed[index, ]
inx = seq(1,nrow(nhis_imputed), 1)

race_impute = function(x)
{
  set.seed(x)
  temp = which(rmultinom(1, 1, as.numeric(proportion.race))==1)
  return(temp)
}
race_new = unlist(lapply(inx, race_impute))


nhis_imputed_black = nhis_imputed[which(nhis_imputed$black ==1), ]
nhis_imputed_white = nhis_imputed[which(nhis_imputed$white ==1), ]
nhis_imputed_asian = nhis_imputed[which(nhis_imputed$asian ==1), ]
nhis_imputed_native = nhis_imputed[which(nhis_imputed$native ==1), ]
nhis_imputed_hispanic = nhis_imputed[which(nhis_imputed$hispanic ==1), ]

samp_inx_black = seq(1, nrow(nhis_imputed_black), 1)
samp_inx_white = seq(1, nrow(nhis_imputed_white), 1)
samp_inx_asian = seq(1, nrow(nhis_imputed_asian), 1)
samp_inx_native = seq(1, nrow(nhis_imputed_native), 1)
samp_inx_hispanic = seq(1, nrow(nhis_imputed_hispanic), 1)

samp_inx_black = sample(samp_inx_black, size = 373708, replace = T)
samp_inx_white = sample(samp_inx_white, size = 910698, replace = T)
samp_inx_asian = sample(samp_inx_asian, size = 181767, replace = T)
samp_inx_native = sample(samp_inx_native, size = 9471, replace = T)
samp_inx_hispanic = sample(samp_inx_hispanic, size = 563686, replace = T)


nhis_imputed_replicated_black = nhis_imputed_black[samp_inx_black, ]
nhis_imputed_replicated_black = nhis_imputed_replicated_black %>% mutate(race.imputed = "black")
nhis_imputed_replicated_white = nhis_imputed_white[samp_inx_white, ]
nhis_imputed_replicated_white = nhis_imputed_replicated_white %>% mutate(race.imputed = "white")
nhis_imputed_replicated_asian = nhis_imputed_asian[samp_inx_asian, ]
nhis_imputed_replicated_asian = nhis_imputed_replicated_asian %>% mutate(race.imputed = "asian")
nhis_imputed_replicated_native = nhis_imputed_native[samp_inx_native, ]
nhis_imputed_replicated_native = nhis_imputed_replicated_native %>% mutate(race.imputed = "native")
nhis_imputed_replicated_hispanic = nhis_imputed_hispanic[samp_inx_hispanic, ]
nhis_imputed_replicated_hispanic = nhis_imputed_replicated_hispanic %>% mutate(race.imputed = "hispanic")

nhis_imputed_replicated = rbind(nhis_imputed_replicated_black, nhis_imputed_replicated_white, nhis_imputed_replicated_asian, nhis_imputed_replicated_native, nhis_imputed_replicated_hispanic)
nhis_imputed_replicated = nhis_imputed_replicated %>% mutate(white = if_else(race.imputed == "white", 1, 0))
nhis_imputed_replicated = nhis_imputed_replicated %>% mutate(black = if_else(race.imputed == "black", 1, 0))
nhis_imputed_replicated = nhis_imputed_replicated %>% mutate(asian = if_else(race.imputed == "asian", 1, 0))
nhis_imputed_replicated = nhis_imputed_replicated %>% mutate(native = if_else(race.imputed == "native", 1, 0))
nhis_imputed_replicated = nhis_imputed_replicated %>% mutate(hispanic = if_else(race.imputed == "hispanic", 1, 0))

nhis_imputed_replicated = nhis_imputed_replicated %>% dplyr::select(-race.imputed)
nhis_imputed_replicated$sampling_weights = 1
saveRDS(nhis_imputed_replicated, "data_created/Bootstrap_samples/nhis_imputed_replicated.rds")

#########################
######## Simulate replicate samples for NHIS data for creating Bootstrap samples for NHIS data
library(openxlsx)
library(dplyr)
library(survey)
library(MASS)
library(SDMTools)
# --------------------------- load the city-level data ---------------------------
dat = readRDS('data_created/Bootstrap_samples/nhis_imputed_replicated.rds')
# ------------------- estimate SDI prevalence -------------------
update_p11 = function(p1,p2,r){
  a = 1 - r
  b = 1 + (r-1)*(p1+p2)
  c = -r*p1*p2
  (-b+sqrt(b^2-4*a*c))/(2*a)
}
nhis = dat
data = readRDS("data_created/combined_updated.rds")
data$population = as.numeric(as.character(data$population))
nhis.black = subset(nhis,black==1)
nhis.white = subset(nhis,white==1)
nhis.hispanic = subset(nhis,hispanic==1)
nhis.native = subset(nhis,native==1)
nhis.asian = subset(nhis,asian==1)
# Black
city.size = as.numeric(as.character(data$population))/sum(as.numeric(as.character(data$population)))
prob.black = sum(data$proportion_black  * as.numeric(as.character(data$population)))/sum(as.numeric(as.character(data$population)))
prob.cities.given.black = (data$proportion_black * city.size)/prob.black
data.black = data.frame(data$sdi, prob.cities.given.black)
colnames(data.black) = c("sdi", "prob.cities.given.black")
theta = c(1,0.148420005,0.231111721,0.431782416,0.570979547)#log(c(1, 1.19, 1.26, 1.53, 1.70))
data.black = data.black %>% mutate(coefficients = case_when(sdi == 1 ~ theta[1],
                                                            sdi == 2 ~ theta[2],
                                                            sdi == 3 ~ theta[3],
                                                            sdi == 4 ~ theta[4],
                                                            sdi == 5 ~ theta[5]))
nhis.black.risk.sdi = sum(data.black$prob.cities.given.black*exp(data.black$coefficients))
nhis.black.risk.sdi1 = sum(data.black$prob.cities.given.black*data.black$coefficients)
prev.sdi.black = sapply(1:5,function(x){sum(data.black[data.black$sdi == x,'prob.cities.given.black'])})
#Non_hispanic_white
city.size = as.numeric(as.character(data$population))/sum(as.numeric(as.character(data$population)))
prob.white = sum(data$proportion_white * as.numeric(as.character(data$population)))/sum(as.numeric(as.character(data$population)))
prob.cities.given.white = (data$proportion_white * city.size)/prob.white
data.white = data.frame(data$sdi, prob.cities.given.white)
colnames(data.white) = c("sdi", "prob.cities.given.white")
theta = c(1,0.148420005,0.231111721,0.431782416,0.570979547)#log(c(1, 1.19, 1.26, 1.53, 1.70))
data.white = data.white %>% mutate(coefficients = case_when(sdi == 1 ~ theta[1],
                                                            sdi == 2 ~ theta[2],
                                                            sdi == 3 ~ theta[3],
                                                            sdi == 4 ~ theta[4],
                                                            sdi == 5 ~ theta[5]))
nhis.white.risk.sdi = sum(data.white$prob.cities.given.white*exp(data.white$coefficients))
nhis.white.risk.sdi1 = sum(data.white$prob.cities.given.white*data.white$coefficients)
prev.sdi.white = sapply(1:5,function(x){sum(data.white[data.white$sdi == x,'prob.cities.given.white'])})
#Hispanic
city.size = as.numeric(as.character(data$population))/sum(as.numeric(as.character(data$population)))
prob.hispanic = sum(data$proportion_hispanic  * as.numeric(as.character(data$population)))/sum(as.numeric(as.character(data$population)))
prob.cities.given.hispanic = (data$proportion_hispanic * city.size)/prob.hispanic
data.hispanic = data.frame(data$sdi, prob.cities.given.hispanic)
colnames(data.hispanic) = c("sdi", "prob.cities.given.hispanic")
theta = c(1,0.148420005,0.231111721,0.431782416,0.570979547)#log(c(1, 1.19, 1.26, 1.53, 1.70))
data.hispanic = data.hispanic %>% mutate(coefficients = case_when(sdi == 1 ~ theta[1],
                                                                  sdi == 2 ~ theta[2],
                                                                  sdi == 3 ~ theta[3],
                                                                  sdi == 4 ~ theta[4],
                                                                  sdi == 5 ~ theta[5]))
nhis.hispanic.risk.sdi = sum(data.hispanic$prob.cities.given.hispanic*exp(data.hispanic$coefficients))
nhis.hispanic.risk.sdi1 = sum(data.hispanic$prob.cities.given.hispanic*data.hispanic$coefficients)
prev.sdi.hispanic = sapply(1:5,function(x){sum(data.hispanic[data.hispanic$sdi == x,'prob.cities.given.hispanic'])})
# Asian
city.size = as.numeric(as.character(data$population))/sum(as.numeric(as.character(data$population)))
prob.asian = sum(data$proportion_asian  * as.numeric(as.character(data$population)))/sum(as.numeric(as.character(data$population)))
prob.cities.given.asian = (data$proportion_asian * city.size)/prob.asian
data.asian = data.frame(data$sdi, prob.cities.given.asian)
colnames(data.asian) = c("sdi", "prob.cities.given.asian")
theta = c(1,0.148420005,0.231111721,0.431782416,0.570979547)#log(c(1, 1.19, 1.26, 1.53, 1.70))
data.asian = data.asian %>% mutate(coefficients = case_when(sdi == 1 ~ theta[1],
                                                            sdi == 2 ~ theta[2],
                                                            sdi == 3 ~ theta[3],
                                                            sdi == 4 ~ theta[4],
                                                            sdi == 5 ~ theta[5]))
nhis.asian.risk.sdi = sum(data.asian$prob.cities.given.asian*exp(data.asian$coefficients))
nhis.asian.risk.sdi1 = sum(data.asian$prob.cities.given.asian*data.asian$coefficients)
prev.sdi.asian = sapply(1:5,function(x){sum(data.asian[data.asian$sdi == x,'prob.cities.given.asian'])})
# Native
city.size = as.numeric(as.character(data$population))/sum(as.numeric(as.character(data$population)))
prob.native = sum(data$proportion_american_indian_alaska_native  * as.numeric(as.character(data$population)))/sum(as.numeric(as.character(data$population)))
prob.cities.given.native = (data$proportion_american_indian_alaska_native * city.size)/prob.native
data.native = data.frame(data$sdi, prob.cities.given.native)
colnames(data.native) = c("sdi", "prob.cities.given.native")
theta = c(1,0.148420005,0.231111721,0.431782416,0.570979547)#log(c(1, 1.19, 1.26, 1.53, 1.70))
data.native = data.native %>% mutate(coefficients = case_when(sdi == 1 ~ theta[1],
                                                              sdi == 2 ~ theta[2],
                                                              sdi == 3 ~ theta[3],
                                                              sdi == 4 ~ theta[4],
                                                              sdi == 5 ~ theta[5]))
nhis.native.risk.sdi = sum(data.native$prob.cities.given.native*exp(data.native$coefficients))
nhis.native.risk.sdi1 = sum(data.native$prob.cities.given.native*data.native$coefficients)
prev.sdi.native = sapply(1:5,function(x){sum(data.native[data.native$sdi == x,'prob.cities.given.native'])})


# ----------- marginal prevalence of SDI:
# make sure nhis includes only the 5 race categories
prev.sdi.marginal = 
  prev.sdi.black * sum(nhis$black * nhis$sampling_weights)/sum(nhis$sampling_weights) + 
  prev.sdi.white * sum(nhis$white * nhis$sampling_weights)/sum(nhis$sampling_weights) + 
  prev.sdi.hispanic * sum(nhis$hispanic * nhis$sampling_weights)/sum(nhis$sampling_weights) + 
  prev.sdi.asian * sum(nhis$asian * nhis$sampling_weights)/sum(nhis$sampling_weights) + 
  prev.sdi.native * sum(nhis$native * nhis$sampling_weights)/sum(nhis$sampling_weights)


####### impute SDI
indscore = dat
set.seed(2020)
indscore$sdi = 0
prevs = rbind(prev.sdi.white, prev.sdi.hispanic, prev.sdi.black, prev.sdi.asian, prev.sdi.native)
rownames(prevs) = c('Non_hispanic_white', 'Hispanic', 'Non_hispanic_Black', 'Non_hispanic_asian', 'Non_hispanic_american_Indian')
tem = sapply(indscore$race_ethnicity.cdc,function(x){which(rmultinom(1, 1, prevs[x,])==1)})
indscore$sdi = tem
indscore$sdi2 = ifelse(indscore$sdi == 2, 1, 0)
indscore$sdi3 = ifelse(indscore$sdi == 3, 1, 0)
indscore$sdi4 = ifelse(indscore$sdi == 4, 1, 0)
indscore$sdi5 = ifelse(indscore$sdi == 5, 1, 0)

### impute arthritis
# among arthritis, the ratio_rheumatoid_to_non_rheumatoid =  0.269098
ratio.rheumatoid.arthritis = 0.269098/(1+0.269098)
indscore$rheumatoid = 0
set.seed(2020)
indscore$rheumatoid = sapply(indscore$arthritis,function(x){ifelse(x == 1, rbinom(n=1, size=1, prob=ratio.rheumatoid.arthritis), 0)})
nhis_imputed = indscore
saveRDS(nhis_imputed, file = 'data_created/nhis_imputed_replicate.rds')
################################


nhis_imputed_replicated_with_sdi = readRDS("data_created/nhis_imputed_replicate.rds")



nhis_imputed_replicated_with_sdi_yes_hemo = nhis_imputed_replicated_with_sdi[which(nhis_imputed_replicated_with_sdi$hematologic_cancer1 == 1 | nhis_imputed_replicated_with_sdi$hematologic_cancer2 == 1 | nhis_imputed_replicated_with_sdi$hematologic_cancer3 == 1), ]
nhis_imputed_replicated_with_sdi_no_hemo = nhis_imputed_replicated_with_sdi[which(nhis_imputed_replicated_with_sdi$hematologic_cancer1 == 0 & nhis_imputed_replicated_with_sdi$hematologic_cancer2 == 0 & nhis_imputed_replicated_with_sdi$hematologic_cancer3 == 0), ]

inx = seq(1, nrow(nhis_imputed_replicated_with_sdi_yes_hemo), 1)
prop.hemato.cat.UK = c(8704, 27742, 63460)/sum(c(8704, 27742, 63460)) 
hemato_cat_impute = function(x)
{
  set.seed(x)
  temp = which(rmultinom(1, 1, prop.hemato.cat.UK)==1)
  return(temp)
}
hemato_cat_new = unlist(lapply(inx, hemato_cat_impute))

nhis_imputed_replicated_with_sdi_yes_hemo = nhis_imputed_replicated_with_sdi_yes_hemo %>% mutate(hematologic_cancer1 = if_else(hemato_cat_new == 1, 1, 0))
nhis_imputed_replicated_with_sdi_yes_hemo = nhis_imputed_replicated_with_sdi_yes_hemo %>% mutate(hematologic_cancer2= if_else(hemato_cat_new == 2, 1, 0))
nhis_imputed_replicated_with_sdi_yes_hemo = nhis_imputed_replicated_with_sdi_yes_hemo %>% mutate(hematologic_cancer3= if_else(hemato_cat_new == 3, 1, 0))

nhis_imputed_replicated_with_sdi = rbind(nhis_imputed_replicated_with_sdi_yes_hemo, nhis_imputed_replicated_with_sdi_no_hemo)

reference_data = nhis_imputed_replicated_with_sdi[, c(27:32, 42, 44, 41, 43, 59:62, 34:39, 49:50, 13, 51, 10, 52:57, 14, 18, 12, 5, 25,26)]
reference_data = cbind(rep(1, nrow(reference_data)), reference_data)
colnames(reference_data)[1] = c("Intercept")
covariates = colnames(reference_data)

beta = c(-9.1304992, -2.659260037, -1.237874356,
         0.862889955,
         1.800058272,
         3.005187432,
         1.1575044,
         1.0171116,
         0.3220496,
         0.5443152,
         0.148,
         0.231,
         0.432,
         0.571,
         0.432,
         0.049,
         0.344,
         0.652,
         0.199,
         -0.072,
         -0.094,
         0.501,
         -0.047,
         0.148,
         0.366,
         0.513,
         0.191,
         -0.02,
         0.845,
         0.928,
         0.438,
         0.77,
         0.362,
         0.14)


p = as.numeric(expit(as.matrix(reference_data[, c(1:3,5:35)]) %*% beta))
set.seed(2019)
Y = rbinom(nrow(reference_data), 1, prob = p)
reference_data = cbind(Y, reference_data)

logistic_fit =glm(as.formula(paste0("Y~", paste0(colnames(reference_data)[c(3:4, 6:36)], collapse = "+"))), data = reference_data, family = binomial())

round(sqrt(exp(2*logistic_fit$coefficients)*diag(vcov(logistic_fit)))/2.75021,3)


nhis_bootstrap_sample_list = list()


nhissvy <- svydesign(id=~PPSU, strata=~PSTRAT,
                     nest = TRUE,
                     weights=~sampling_weights,
                     data=nhis_imputed)
bootstrap_weights = list()
for(sim in 1: 100)
{
  set.seed(sim)
  nhissvy_svrepdesign = as.svrepdesign(nhissvy, type=c("subbootstrap"), replicates = 50)
  bootstrap_weights[[sim]] = as.matrix(nhissvy_svrepdesign$repweights)
}

nhis_bootstrap_sample_list = list()
for(sim in 1: 100)
{
  for(j in 1:50)
  {
    k = 50*(sim-1) + j
    temp = nhis_imputed
    temp$sampling_weights = bootstrap_weights[[sim]][,j]
    nhis_bootstrap_sample_list[[k]] = temp
  }
 
}

nhis_imputed = readRDS("data_created/nhis_imputed.rds")
reference_data = nhis_imputed[, c(27:32, 42, 44, 41, 43, 59:62, 34:39, 49:50, 13, 51, 10, 52:57, 14, 18, 12, 5, 25,26)]
covariates = colnames(reference_data)
simulated_data_X = list()
for(sim in 1:100)
{
  nhis_data = c()
  j = 50*sim
  for(k in sort(seq(from = j, length.out = 50, by = -1)))
  {
    temp = nhis_bootstrap_sample_list[[k]] %>% dplyr::select(all_of(covariates))
    temp = cbind(rep(1, nrow(temp)), temp)
    colnames(temp)[1] = c("Intercept")
    nhis_data = rbind(nhis_data, temp)
  }
  simulated_data_X[[sim]] = nhis_data
  print(sim)
}

beta = c(-9.1304992, -2.659260037, -1.237874356,
         0.862889955,
         1.800058272,
         3.005187432,
         1.1575044,
         1.0171116,
         0.3220496,
         0.5443152,
         0.148,
         0.231,
         0.432,
         0.571,
         0.432,
         0.049,
         0.344,
         0.652,
         0.199,
         -0.072,
         -0.094,
         0.501,
         -0.047,
         0.148,
         0.366,
         0.513,
         0.191,
         -0.02,
         0.845,
         0.928,
         0.438,
         0.77,
         0.362,
         0.14)

for(sim in 1:100)
{
  p = as.numeric(expit(as.matrix(simulated_data_X[[sim]][, 1:34]) %*% beta))
  set.seed(sim)
  Y = rbinom(nrow(simulated_data_X[[sim]]), 1, prob = p)
  simulated_data_X[[sim]] = cbind(Y, simulated_data_X[[sim]])
  print(sim)
}

beta.matrix = matrix(NA, 100,34)
beta.var = matrix(NA, 100,34)
for(sim in 1:100)
{
  nhissvy <- svydesign(id=~PPSU, strata=~PSTRAT,
                       nest = TRUE,
                       weights=~sampling_weights,
                       data=simulated_data_X[[sim]])
  logistic_fit =svyglm(as.formula(paste0("Y~", paste0(colnames(simulated_data_X[[sim]])[c(3:4, 6:36)], collapse = "+"))), design = nhissvy, family = quasibinomial())
  beta.matrix[sim, ] = as.numeric(logistic_fit$coefficients)
  beta.var[sim, ] = as.numeric(diag(vcov(logistic_fit)))
  print(sim)
}
beta.var.OR = matrix(NA, 100,34)
for(sim in 1:100)
{
  beta.var.OR[sim,] = exp(2*beta.matrix[sim,])*beta.var[sim,]
}

result = cbind(c("Intercept", colnames(simulated_data_X[[sim]])[c(3:4, 6:36)]), round(apply(sqrt(beta.var.OR), 2, mean),3)/2.457627)
result2 = cbind(c("Intercept", colnames(simulated_data_X[[sim]])[c(3:4, 6:36)]), round(sqrt(apply(exp(beta.matrix), 2, var)),3))






#------Variance-Covariance matrix from nature paper---#
var.cov.beta.nature = read.xlsx("data_created/Variance_covariance_beta_opensafely_nature.xlsx")
var.cov.beta.nature = var.cov.beta.nature[, -1]

index = c(7:12, 15, 17:34, 36, 38:39, 42) -1

var.cov.beta.nature = var.cov.beta.nature[index, index]
var.cov.beta.nature = var.cov.beta.nature %>% mutate_all(funs(replace_na(.,0)))
var.cov.beta.nature$reduced_kidney_1 = as.numeric(var.cov.beta.nature$reduced_kidney_1)

names_var.cov.beta.nature = colnames(var.cov.beta.nature)
var.cov.beta.nature_upper = t(var.cov.beta.nature)

var.cov.beta.nature = as.matrix(var.cov.beta.nature + var.cov.beta.nature_upper)
diag(var.cov.beta.nature) = diag(var.cov.beta.nature)/2

hazard_ratio = c(1.54, 1.05, 1.41, 1.92, 1.22, 0.93, 1.48, 1.16, 1.26, 1.54, 1.77, 0.91, 1.65, 0.94, 1.08, 1.16, 1.28, 1.86, 1.86, 1.67, 1.21, .98, 2.33, 2.53, 1.55, 2.16, 1.37, 2.5, 1.15)

var.cov.hazard.ratio.nature = diag(hazard_ratio) %*% as.matrix(var.cov.beta.nature) %*% diag(hazard_ratio)
colnames(var.cov.hazard.ratio.nature) = names_var.cov.beta.nature


w_asthma = c(0.142, 0.017)/sum(c(0.142, 0.017))
coeff_asthma = c(0.94, 1.08)

w_kidney = c(0.058, 0.005)/sum(c(0.058, 0.005))
coeff_kidney = c(1.37, 2.50)

var.cov.hazard.ratio.nature.appended = matrix(0, 31, 31)
var.cov.hazard.ratio.nature.appended[1:29, 1:29] = var.cov.hazard.ratio.nature

var.cov.hazard.ratio.nature.appended[30, 30] = w_asthma[1]^2* var.cov.hazard.ratio.nature.appended[14,14] + w_asthma[2]^2* var.cov.hazard.ratio.nature.appended[15,15] + 2*w_asthma[1]*w_asthma[2]*var.cov.hazard.ratio.nature.appended[14, 15]
var.cov.hazard.ratio.nature.appended[31, 31] = w_kidney[1]^2* var.cov.hazard.ratio.nature.appended[27,27] + w_kidney[2]^2* var.cov.hazard.ratio.nature.appended[28,28] + 2*w_kidney[1]*w_kidney[2]*var.cov.hazard.ratio.nature.appended[27, 28]

for(i in 1:29)
{
  if(i == 14 | i == 15 | i == 27 | i == 28)
  {
    var.cov.hazard.ratio.nature.appended[i, 30] = 0
  } else{
    var.cov.hazard.ratio.nature.appended[i, 30] = w_asthma[1] * var.cov.hazard.ratio.nature.appended[i, 14] + w_asthma[2] * var.cov.hazard.ratio.nature.appended[i, 15]
  }
}

var.cov.hazard.ratio.nature.appended[30, 1:29] = var.cov.hazard.ratio.nature.appended[1:29, 30]


for(i in 1:29)
{
  if(i == 14 | i == 15 | i == 27 | i == 28)
  {
    var.cov.hazard.ratio.nature.appended[i, 31] = 0
  } else{
    var.cov.hazard.ratio.nature.appended[i, 31] = w_kidney[1] * var.cov.hazard.ratio.nature.appended[i, 27] + w_kidney[2] * var.cov.hazard.ratio.nature.appended[i, 28]
  }
}

var.cov.hazard.ratio.nature.appended[31, 1:29] = var.cov.hazard.ratio.nature.appended[1:29, 31]

var.cov.hazard.ratio.nature.appended[30, 31] = var.cov.hazard.ratio.nature.appended[31,30] = w_asthma[1] * w_kidney[1] * var.cov.hazard.ratio.nature.appended[14, 27] +
  w_asthma[1] * w_kidney[2] * var.cov.hazard.ratio.nature.appended[14, 28] +
  w_asthma[2] * w_kidney[1] * var.cov.hazard.ratio.nature.appended[15, 27] +
  w_asthma[2] * w_kidney[2] * var.cov.hazard.ratio.nature.appended[15, 28]

var.cov.hazard.ratio.nature.appended = var.cov.hazard.ratio.nature.appended[-c(14,15, 27, 28), -c(14, 15, 27, 28)]


colnames(var.cov.hazard.ratio.nature.appended) = c(names_var.cov.beta.nature[-c(14,15, 27, 28)], "Asthma", "Kidney")

hazard_ratio = c(hazard_ratio[-c(14,15, 27, 28)], sum(coeff_asthma*w_asthma), sum(coeff_kidney*w_kidney))

var.cov.beta.nature.appended = diag(1/hazard_ratio) %*% var.cov.hazard.ratio.nature.appended %*% diag(1/hazard_ratio)

colnames(var.cov.beta.nature.appended) = colnames(var.cov.hazard.ratio.nature.appended)
rownames(var.cov.beta.nature.appended) = colnames(var.cov.hazard.ratio.nature.appended)
var.cov.beta.nature.appended = var.cov.beta.nature.appended[c(1:4, 6,5,12,13,26, 14, 16, 15, 18:23, 24, 27, 25,8:11), c(c(1:4, 6,5,12,13,26, 14, 16, 15, 18:23, 24, 27, 25,8:11))]
saveRDS(var.cov.beta.nature.appended, file = "data_created/Bootstrap_samples/var_covar_beta_nature.rds")

beta.nature = log(hazard_ratio[c(1:4, 6,5,12,13,26, 14, 16, 15, 18:23, 24, 27, 25,8:11)])
