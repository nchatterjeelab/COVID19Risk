library(dplyr)
library(survey)
library(MASS)
library(SDMTools)

### update p11 for each place from the odds ratio
update_p11 = function(p1,p2,r){
  a = 1 - r
  b = 1 + (r-1)*(p1+p2)
  c = -r*p1*p2
  (-b+sqrt(b^2-4*a*c))/(2*a)
}




#------------NO-model assumption for imputing SDI-------#
nhis = readRDS("~/Dropbox/NHANES_risk_score/500cities_data/Analysis/data_created/individual_rs_covariates.rds")
data = readRDS("~/Dropbox/NHANES_risk_score/500cities_data/Analysis/data_created/full_output_updated.rds")
data$population = as.numeric(as.character(data$population))
nhis.black = subset(nhis,race_ethnicity=="Black")
nhis.white = subset(nhis,race_ethnicity=="Non_hispanic_white")
nhis.hispanic = subset(nhis,race_ethnicity=="Hispanic")

#Black
city.size = as.numeric(as.character(data$population))/sum(as.numeric(as.character(data$population)))
prob.black = sum(data$proportion_black  * as.numeric(as.character(data$population)))/sum(as.numeric(as.character(data$population)))
prob.cities.given.black = (data$proportion_black * city.size)/prob.black
data.black = data.frame(data$sdi, prob.cities.given.black)
colnames(data.black) = c("sdi", "prob.cities.given.black")
theta = log(c(1, 1.19, 1.26, 1.53, 1.70))
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
prob.white = sum(data$proportion_non_hispanic_white_asian  * as.numeric(as.character(data$population)))/sum(as.numeric(as.character(data$population)))
prob.cities.given.white = (data$proportion_non_hispanic_white_asian * city.size)/prob.white
data.white = data.frame(data$sdi, prob.cities.given.white)
colnames(data.white) = c("sdi", "prob.cities.given.white")
theta = log(c(1, 1.19, 1.26, 1.53, 1.70))
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
theta = log(c(1, 1.19, 1.26, 1.53, 1.70))
data.hispanic = data.hispanic %>% mutate(coefficients = case_when(sdi == 1 ~ theta[1],
                                                                  sdi == 2 ~ theta[2],
                                                                  sdi == 3 ~ theta[3],
                                                                  sdi == 4 ~ theta[4],
                                                                  sdi == 5 ~ theta[5]))
nhis.hispanic.risk.sdi = sum(data.hispanic$prob.cities.given.hispanic*exp(data.hispanic$coefficients))
nhis.hispanic.risk.sdi1 = sum(data.hispanic$prob.cities.given.hispanic*data.hispanic$coefficients)
prev.sdi.hispanic = sapply(1:5,function(x){sum(data.hispanic[data.hispanic$sdi == x,'prob.cities.given.hispanic'])})



###########
nhis = nhis %>% mutate(sdi_rs_est = case_when(race_ethnicity == "Black" ~ nhis.black.risk.sdi1,
                                              race_ethnicity == "Hispanic" ~ nhis.hispanic.risk.sdi1,
                                              race_ethnicity == "Non_hispanic_white" ~ nhis.white.risk.sdi1))
nhis = nhis %>% mutate(imputated_risk = exp(nhis$rs_est) * exp(nhis$sdi_rs_est))

reference = weighted.mean(nhis$imputated_risk, w = nhis$sampling_weights )
sum(nhis$imputated_risk * nhis$sampling_weights)/sum(nhis$sampling_weights)

e1 = c(weighted.mean(exp(nhis.black$rs_est),weights=nhis.black$sampling_weights),
       weighted.mean(exp(nhis.white$rs_est),weights=nhis.white$sampling_weights),
       weighted.mean(exp(nhis.hispanic$rs_est),weights=nhis.hispanic$sampling_weights))

### distribution of Race
prop = c(sum((nhis$race_ethnicity=="Black")*nhis$sampling_weights)/sum(nhis$sampling_weights),
         sum((nhis$race_ethnicity=="Non_hispanic_white")*nhis$sampling_weights)/sum(nhis$sampling_weights),
         sum((nhis$race_ethnicity=="Hispanic")*nhis$sampling_weights)/sum(nhis$sampling_weights))
e2 = exp(c(nhis.black.risk.sdi1,nhis.white.risk.sdi1, nhis.hispanic.risk.sdi1))

sum(e1*e2*prop)


prop[1] * weighted.mean(exp(nhis.black$rs_est+nhis.black.risk.sdi1), nhis.black$sampling_weights) +
  prop[2] * weighted.mean(exp(nhis.white$rs_est+nhis.white.risk.sdi1), nhis.white$sampling_weights) + 
  prop[3] * weighted.mean(exp(nhis.hispanic$rs_est+nhis.hispanic.risk.sdi1), nhis.hispanic$sampling_weights)

prop[1] * wt.mean(exp(nhis.black$rs_est+nhis.black.risk.sdi1), nhis.black$sampling_weights) +
  prop[2] * wt.mean(exp(nhis.white$rs_est+nhis.white.risk.sdi1), nhis.white$sampling_weights) + 
  prop[3] * wt.mean(exp(nhis.hispanic$rs_est+nhis.hispanic.risk.sdi1), nhis.hispanic$sampling_weights)


prop[1] * sum(exp(nhis.black$rs_est+nhis.black.risk.sdi1) * nhis.black$sampling_weights)/ sum(nhis.black$sampling_weights) +
  prop[2] * sum(exp(nhis.white$rs_est+nhis.white.risk.sdi1)*nhis.white$sampling_weights)/sum(nhis.white$sampling_weights) + 
  prop[3] * sum(exp(nhis.hispanic$rs_est+nhis.hispanic.risk.sdi1)*nhis.hispanic$sampling_weights)/sum(nhis.hispanic$sampling_weights)






sum(data$rs_est*data$population)/sum(data$population)


sum(data$proportion_non_hispanic_white_asian*data$population)/sum(data$population)
#[1] 0.5326193
sum(data$proportion_black*data$population)/sum(data$population)
#[1] 0.1849203
sum(data$proportion_hispanic*data$population)/sum(data$population)
#[1] 0.2824604



sum(nhis$white*nhis$sampling_weights)/sum(nhis$sampling_weights)
# 0.7037475
sum(nhis$black*nhis$sampling_weights)/sum(nhis$sampling_weights)
# 0.1296687
sum(nhis$hispanic*nhis$sampling_weights)/sum(nhis$sampling_weights)
# 0.1665837






sum(data[,paste0('IMD',1:5)] * theta * data$population)/sum(data$population)
# 0.2710043



Rl = 8.300365

# ----------- marginal prevalence of SDI:
prev.sdi.marginal = 
prev.sdi.black * sum(nhis$black * nhis$sampling_weights)/sum(nhis$sampling_weights) + 
  prev.sdi.white * sum(nhis$white * nhis$sampling_weights)/sum(nhis$sampling_weights) + 
  prev.sdi.hispanic * sum(nhis$hispanic * nhis$sampling_weights)/sum(nhis$sampling_weights)
prev.sdi.marginal
# 0.03311487 0.06838399 0.13047751 0.23568351 0.53234012

# ------------ co-prevalence:
prev.sdi.white * sum(nhis$white * nhis$sampling_weights)/sum(nhis$sampling_weights)
# 0.02859843 0.05857266 0.10739925 0.17510595 0.33407124
prev.sdi.hispanic * sum(nhis$hispanic * nhis$sampling_weights)/sum(nhis$sampling_weights)
# 0.002215239 0.004875154 0.013503700 0.037405636 0.108584007
prev.sdi.black * sum(nhis$black * nhis$sampling_weights)/sum(nhis$sampling_weights)
# 0.002301210 0.004936170 0.009574563 0.023171926 0.089684869



