library(openxlsx)
library(dplyr)
library(survey)
library(MASS)
library(SDMTools)
# --------------------------- load the city-level data ---------------------------
dat = readRDS('data_created/nhis_2017.rds')
col.keep = c("SRVY_YR", "HHX", "FMX", "FPX", "sampling_weights",
             "sex", "age", "BMI", "agegroup", "diabetes",
             "smoking_status", "rheumatoid", "asthma", "stroke", 
             "heart_disease", "hypertension", "resp_ex_asthma", "kidney_disease",
             "liver_disease","race_ethnicity.cdc",
             "hematologic_cancer", "non_hematologic_cancer","diagnoses_cancer",
             "Obesity" )
missing.dat = sapply(1:length(col.keep),function(x){sum(!complete.cases(dat[,col.keep[x]]))})
names(missing.dat) = col.keep
missing.dat
dat = dat[complete.cases(dat[,col.keep[-which(col.keep == 'diagnoses_cancer')]]),col.keep] # 22179
dat$Age_15_44 = ifelse((dat$age>=15)&(dat$age<=44),1,0)
dat$Age_45_54 = ifelse((dat$age>=45)&(dat$age<=54),1,0)
dat$Age_55_64 = ifelse((dat$age>=55)&(dat$age<=64),1,0)
dat$Age_65_74 = ifelse((dat$age>=65)&(dat$age<=74),1,0)
dat$Age_75_84 = ifelse((dat$age>=75)&(dat$age<=84),1,0)
dat$Age_85 = ifelse((dat$age>=85),1,0)
# gender
dat$female = ifelse(dat$sex=='Female',1,0)
dat$male = ifelse(dat$sex=='Male',1,0)
# obesity
dat$obesity1 = ifelse((dat$BMI>=30)&(dat$BMI<35),1,0)
dat$obesity2 = ifelse((dat$BMI>=35)&(dat$BMI<40),1,0)
dat$obesity3 = ifelse((dat$BMI>=40),1,0)
# smoking
dat$smoking_ex = ifelse(dat$smoking_status=='Former',1,0)
dat$smoking_current = ifelse(dat$smoking_status=='Current',1,0)
# ethnicity
dat$white = ifelse(dat$race_ethnicity.cdc=='Non_hispanic_white',1,0) # 15756
dat$asian = ifelse(dat$race_ethnicity.cdc=='Non_hispanic_asian',1,0) # 801 
dat$hispanic = ifelse(dat$race_ethnicity.cdc=='Hispanic',1,0) # 2791
dat$native = ifelse(dat$race_ethnicity.cdc=='Non_hispanic_american_Indian',1,0) # 231
dat$black = ifelse(dat$race_ethnicity.cdc=='Non_hispanic_Black',1,0) # 2548
dat = dat[dat$race_ethnicity.cdc %in% c('Non_hispanic_white', 'Non_hispanic_asian',
                                    'Hispanic', 'Non_hispanic_american_Indian', 'Non_hispanic_Black'),]
# missing SDI information. Will impute SDI later.
dat$IMD2 = 0
dat$IMD3 = 0
dat$IMD4 = 0
dat$IMD5 = 0
# high blood pressure
dat$hbp = ifelse(dat$hypertension=='Hypertension_high_bp',1,0)
# COPD
dat$copd = ifelse(dat$resp_ex_asthma=='Yes',1,0)
# asthma - combine
dat$asthma = ifelse(dat$asthma=='Yes',1,0)
# CHD
dat$chd = ifelse(dat$heart_disease=='Yes',1,0)
# diabetes - combine
dat$diabetes = ifelse(dat$diabetes=='Yes',1,0)
# non_hematologic_cancer
dat$non_hematologic_cancer1 = ifelse((dat$non_hematologic_cancer=='Non_hematological')&(dat$diagnoses_cancer == 'less_than_1_yr'),1,0)
dat$non_hematologic_cancer2 = ifelse((dat$non_hematologic_cancer=='Non_hematological')&(dat$diagnoses_cancer == '1_5yr'),1,0)
dat$non_hematologic_cancer3 = ifelse((dat$non_hematologic_cancer=='Non_hematological')&(dat$diagnoses_cancer == 'greater_than_5_yr'),1,0)
# hematologic_cancer
dat$hematologic_cancer1 = ifelse((dat$hematologic_cancer=='Hematological')&(dat$diagnoses_cancer == 'less_than_1_yr'),1,0)
dat$hematologic_cancer2 = ifelse((dat$hematologic_cancer=='Hematological')&(dat$diagnoses_cancer == '1_5yr'),1,0)
dat$hematologic_cancer3 = ifelse((dat$hematologic_cancer=='Hematological')&(dat$diagnoses_cancer == 'greater_than_5_yr'),1,0)
# remove the ones that do not have diagnosis year
dat = dat[complete.cases(dat[,-which(colnames(dat) %in% c('diagnoses_cancer'))]),]  # 22161
sapply(1:ncol(dat),function(x){sum(is.na(dat[,x]))})
# stroke
dat$stroke = ifelse(dat$stroke=='Yes',1,0)
# kidney disease
dat$kidney_disease = ifelse(dat$kidney_disease=='Yes',1,0)
# rheumatoid
dat$rheumatoid = ifelse(dat$rheumatoid=='Yes',1,0)


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
theta = log(c(1, 1.19, 1.26, 1.53, 1.70))
data.black = data.black %>% mutate(coefficients = case_when(sdi == 1 ~ theta[1],
                                                            sdi == 2 ~ theta[2],
                                                            sdi == 3 ~ theta[3],
                                                            sdi == 4 ~ theta[4],
                                                            sdi == 5 ~ theta[5]))
nhis.black.risk.sdi = sum(data.black$prob.cities.given.black*exp(data.black$coefficients))
nhis.black.risk.sdi1 = sum(data.black$prob.cities.given.black*data.black$coefficients)
prev.sdi.black = sapply(1:5,function(x){sum(data.black[data.black$sdi == x,'prob.cities.given.black'])})
# 0.01832218 0.03682176 0.08825083 0.17395554 0.68264968
#Non_hispanic_white
city.size = as.numeric(as.character(data$population))/sum(as.numeric(as.character(data$population)))
prob.white = sum(data$proportion_white * as.numeric(as.character(data$population)))/sum(as.numeric(as.character(data$population)))
prob.cities.given.white = (data$proportion_white * city.size)/prob.white
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
# 0.04599002 0.09003797 0.16981682 0.23999593 0.45415925
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
# 0.01423473 0.02870951 0.09025737 0.22035591 0.64644249
# Asian
city.size = as.numeric(as.character(data$population))/sum(as.numeric(as.character(data$population)))
prob.asian = sum(data$proportion_asian  * as.numeric(as.character(data$population)))/sum(as.numeric(as.character(data$population)))
prob.cities.given.asian = (data$proportion_asian * city.size)/prob.asian
data.asian = data.frame(data$sdi, prob.cities.given.asian)
colnames(data.asian) = c("sdi", "prob.cities.given.asian")
theta = log(c(1, 1.19, 1.26, 1.53, 1.70))
data.asian = data.asian %>% mutate(coefficients = case_when(sdi == 1 ~ theta[1],
                                                                  sdi == 2 ~ theta[2],
                                                                  sdi == 3 ~ theta[3],
                                                                  sdi == 4 ~ theta[4],
                                                                  sdi == 5 ~ theta[5]))
nhis.asian.risk.sdi = sum(data.asian$prob.cities.given.asian*exp(data.asian$coefficients))
nhis.asian.risk.sdi1 = sum(data.asian$prob.cities.given.asian*data.asian$coefficients)
prev.sdi.asian = sapply(1:5,function(x){sum(data.asian[data.asian$sdi == x,'prob.cities.given.asian'])})
# 0.04186209 0.04180604 0.17058649 0.25657903 0.48916635
# Native
city.size = as.numeric(as.character(data$population))/sum(as.numeric(as.character(data$population)))
prob.native = sum(data$proportion_american_indian_alaska_native  * as.numeric(as.character(data$population)))/sum(as.numeric(as.character(data$population)))
prob.cities.given.native = (data$proportion_american_indian_alaska_native * city.size)/prob.native
data.native = data.frame(data$sdi, prob.cities.given.native)
colnames(data.native) = c("sdi", "prob.cities.given.native")
theta = log(c(1, 1.19, 1.26, 1.53, 1.70))
data.native = data.native %>% mutate(coefficients = case_when(sdi == 1 ~ theta[1],
                                                            sdi == 2 ~ theta[2],
                                                            sdi == 3 ~ theta[3],
                                                            sdi == 4 ~ theta[4],
                                                            sdi == 5 ~ theta[5]))
nhis.native.risk.sdi = sum(data.native$prob.cities.given.native*exp(data.native$coefficients))
nhis.native.risk.sdi1 = sum(data.native$prob.cities.given.native*data.native$coefficients)
prev.sdi.native = sapply(1:5,function(x){sum(data.native[data.native$sdi == x,'prob.cities.given.native'])})
# 0.01727652 0.09574445 0.18060266 0.28700502 0.41937135


# ----------- marginal prevalence of SDI:
# make sure nhis includes only the 5 race categories
prev.sdi.marginal = 
  prev.sdi.black * sum(nhis$black * nhis$sampling_weights)/sum(nhis$sampling_weights) + 
  prev.sdi.white * sum(nhis$white * nhis$sampling_weights)/sum(nhis$sampling_weights) + 
  prev.sdi.hispanic * sum(nhis$hispanic * nhis$sampling_weights)/sum(nhis$sampling_weights) + 
  prev.sdi.asian * sum(nhis$asian * nhis$sampling_weights)/sum(nhis$sampling_weights) + 
  prev.sdi.native * sum(nhis$native * nhis$sampling_weights)/sum(nhis$sampling_weights)
prev.sdi.marginal
# 0.03675796 0.07101181 0.14635682 0.22943617 0.51643724
sum(prev.sdi.marginal) # 1

# ------------ co-prevalence:
prev.sdi.white * sum(nhis$white * nhis$sampling_weights)/sum(nhis$sampling_weights)
# 0.03004995 0.05883095 0.11095857 0.15681370 0.29674834
prev.sdi.hispanic * sum(nhis$hispanic * nhis$sampling_weights)/sum(nhis$sampling_weights)
# 0.002346586 0.004732744 0.014878871 0.036325534 0.106565639
prev.sdi.black * sum(nhis$black * nhis$sampling_weights)/sum(nhis$sampling_weights)
# 0.002353387 0.004729559 0.011335349 0.022343662 0.087682716
prev.sdi.asian * sum(nhis$asian * nhis$sampling_weights)/sum(nhis$sampling_weights)
# 0.001851055 0.001848577 0.007542983 0.011345395 0.021629927
prev.sdi.native * sum(nhis$native * nhis$sampling_weights)/sum(nhis$sampling_weights)
# 0.0001569833 0.0008699828 0.0016410477 0.0026078737 0.0038106217



####### impute SDI
indscore = dat
set.seed(2020)
indscore$sdi = 0
for (i in 1:nrow(indscore)){
  tem = indscore$race_ethnicity.cdc[i]
  if (tem == 'Non_hispanic_white') indscore$sdi[i] = which(rmultinom(1, 1, prev.sdi.white)==1)
  if (tem == 'Hispanic') indscore$sdi[i] = which(rmultinom(1, 1, prev.sdi.hispanic)==1)
  if (tem == 'Non_hispanic_Black') indscore$sdi[i] = which(rmultinom(1, 1, prev.sdi.black)==1)
  if (tem == 'Non_hispanic_asian') indscore$sdi[i] = which(rmultinom(1, 1, prev.sdi.asian)==1)
  if (tem == 'Non_hispanic_american_Indian') indscore$sdi[i] = which(rmultinom(1, 1, prev.sdi.native)==1)
}
indscore$sdi2 = ifelse(indscore$sdi == 2, 1, 0)
indscore$sdi3 = ifelse(indscore$sdi == 3, 1, 0)
indscore$sdi4 = ifelse(indscore$sdi == 4, 1, 0)
indscore$sdi5 = ifelse(indscore$sdi == 5, 1, 0)

### impute arthritis
# among arthritis, the ratio_rheumatoid_to_non_rheumatoid =  0.269098
# colnames(indscore)[which(colnames(indscore) == 'rhemumatoid')]='rheumatoid'
ratio.rheumatoid.arthritis = 0.269098/(1+0.269098)
indscore$arthritis = indscore$rheumatoid
indscore$rheumatoid = 0
set.seed(2020)
for (i in 1:nrow(indscore)){
  if (indscore$arthritis[i] == 1){
    indscore$rheumatoid[i] = rbinom(n=1, size=1, prob=ratio.rheumatoid.arthritis)
  }
}
nhis_imputed = indscore
saveRDS(nhis_imputed, file = 'data_created/nhis_imputed.rds')
