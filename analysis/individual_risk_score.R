library(openxlsx)
# install.packages("latticeExtra", repos="http://R-Forge.R-project.org")
# not available for R 3.5.2?
# library(Hmisc)
# library(weights)
# load model coefficients
coeffs = read.xlsx('~/Dropbox/NHANES_risk_score/500cities_data/Analysis/data_created/UK_model.xlsx',
                   sheet = 'coefficients_individual_level')
rownames(coeffs) = coeffs$Variable_Name_UK
#coeffs$Variable_Name_UK
# load data:
coef_name = coeffs$Variable_Name_UK
coef_name
#"Age_18_39"                 "Age_40_49"                 "Age_60_69"                 "Age_70_79"                
#[5] "Age_80_150"                "Sex_M"                     "BMI_obsese1"               "BMI_obsese2"              
#[9] "BMI_obsese3"               "Smoking_ex"                "Smoking_current"           "Hispanic"                 
#[13] "Black"                     "IMD2"                      "IMD3"                      "IMD4"                     
#[17] "IMD5"                      "BP_high"                   "Respiratory_disease"       "Asthma"                   
#[21] "CHD"                       "Diabetes"                  "Cancer_nonhaematological1" "Cancer_nonhaematological2"
#[25] "Cancer_nonhaematological3" "Cancer_haematological1"    "Cancer_haematological2"    "Cancer_haematological3"   
#[29] "Stroke"                    "Kidney"                    "Arthritis"
length(coef_name) # 31
# -------- read data
dat = readRDS('~/Dropbox/NHANES_risk_score/500cities_data/Analysis/data_created/nhis_2017.rds')
#dat[which(is.na(dat$non_hematologic_cancer)),'non_hematologic_cancer']='None'
#dat[which(is.na(dat$hematologic_cancer)),'hematologic_cancer']='None'
sapply(1:ncol(dat),function(x){sum(is.na(dat[,x]))})
colnames(dat)
#[1] "SRVY_YR"                "HHX"                    "FMX"                    "FPX"                    "sampling_weights"      
#[6] "sex"                    "age"                    "BMI"                    "agegroup"               "diabetes"              
#[11] "smoking_status"         "rhemumatoid"            "asthma"                 "stroke"                 "heart_disease"         
#[16] "hypertension"           "resp_ex_asthma"         "kidney_disease"         "liver_disease"          "race_ethnicity"        
#[21] "hematologic_cancer"     "non_hematologic_cancer" "diagnoses_cancer"       "Obesity" 
nrow(dat) # 25875
dat = dat[complete.cases(dat[,-which(colnames(dat) %in% c('diagnoses_cancer'))]),] 
sapply(1:ncol(dat),function(x){sum(is.na(dat[,x]))})
nrow(dat) # 22746 -> 22005
dat$Age_18_39 = ifelse(dat$agegroup=='18_40',1,0)
dat$Age_40_49 = ifelse(dat$agegroup=='40_50',1,0)
dat$Age_50_59 = ifelse(dat$agegroup=='50_60',1,0)
dat$Age_60_69 = ifelse(dat$agegroup=='60_70',1,0)
dat$Age_70_79 = ifelse(dat$agegroup=='70_80',1,0)
dat$Age_80_150 = ifelse(dat$agegroup=='80_and_above',1,0)
# sex
dat$female = ifelse(dat$sex=='Female',1,0)
dat$male = ifelse(dat$sex=='Male',1,0)
# obesity no missing data for bmi
dat$obesity1 = ifelse((dat$BMI>=30)&(dat$BMI<35),1,0)
dat$obesity2 = ifelse((dat$BMI>=35)&(dat$BMI<40),1,0)
dat$obesity3 = ifelse((dat$BMI>=40),1,0)

# smoking
dat$smoking_ex = ifelse(dat$smoking_status=='Former',1,0)
dat$smoking_current = ifelse(dat$smoking_status=='Current',1,0)
# ethnicity
# Note: white means non-hispanic white
dat$white = ifelse(dat$race_ethnicity=='Non_hispanic_white',1,0)
#dat$mixed = ifelse(dat$race_ethnicity=='Mixed',1,0)
#dat$asian = ifelse(dat$race_ethnicity=='Asian',1,0)
dat$hispanic = ifelse(dat$race_ethnicity=='Hispanic',1,0)
dat$black = ifelse(dat$race_ethnicity=='Black',1,0)
#dat$others = ifelse(dat$race_ethnicity=='Others',1,0)
#IMD - missing
dat$IMD1 = 0
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
dat = dat[complete.cases(dat[,-which(colnames(dat) %in% c('diagnoses_cancer'))]),] 
sapply(1:ncol(dat),function(x){sum(is.na(dat[,x]))})
nrow(dat) # 22746 -> 21987
# stroke
dat$stroke = ifelse(dat$stroke=='Yes',1,0)
# kidney disease
dat$kidney_disease = ifelse(dat$kidney_disease=='Yes',1,0)
# rhemumatoid
dat$rhemumatoid = ifelse(dat$rhemumatoid=='Yes',1,0)
covariates = c('Age_18_39','Age_40_49','Age_60_69','Age_70_79','Age_80_150',
               'male',paste0('obesity',1:3),'smoking_ex','smoking_current',
               #'mixed','asian','hispanic','black','others',
               'hispanic','black',
               paste0('IMD',2:5),'hbp','copd', 'asthma', 'chd', 'diabetes',
               paste0('non_hematologic_cancer',1:3),paste0('hematologic_cancer',1:3),'stroke',
               'kidney_disease','rhemumatoid')
length(covariates) # 31

# check if the names match
a=cbind(coef_name,covariates)
a

Covariate_matrix = as.matrix(dat[,covariates])
## replace NA by 0
sapply(1:ncol(Covariate_matrix),function(x){sum(is.na(Covariate_matrix[,x]))})
Covariate_matrix[is.na(Covariate_matrix)] = 0
# risk score (log hazard ratio):
rs_est = Covariate_matrix %*% coeffs$estimate
#rs_lb = Covariate_matrix %*% coeffs$lb[-exclude.coeffs]
#rs_ub = Covariate_matrix %*% coeffs$ub[-exclude.coeffs]

risk_score = cbind(dat[,1:5],rs_est)
colnames(risk_score) = c(colnames(dat)[1:5],'rs')
full_output = cbind(dat[,1:5],rs_est,dat[,6:ncol(dat)])

saveRDS(risk_score,file='~/Dropbox/NHANES_risk_score/500cities_data/Analysis/data_created/individual_rs.rds')
saveRDS(full_output,file='~/Dropbox/NHANES_risk_score/500cities_data/Analysis/data_created/individual_rs_covariates.rds')
write.xlsx(risk_score,file='~/Dropbox/NHANES_risk_score/500cities_data/Analysis/data_created/individual_rs.xlsx')
write.xlsx(full_output,file='~/Dropbox/NHANES_risk_score/500cities_data/Analysis/data_created/individual_rs_covariates.xlsx')




# read data
risk_score = readRDS('~/Dropbox/NHANES_risk_score/500cities_data/Analysis/data_created/individual_rs.rds')

par(mfrow=c(1,3))
hist(risk_score$rs)
# normality after incorporating sampling weight?
# min(risk_score$sampling_weights) # 668
rs = lapply(1:nrow(risk_score),function(x){rep(risk_score$rs[x],round(risk_score$sampling_weights[x]/20,0))})
rs = unlist(rs)
length(rs)
hist(rs)
wtd.hist(risk_score$rs,weight = risk_score$sampling_weights)

# calculate proportion of smoking subcategories
dat = readRDS('~/Dropbox/NHANES_risk_score/500cities_data/Analysis/data_created/nhis_2017.rds')
# missing data for smoking: sum((dat$smoking_status)=='NA')
dat = dat[dat$smoking_status!='NA',]
dat$smoking_ex = ifelse(dat$smoking_status=='Former',1,0)
dat$smoking_current = ifelse(dat$smoking_status=='Current',1,0)

Pr.smoking_current = sum(dat[dat$smoking_current==1,'sampling_weights'])/sum(dat$sampling_weights)
Pr.smoking_ex = sum(dat[dat$smoking_ex==1,'sampling_weights'])/sum(dat$sampling_weights)
Pr.smoking_never = sum(dat[dat$smoking_status=='Never','sampling_weights'])/sum(dat$sampling_weights)
Pr.smoking_current # 0.1395476 -> 0.1395412
Pr.smoking_ex # 0.2247133 -> 0.2263748
Pr.smoking_never # 0.6357391 -> 0.6340841


# calculate proportion of Obesity subcategories
dat = readRDS('~/Dropbox/NHANES_risk_score/500cities_data/Analysis/data_created/nhis_2017.rds')
# no missing data for bmi
dat$obesity1 = ifelse((dat$BMI>=30)&(dat$BMI<35),1,0)
dat$obesity2 = ifelse((dat$BMI>=35)&(dat$BMI<40),1,0)
dat$obesity3 = ifelse((dat$BMI>=40),1,0)

Pr.obesity0 = sum(dat[dat$BMI<30,'sampling_weights'])/sum(dat$sampling_weights)
Pr.obesity1 = sum(dat[dat$obesity1==1,'sampling_weights'])/sum(dat$sampling_weights)
Pr.obesity2 = sum(dat[dat$obesity2==1,'sampling_weights'])/sum(dat$sampling_weights)
Pr.obesity3 = sum(dat[dat$obesity3==1,'sampling_weights'])/sum(dat$sampling_weights)
Pr.obesity0 # 0.6692845 -> 0.6662283
Pr.obesity1 # 0.1772857 -> 0.1792105
Pr.obesity2 # 0.07338 -> 0.07423196
Pr.obesity3 # 0.0800498 -> 0.08032927
#smoking_total = Pr.obesity0 + Pr.obesity1 + Pr.obesity2 + Pr.obesity3 # 1


# calculate the average risk
prev_sdi = c(0.06894011,	0.12572277,	0.24237342,	0.31754727,	0.24541643)[-1]
#prev_sdi = c(0.07658949,	0.13919645,	0.25522116,	0.30836921,	0.22062368)[-1]
### no need to match with the 37 covariates in the city-level data...
##### overall mean: shouldn't use individual risk score cause it doesn't include IMD!! big difference
# load individual risk score
rs.nhis = readRDS('~/Dropbox/NHANES_risk_score/500cities_data/Analysis/data_created/individual_rs.rds')
sapply(1:ncol(rs.nhis),function(x){sum(is.na(rs.nhis[,x]))}) # no missing data
# mean of NHIS score
# mean(rs.nhis$rs) # old: 0.1217569 # new: 0.3185659
# median(rs.nhis$rs) # old: 0 # new:0.4301246
# weighted mean of NHIS score without SDI:
# sum(rs.nhis$rs * rs.nhis$sampling_weights/sum(rs.nhis$sampling_weights)) # old: -0.124386; new: 0.0396505
# weighted mean of NHIS score
mean.rs = sum(rs.nhis$rs * rs.nhis$sampling_weights)/sum(rs.nhis$sampling_weights) + sum(prev_sdi * coeffs[which(coeffs$Variable_Name_UK %in% paste0('IMD',2:5)),'estimate']) # -0.124386
mean.rs # 0.3909936 -> 0.3827882
save(mean.rs,file='~/Dropbox/NHANES_risk_score/500cities_data/Analysis/data_created/mean.rs.RData')


#### mean risk without SDI:
mu.rs.nosdi = sum(exp(rs.nhis$rs) * rs.nhis$sampling_weights)/sum(rs.nhis$sampling_weights) #+ sum(prev_sdi * coeffs[which(coeffs$Variable_Name_UK %in% paste0('IMD',2:5)),'estimate']) # -0.124386

rs = lapply(1:nrow(rs.nhis),function(x){rep(rs.nhis$rs[x],rs.nhis$sampling_weights[x])})
rs = unlist(rs)
median(exp(rs))


mean(rs.nhis)

# city-level
dat.city = readRDS('~/Dropbox/NHANES_risk_score/500cities_data/Analysis/data_created/full_output_updated.rds')
dat.city$population = as.character(dat.city$population)
dat.city$population = as.numeric(dat.city$population)
dat.city = dat.city[order(-dat.city$rs_est),]


# 
hr = read.xlsx('~/Dropbox/NHANES_risk_score/results/highrisk_overall-mean/highrisk_all_mixtureN_overall-mean.xlsx')
full.city = readRDS('~/Dropbox/NHANES_risk_score/500cities_data/Analysis/data_created/full_output_updated.rds')
W1 = full.city$Age_18_39

mean.city.risk = W1*exp(hr$mu_under40+0.5*hr$var_under40) + (1-W1)*exp(hr$mu_over40+0.5*hr$var_over40)


# calculate variance of risk score:
# prevalence of each ethnic group
prev = readRDS('~/Dropbox/NHANES_risk_score/500cities_data/Analysis/data_created/combined_updated.rds')
prev$population = as.character(prev$population)
prev$population = as.numeric(prev$population)
colnames(prev)
#ethnic = c('mixed','asian','hispanic','black','others')
ethnic = c('hispanic','black')
S = 2:5 # SDI
prev_ethnic = prev[,paste0('proportion_',ethnic)]
# prevalence of each SDI quintile: prev_sdi

##### 
covariates.ethnic.imd = c('population',paste0('proportion_',ethnic), paste0('IMD',2:5))
prev.ethnic.imd.city = prev[,covariates.ethnic.imd]
OR.race.sdi = cov.race.sdi = matrix(NA,length(ethnic),length(S))
# Ethnic (2) x SDI (4):
for (i in 1:length(ethnic)){
  for (j in 1:length(S)){
    # calculate odds ratio using BRFSS data:
    data = prev.ethnic.imd.city[, c('population',paste0('proportion_',ethnic[i]), paste0('IMD',S[j]))]
    colnames(data) = c('population','race','imd')
    i1 = data[(data$imd == 1),]
    p11 = sum(i1$population * i1$race)/sum(data$population)
    p01 = sum(i1$population * (1-i1$race))/sum(data$population)
    i0 = data[(data$imd == 0),]
    p10 = sum(i0$population * i0$race)/sum(data$population)
    p00 = sum(i0$population * (1-i0$race))/sum(data$population)
    p1 = sum(i1$population)/sum(data$population)
    p2 = sum(data$population * data$race)/sum(data$population)
    OR.race.sdi[i,j] = p11*p00/(p10*p01)
    cov.race.sdi[i,j] = p11 - p1*p2
  }
}

#weighted mean of BRFSS score
#mean.rs = sum(rs.city.info$rs_est * rs.city.info$population/sum(rs.city.info$population)) # 0.4128684
#weighted variance of BRFSS score
#rs = lapply(1:nrow(rs.city.info),function(x){rep(rs.city.info$rs_est[x],rs.city.info$population[x])})
#rs = unlist(rs) 
# length(rs) # 95256515
#var(rs) # 0.06842061

# variance can be approximated using the NHIS data:
rs.nhis = readRDS('~/Dropbox/NHANES_risk_score/500cities_data/Analysis/data_created/individual_rs_covariates.rds')
rs = lapply(1:nrow(rs.nhis),function(x){rep(rs.nhis$rs[x],rs.nhis$sampling_weights[x])})
rs = unlist(rs)
# length(rs) # 204138597
#wv.rs = var(rs.nhis$rs) # 3.60385
wv.rs = var(rs) 
wv.rs # 3.361482

# add the variance of SDI and the covariance between SDI and race:
#crace = c('Mixed','Asian','Hispanic','Black','Other')
crace = c('Hispanic','Black')
csdi = paste0('IMD',2:5)
beta.race.sdi = coeffs[c(crace,csdi),'estimate']
names(beta.race.sdi) = c(crace,csdi)
# covariance matrix of sdi2 - sdi5:
cov.sdi = matrix(NA,length(S),length(S))
for (i in 1:length(S)){
  for (j in 1:length(S)){
    if (i==j) cov.sdi[i,j] = prev_sdi[i] * (1-prev_sdi[i])
    if (i!=j) cov.sdi[i,j] = cov.sdi[j,i] = - prev_sdi[i] * prev_sdi[j]
  }
}
var.sdi = beta.race.sdi[csdi] %*% cov.sdi %*% beta.race.sdi[csdi] # 0.025551
cov.race.sdi = beta.race.sdi[crace] %*% cov.race.sdi %*% beta.race.sdi[csdi] # 0.00745908

## combine two sources of variance:
wv.rs = wv.rs + var.sdi + cov.race.sdi
wv.rs # 3.371711 -> 3.394492
save(wv.rs,file='~/Dropbox/NHANES_risk_score/500cities_data/Analysis/data_created/wv.rs.RData')



# cov.wt(rs.nhis$rs,wt = rep(1/nrow(rs.nhis),nrow(rs.nhis)))



rs.nhis = readRDS('~/Dropbox/NHANES_risk_score/500cities_data/Analysis/data_created/individual_rs_covariates.rds')
### tail probabilities based on normal approximation (or mixture-normal) and empirically and if those two numbers are close.
# empirical:

#RS = lapply(1:nrow(rs.nhis),function(x){rep(rs.nhis$rs_est[x],rs.nhis$sampling_weights[x])})
#RS = unlist(RS)
RS = rs.nhis$rs_est


load('~/Dropbox/NHANES_risk_score/500cities_data/Analysis/data_created/mean.rs.RData')
load('~/Dropbox/NHANES_risk_score/500cities_data/Analysis/data_created/wv.rs.RData')
Rl = exp(mean.rs + wv.rs/2)
k=2
mean(RS>log(k)+log(Rl)) 
#mean(RS>log(k)+log(Rl)) 
# normal:
mu.nhis = mean(RS)
var.nhis = var(RS)
pnorm(log(k)+log(Rl),mu.nhis,sqrt(var.nhis),lower.tail = F) # 0.1902517
#pnorm(log(k)+log(Rl),mu.nhis,sqrt(var.nhis),lower.tail = F) # 0.1902517
# mixture normal:
mu.nhis1 = mean(RS[rs.nhis$age<40])
var.nhis1 = var(RS[rs.nhis$age<40])
w.nhis1 = mean(rs.nhis$age<40)
mu.nhis2 = mean(RS[rs.nhis$age>=40])
var.nhis2 = var(RS[rs.nhis$age>=40])
w.nhis2 = mean(rs.nhis$age>=40)
ntotal = 100000
mn.sample1 = rnorm(ntotal*w.nhis1,mu.nhis1,sqrt(var.nhis1))
mn.sample2 = rnorm(ntotal*w.nhis2,mu.nhis2,sqrt(var.nhis2))
mn.sample = c(mn.sample1,mn.sample2)
mean(mn.sample>log(k)+log(Rl)) # 0.2232022
#mean(mn.sample>log(k)+log(Rl)) # 0.2232022
w.nhis1 * pnorm(log(k)+log(Rl),mu.nhis1,sqrt(var.nhis1),lower.tail = F) + w.nhis2 * pnorm(log(k)+log(Rl),mu.nhis2,sqrt(var.nhis2),lower.tail = F)
par(mfrow=c(1,2))
hist(RS,breaks=50)
hist(mn.sample,breaks=50)


k=50
# among cases:
# normal:
mu.nhis = mean(RS)
var.nhis = var(RS)
pnorm(log(k)+log(Rl),mu.nhis+var.nhis,sqrt(var.nhis),lower.tail = F) # 0.1902517
#pnorm(log(k)+log(Rl),mu.nhis,sqrt(var.nhis),lower.tail = F) # 0.1902517
# mixture normal:
mu.nhis1 = mean(RS[rs.nhis$age<40])
var.nhis1 = var(RS[rs.nhis$age<40])
w.nhis1 = mean(rs.nhis$age<40)
mu.nhis2 = mean(RS[rs.nhis$age>=40])
var.nhis2 = var(RS[rs.nhis$age>=40])
w.nhis2 = mean(rs.nhis$age>=40)
ntotal = 100000
mn.sample1 = rnorm(ntotal*w.nhis1,mu.nhis1+var.nhis1,sqrt(var.nhis1))
mn.sample2 = rnorm(ntotal*w.nhis2,mu.nhis2+var.nhis2,sqrt(var.nhis2))
mn.sample = c(mn.sample1,mn.sample2)
mean(mn.sample>log(k)+log(Rl)) # 0.2232022
#mean(mn.sample>log(k)+log(Rl)) # 0.2232022
par(mfrow=c(1,3))
hist(RS,breaks=50)
hist(mn.sample0,breaks=50)
hist(mn.sample,breaks=50)




