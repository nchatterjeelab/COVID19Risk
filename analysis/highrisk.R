library(openxlsx)
library(survey)
library(ggpubr) # for gghistogram
library(gghighlight)
library(ggExtra)
library(ggplot2)
library(data.table)
library(gapminder)
library(tidyverse)
library(grid)
library(gridExtra)
library(ggrepel)
update_p11 = function(p1,p2,r){
  a = 1 - r
  b = 1 + (r-1)*(p1+p2)
  c = -r*p1*p2
  (-b+sqrt(b^2-4*a*c))/(2*a)
}


# load model coefficients
coeffs = read.xlsx('data_created/UK_model.xlsx', sheet = 'coefficients_individual_level')
rownames(coeffs) = coeffs$Variable_Name_UK
coef_name = coeffs$Variable_Name_UK
coef_name
#[1] "Age_18_39"                 "Age_40_49"                 "Age_60_69"                 "Age_70_79"                
#[5] "Age_80_150"                "Sex_M"                     "BMI_obsese1"               "BMI_obsese2"              
#[9] "BMI_obsese3"               "Smoking_ex"                "Smoking_current"           "Hispanic"                 
#[13] "Black"                     "IMD2"                      "IMD3"                      "IMD4"                     
#[17] "IMD5"                      "BP_high"                   "Respiratory_disease"       "Asthma"                   
#[21] "CHD"                       "Diabetes"                  "Cancer_nonhaematological1" "Cancer_nonhaematological2"
#[25] "Cancer_nonhaematological3" "Cancer_haematological1"    "Cancer_haematological2"    "Cancer_haematological3"   
#[29] "Stroke"                    "Kidney"                    "Arthritis"
Beta = coeffs$estimate#[-exclude.coeffs]
names(Beta) = rownames(coeffs)#[-exclude.coeffs]



# -----------------------------------------------------------------------------------------
# ------------------------------------ 18-39 age group ------------------------------------
# -----------------------------------------------------------------------------------------
# ------------ Create the covariates matrix. Don't need to exclude missing rows!
dat = readRDS('data_created/nhis_2017.rds')
dat = dat[dat$agegroup=='18_40',]
dat = dat[(!is.na(dat$hematologic_cancer))|(!is.na(dat$non_hematologic_cancer)),]
colnames(dat)
#[1] "SRVY_YR"                "HHX"                    "FMX"                    "FPX"                    "sampling_weights"      
#[6] "sex"                    "age"                    "BMI"                    "agegroup"               "diabetes"              
#[11] "smoking_status"         "rhemumatoid"            "asthma"                 "stroke"                 "heart_disease"         
#[16] "hypertension"           "resp_ex_asthma"         "kidney_disease"         "liver_disease"          "race_ethnicity"        
#[21] "hematologic_cancer"     "non_hematologic_cancer" "diagnoses_cancer"       "Obesity"
# age
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
dat$white = ifelse(dat$race_ethnicity=='Non_hispanic_white',1,0)
dat$hispanic = ifelse(dat$race_ethnicity=='Hispanic',1,0)
dat$black = ifelse(dat$race_ethnicity=='Black',1,0)
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
#dat[is.na(dat$non_hematologic_cancer1),] = 0
# hematologic_cancer
dat$hematologic_cancer1 = ifelse((dat$hematologic_cancer=='Hematological')&(dat$diagnoses_cancer == 'less_than_1_yr'),1,0)
dat$hematologic_cancer2 = ifelse((dat$hematologic_cancer=='Hematological')&(dat$diagnoses_cancer == '1_5yr'),1,0)
dat$hematologic_cancer3 = ifelse((dat$hematologic_cancer=='Hematological')&(dat$diagnoses_cancer == 'greater_than_5_yr'),1,0)
# stroke
dat$stroke = ifelse(dat$stroke=='Yes',1,0)
# kidney disease
dat$kidney_disease = ifelse(dat$kidney_disease=='Yes',1,0)
# rhemumatoid
dat$rhemumatoid = ifelse(dat$rhemumatoid=='Yes',1,0)


covariates = c('Age_18_39','Age_40_49','Age_60_69','Age_70_79','Age_80_150',
               'male',paste0('obesity',1:3),'smoking_ex','smoking_current',
               'hispanic','black',paste0('IMD',2:5),
               'hbp','copd', 'asthma', 'chd', 'diabetes',
               paste0('non_hematologic_cancer',1:3),paste0('hematologic_cancer',1:3),'stroke',
               'kidney_disease','rhemumatoid')

# ------- step 1: calculate P (prevalence) i.e. city-level prevalence from brfss data, which is the mean of CDRS for each city
dat.city = as.data.frame(readRDS('data_created/prevalance_age_stratification.rds'))
dat.city[,9:54] = sapply(dat.city[,9:54], as.character)
dat.city[,9:54] = sapply(dat.city[,9:54], as.numeric)
dat.city$prevalance_of_diabetes_given_age_less_than_40 = dat.city$prevalance_of_diabetesYes_controlled_given_age_less_than_40 + dat.city$prevalance_of_diabetesYes_uncontrolled_given_age_less_than_40
#IMD - missing
dat.city$IMD2 = 0
dat.city$IMD3 = 0
dat.city$IMD4 = 0
dat.city$IMD5 = 0
dat.city$age1 = 1
dat.city$age2 = 0
dat.city$age3 = 0
dat.city$age4 = 0
dat.city$age5 = 0
covariates_ex_age = c(paste0('age',1:5),paste0('prevalance_of_',
                                               c('male',paste0('ObesityObese_',c('I','II','III')),
                                                 'smoking_statusFormer','smoking_statusCurrent',
                                                 'race_ethnicityHispanic','race_ethnicityBlack'),
                                               '_given_age_less_than_40'),
                      paste0('IMD',2:5),
                      paste0('prevalance_of_',
                             c('hypertensionhighbp','resp_ex_asthmaYes','asthmaYes','heart_diseaseYes','diabetes',
                               'nonhematoless_than_1_yr','nonhemato1_5yr','nonhematogreater_than_5_yr',
                               'hematoless_than_1_yr','hemato1_5yr','hematogreater_than_5_yr',
                               'strokeYes','kidney_diseaseYes','rhemumatoidYes'),
                             '_given_age_less_than_40'))
dat.city = dat.city[,covariates_ex_age]
covariates.city = covariates_ex_age
dat.city = dat.city[,covariates.city]

# -------- matrix of marginal prevalence
P = dat.city # row: city; column: covariates

# ------- step 2: calculate OR (odds ratio matrix) using NHIS individual-level data
M = length(covariates.city)
OR = matrix(NA, M, M)
for (i in 1:M){
  for (j in 1:M){
    if (i!=j){
      tem.i = dat[,covariates[i]]
      tem.j = dat[,covariates[j]]
      tem = dat[((!is.na(tem.i))&(!is.na(tem.j))),]
      temi = tem.i[((!is.na(tem.i))&(!is.na(tem.j)))]
      temj = tem.j[((!is.na(tem.i))&(!is.na(tem.j)))]
      # here p does not mean probability...
      p.11 = sum(tem[(temi==1)&(temj==1),'sampling_weights'])
      p.10 = sum(tem[(temi==0)&(temj==1),'sampling_weights'])
      p.01 = sum(tem[(temi==1)&(temj==0),'sampling_weights'])
      p.00 = sum(tem[(temi==0)&(temj==0),'sampling_weights'])
      p.11;p.10;p.01;p.00;sum(p.11+p.10+p.01+p.00)
      OR[i,j] = OR[j,i] = p.11*p.00/(p.10*p.01)
    }
  }
}
# sum(OR==Inf,na.rm=T) # 2


# ------- step 3: extract mean of CDRS for each city for <40 age group
rs.city.info = readRDS('data_created/risk_score_age_under_40.rds')
rs.city.info$population = gsub(',','',rs.city.info$population)
rs.city.info$population = as.numeric(rs.city.info$population)
rs.city = rs.city.info$rs_est


# ------- step 4: calculate covariance matrix of CDRS for each city
multinom.list = list(c(1:5),7:9,10:11,12:13,14:17,23:25,26:28)
multinom.index = unlist(multinom.list)
binomial.index = c(1:M)[-multinom.index]

cdrs_city_dist = function(P,OR,city,Beta,rs.mu.city){
  cov.cdrs = matrix(NA,M,M)
  # variance
  for (i in 1:M){
    cov.cdrs[i,i] = P[city,i]*(1-P[city,i])
  }
  # covariance
  # 1. between all binomial covariates:
  for (i in binomial.index){
    for (j in binomial.index){
      if (i!=j){
        if (OR[i,j] == 'NaN') cov.cdrs[i,j] = cov.cdrs[j,i] = 0
        if (OR[i,j] != 'NaN'){
          p11 = update_p11(P[city,i],P[city,j],OR[i,j])
          cov.cdrs[i,j] = cov.cdrs[j,i] = p11 - P[city,i] * P[city,j]
        }
      }
    }
  }
  cov.cdrs[binomial.index,binomial.index] #NaN: the correlation=1!
  for (k in 1:length(multinom.list)){
    # 2. between binomial and each multinomial category:
    for (i in multinom.list[[k]]){ # multinomial covariates
      for (j in c(1:M)[-multinom.list[[k]]]){ # binomial covariates
        # i != j
        if (OR[i,j] == 'NaN') cov.cdrs[i,j] = cov.cdrs[j,i] = 0
        if (OR[i,j] != 'NaN'){
          if (OR[i,j] == Inf) cov.cdrs[i,j] = cov.cdrs[j,i] = 0 #sqrt(cov.cdrs[i,i]*cov.cdrs[j,j])
          if (OR[i,j] < Inf){
            p11 = update_p11(P[city,i],P[city,j],OR[i,j])
            cov.cdrs[i,j] = cov.cdrs[j,i] = p11 - P[city,i] * P[city,j]
          }
        }
      }
    }
    # 2. between binomial and each multinomial category:
    for (i in multinom.list[[k]]){ # multinomial covariates
      for (j in multinom.list[[k]]){ # binomial covariates
        if(i != j){
          if (OR[i,j] == 'NaN') cov.cdrs[i,j] = cov.cdrs[j,i] = 0
          if (OR[i,j] != 'NaN'){
            cov.cdrs[i,j] = cov.cdrs[j,i] = - P[city,i] * P[city,j]
          }
        }
      }
    }
  }
  # fix the issue for the following 2 variables:
  i = which(covariates=='hispanic'); j = which(covariates=='hematologic_cancer2')
  p11 = P[city,j]
  cov.cdrs[i,j] = cov.cdrs[j,i] = p11 - P[city,i] * P[city,j]
  # calculate the mean and variance of RS for the city:
  rs.var.city = Beta[-c(1:5)] %*% cov.cdrs[-c(1:5),-c(1:5)] %*% Beta[-c(1:5)]
  return(c(mu.rs = rs.mu.city, var.rs = rs.var.city))
}

dist.rs.city_under40 = t(sapply(1:nrow(dat.city),function(x){cdrs_city_dist(P,OR,x,Beta,rs.city[x])}))



# -----------------------------------------------------------------------------------------
# ------------------------------------ 40+ age group ------------------------------------
# -----------------------------------------------------------------------------------------
# ------------ Create the covariates matrix. Don't need to exclude missing rows!
dat = readRDS('data_created/nhis_2017.rds')
dat = dat[dat$agegroup!='18_40',]
dat = dat[(!is.na(dat$hematologic_cancer))|(!is.na(dat$non_hematologic_cancer)),]
colnames(dat)
#[1] "SRVY_YR"                "HHX"                    "FMX"                    "FPX"                    "sampling_weights"      
#[6] "sex"                    "age"                    "BMI"                    "agegroup"               "diabetes"              
#[11] "smoking_status"         "rhemumatoid"            "asthma"                 "stroke"                 "heart_disease"         
#[16] "hypertension"           "resp_ex_asthma"         "kidney_disease"         "liver_disease"          "race_ethnicity"        
#[21] "hematologic_cancer"     "non_hematologic_cancer" "diagnoses_cancer"       "Obesity"
# age
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
dat$white = ifelse(dat$race_ethnicity=='Non_hispanic_white',1,0)
dat$hispanic = ifelse(dat$race_ethnicity=='Hispanic',1,0)
dat$black = ifelse(dat$race_ethnicity=='Black',1,0)
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
# stroke
dat$stroke = ifelse(dat$stroke=='Yes',1,0)
# kidney disease
dat$kidney_disease = ifelse(dat$kidney_disease=='Yes',1,0)
# rhemumatoid
dat$rhemumatoid = ifelse(dat$rhemumatoid=='Yes',1,0)

covariates = c('Age_18_39','Age_40_49','Age_60_69','Age_70_79','Age_80_150',
               'male',paste0('obesity',1:3),'smoking_ex','smoking_current',
               'hispanic','black',paste0('IMD',2:5),
               'hbp','copd', 'asthma', 'chd', 'diabetes',
               paste0('non_hematologic_cancer',1:3),paste0('hematologic_cancer',1:3),'stroke',
               'kidney_disease','rhemumatoid')

# ------- step 1: calculate P (prevalence) i.e. city-level prevalence from brfss data, which is the mean of CDRS for each city
dat.age = readRDS('data_created/combined_updated.rds')
dat.city = as.data.frame(readRDS('data_created/prevalance_age_stratification.rds'))
dat.city[,9:54] = sapply(dat.city[,9:54], as.character)
dat.city[,9:54] = sapply(dat.city[,9:54], as.numeric)
dat.city$prevalance_of_diabetes_given_age_greater_than_40 = dat.city$prevalance_of_diabetesYes_controlled_given_age_greater_than_40 + dat.city$prevalance_of_diabetesYes_uncontrolled_given_age_greater_than_40
#IMD - missing
dat.city$IMD2 = 0
dat.city$IMD3 = 0
dat.city$IMD4 = 0
dat.city$IMD5 = 0
dat.city$age_over40 = 1 - dat.age$Age_18_39
dat.city$age1 = 0
dat.city$age2 = dat.age$Age_40_49/dat.city$age_over40
dat.city$age3 = dat.age$Age_60_69/dat.city$age_over40
dat.city$age4 = dat.age$Age_70_79/dat.city$age_over40
dat.city$age5 = dat.age$Age_80_150/dat.city$age_over40
dat.city$age2+dat.city$age3+dat.city$age4+dat.city$age5+dat.age$Age_50_59/dat.city$age_over40 # 1
covariates_ex_age = c(paste0('age',1:5),paste0('prevalance_of_',
                                               c('male',paste0('ObesityObese_',c('I','II','III')),
                                                 'smoking_statusFormer','smoking_statusCurrent',
                                                 'race_ethnicityHispanic','race_ethnicityBlack'),
                                               '_given_age_greater_than_40'),
                      paste0('IMD',2:5),
                      paste0('prevalance_of_',
                             c('hypertensionhighbp','resp_ex_asthmaYes','asthmaYes','heart_diseaseYes','diabetes',
                               'nonhematoless_than_1_yr','nonhemato1_5yr','nonhematogreater_than_5_yr',
                               'hematoless_than_1_yr','hemato1_5yr','hematogreater_than_5_yr',
                               'strokeYes','kidney_diseaseYes','rhemumatoidYes'),
                             '_given_age_greater_than_40'))
dat.city = dat.city[,covariates_ex_age]
covariates.city = covariates_ex_age
dat.city = dat.city[,covariates.city]

P = dat.city # row: city; column: covariates

# ------- step 2: calculate OR (odds ratio matrix) using individual-level data
M = length(covariates.city)
OR = matrix(NA, M, M)
for (i in 1:M){
  for (j in 1:M){
    if (i!=j){
      tem.i = dat[,covariates[i]]
      tem.j = dat[,covariates[j]]
      tem = dat[((!is.na(tem.i))&(!is.na(tem.j))),]
      temi = tem.i[((!is.na(tem.i))&(!is.na(tem.j)))]
      temj = tem.j[((!is.na(tem.i))&(!is.na(tem.j)))]
      # here p does not mean probability...
      p.11 = sum(tem[(temi==1)&(temj==1),'sampling_weights'])
      p.10 = sum(tem[(temi==0)&(temj==1),'sampling_weights'])
      p.01 = sum(tem[(temi==1)&(temj==0),'sampling_weights'])
      p.00 = sum(tem[(temi==0)&(temj==0),'sampling_weights'])
      p.11;p.10;p.01;p.00;sum(p.11+p.10+p.01+p.00)
      OR[i,j] = OR[j,i] = p.11*p.00/(p.10*p.01)
    }
  }
}


# ------- step 3: extract mean of CDRS for each city for <40 age group from 'risk_score_age_under_40.rds'
rs.city.info = readRDS('data_created/risk_score_age_over_40.rds')
rs.city.info$population = gsub(',','',rs.city.info$population)
rs.city.info$population = as.numeric(rs.city.info$population)
rs.city = rs.city.info$rs_est


# ------- step 4: calculate covariance matrix of CDRS for each city
multinom.list = list(c(1:5),7:9,10:11,12:13,14:17,23:25,26:28)
multinom.index = unlist(multinom.list)
binomial.index = c(1:M)[-multinom.index]

cdrs_city_dist = function(P,OR,city,Beta,rs.mu.city){
  cov.cdrs = matrix(NA,M,M)
  # variance
  for (i in 1:M){
    cov.cdrs[i,i] = P[city,i]*(1-P[city,i])
  }
  # covariance
  # 1. between all binomial covariates:
  for (i in binomial.index){
    for (j in binomial.index){
      if (i!=j){
        if (OR[i,j] == 'NaN') cov.cdrs[i,j] = cov.cdrs[j,i] = 0
        if (OR[i,j] != 'NaN'){
          p11 = update_p11(P[city,i],P[city,j],OR[i,j])
          cov.cdrs[i,j] = cov.cdrs[j,i] = p11 - P[city,i] * P[city,j]
        }
      }
    }
  }
  cov.cdrs[binomial.index,binomial.index] #NaN: the correlation=1!
  for (k in 1:length(multinom.list)){
    # 2. between binomial and each multinomial category:
    for (i in multinom.list[[k]]){ # multinomial covariates
      for (j in c(1:M)[-multinom.list[[k]]]){ # binomial covariates
        # i != j
        if (OR[i,j] == 'NaN') cov.cdrs[i,j] = cov.cdrs[j,i] = 0
        if (OR[i,j] != 'NaN'){
          if (OR[i,j] == Inf) cov.cdrs[i,j] = cov.cdrs[j,i] = 0 #sqrt(cov.cdrs[i,i]*cov.cdrs[j,j])
          if (OR[i,j] < Inf){
            p11 = update_p11(P[city,i],P[city,j],OR[i,j])
            cov.cdrs[i,j] = cov.cdrs[j,i] = p11 - P[city,i] * P[city,j]
          }
        }
      }
    }
    # 2. between binomial and each multinomial category:
    for (i in multinom.list[[k]]){ # multinomial covariates
      for (j in multinom.list[[k]]){ # binomial covariates
        if(i != j){
          if (OR[i,j] == 'NaN') cov.cdrs[i,j] = cov.cdrs[j,i] = 0
          if (OR[i,j] != 'NaN'){
            cov.cdrs[i,j] = cov.cdrs[j,i] = - P[city,i] * P[city,j]
          }
        }
      }
    }
  }
  # calculate the mean and variance of RS for the city:
  rs.var.city = Beta[-1] %*% cov.cdrs[-1,-1] %*% Beta[-1]
  return(c(mu.rs = rs.mu.city, var.rs = rs.var.city))
}

dist.rs.city_over40 = t(sapply(1:nrow(dat.city),function(x){cdrs_city_dist(P,OR,x,Beta,rs.city[x])}))


dat.city = readRDS('data_created/combined_updated.rds')

approxtype = 'mixtureN'
# ------ Estimate size of vulnerable population within cities
highrisk.size = function(k, mul1,sigma2l1, mul2,sigma2l2, mean.rs,wv.rs,
                         population, threshold_type, sample, dat.city,city){
  w1 = dat.city$Age_18_39[city]
  if (threshold_type == 'overall-mean') Rl = 6.36555
  if (sample == 'all'){
    Pr.k = w1 * pnorm(log(k)+log(Rl),mul1,sqrt(sigma2l1),lower.tail = F) + (1-w1) * pnorm(log(k)+log(Rl),mul2,sqrt(sigma2l2),lower.tail = F)
  }
  if (sample == 'cases'){
    Pr.k = w1 * pnorm(log(k)+log(Rl),mul1+sigma2l1,sqrt(sigma2l1),lower.tail = F) + (1-w1) * pnorm(log(k)+log(Rl),mul2+sigma2l2,sqrt(sigma2l2),lower.tail = F)
  }
  n.k = Pr.k * population
  c(Proportion=Pr.k,N=n.k)
}
colnames(dist.rs.city_under40) = c("mu_under40", "var_under40")
colnames(dist.rs.city_over40) = c("mu_over40", "var_over40")


scenario = expand.grid(sample = c('all','cases'),
                       threshold_type = c('overall-mean')
)
for (s in 1:nrow(scenario)){
  threshold_type = scenario[s,'threshold_type']
  sample = scenario[s,'sample']
  # 
  k = 2
  highrisk.city_k2 = t(sapply(1:nrow(dat.city),function(x){highrisk.size(k, mul1 = dist.rs.city_under40[x,'mu_under40'], 
                                                                         sigma2l1 = dist.rs.city_under40[x,'var_under40'], 
                                                                         mul2 = dist.rs.city_over40[x,'mu_over40'], 
                                                                         sigma2l2 = dist.rs.city_over40[x,'var_over40'], 
                                                                         mean.rs = mean.rs, wv.rs = wv.rs,
                                                                         population = rs.city.info$population[x],
                                                                         threshold_type, sample,
                                                                         dat.city = dat.city,city=x)}))
  highrisk.city_k2[,2] = round(highrisk.city_k2[,2],0)
  # 
  k = 3
  highrisk.city_k3 = t(sapply(1:nrow(dat.city),function(x){highrisk.size(k, mul1 = dist.rs.city_under40[x,'mu_under40'], 
                                                                         sigma2l1 = dist.rs.city_under40[x,'var_under40'], 
                                                                         mul2 = dist.rs.city_over40[x,'mu_over40'], 
                                                                         sigma2l2 = dist.rs.city_over40[x,'var_over40'], 
                                                                         mean.rs = mean.rs, wv.rs = wv.rs,
                                                                         population = rs.city.info$population[x],
                                                                         threshold_type, sample,
                                                                         dat.city = dat.city,city=x)}))
  highrisk.city_k3[,2] = round(highrisk.city_k3[,2],0)
  # 
  k = 5
  highrisk.city_k5 = t(sapply(1:nrow(dat.city),function(x){highrisk.size(k, mul1 = dist.rs.city_under40[x,'mu_under40'], 
                                                                         sigma2l1 = dist.rs.city_under40[x,'var_under40'], 
                                                                         mul2 = dist.rs.city_over40[x,'mu_over40'], 
                                                                         sigma2l2 = dist.rs.city_over40[x,'var_over40'], 
                                                                         mean.rs = mean.rs, wv.rs = wv.rs,
                                                                         population = rs.city.info$population[x],
                                                                         threshold_type, sample,
                                                                         dat.city = dat.city,city=x)}))
  highrisk.city_k5[,2] = round(highrisk.city_k5[,2],0)
  # 
  k = 10
  highrisk.city_k10 = t(sapply(1:nrow(dat.city),function(x){highrisk.size(k, mul1 = dist.rs.city_under40[x,'mu_under40'], 
                                                                          sigma2l1 = dist.rs.city_under40[x,'var_under40'], 
                                                                          mul2 = dist.rs.city_over40[x,'mu_over40'], 
                                                                          sigma2l2 = dist.rs.city_over40[x,'var_over40'], 
                                                                          mean.rs = mean.rs, wv.rs = wv.rs,
                                                                          population = rs.city.info$population[x],
                                                                          threshold_type, sample,
                                                                          dat.city = dat.city,city=x)}))
  highrisk.city_k10[,2] = round(highrisk.city_k10[,2],0)
  # 
  k = 25
  highrisk.city_k25 = t(sapply(1:nrow(dat.city),function(x){highrisk.size(k, mul1 = dist.rs.city_under40[x,'mu_under40'], 
                                                                          sigma2l1 = dist.rs.city_under40[x,'var_under40'], 
                                                                          mul2 = dist.rs.city_over40[x,'mu_over40'], 
                                                                          sigma2l2 = dist.rs.city_over40[x,'var_over40'], 
                                                                          mean.rs = mean.rs, wv.rs = wv.rs,
                                                                          population = rs.city.info$population[x],
                                                                          threshold_type, sample,
                                                                          dat.city = dat.city,city=x)}))
  highrisk.city_k25[,2] = round(highrisk.city_k25[,2],0)

  highrisk.city = cbind(highrisk.city_k2,highrisk.city_k3,highrisk.city_k5,highrisk.city_k10,highrisk.city_k25)
  colnames(highrisk.city) = unlist(lapply(c(2,3,5,10,25),function(x){paste0(c('Proportion.k=','N.k='),x)}))
  
  highrisk.city = cbind(rs.city.info[,c('StateAbbr', 'PlaceName', 'PlaceFIPS', 'population')], dist.rs.city_under40, dist.rs.city_over40, highrisk.city)
  #highrisk.city = cbind(rs.city.info[,c('StateAbbr', 'PlaceName', 'PlaceFIPS', 'population')], highrisk.city)
  colnames(highrisk.city)
  
  # -------- output
  write.xlsx(highrisk.city,
             file=paste0('data_created/highrisk_',sample,'_',approxtype,'_',threshold_type,'.xlsx'))
  print(s)
}
