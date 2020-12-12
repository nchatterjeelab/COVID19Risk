###### Bootstrap - city-level analysis
rm(list=ls())
library(openxlsx)
library(survey)
library(data.table)
library(gapminder)
library(tidyverse)
library(grid)
temp <- commandArgs(TRUE)
b = as.numeric(temp[1])
update_p11 = function(p1,p2,r,p10star,p01star){
  if (abs(r) <=1e10){
    a = 1 - r
    b = 1 + (r-1)*(p1+p2)
    c = -r*p1*p2
    te = (b^2-4*a*c)
    if (te >= 0){
      p_11 = (-b+sqrt(b^2-4*a*c))/(2*a)
    }
  }
  if ((abs(r) >1e10)&(p10star==1)){#p10=0
    p_11 = p2
  }
  if ((abs(r) >1e10)&(p01star==1)){#p01=0
    p_11 = p1
  }
  return(p_11)
}
highrisk.size = function(k, mul1, sigma2l1, mul2, sigma2l2, mul3, sigma2l3,
                         population, sample, data, city){
  w1 = data$w1[city]
  w2 = data$w2[city]
  w3 = data$w3[city]
  if (sample == 'all'){
    Pr.k = w1 * pnorm(log(k)+log(Rl),mul1,sqrt(sigma2l1),lower.tail = F) + 
      w2 * pnorm(log(k)+log(Rl),mul2,sqrt(sigma2l2),lower.tail = F) + 
      w3 * pnorm(log(k)+log(Rl),mul3,sqrt(sigma2l3),lower.tail = F)
  }
  if (sample == 'deaths'){
    Pr.k.above = w1 * pnorm(log(k)+log(Rl),mul1+sigma2l1,sqrt(sigma2l1),lower.tail = F) * exp(mul1+0.5*sigma2l1) + 
      w2 * pnorm(log(k)+log(Rl),mul2+sigma2l2,sqrt(sigma2l2),lower.tail = F) * exp(mul2+0.5*sigma2l2) + 
      w3 * pnorm(log(k)+log(Rl),mul3+sigma2l3,sqrt(sigma2l3),lower.tail = F) * exp(mul3+0.5*sigma2l3)
    Pr.k.below = w1 * pnorm(log(k)+log(Rl),mul1+sigma2l1,sqrt(sigma2l1),lower.tail = T) * exp(mul1+0.5*sigma2l1) + 
      w2 * pnorm(log(k)+log(Rl),mul2+sigma2l2,sqrt(sigma2l2),lower.tail = T) * exp(mul2+0.5*sigma2l2) + 
      w3 * pnorm(log(k)+log(Rl),mul3+sigma2l3,sqrt(sigma2l3),lower.tail = T) * exp(mul3+0.5*sigma2l3)
    Pr.k = Pr.k.above/(Pr.k.above + Pr.k.below)
  }
  n.k = Pr.k * population
  c(Proportion=Pr.k,N=n.k)
}

betaraw = readRDS('data_created/Bootstrap_samples/beta_bootstrap_nature.rds')
beta = betaraw[,c("agegroup.cdc15_45", "agegroup.cdc45_54", "agegroup.cdc65_74", "agegroup.cdc75_84", 
                  "agegroup.cdc85+", "Male", "Obese_I", "Obese_II", "Obese_III", "Smk_ex", "Smk_current",
                  "race_ethnicity.cdcHispanic", "race_ethnicity.cdcNon_hispanic_Black", "race_ethnicity.cdcNon_hispanic_asian",
                  "race_ethnicity.cdcNon_hispanic_american_Indian", paste0("IMD",2:5), "Hypertension",
                  "Resp_ex_asthma", "Asthma", "CHD", "Diabetes_unctrl","Diabetes_ctrl", paste0("Non_hema_", 1:3),paste0("Hema_", 1:3),
                  "Stroke", "Kidney", "Arthritis")]

load(paste0('data_created/Bootstrap_samples/bdata/',b,'_updated.RData'))
print(paste0('Complete loading data'))

dist1 = dist2 = dist3 = full_city = IER = highrisk_all = highrisk_deaths = Rmean = list()
  names(Beta) = colnames(beta)
  m1 = Beta$Diabetes_ctrl
  m2 = Beta$Diabetes_unctrl
  p1 = 6.0; p2 = 2.8
  tem = which(colnames(Beta) %in% c('Diabetes_ctrl','Diabetes_unctrl') )
  Beta = as.numeric(c(Beta[1:(tem[1]-1)],Beta[(tem[2]+1):length(Beta)]))
  Beta = c(Beta[1:(tem[1]-1)], (m1 * p1 + m2 * p2)/(p1+p2), Beta[(tem[1]):length(Beta)])
  names(Beta) = c(colnames(beta)[1:(tem[1]-1)],'Diabetes',colnames(beta)[(tem[2]+1):ncol(beta)])
  
  age_stratified[,-1] = sapply(age_stratified[,-1], as.numeric)
  age_stratified = merge(additionalinfo, age_stratified, by='PlaceFIPS')
  # ------------------------------ Calculate (log-scale) risk score for each city ------------------------------
  dat = combined
  covariates = c('Age_15_44','Age_45_54','Age_65_74','Age_75_84','Age_85',
                 'male_proportion',paste0('obesity',1:3),'smoking_ex_proportion','smoking_current_proportion',
                 paste0('proportion_',c('hispanic', 'black', 'asian', 'american_indian_alaska_native')),
                 paste0('IMD',2:5),'BPHIGH_CrudePrev','COPD_CrudePrev',
                 'CASTHMA_CrudePrev', 'CHD_CrudePrev',
                 'DIABETES_unctrled_CrudePrev','DIABETES_ctrled_CrudePrev',
                 paste0('non_hematologic_cancer',1:3),paste0('hematologic_cancer',1:3),'STROKE_CrudePrev',
                 'KIDNEY_CrudePrev','rheumatoid')
  Covariate_matrix = as.matrix(dat[,covariates])
  rs_est = Covariate_matrix %*% t(beta[b,])
  risk_score_cities = cbind(dat[,c('StateAbbr','PlaceName','PlaceFIPS','population')],rs_est)
  full_output_cities = cbind(dat, rs_est)
  colnames(risk_score_cities)[ncol(risk_score_cities)] = colnames(full_output_cities)[ncol(full_output_cities)] = 'rs_est'
  
  # ----------------------- Calculate age-stratified (log-scale) risk score for each city -----------------------
  dat.pop = combined
  dat.pop = dat.pop[,c('PlaceFIPS','population')]
  # -----------------------------------------------------------------------------------------
  # ------------------------------------ 15-44 age group ------------------------------------
  # -----------------------------------------------------------------------------------------
  dat = age_stratified
  dat = merge(dat, dat.pop,by='PlaceFIPS')
  dat$agetotal = dat$Age_15_44
  dat$Age_15_44 = dat$Age_15_44/dat$agetotal
  dat$Age_45_54 = 0
  dat$Age_65_74 = 0
  dat$Age_75_84 = 0
  dat$Age_85 = 0
  covariates_ex_age = c('Age_15_44', 'Age_45_54', 'Age_65_74', 'Age_75_84', 'Age_85',
                        paste0('prevalance_of_',
                               c('male',paste0('Obesity_Obese_',c('I','II','III')),
                                 'smoking_statusFormer','smoking_statusCurrent',
                                 'hispanic','black','asian','aian'),
                               '_given_age_15_44'),
                        paste0('IMD',2:5),
                        paste0('prevalance_of_',
                               c('hypertensionYes','resp_ex_asthmaYes','asthmaYes','heart_diseaseYes',
                                 'diabetesYes_controlled','diabetesYes_uncontrolled',
                                 'nonhemato_less_than_1_yr','nonhemato_1_5yr','nonhemato_greater_than_5_yr',
                                 'hemato_less_than_1_yr','hemato_1_5yr','hemato_greater_than_5_yr',
                                 'strokeYes','kidney_diseaseYes','rheumatoidYes'),
                               '_given_age_15_44'))
  Covariate_matrix = dat[,covariates_ex_age]
  Covariate_matrix = as.matrix(Covariate_matrix)
  
  # risk score (log scale):
  rs_est = Covariate_matrix %*% t(beta[b,])
  risk_score = cbind(dat[,c('StateAbbr','PlaceName','PlaceFIPS','population')],rs_est)
  full_output = cbind(dat,rs_est)
  colnames(risk_score)[ncol(risk_score)] = colnames(full_output)[ncol(full_output)] = 'rs_est'
  risk_score_age_15_44 = risk_score
  full_output_age_15_44 = full_output
  # -----------------------------------------------------------------------------------------
  # ------------------------------------ 45-74 age group ------------------------------------
  # -----------------------------------------------------------------------------------------
  dat = age_stratified
  dat = merge(dat, dat.pop,by='PlaceFIPS')
  dat$agetotal = dat$Age_45_54 + dat$Age_55_64 + dat$Age_65_74
  dat$Age_15_44 = 0
  dat$Age_45_54 = dat$Age_45_54/dat$agetotal
  dat$Age_65_74 = dat$Age_65_74/dat$agetotal
  dat$Age_75_84 = 0
  dat$Age_85 = 0
  
  covariates_ex_age = c('Age_15_44', 'Age_45_54', 'Age_65_74', 'Age_75_84', 'Age_85',
                        paste0('prevalance_of_',
                               c('male',paste0('Obesity_Obese_',c('I','II','III')),
                                 'smoking_statusFormer','smoking_statusCurrent',
                                 'hispanic','black','asian','aian'),
                               '_given_age_45_74'),
                        paste0('IMD',2:5),
                        paste0('prevalance_of_',
                               c('hypertensionYes','resp_ex_asthmaYes','asthmaYes','heart_diseaseYes',
                                 'diabetesYes_controlled','diabetesYes_uncontrolled',
                                 'nonhemato_less_than_1_yr','nonhemato_1_5yr','nonhemato_greater_than_5_yr',
                                 'hemato_less_than_1_yr','hemato_1_5yr','hemato_greater_than_5_yr',
                                 'strokeYes','kidney_diseaseYes','rheumatoidYes'),
                               '_given_age_45_74'))
  Covariate_matrix = dat[,covariates_ex_age]
  Covariate_matrix = as.matrix(Covariate_matrix)
  
  # risk score (log scale):
  rs_est = Covariate_matrix %*% t(beta[b,])
  risk_score = cbind(dat[,c('StateAbbr','PlaceName','PlaceFIPS','population')],rs_est)
  full_output = cbind(dat,rs_est)
  colnames(risk_score)[ncol(risk_score)] = colnames(full_output)[ncol(full_output)] = 'rs_est'
  risk_score_age_45_74 = risk_score
  full_output_age_45_74 = full_output
  # ----------------------------------------------------------------------------------------
  # ------------------------------------ 75 + age group ------------------------------------
  # ----------------------------------------------------------------------------------------
  dat = age_stratified
  dat = merge(dat, dat.pop,by='PlaceFIPS')
  
  dat$agetotal = dat$Age_75_84 + dat$Age_85
  dat$Age_15_44 = 0
  dat$Age_45_54 = 0
  dat$Age_65_74 = 0
  dat$Age_75_84 = dat$Age_75_84/dat$agetotal
  dat$Age_85 = dat$Age_85/dat$agetotal
  
  covariates_ex_age = c('Age_15_44', 'Age_45_54', 'Age_65_74', 'Age_75_84', 'Age_85',
                        paste0('prevalance_of_',
                               c('male',paste0('Obesity_Obese_',c('I','II','III')),
                                 'smoking_statusFormer','smoking_statusCurrent',
                                 'hispanic','black','asian','aian'),
                               '_given_age_75above'),
                        paste0('IMD',2:5),
                        paste0('prevalance_of_',
                               c('hypertensionYes','resp_ex_asthmaYes','asthmaYes','heart_diseaseYes',
                                 'diabetesYes_controlled','diabetesYes_uncontrolled',
                                 'nonhemato_less_than_1_yr','nonhemato_1_5yr','nonhemato_greater_than_5_yr',
                                 'hemato_less_than_1_yr','hemato_1_5yr','hemato_greater_than_5_yr',
                                 'strokeYes','kidney_diseaseYes','rheumatoidYes'),
                               '_given_age_75above'))
  
  Covariate_matrix = dat[,covariates_ex_age]
  Covariate_matrix = as.matrix(Covariate_matrix)
  
  rs_est = Covariate_matrix %*% t(beta[b,])
  risk_score = cbind(dat[,c('StateAbbr','PlaceName','PlaceFIPS','population')],rs_est)
  full_output = cbind(dat,rs_est)
  colnames(risk_score)[ncol(risk_score)] = colnames(full_output)[ncol(full_output)] = 'rs_est'
  risk_score_age_75 = risk_score
  full_output_age_75 = full_output
  
  
  # ----------------------- Calculate IER and proportion of high-risk individuals/deaths within each city -----------------------
  # -----------------------------------------------------------------------------------------
  # ------------------------------------ 15-44 age group ------------------------------------
  # -----------------------------------------------------------------------------------------
  dat = nhis
  dat = dat[dat$age<45,]
  dat = dat[dat$sampling_weights!=0,]
  dat = dat %>% mutate(inAnalysis_less_45 = if_else(age<45, 1, 0))
  covariates = c('Age_15_44','Age_45_54','Age_65_74','Age_75_84','Age_85',
                 'male',paste0('obesity',1:3),'smoking_ex','smoking_current',
                 'hispanic','black','asian','native', paste0('sdi',2:5),
                 'hbp','copd', 'asthma', 'chd', 'diabetes',
                 paste0('non_hematologic_cancer',1:3),paste0('hematologic_cancer',1:3),'stroke',
                 'kidney_disease','rheumatoid')
  
  # ------- step 1: calculate P (prevalence) i.e. city-level prevalence based on brfss data
  dat.pop = combined
  dat.pop = dat.pop[,c('PlaceFIPS','population')]
  # ------- extract mean RS for each city
  rs.city.info = risk_score_age_15_44
  rs.city.info = rs.city.info[,c('PlaceFIPS','rs_est')]
  dat.city = age_stratified
  dat.city = merge(dat.city, dat.pop, by='PlaceFIPS')
  dat.city = merge(dat.city, rs.city.info, by='PlaceFIPS')
  
  rs.city = dat.city$rs_est
  dat.city$prevalance_of_diabetes_given_age_15_44 = dat.city$prevalance_of_diabetesYes_controlled_given_age_15_44 + dat.city$prevalance_of_diabetesYes_uncontrolled_given_age_15_44
  dat.city$agetotal = dat.city$Age_15_44
  dat.city$Age_15_44 = dat.city$Age_15_44/dat.city$agetotal
  dat.city$Age_45_54 = 0
  dat.city$Age_65_74 = 0
  dat.city$Age_75_84 = 0
  dat.city$Age_85 = 0
  covariates.city = c('Age_15_44', 'Age_45_54', 'Age_65_74', 'Age_75_84', 'Age_85',
                      paste0('prevalance_of_',
                             c('male',paste0('Obesity_Obese_',c('I','II','III')),
                               'smoking_statusFormer','smoking_statusCurrent',
                               'hispanic','black','asian','aian'),
                             '_given_age_15_44'),
                      paste0('IMD',2:5),
                      paste0('prevalance_of_',
                             c('hypertensionYes','resp_ex_asthmaYes','asthmaYes','heart_diseaseYes',
                               'diabetes',
                               'nonhemato_less_than_1_yr','nonhemato_1_5yr','nonhemato_greater_than_5_yr',
                               'hemato_less_than_1_yr','hemato_1_5yr','hemato_greater_than_5_yr',
                               'strokeYes','kidney_diseaseYes','rheumatoidYes'),
                             '_given_age_15_44'))
  dat.city = dat.city[,covariates.city]

  # -------- matrix of marginal prevalence
  P = dat.city # row: city; column: covariates
  # ------- step 2: calculate OR (odds ratio matrix) based on NHIS individual-level data
  M = length(covariates.city)
  OR = matrix(NA, M, M)
  P10star = P01star = matrix(0, M, M)
  for (i in 1:M){
    for (j in 1:M){
      if (i!=j){
        tem.i = dat[,covariates[i]]
        tem.j = dat[,covariates[j]]
        tem = dat[((!is.na(tem.i))&(!is.na(tem.j))),]
        temi = tem.i[((!is.na(tem.i))&(!is.na(tem.j)))]
        temj = tem.j[((!is.na(tem.i))&(!is.na(tem.j)))]
        p.11 = sum(tem[(temi==1)&(temj==1),'sampling_weights'])
        p.10 = sum(tem[(temi==0)&(temj==1),'sampling_weights'])
        p.01 = sum(tem[(temi==1)&(temj==0),'sampling_weights'])
        p.00 = sum(tem[(temi==0)&(temj==0),'sampling_weights'])
        p.11;p.10;p.01;p.00;sum(p.11+p.10+p.01+p.00)
        OR[i,j] = OR[j,i] = p.11*p.00/(p.10*p.01)
        if (p.10 == 0) P10star[i,j] = 1
        if (p.01 == 0) P01star[i,j] = 1
      }
    }
  }

  # ------- step 3: calculate covariance matrix of the risk score within each city
  multinom.list = list(1:5,7:9,10:11,12:15,16:19,25:27,28:30)
  multinom.index = unlist(multinom.list)
  binomial.index = c(1:M)[-multinom.index]
  
  cdrs_city_dist = function(P,OR,city,Beta,rs.mu.city){
    cov.cdrs = matrix(0,M,M)
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
            p11 = update_p11(P[city,i],P[city,j],OR[i,j],P10star[i,j],P01star[i,j])
            cov.cdrs[i,j] = cov.cdrs[j,i] = p11 - P[city,i] * P[city,j]
          }
        }
      }
    }
    cov.cdrs[binomial.index,binomial.index] #NaN: correlation=1
    for (k in 1:length(multinom.list)){
      # 2. between binomial and each multinomial category:
      for (i in multinom.list[[k]]){ # multinomial covariates
        for (j in c(1:M)[-multinom.list[[k]]]){ # binomial covariates
          # i != j
          if (OR[i,j] == 'NaN') cov.cdrs[i,j] = cov.cdrs[j,i] = 0
          if (OR[i,j] != 'NaN'){
            p11 = update_p11(P[city,i],P[city,j],OR[i,j],P10star[i,j],P01star[i,j])
            cov.cdrs[i,j] = cov.cdrs[j,i] = p11 - P[city,i] * P[city,j]
          }
        }
      }
      # 2. between binomial and each multinomial category:
      for (i in multinom.list[[k]]){ # multinomial covariates
        for (j in multinom.list[[k]]){ # binomial covariates
          if(i != j){
            if (OR[i,j] == 'NaN') cov.cdrs[i,j] = cov.cdrs[j,i] = 0
            if (OR[i,j] != 'NaN'){
              p11 = update_p11(P[city,i],P[city,j],OR[i,j],P10star[i,j],P01star[i,j])
              cov.cdrs[i,j] = cov.cdrs[j,i] = p11 - P[city,i] * P[city,j]
            }
          }
        }
      }
    }
    rs.var.city = Beta %*% cov.cdrs %*% Beta
    return(c(mu.rs = rs.mu.city, var.rs = rs.var.city))
  }
  dist.rs.city1 = t(sapply(1:nrow(dat.city),function(x){cdrs_city_dist(P,OR,x,Beta,rs.city[x])}))
  print('Age group 1')
  # -----------------------------------------------------------------------------------------
  # ------------------------------------ 45-74 age group ------------------------------------
  # -----------------------------------------------------------------------------------------
  dat = nhis
  dat = dat[(dat$age>=45)&(dat$age<75),]
  dat = dat[dat$sampling_weights!=0,]
  dat = dat %>% mutate(inAnalysis_greater_45_less_75 = if_else((age>=45 & age < 75), 1, 0))
  covariates = c('Age_15_44','Age_45_54','Age_65_74','Age_75_84','Age_85',
                 'male',paste0('obesity',1:3),'smoking_ex','smoking_current',
                 'hispanic','black','asian','native', paste0('sdi',2:5),
                 'hbp','copd', 'asthma', 'chd', 'diabetes',
                 paste0('non_hematologic_cancer',1:3),paste0('hematologic_cancer',1:3),'stroke',
                 'kidney_disease','rheumatoid')
  
  # ------- step 1: calculate P (prevalence) i.e. city-level prevalence based on brfss data
  dat.pop = combined
  dat.pop = dat.pop[,c('PlaceFIPS','population')]
  # ------- extract mean RS for each city
  rs.city.info = risk_score_age_45_74
  rs.city.info = rs.city.info[,c('PlaceFIPS','rs_est')]
  # -------
  dat.city = age_stratified
  dat.city = merge(dat.city, dat.pop,by='PlaceFIPS')
  dat.city = merge(dat.city, rs.city.info, by='PlaceFIPS')
  
  rs.city = dat.city$rs_est
  dat.city$prevalance_of_diabetes_given_age_45_74 = dat.city$prevalance_of_diabetesYes_controlled_given_age_45_74 + dat.city$prevalance_of_diabetesYes_uncontrolled_given_age_45_74
  dat.city$agetotal = dat.city$Age_45_54 + dat.city$Age_55_64 + dat.city$Age_65_74
  dat.city$Age_15_44 = 0
  dat.city$Age_45_54 = dat.city$Age_45_54/dat.city$agetotal
  dat.city$Age_65_74 = dat.city$Age_65_74/dat.city$agetotal
  dat.city$Age_75_84 = 0
  dat.city$Age_85 = 0
  covariates.city = c('Age_15_44', 'Age_45_54', 'Age_65_74', 'Age_75_84', 'Age_85',
                      paste0('prevalance_of_',
                             c('male',paste0('Obesity_Obese_',c('I','II','III')),
                               'smoking_statusFormer','smoking_statusCurrent',
                               'hispanic','black','asian','aian'),
                             '_given_age_45_74'),
                      paste0('IMD',2:5),
                      paste0('prevalance_of_',
                             c('hypertensionYes','resp_ex_asthmaYes','asthmaYes','heart_diseaseYes',
                               'diabetes',
                               'nonhemato_less_than_1_yr','nonhemato_1_5yr','nonhemato_greater_than_5_yr',
                               'hemato_less_than_1_yr','hemato_1_5yr','hemato_greater_than_5_yr',
                               'strokeYes','kidney_diseaseYes','rheumatoidYes'),
                             '_given_age_45_74'))
  dat.city = dat.city[,covariates.city]

  # -------- matrix of marginal prevalence
  P = dat.city # row: city; column: covariates
  # ------- step 2: calculate OR (odds ratio matrix) based on NHIS individual-level data
  M = length(covariates.city)
  OR = matrix(NA, M, M)
  P10star = P01star = matrix(0, M, M)
  for (i in 1:M){
    for (j in 1:M){
      if (i!=j){
        tem.i = dat[,covariates[i]]
        tem.j = dat[,covariates[j]]
        tem = dat[((!is.na(tem.i))&(!is.na(tem.j))),]
        temi = tem.i[((!is.na(tem.i))&(!is.na(tem.j)))]
        temj = tem.j[((!is.na(tem.i))&(!is.na(tem.j)))]
        p.11 = sum(tem[(temi==1)&(temj==1),'sampling_weights'])
        p.10 = sum(tem[(temi==0)&(temj==1),'sampling_weights'])
        p.01 = sum(tem[(temi==1)&(temj==0),'sampling_weights'])
        p.00 = sum(tem[(temi==0)&(temj==0),'sampling_weights'])
        p.11;p.10;p.01;p.00;sum(p.11+p.10+p.01+p.00)
        OR[i,j] = OR[j,i] = p.11*p.00/(p.10*p.01)
        if (p.10 == 0) P10star[i,j] = 1
        if (p.01 == 0) P01star[i,j] = 1
      }
    }
  }

  # ------- step 3: calculate covariance matrix of the risk score within each city
  multinom.list = list(c(1:5),7:9,10:11,12:15,16:19,25:27,28:30)
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
            p11 = update_p11(P[city,i],P[city,j],OR[i,j],P10star[i,j],P01star[i,j])
            cov.cdrs[i,j] = cov.cdrs[j,i] = p11 - P[city,i] * P[city,j]
          }
        }
      }
    }
    cov.cdrs[binomial.index,binomial.index] #NaN: correlation=1
    for (k in 1:length(multinom.list)){
      # 2. between binomial and each multinomial category:
      for (i in multinom.list[[k]]){ # multinomial covariates
        for (j in c(1:M)[-multinom.list[[k]]]){ # binomial covariates
          # i != j
          if (OR[i,j] == 'NaN') cov.cdrs[i,j] = cov.cdrs[j,i] = 0
          if (OR[i,j] != 'NaN'){
            p11 = update_p11(P[city,i],P[city,j],OR[i,j],P10star[i,j],P01star[i,j])
            cov.cdrs[i,j] = cov.cdrs[j,i] = p11 - P[city,i] * P[city,j]
          }
        }
      }
      # 2. between binomial and each multinomial category:
      for (i in multinom.list[[k]]){ # multinomial covariates
        for (j in multinom.list[[k]]){ # binomial covariates
          if(i != j){
            if (OR[i,j] == 'NaN') cov.cdrs[i,j] = cov.cdrs[j,i] = 0
            if (OR[i,j] != 'NaN'){
              p11 = update_p11(P[city,i],P[city,j],OR[i,j],P10star[i,j],P01star[i,j])
              cov.cdrs[i,j] = cov.cdrs[j,i] = p11 - P[city,i] * P[city,j]
            }
          }
        }
      }
    }
    rs.var.city = Beta %*% cov.cdrs %*% Beta
    return(c(mu.rs = rs.mu.city, var.rs = rs.var.city))
  }
  
  dist.rs.city2 = t(sapply(1:nrow(dat.city),function(x){cdrs_city_dist(P,OR,x,Beta,rs.city[x])}))
  print('Age group 2')
  
  # ----------------------------------------------------------------------------------------
  # ------------------------------------ 75 + age group ------------------------------------
  # ----------------------------------------------------------------------------------------
  dat = nhis
  dat = dat[(dat$age>=75),]
  dat = dat[dat$sampling_weights!=0,]
  dat = dat %>% mutate(inAnalysis_greater_75 = if_else((age>=75), 1, 0))
  covariates = c('Age_15_44','Age_45_54','Age_65_74','Age_75_84','Age_85',
                 'male',paste0('obesity',1:3),'smoking_ex','smoking_current',
                 'hispanic','black','asian','native', paste0('sdi',2:5),
                 'hbp','copd', 'asthma', 'chd', 'diabetes',
                 paste0('non_hematologic_cancer',1:3),paste0('hematologic_cancer',1:3),'stroke',
                 'kidney_disease','rheumatoid')
  
  # ------- step 1: calculate P (prevalence) i.e. city-level prevalence based on brfss data
  dat.pop = combined
  dat.pop = dat.pop[,c('PlaceFIPS','population')]
  # ------- extract mean RS for each city
  rs.city.info = risk_score_age_75
  rs.city.info = rs.city.info[,c('PlaceFIPS','rs_est')]
  # -------
  dat.city = age_stratified
  dat.city = merge(dat.city, dat.pop,by='PlaceFIPS')
  dat.city = merge(dat.city, rs.city.info, by='PlaceFIPS')
  
  rs.city = dat.city$rs_est
  dat.city$prevalance_of_diabetes_given_age_75above = dat.city$prevalance_of_diabetesYes_controlled_given_age_75above + dat.city$prevalance_of_diabetesYes_uncontrolled_given_age_75above
  dat.city$agetotal = dat.city$Age_75_84 + dat.city$Age_85
  dat.city$Age_15_44 = 0
  dat.city$Age_45_54 = 0
  dat.city$Age_65_74 = 0
  dat.city$Age_75_84 = dat.city$Age_75_84/dat.city$agetotal
  dat.city$Age_85 = dat.city$Age_85/dat.city$agetotal
  covariates.city = c('Age_15_44', 'Age_45_54', 'Age_65_74', 'Age_75_84', 'Age_85',
                      paste0('prevalance_of_',
                             c('male',paste0('Obesity_Obese_',c('I','II','III')),
                               'smoking_statusFormer','smoking_statusCurrent',
                               'hispanic','black','asian','aian'),
                             '_given_age_75above'),
                      paste0('IMD',2:5),
                      paste0('prevalance_of_',
                             c('hypertensionYes','resp_ex_asthmaYes','asthmaYes','heart_diseaseYes',
                               'diabetes',
                               'nonhemato_less_than_1_yr','nonhemato_1_5yr','nonhemato_greater_than_5_yr',
                               'hemato_less_than_1_yr','hemato_1_5yr','hemato_greater_than_5_yr',
                               'strokeYes','kidney_diseaseYes','rheumatoidYes'),
                             '_given_age_75above'))
  dat.city = dat.city[,covariates.city]

  # -------- matrix of marginal prevalence
  P = dat.city # row: city; column: covariates
  # ------- step 2: calculate OR (odds ratio matrix) based on NHIS individual-level data
  M = length(covariates.city)
  OR = matrix(NA, M, M)
  P10star = P01star = matrix(0, M, M)
  for (i in 1:M){
    for (j in 1:M){
      if (i!=j){
        tem.i = dat[,covariates[i]]
        tem.j = dat[,covariates[j]]
        tem = dat[((!is.na(tem.i))&(!is.na(tem.j))),]
        temi = tem.i[((!is.na(tem.i))&(!is.na(tem.j)))]
        temj = tem.j[((!is.na(tem.i))&(!is.na(tem.j)))]
        p.11 = sum(tem[(temi==1)&(temj==1),'sampling_weights'])
        p.10 = sum(tem[(temi==0)&(temj==1),'sampling_weights'])
        p.01 = sum(tem[(temi==1)&(temj==0),'sampling_weights'])
        p.00 = sum(tem[(temi==0)&(temj==0),'sampling_weights'])
        p.11;p.10;p.01;p.00;sum(p.11+p.10+p.01+p.00)
        OR[i,j] = OR[j,i] = p.11*p.00/(p.10*p.01)
        if (p.10 == 0) P10star[i,j] = 1
        if (p.01 == 0) P01star[i,j] = 1
      }
    }
  }

  # ------- step 3: calculate covariance matrix of CDRS for each city
  multinom.list = list(c(1:5),7:9,10:11,12:15,16:19,25:27,28:30)
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
            p11 = update_p11(P[city,i],P[city,j],OR[i,j],P10star[i,j],P01star[i,j])
            cov.cdrs[i,j] = cov.cdrs[j,i] = p11 - P[city,i] * P[city,j]
          }
        }
      }
    }
    cov.cdrs[binomial.index,binomial.index] #NaN: correlation=1
    for (k in 1:length(multinom.list)){
      # 2. between binomial and each multinomial category:
      for (i in multinom.list[[k]]){ # multinomial covariates
        for (j in c(1:M)[-multinom.list[[k]]]){ # binomial covariates
          # i != j
          if (OR[i,j] == 'NaN') cov.cdrs[i,j] = cov.cdrs[j,i] = 0
          if (OR[i,j] != 'NaN'){
            p11 = update_p11(P[city,i],P[city,j],OR[i,j],P10star[i,j],P01star[i,j])
            cov.cdrs[i,j] = cov.cdrs[j,i] = p11 - P[city,i] * P[city,j]
          }
        }
      }
      # 2. between binomial and each multinomial category:
      for (i in multinom.list[[k]]){ # multinomial covariates
        for (j in multinom.list[[k]]){ # binomial covariates
          if(i != j){
            if (OR[i,j] == 'NaN') cov.cdrs[i,j] = cov.cdrs[j,i] = 0
            if (OR[i,j] != 'NaN'){
              p11 = update_p11(P[city,i],P[city,j],OR[i,j],P10star[i,j],P01star[i,j])
              cov.cdrs[i,j] = cov.cdrs[j,i] = p11 - P[city,i] * P[city,j]
            }
          }
        }
      }
    }
    rs.var.city = Beta %*% cov.cdrs %*% Beta
    return(c(mu.rs = rs.mu.city, var.rs = rs.var.city))
  }
  dist.rs.city3 = t(sapply(1:nrow(dat.city),function(x){cdrs_city_dist(P,OR,x,Beta,rs.city[x])}))
  print('Age group 3')
  
  # --------- Calculate Index of Excess Risk (IER) of each city ---------
  full.city = full_output_cities
  full.city$w1 = full.city$Age_15_44
  full.city$w2 = full.city$Age_45_54 + full.city$Age_55_64 + full.city$Age_65_74
  full.city$w3 = full.city$Age_75_84 + full.city$Age_85
  full.city$population = as.numeric(as.character(full.city$population))
  
  mean.city.risks = full.city$w1 * exp(dist.rs.city1[,'mu.rs']+0.5*dist.rs.city1[,'var.rs']) + 
    full.city$w2 * exp(dist.rs.city2[,'mu.rs']+0.5*dist.rs.city2[,'var.rs']) + 
    full.city$w3 * exp(dist.rs.city3[,'mu.rs']+0.5*dist.rs.city3[,'var.rs'])
  Rl = sum(mean.city.risks * as.numeric(as.character(full.city$population)))/sum(as.numeric(as.character(full.city$population)))
  #Rl # ~4.56
  # proportion of 3 mixture components within each state
  colnames(dist.rs.city1) = c("mu1", "var1")
  colnames(dist.rs.city2) = c("mu2", "var2")
  colnames(dist.rs.city3) = c("mu3", "var3")
  
  # Index of Excess Risk (iER)
  mean.city.risks = mean.city.risks/Rl
  IER[[b]] = data.frame(IER = mean.city.risks,
                        state = full.city$StateAbbr,
                        city = full.city$PlaceName,
                        PlaceFIPS = full.city$PlaceFIPS,
                        population = full.city$population)

  # ----------------- Estimate proportion & size of vulnerable population within each city ----------------- 
  scenario = c(sample = c('all','deaths')) 
  for (s in 1:length(scenario)){
    sample = scenario[s]
    # 
    k = 1.2
    highrisk.city_k1.2 = t(sapply(1:nrow(full.city),function(x){highrisk.size(k, mul1 = dist.rs.city1[x,'mu1'], 
                                                                              sigma2l1 = dist.rs.city1[x,'var1'], 
                                                                              mul2 = dist.rs.city2[x,'mu2'], 
                                                                              sigma2l2 = dist.rs.city2[x,'var2'], 
                                                                              mul3 = dist.rs.city3[x,'mu3'], 
                                                                              sigma2l3 = dist.rs.city3[x,'var3'],
                                                                              population = full.city$population[x],
                                                                              sample,
                                                                              data = full.city,city=x)}))
    highrisk.city_k1.2[,2] = round(highrisk.city_k1.2[,2],0)
    # 
    k = 2
    highrisk.city_k2 = t(sapply(1:nrow(full.city),function(x){highrisk.size(k, mul1 = dist.rs.city1[x,'mu1'], 
                                                                            sigma2l1 = dist.rs.city1[x,'var1'], 
                                                                            mul2 = dist.rs.city2[x,'mu2'], 
                                                                            sigma2l2 = dist.rs.city2[x,'var2'], 
                                                                            mul3 = dist.rs.city3[x,'mu3'], 
                                                                            sigma2l3 = dist.rs.city3[x,'var3'],
                                                                            population = full.city$population[x],
                                                                            sample,
                                                                            data = full.city,city=x)}))
    highrisk.city_k2[,2] = round(highrisk.city_k2[,2],0)
    # 
    k = 5
    highrisk.city_k5 = t(sapply(1:nrow(full.city),function(x){highrisk.size(k, mul1 = dist.rs.city1[x,'mu1'], 
                                                                            sigma2l1 = dist.rs.city1[x,'var1'], 
                                                                            mul2 = dist.rs.city2[x,'mu2'], 
                                                                            sigma2l2 = dist.rs.city2[x,'var2'], 
                                                                            mul3 = dist.rs.city3[x,'mu3'], 
                                                                            sigma2l3 = dist.rs.city3[x,'var3'],
                                                                            population = full.city$population[x],
                                                                            sample,
                                                                            data = full.city,city=x)}))
    highrisk.city_k5[,2] = round(highrisk.city_k5[,2],0)
    # 
    k = 10
    highrisk.city_k10 = t(sapply(1:nrow(full.city),function(x){highrisk.size(k, mul1 = dist.rs.city1[x,'mu1'], 
                                                                             sigma2l1 = dist.rs.city1[x,'var1'], 
                                                                             mul2 = dist.rs.city2[x,'mu2'], 
                                                                             sigma2l2 = dist.rs.city2[x,'var2'], 
                                                                             mul3 = dist.rs.city3[x,'mu3'], 
                                                                             sigma2l3 = dist.rs.city3[x,'var3'],
                                                                             population = full.city$population[x],
                                                                             sample,
                                                                             data = full.city,city=x)}))
    highrisk.city_k10[,2] = round(highrisk.city_k10[,2],0)
    
    highrisk.city = cbind(highrisk.city_k1.2,highrisk.city_k2,highrisk.city_k5,highrisk.city_k10)
    colnames(highrisk.city) = unlist(lapply(c(1.2,2,5,10),function(x){paste0(c('Proportion.k=','N.k='),x)}))
    
    highrisk.city = cbind(full.city[,c('StateAbbr', 'PlaceName', 'PlaceFIPS', 'population')], highrisk.city)
    colnames(highrisk.city)
    # -------- output
    if (sample == 'all') highrisk_all[[b]] = highrisk.city
    if (sample == 'deaths') highrisk_deaths[[b]] = highrisk.city
  }
  dist1[[b]] = dist.rs.city1; dist2[[b]] = dist.rs.city2; dist3[[b]] = dist.rs.city3
  full_city[[b]] = full.city
  Rmean[[b]] = Rl
  print(paste0('Complete Bootstrap ', b))

dist1.b = dist1#[[b]]
dist2.b = dist2#[[b]]
dist3.b = dist3#[[b]]
full_city.b = full_city#[[b]]
IER.b = IER#[[b]] 
highrisk_all.b = highrisk_all#[[b]]
highrisk_deaths.b = highrisk_deaths#[[b]]
Rmean.b = Rmean#[[b]]

save(dist1.b,dist2.b,dist3.b,full_city.b,IER.b,highrisk_all.b,highrisk_deaths.b,Rmean.b,file=paste0('data_created/Bootstrap_results_all/b',b,'.RData'))

