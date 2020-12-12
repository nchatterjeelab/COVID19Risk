rm(list=ls())
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
library(grid)
###### construct Bootstrap samples
update_p11 = function(p1,p2,r,p10star,p01star){
  if (r!=Inf){
    a = 1 - r
    b = 1 + (r-1)*(p1+p2)
    c = -r*p1*p2
    p11 = (-b+sqrt(b^2-4*a*c))/(2*a)
  }
  if ((r == Inf)&(p10star==1)){#p10=0
    p11 = p2
  }
  if ((r == Inf)&(p01star==1)){#p01=0
    p11 = p1
  }
  return(p11)
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

betaraw = readRDS('data_created/Bootstrap_samples/beta_bootstrap.rds')
beta = betaraw[,c("agegroup.cdc15_45", "agegroup.cdc45_54", "agegroup.cdc65_74", "agegroup.cdc75_84", 
                  "agegroup.cdc85+", "Male", "Obese_I", "Obese_II", "Obese_III", "Smk_ex", "Smk_current",
                  "race_ethnicity.cdcHispanic", "race_ethnicity.cdcNon_hispanic_Black", "race_ethnicity.cdcNon_hispanic_asian",
                  "race_ethnicity.cdcNon_hispanic_american_Indian", paste0("IMD",2:5), "Hypertension",
                  "Resp_ex_asthma", "Asthma", "CHD", "Diabetes_unctrl","Diabetes_ctrl", paste0("Non_hema_", 1:3),paste0("Hema_", 1:3),
                  "Stroke", "Kidney", "Arthritis")]
brfss = readRDS('data_created/Bootstrap_samples/BRFSS_cities_updated_oct27.rds')
diabetes = readRDS('data_created/Bootstrap_samples/NHANES_diabetes_bootstrap.rds')
nhis = readRDS('data_created/Bootstrap_samples/NHIS_imputed_bootstrap.rds')
beta_cov = readRDS('data_created/Bootstrap_samples/var_covar_beta.rds')
rawdat = readRDS('data_created/combined_raw.rds')
rawdat = rawdat[,c("PlaceFIPS", "Age_15_44", "Age_45_54", 'Age_55_64', "Age_65_74", "Age_75_84", "Age_85", 
           "male_proportion", paste0('proportion_',c('white','hispanic', 'black', 'asian', 'american_indian_alaska_native')),
           'sdi', paste0('non_hematologic_cancer',1:3),paste0('hematologic_cancer',1:3))]
nhis_data = readRDS('data_created/nhis_2017.rds')
covariates = c('Age_15_44','Age_45_54','Age_65_74','Age_75_84','Age_85',
               'male_proportion',paste0('obesity',1:3),'smoking_ex_proportion','smoking_current_proportion',
               paste0('proportion_',c('hispanic', 'black', 'asian', 'american_indian_alaska_native')),
               paste0('IMD',2:5),'BPHIGH_CrudePrev','COPD_CrudePrev',
               'CASTHMA_CrudePrev', 'CHD_CrudePrev',
               'DIABETES_ctrled_CrudePrev', 'DIABETES_unctrled_CrudePrev',
               paste0('non_hematologic_cancer',1:3),paste0('hematologic_cancer',1:3),'STROKE_CrudePrev',
               'KIDNEY_CrudePrev','rheumatoid')

n.boot = 1000
citydata = list()
for (b in 1:n.boot){
  ## merge city data
  combined = merge(rawdat,brfss[[b]],by='PlaceFIPS')
  # update diabetes subcategories
  ratio.unctrl.ctrl = diabetes[b,'ratio_uncontrolled_to_controlled']
  pr.unctrl = ratio.unctrl.ctrl/(1+ratio.unctrl.ctrl)
  combined$DIABETES_unctrled_CrudePrev = combined$DIABETES_CrudePrev * pr.unctrl
  combined$DIABETES_ctrled_CrudePrev = combined$DIABETES_CrudePrev * (1-pr.unctrl)
  
  # here IMD quntiles are defined as SDI quintiles
  combined$IMD1 = ifelse(combined$sdi==1,1,0)
  combined$IMD2 = ifelse(combined$sdi==2,1,0)
  combined$IMD3 = ifelse(combined$sdi==3,1,0)
  combined$IMD4 = ifelse(combined$sdi==4,1,0)
  combined$IMD5 = ifelse(combined$sdi==5,1,0)
  
  # update smoking:
  Pr.smoking_current = mean(nhis[[b]]$smoking_current) # 0.1395412
  Pr.smoking_ex = mean(nhis[[b]]$smoking_ex) # 0.2263748
  combined$smoking_ex_proportion = combined$CSMOKING_CrudePrev * (Pr.smoking_ex)/(Pr.smoking_current+Pr.smoking_ex)
  combined$smoking_current_proportion = combined$CSMOKING_CrudePrev * (Pr.smoking_current)/(Pr.smoking_current+Pr.smoking_ex)
  
  # update obesity:
  Pr.obesity1 = mean(nhis[[b]]$obesity1)
  Pr.obesity2 = mean(nhis[[b]]$obesity2)
  Pr.obesity3 = mean(nhis[[b]]$obesity3)
  Pr.obesity0 = 1 - Pr.obesity1 - Pr.obesity2 - Pr.obesity3
  Pr.obesity = Pr.obesity1 + Pr.obesity2 + Pr.obesity3
  combined$obesity1 = combined$OBESITY_CrudePrev * Pr.obesity1/(Pr.obesity)
  combined$obesity2 = combined$OBESITY_CrudePrev * Pr.obesity2/(Pr.obesity)
  combined$obesity3 = combined$OBESITY_CrudePrev * Pr.obesity3/(Pr.obesity)
  
  # update arthiritis:
  # Among arthritis, the ratio_rheumatoid_to_non_rheumatoid =  0.269098
  ratio.rheumatoid.arthritis = mean(nhis[[b]]$rheumatoid)/mean(nhis[[b]]$arthritis) # 0.269098/(1+0.269098)
  combined$rheumatoid = combined$ARTHRITIS_CrudePrev * ratio.rheumatoid.arthritis
  
  combined$totalethnic = combined$proportion_hispanic+combined$proportion_asian+combined$proportion_american_indian_alaska_native+combined$proportion_black+combined$proportion_white
  combined$proportion_white = combined$proportion_white/combined$totalethnic
  combined$proportion_hispanic = combined$proportion_hispanic/combined$totalethnic
  combined$proportion_black = combined$proportion_black/combined$totalethnic
  combined$proportion_asian = combined$proportion_asian/combined$totalethnic
  combined$proportion_american_indian_alaska_native = combined$proportion_american_indian_alaska_native/combined$totalethnic
  citydata[[b]] = combined
  if (b %% 10 == 0) print(b)
}
save(citydata,file='data_created/Bootstrap_samples/combined_updated_bootstrap_updated.RData')



load('data_created/Bootstrap_samples/combined_updated_bootstrap.RData')
citydata_age = readRDS('~/Dropbox/NHANES_risk_score/Nature Medicine Revision/Github_revision/COVID19Risk/data_created/Bootstrap_samples/prevalance_age_stratification_bootstrap.rds')





betaraw = readRDS('data_created/Bootstrap_samples/beta_bootstrap_nature.rds')
brfss = readRDS('data_created/Bootstrap_samples/BRFSS_cities_updated_oct27.rds')
diabetes = readRDS('data_created/Bootstrap_samples/NHANES_diabetes_bootstrap.rds')
nhisdata = readRDS('data_created/Bootstrap_samples/NHIS_imputed_bootstrap.rds')
beta_cov = readRDS('data_created/Bootstrap_samples/var_covar_beta.rds')
rawdat = readRDS('data_created/combined_raw.rds')
rawdat = rawdat[,c("PlaceFIPS", "Age_15_44", "Age_45_54", 'Age_55_64', "Age_65_74", "Age_75_84", "Age_85", 
                   "male_proportion", paste0('proportion_',c('white','hispanic', 'black', 'asian', 'american_indian_alaska_native')),
                   'sdi', paste0('non_hematologic_cancer',1:3),paste0('hematologic_cancer',1:3))]
covariates = c('Age_15_44','Age_45_54','Age_65_74','Age_75_84','Age_85',
               'male_proportion',paste0('obesity',1:3),'smoking_ex_proportion','smoking_current_proportion',
               paste0('proportion_',c('hispanic', 'black', 'asian', 'american_indian_alaska_native')),
               paste0('IMD',2:5),'BPHIGH_CrudePrev','COPD_CrudePrev',
               'CASTHMA_CrudePrev', 'CHD_CrudePrev',
               'DIABETES_ctrled_CrudePrev', 'DIABETES_unctrled_CrudePrev',
               paste0('non_hematologic_cancer',1:3),paste0('hematologic_cancer',1:3),'STROKE_CrudePrev',
               'KIDNEY_CrudePrev','rheumatoid')
load('data_created/Bootstrap_samples/combined_updated_bootstrap_updated.RData')
citydata_age = readRDS('data_created/Bootstrap_samples/prevalance_age_stratification_bootstrap_updated_oct27.rds')

n.boot = 1000
for (b in 1:n.boot){
  # log hazard ratio of diabetes:
  Beta = beta[b,]
  combined = citydata[[b]]
  age_stratified = citydata_age[[b]]
  additionalinfo = brfss[[b]][,c('StateAbbr','PlaceName','PlaceFIPS')]
  nhis = nhisdata[[b]]
  print(paste0('Complete Bootstrap ', b))
  save(Beta, combined, age_stratified,additionalinfo,nhis, file=paste0('data_created/Bootstrap_samples/bdata/',b,'_updated.RData'))
}






