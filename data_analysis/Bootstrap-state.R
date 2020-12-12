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
                         population, sample, data, state){
  w1 = data$Age_15_44_Prev[state]
  w2 = data$Age_45_64_prev[state]
  w3 = data$Age_65_and_above_prev[state]
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

load(paste0('data_created/Bootstrap_samples/state/bdata/',b,'.RData'))
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

  # ----------------------- Calculate age-stratified (log-scale) risk score for each city -----------------------
  dat = age_stratified
  # -----------------------------------------------------------------------------------------
  # ------------------------------------ 15-44 age group ------------------------------------
  # -----------------------------------------------------------------------------------------
  covariates.state = paste0(c('Age_15_44_Prev', 'Age_45_54_Prev', 'Age_65_74_Prev', 'Age_75_84_Prev', 'Age_85_Prev',
                              'Male_Prev', paste0('obesity',1:3), 'smoking_ex_proportion', 'smoking_current_proportion',
                              'Hispanic_Prev', 'Black_Prev','Asian_Prev','Native_Prev',
                              paste0('SDI_',2:5),
                              'BPHIGH_CrudePrev', 'COPD_CrudePrev', 'CASTHMA_CrudePrev', 'CHD_CrudePrev',
                              'DIABETES_unctrled_CrudePrev','DIABETES_ctrled_CrudePrev',
                              paste0('non_hemo_cat',1:3), paste0('hemo_cat',1:3),
                              'STROKE_CrudePrev', 'KIDNEY_CrudePrev', 'ARTHRITIS_CrudePrev'), '.Age_18_44')
  Covariate_matrix = dat[,covariates.state]
  Covariate_matrix = as.matrix(Covariate_matrix)
  
  # risk score (log scale):
  rs_est = Covariate_matrix %*% t(beta[b,])
  risk_scores = cbind(dat,rs_est)
  colnames(risk_scores)[ncol(risk_scores)] = 'rs1'
  # -----------------------------------------------------------------------------------------
  # ------------------------------------ 45-74 age group ------------------------------------
  # -----------------------------------------------------------------------------------------
  covariates.state = paste0(c('Age_15_44_Prev', 'Age_45_54_Prev', 'Age_65_74_Prev', 'Age_75_84_Prev', 'Age_85_Prev',
                              'Male_Prev', paste0('obesity',1:3), 'smoking_ex_proportion', 'smoking_current_proportion',
                              'Hispanic_Prev', 'Black_Prev','Asian_Prev','Native_Prev',
                              paste0('SDI_',2:5),
                              'BPHIGH_CrudePrev', 'COPD_CrudePrev', 'CASTHMA_CrudePrev', 'CHD_CrudePrev',
                              'DIABETES_unctrled_CrudePrev','DIABETES_ctrled_CrudePrev',
                              paste0('non_hemo_cat',1:3), paste0('hemo_cat',1:3),
                              'STROKE_CrudePrev', 'KIDNEY_CrudePrev', 'ARTHRITIS_CrudePrev'), '.Age_45_64')
  Covariate_matrix = dat[,covariates.state]
  Covariate_matrix = as.matrix(Covariate_matrix)
  
  # risk score (log scale):
  rs_est = Covariate_matrix %*% t(beta[b,])
  risk_scores = cbind(risk_scores,rs_est)
  colnames(risk_scores)[ncol(risk_scores)] = 'rs2'
  # ----------------------------------------------------------------------------------------
  # ------------------------------------ 75 + age group ------------------------------------
  # ----------------------------------------------------------------------------------------
  covariates.state = paste0(c('Age_15_44_Prev', 'Age_45_54_Prev', 'Age_65_74_Prev', 'Age_75_84_Prev', 'Age_85_Prev',
                              'Male_Prev', paste0('obesity',1:3), 'smoking_ex_proportion', 'smoking_current_proportion',
                              'Hispanic_Prev', 'Black_Prev','Asian_Prev','Native_Prev',
                              paste0('SDI_',2:5),
                              'BPHIGH_CrudePrev', 'COPD_CrudePrev', 'CASTHMA_CrudePrev', 'CHD_CrudePrev',
                              'DIABETES_unctrled_CrudePrev','DIABETES_ctrled_CrudePrev',
                              paste0('non_hemo_cat',1:3), paste0('hemo_cat',1:3),
                              'STROKE_CrudePrev', 'KIDNEY_CrudePrev', 'ARTHRITIS_CrudePrev'), '.Age_65above')
  Covariate_matrix = dat[,covariates.state]
  Covariate_matrix = as.matrix(Covariate_matrix)
  
  # risk score (log scale):
  rs_est = Covariate_matrix %*% t(beta[b,])
  risk_scores = cbind(risk_scores,rs_est)
  colnames(risk_scores)[ncol(risk_scores)] = 'rs3'

  # --------------------------------------------------------------------------------------------------
  # -------------------------- Calculate variance of risk score for each state -----------------------
  # --------------------------------------------------------------------------------------------------
  # -----------------------------------------------------------------------------------------
  # ------------------------------------ 15-44 age group ------------------------------------
  # -----------------------------------------------------------------------------------------
  # ----- Create the covariates matrix using NHIS individual-level data. Don't need to exclude missing rows for now
  dat = nhis
  dat = dat[(dat$age>=15)&(dat$age<45),]
  dat = dat[dat$sampling_weights!=0,]
  covariates = c('Age_15_44','Age_45_54','Age_65_74','Age_75_84','Age_85',
                 'male',paste0('obesity',1:3),'smoking_ex','smoking_current',
                 'hispanic','black','asian','native', paste0('sdi',2:5),
                 'hbp','copd', 'asthma', 'chd', 'diabetes',
                 paste0('non_hematologic_cancer',1:3),paste0('hematologic_cancer',1:3),'stroke',
                 'kidney_disease','rheumatoid')
  
  # ------- step 1: calculate P (prevalence) i.e. state-level prevalence from brfss data, which is the mean of CDRS for each state
  dat.state = age_stratified
  covariates.state = paste0(c('Age_15_44_Prev', 'Age_45_54_Prev', 'Age_65_74_Prev', 'Age_75_84_Prev', 'Age_85_Prev',
                              'Male_Prev', paste0('obesity',1:3), 'smoking_ex_proportion', 'smoking_current_proportion',
                              'Hispanic_Prev', 'Black_Prev','Asian_Prev','Native_Prev',
                              paste0('SDI_',2:5),
                              'BPHIGH_CrudePrev', 'COPD_CrudePrev', 'CASTHMA_CrudePrev', 'CHD_CrudePrev',
                              'DIABETES_CrudePrev', 
                              paste0('non_hemo_cat',1:3), paste0('hemo_cat',1:3),
                              'STROKE_CrudePrev', 'KIDNEY_CrudePrev', 'ARTHRITIS_CrudePrev'), '.Age_18_44')
  dat.state = dat.state[,covariates.state]
  # -------- matrix of marginal prevalence
  P = dat.state # row: state; column: covariates
  # ------- step 2: calculate OR (odds ratio matrix) excluding SDI using NHIS individual-level data
  M = length(covariates.state)
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
        # here p does not mean probability...
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
  
  # ------- step 3: extract mean RS for each state for <45 age group
  rs.state = risk_scores$rs1
  # ------- step 4: calculate covariance matrix of CDRS for each state
  multinom.list = list(c(1:5),7:9,10:11,12:15,16:19,25:27,28:30)
  multinom.index = unlist(multinom.list)
  binomial.index = c(1:M)[-multinom.index]
  cdrs_state_dist = function(P,OR,state,Beta,rs.mu.state){
    cov.cdrs = matrix(NA,M,M)
    # variance
    for (i in 1:M){
      cov.cdrs[i,i] = P[state,i]*(1-P[state,i])
    }
    # covariance
    # 1. between all binomial covariates:
    for (i in binomial.index){
      for (j in binomial.index){
        if (i!=j){
          if (OR[i,j] == 'NaN') cov.cdrs[i,j] = cov.cdrs[j,i] = 0
          if (OR[i,j] != 'NaN'){
            p11 = update_p11(P[state,i],P[state,j],OR[i,j],P10star[i,j],P01star[i,j])
            cov.cdrs[i,j] = cov.cdrs[j,i] = p11 - P[state,i] * P[state,j]
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
            p11 = update_p11(P[state,i],P[state,j],OR[i,j],P10star[i,j],P01star[i,j])
            cov.cdrs[i,j] = cov.cdrs[j,i] = p11 - P[state,i] * P[state,j]
          }
        }
      }
      # 2. between binomial and each multinomial category:
      for (i in multinom.list[[k]]){ # multinomial covariates
        for (j in multinom.list[[k]]){ # binomial covariates
          if(i != j){
            if (OR[i,j] == 'NaN') cov.cdrs[i,j] = cov.cdrs[j,i] = 0
            if (OR[i,j] != 'NaN'){
              p11 = update_p11(P[state,i],P[state,j],OR[i,j],P10star[i,j],P01star[i,j])
              cov.cdrs[i,j] = cov.cdrs[j,i] = p11 - P[state,i] * P[state,j]
            }
          }
        }
      }
    }
    # calculate the mean and variance of RS for the state:
    rs.var.state = Beta %*% cov.cdrs %*% Beta
    return(c(mu.rs = rs.mu.state, var.rs = rs.var.state))
  }
  dist.rs.state1 = t(sapply(1:nrow(dat.state),function(x){cdrs_state_dist(P,OR,x,Beta,rs.state[x])}))
  
  
  # -----------------------------------------------------------------------------------------
  # ------------------------------------ 45-64 age group ------------------------------------
  # -----------------------------------------------------------------------------------------
  # ------------ Create the covariates matrix. Don't need to exclude missing rows!
  dat = nhis
  dat = dat[(dat$age>=45)&(dat$age<65),]
  dat = dat[dat$sampling_weights!=0,]
  covariates = c('Age_15_44','Age_45_54','Age_65_74','Age_75_84','Age_85',
                 'male',paste0('obesity',1:3),'smoking_ex','smoking_current',
                 'hispanic','black','asian','native', paste0('sdi',2:5),
                 'hbp','copd', 'asthma', 'chd', 'diabetes',
                 paste0('non_hematologic_cancer',1:3),paste0('hematologic_cancer',1:3),'stroke',
                 'kidney_disease','rheumatoid')
  
  # ------- step 1: calculate P (prevalence) i.e. state-level prevalence from brfss data, which is the mean of CDRS for each state
  dat.state = age_stratified
  covariates.state = paste0(c('Age_15_44_Prev', 'Age_45_54_Prev', 'Age_65_74_Prev', 'Age_75_84_Prev', 'Age_85_Prev',
                              'Male_Prev', paste0('obesity',1:3), 'smoking_ex_proportion', 'smoking_current_proportion',
                              'Hispanic_Prev', 'Black_Prev','Asian_Prev','Native_Prev',
                              paste0('SDI_',2:5),
                              'BPHIGH_CrudePrev', 'COPD_CrudePrev', 'CASTHMA_CrudePrev', 'CHD_CrudePrev',
                              'DIABETES_CrudePrev', 
                              paste0('non_hemo_cat',1:3), paste0('hemo_cat',1:3),
                              'STROKE_CrudePrev', 'KIDNEY_CrudePrev', 'ARTHRITIS_CrudePrev'), '.Age_45_64')
  dat.state = dat.state[,covariates.state]
  # -------- matrix of marginal prevalence
  P = dat.state # row: state; column: covariates
  # ------- step 2: calculate OR (odds ratio matrix) excluding SDI using NHIS individual-level data
  M = length(covariates.state)
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
        # here p does not mean probability...
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
  
  # ------- step 3: extract mean RS for each state for <45 age group
  rs.state = risk_scores$rs2
  # ------- step 4: calculate covariance matrix of CDRS for each state
  multinom.list = list(c(1:5),7:9,10:11,12:15,16:19,25:27,28:30)
  multinom.index = unlist(multinom.list)
  binomial.index = c(1:M)[-multinom.index]
  cdrs_state_dist = function(P,OR,state,Beta,rs.mu.state){
    cov.cdrs = matrix(NA,M,M)
    # variance
    for (i in 1:M){
      cov.cdrs[i,i] = P[state,i]*(1-P[state,i])
    }
    # covariance
    # 1. between all binomial covariates:
    for (i in binomial.index){
      for (j in binomial.index){
        if (i!=j){
          if (OR[i,j] == 'NaN') cov.cdrs[i,j] = cov.cdrs[j,i] = 0
          if (OR[i,j] != 'NaN'){
            p11 = update_p11(P[state,i],P[state,j],OR[i,j],P10star[i,j],P01star[i,j])
            cov.cdrs[i,j] = cov.cdrs[j,i] = p11 - P[state,i] * P[state,j]
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
            p11 = update_p11(P[state,i],P[state,j],OR[i,j],P10star[i,j],P01star[i,j])
            cov.cdrs[i,j] = cov.cdrs[j,i] = p11 - P[state,i] * P[state,j]
          }
        }
      }
      # 2. between binomial and each multinomial category:
      for (i in multinom.list[[k]]){ # multinomial covariates
        for (j in multinom.list[[k]]){ # binomial covariates
          if(i != j){
            if (OR[i,j] == 'NaN') cov.cdrs[i,j] = cov.cdrs[j,i] = 0
            if (OR[i,j] != 'NaN'){
              p11 = update_p11(P[state,i],P[state,j],OR[i,j],P10star[i,j],P01star[i,j])
              cov.cdrs[i,j] = cov.cdrs[j,i] = p11 - P[state,i] * P[state,j]
            }
          }
        }
      }
    }
    # calculate the mean and variance of RS for the state:
    rs.var.state = Beta %*% cov.cdrs %*% Beta
    return(c(mu.rs = rs.mu.state, var.rs = rs.var.state))
  }
  dist.rs.state2 = t(sapply(1:nrow(dat.state),function(x){cdrs_state_dist(P,OR,x,Beta,rs.state[x])}))
  
  
  # -----------------------------------------------------------------------------------------
  # ------------------------------------- 65+ age group -------------------------------------
  # -----------------------------------------------------------------------------------------
  # ------------ Create the covariates matrix. Don't need to exclude missing rows!
  dat = nhis
  dat = dat[(dat$age>=65),]
  dat = dat[dat$sampling_weights!=0,]
  covariates = c('Age_15_44','Age_45_54','Age_65_74','Age_75_84','Age_85',
                 'male',paste0('obesity',1:3),'smoking_ex','smoking_current',
                 'hispanic','black','asian','native', paste0('sdi',2:5),
                 'hbp','copd', 'asthma', 'chd', 'diabetes',
                 paste0('non_hematologic_cancer',1:3),paste0('hematologic_cancer',1:3),'stroke',
                 'kidney_disease','rheumatoid')
  
  # ------- step 1: calculate P (prevalence) i.e. state-level prevalence from brfss data, which is the mean of CDRS for each state
  dat.state = age_stratified
  covariates.state = paste0(c('Age_15_44_Prev', 'Age_45_54_Prev', 'Age_65_74_Prev', 'Age_75_84_Prev', 'Age_85_Prev',
                              'Male_Prev', paste0('obesity',1:3), 'smoking_ex_proportion', 'smoking_current_proportion',
                              'Hispanic_Prev', 'Black_Prev','Asian_Prev','Native_Prev',
                              paste0('SDI_',2:5),
                              'BPHIGH_CrudePrev', 'COPD_CrudePrev', 'CASTHMA_CrudePrev', 'CHD_CrudePrev',
                              'DIABETES_CrudePrev', 
                              paste0('non_hemo_cat',1:3), paste0('hemo_cat',1:3),
                              'STROKE_CrudePrev', 'KIDNEY_CrudePrev', 'ARTHRITIS_CrudePrev'), '.Age_65above')
  dat.state = dat.state[,covariates.state]
  # -------- matrix of marginal prevalence
  P = dat.state # row: state; column: covariates
  # ------- step 2: calculate OR (odds ratio matrix) excluding SDI using NHIS individual-level data
  M = length(covariates.state)
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
        # here p does not mean probability...
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
  
  # ------- step 3: extract mean RS for each state for <45 age group
  rs.state = risk_scores$rs3
  # ------- step 4: calculate covariance matrix of CDRS for each state
  multinom.list = list(c(1:5),7:9,10:11,12:15,16:19,25:27,28:30)
  multinom.index = unlist(multinom.list)
  binomial.index = c(1:M)[-multinom.index]
  cdrs_state_dist = function(P,OR,state,Beta,rs.mu.state){
    cov.cdrs = matrix(NA,M,M)
    # variance
    for (i in 1:M){
      cov.cdrs[i,i] = P[state,i]*(1-P[state,i])
    }
    # covariance
    # 1. between all binomial covariates:
    for (i in binomial.index){
      for (j in binomial.index){
        if (i!=j){
          if (OR[i,j] == 'NaN') cov.cdrs[i,j] = cov.cdrs[j,i] = 0
          if (OR[i,j] != 'NaN'){
            p11 = update_p11(P[state,i],P[state,j],OR[i,j],P10star[i,j],P01star[i,j])
            cov.cdrs[i,j] = cov.cdrs[j,i] = p11 - P[state,i] * P[state,j]
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
            p11 = update_p11(P[state,i],P[state,j],OR[i,j],P10star[i,j],P01star[i,j])
            cov.cdrs[i,j] = cov.cdrs[j,i] = p11 - P[state,i] * P[state,j]
          }
        }
      }
      # 2. between binomial and each multinomial category:
      for (i in multinom.list[[k]]){ # multinomial covariates
        for (j in multinom.list[[k]]){ # binomial covariates
          if(i != j){
            if (OR[i,j] == 'NaN') cov.cdrs[i,j] = cov.cdrs[j,i] = 0
            if (OR[i,j] != 'NaN'){
              p11 = update_p11(P[state,i],P[state,j],OR[i,j],P10star[i,j],P01star[i,j])
              cov.cdrs[i,j] = cov.cdrs[j,i] = p11 - P[state,i] * P[state,j]
            }
          }
        }
      }
    }
    # calculate the mean and variance of RS for the state:
    rs.var.state = Beta %*% cov.cdrs %*% Beta
    return(c(mu.rs = rs.mu.state, var.rs = rs.var.state))
  }
  dist.rs.state3 = t(sapply(1:nrow(dat.state),function(x){cdrs_state_dist(P,OR,x,Beta,rs.state[x])}))
  
  
  
  # --------- Calculate mean risk of each state ---------
  # proportion of 3 mixture components within each state
  state_age_pr = readRDS('data_created/State data/Population_3_Age_Group_Prevalence_0715.rds') # unchanged
  tem = as.data.frame(readRDS('data_created/State data/US_51_State_45_64_Risk_Score_0930.rds'))
  state_age_pr = state_age_pr %>% inner_join(tem,by = 'StateID')
  state_age_pr = state_age_pr[order(state_age_pr$StateID,decreasing = F),]
  mean.state.risks = state_age_pr$Age_15_44_Prev * exp(dist.rs.state1[,'mu.rs']+0.5*dist.rs.state1[,'var.rs']) + 
    state_age_pr$Age_45_64_prev * exp(dist.rs.state2[,'mu.rs']+0.5*dist.rs.state2[,'var.rs']) + 
    state_age_pr$Age_65_and_above_prev * exp(dist.rs.state3[,'mu.rs']+0.5*dist.rs.state3[,'var.rs'])
  Rl = sum(mean.state.risks * state_age_pr$population)/sum(state_age_pr$population)

  colnames(dist.rs.state1) = c("mu1", "var1")
  colnames(dist.rs.state2) = c("mu2", "var2")
  colnames(dist.rs.state3) = c("mu3", "var3")
  
  # Index of Excess Risk
  mean.relative.risk = mean.state.risks
  mean.relative.risk = mean.relative.risk/Rl
  mean.relative.risk = data.frame(mean.relativerisk = mean.relative.risk,
                                  StateID = state_age_pr[,'StateID'],
                                  population = state_age_pr[,'population'])
  LocationInfo = readRDS("data_created/State data/StateIDtoNames.rds")
  mean.relative.risk = merge(LocationInfo, mean.relative.risk, by = "StateID")
  IER[[b]] = mean.relative.risk
  
  
  
  # ------ Estimate size of vulnerable population within each state
  scenario = c(sample = c('all','deaths'))
  for (s in 1:length(scenario)){
    sample = scenario[s]
    # 
    k = 1.2
    highrisk.state_k1.2 = t(sapply(1:nrow(state_age_pr),function(x){highrisk.size(k, mul1 = dist.rs.state1[x,'mu1'], 
                                                                                  sigma2l1 = dist.rs.state1[x,'var1'], 
                                                                                  mul2 = dist.rs.state2[x,'mu2'], 
                                                                                  sigma2l2 = dist.rs.state2[x,'var2'], 
                                                                                  mul3 = dist.rs.state3[x,'mu3'], 
                                                                                  sigma2l3 = dist.rs.state3[x,'var3'],
                                                                                  population = state_age_pr$population[x],
                                                                                  sample,
                                                                                  data = state_age_pr,state=x)}))
    highrisk.state_k1.2[,2] = round(highrisk.state_k1.2[,2],0)
    # 
    k = 2
    highrisk.state_k2 = t(sapply(1:nrow(state_age_pr),function(x){highrisk.size(k, mul1 = dist.rs.state1[x,'mu1'], 
                                                                                sigma2l1 = dist.rs.state1[x,'var1'], 
                                                                                mul2 = dist.rs.state2[x,'mu2'], 
                                                                                sigma2l2 = dist.rs.state2[x,'var2'], 
                                                                                mul3 = dist.rs.state3[x,'mu3'], 
                                                                                sigma2l3 = dist.rs.state3[x,'var3'],
                                                                                population = state_age_pr$population[x],
                                                                                sample,
                                                                                data = state_age_pr,state=x)}))
    highrisk.state_k2[,2] = round(highrisk.state_k2[,2],0)
    # 
    k = 5
    highrisk.state_k5 = t(sapply(1:nrow(state_age_pr),function(x){highrisk.size(k, mul1 = dist.rs.state1[x,'mu1'], 
                                                                                sigma2l1 = dist.rs.state1[x,'var1'], 
                                                                                mul2 = dist.rs.state2[x,'mu2'], 
                                                                                sigma2l2 = dist.rs.state2[x,'var2'], 
                                                                                mul3 = dist.rs.state3[x,'mu3'], 
                                                                                sigma2l3 = dist.rs.state3[x,'var3'],
                                                                                population = state_age_pr$population[x],
                                                                                sample,
                                                                                data = state_age_pr,state=x)}))
    highrisk.state_k5[,2] = round(highrisk.state_k5[,2],0)
    # 
    k = 10
    highrisk.state_k10 = t(sapply(1:nrow(state_age_pr),function(x){highrisk.size(k, mul1 = dist.rs.state1[x,'mu1'], 
                                                                                 sigma2l1 = dist.rs.state1[x,'var1'], 
                                                                                 mul2 = dist.rs.state2[x,'mu2'], 
                                                                                 sigma2l2 = dist.rs.state2[x,'var2'], 
                                                                                 mul3 = dist.rs.state3[x,'mu3'], 
                                                                                 sigma2l3 = dist.rs.state3[x,'var3'],
                                                                                 population = state_age_pr$population[x],
                                                                                 sample,
                                                                                 data = state_age_pr,state=x)}))
    highrisk.state_k10[,2] = round(highrisk.state_k10[,2],0)
    highrisk.state = cbind(highrisk.state_k1.2,highrisk.state_k2,highrisk.state_k5,highrisk.state_k10)
    colnames(highrisk.state) = unlist(lapply(c(1.2,2,5,10),function(x){paste0(c('Proportion.k=','N.k='),x)}))
    if (sample == 'deaths'){
      highrisk.state = highrisk.state[,-c(2,4,6,8)]
    }
    highrisk.state = as.data.frame(highrisk.state)
    highrisk.state = cbind(state_age_pr$StateID, state_age_pr$population, highrisk.state)
    colnames(highrisk.state)[1:2] = c('StateID','population')
    # -------- output
    LocationInfo = readRDS("data_created/State data/StateIDtoNames.rds")
    highrisk.state = merge(LocationInfo, highrisk.state, by = "StateID")
    if (sample == 'all') highrisk_all[[b]] = highrisk.state
    if (sample == 'deaths') highrisk_deaths[[b]] = highrisk.state
    print(s)
  }
  
  dist1[[b]] = dist.rs.state1; dist2[[b]] = dist.rs.state2; dist3[[b]] = dist.rs.state3
  Rmean[[b]] = Rl
  print(paste0('Complete Bootstrap ', b))

dist1.b = dist1#[[b]]
dist2.b = dist2#[[b]]
dist3.b = dist3#[[b]]
IER.b = IER#[[b]] 
highrisk_all.b = highrisk_all#[[b]]
highrisk_deaths.b = highrisk_deaths#[[b]]
Rmean.b = Rmean#[[b]]

save(dist1.b,dist2.b,dist3.b,IER.b,highrisk_all.b,highrisk_deaths.b,Rmean.b,file=paste0('data_created/Bootstrap_results_state/b',b,'.RData'))

