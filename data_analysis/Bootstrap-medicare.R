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

highrisk.size = function(k, mul1, sigma2l1, population, sample, city){
  if (sample == 'all'){
    Pr.k = pnorm(log(k)+log(Rl),mul1,sqrt(sigma2l1),lower.tail = F)
  }
  if (sample == 'deaths'){
    Pr.k = pnorm(log(k)+log(Rl),mul1+sigma2l1,sqrt(sigma2l1),lower.tail = F)
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

load(paste0('data_created/Bootstrap_samples/medicare/bdata/',b,'.RData'))
load(paste0('data_created/Bootstrap_results_all/b',b,'.RData')) # Rmean.b
print(paste0('Complete loading data'))

dist.medicare = IER = highrisk_all = highrisk_deaths = Rmean = list()

names(Beta) = colnames(beta)
m1 = Beta$Diabetes_ctrl
m2 = Beta$Diabetes_unctrl
p1 = 6.0; p2 = 2.8
tem = which(colnames(Beta) %in% c('Diabetes_ctrl','Diabetes_unctrl') )
Beta = as.numeric(c(Beta[1:(tem[1]-1)],Beta[(tem[2]+1):length(Beta)]))
Beta = c(Beta[1:(tem[1]-1)], (m1 * p1 + m2 * p2)/(p1+p2), Beta[(tem[1]):length(Beta)])
names(Beta) = c(colnames(beta)[1:(tem[1]-1)],'Diabetes',colnames(beta)[(tem[2]+1):ncol(beta)])

# --------------------------- load the medicare data ---------------------------
dat = readRDS('data_created/Medicare/medicare_data_combined_age_greater_65_sex_race.rds')
dat$population = round(dat$population * (dat$`Age.65-74`+dat$`Age.75-84`+dat$`Age.85+`),0)
cancer = readRDS('data_created/Medicare/cancer_age_groups.rds')  
cancer = cancer[,-c(which(colnames(cancer) %in% c('county','state')))]
dat = merge(dat, cancer, by='fips')

medicare.info = dat

dat$agesum = dat$`Age.65-74` + dat$`Age.75-84`+dat$`Age.85+`
dat$`Age.15-44` = 0
dat$`Age.45-54` = 0
dat$`Age.65-74` = dat$`Age.65-74`/dat$agesum
dat$`Age.75-84` = dat$`Age.75-84`/dat$agesum
dat$`Age.85+` = dat$`Age.85+`/dat$agesum

# load NHIS 
nhis = NHIS[(NHIS$age>=65)&(NHIS$age<75),]
pr.hema = (sum(nhis$hematologic_cancer1*nhis$sampling_weights) + sum(nhis$hematologic_cancer2*nhis$sampling_weights) + sum(nhis$hematologic_cancer3*nhis$sampling_weights))/sum(nhis$sampling_weights)
pr.hema1.age1 = sum(nhis$hematologic_cancer1*nhis$sampling_weights)/(pr.hema*sum(nhis$sampling_weights))
pr.hema2.age1 = sum(nhis$hematologic_cancer2*nhis$sampling_weights)/(pr.hema*sum(nhis$sampling_weights))
pr.hema3.age1 = sum(nhis$hematologic_cancer3*nhis$sampling_weights)/(pr.hema*sum(nhis$sampling_weights))

pr.nonhema = (sum(nhis$non_hematologic_cancer1*nhis$sampling_weights) + sum(nhis$non_hematologic_cancer2*nhis$sampling_weights) + sum(nhis$non_hematologic_cancer3*nhis$sampling_weights))/sum(nhis$sampling_weights)
pr.nonhema1.age1 = mean(nhis$non_hematologic_cancer1)/pr.nonhema
pr.nonhema2.age1 = mean(nhis$non_hematologic_cancer2)/pr.nonhema
pr.nonhema3.age1 = mean(nhis$non_hematologic_cancer3)/pr.nonhema

nhis = NHIS[(NHIS$age>=75)&(NHIS$age<85),]
pr.hema = (sum(nhis$hematologic_cancer1*nhis$sampling_weights) + sum(nhis$hematologic_cancer2*nhis$sampling_weights) + sum(nhis$hematologic_cancer3*nhis$sampling_weights))/sum(nhis$sampling_weights)
pr.hema1.age2 = sum(nhis$hematologic_cancer1*nhis$sampling_weights)/(pr.hema*sum(nhis$sampling_weights))
pr.hema2.age2 = sum(nhis$hematologic_cancer2*nhis$sampling_weights)/(pr.hema*sum(nhis$sampling_weights))
pr.hema3.age2 = sum(nhis$hematologic_cancer3*nhis$sampling_weights)/(pr.hema*sum(nhis$sampling_weights))

pr.nonhema = (sum(nhis$non_hematologic_cancer1*nhis$sampling_weights) + sum(nhis$non_hematologic_cancer2*nhis$sampling_weights) + sum(nhis$non_hematologic_cancer3*nhis$sampling_weights))/sum(nhis$sampling_weights)
pr.nonhema1.age2 = mean(nhis$non_hematologic_cancer1)/pr.nonhema
pr.nonhema2.age2 = mean(nhis$non_hematologic_cancer2)/pr.nonhema
pr.nonhema3.age2 = mean(nhis$non_hematologic_cancer3)/pr.nonhema

nhis = NHIS[(NHIS$age>=85),]
pr.hema = (sum(nhis$hematologic_cancer1*nhis$sampling_weights) + sum(nhis$hematologic_cancer2*nhis$sampling_weights) + sum(nhis$hematologic_cancer3*nhis$sampling_weights))/sum(nhis$sampling_weights)
pr.hema1.age3 = sum(nhis$hematologic_cancer1*nhis$sampling_weights)/(pr.hema*sum(nhis$sampling_weights))
pr.hema2.age3 = sum(nhis$hematologic_cancer2*nhis$sampling_weights)/(pr.hema*sum(nhis$sampling_weights))
pr.hema3.age3 = sum(nhis$hematologic_cancer3*nhis$sampling_weights)/(pr.hema*sum(nhis$sampling_weights))

pr.nonhema = (sum(nhis$non_hematologic_cancer1*nhis$sampling_weights) + sum(nhis$non_hematologic_cancer2*nhis$sampling_weights) + sum(nhis$non_hematologic_cancer3*nhis$sampling_weights))/sum(nhis$sampling_weights)
pr.nonhema1.age3 = mean(nhis$non_hematologic_cancer1)/pr.nonhema
pr.nonhema2.age3 = mean(nhis$non_hematologic_cancer2)/pr.nonhema
pr.nonhema3.age3 = mean(nhis$non_hematologic_cancer3)/pr.nonhema

missing.ind = which(is.na(dat$medicare.hemo.cat1))
dat$medicare.hema.cat1 = dat$medicare.hemo.cat1
dat$medicare.hema.cat1[missing.ind] = dat$`Age.65-74`[missing.ind] * dat$`65-74_hemato.cancer`[missing.ind] * pr.hema1.age1 + 
  dat$`Age.75-84`[missing.ind] * dat$`75-84_hemato.cancer`[missing.ind] * pr.hema1.age2 + 
  dat$`Age.85+`[missing.ind] * dat$`85+_hemato.cancer`[missing.ind] * pr.hema1.age3

missing.ind = which(is.na(dat$medicare.hemo.cat2))
dat$medicare.hema.cat2 = dat$medicare.hemo.cat2
dat$medicare.hema.cat2[missing.ind] = dat$`Age.65-74`[missing.ind] * dat$`65-74_hemato.cancer`[missing.ind] * pr.hema2.age1 + 
  dat$`Age.75-84`[missing.ind] * dat$`75-84_hemato.cancer`[missing.ind] * pr.hema2.age2 + 
  dat$`Age.85+`[missing.ind] * dat$`85+_hemato.cancer`[missing.ind] * pr.hema2.age3

missing.ind = which(is.na(dat$medicare.hemo.cat3))
dat$medicare.hema.cat3 = dat$medicare.hemo.cat3
dat$medicare.hema.cat3[missing.ind] = dat$`Age.65-74`[missing.ind] * dat$`65-74_hemato.cancer`[missing.ind] * pr.hema3.age1 + 
  dat$`Age.75-84`[missing.ind] * dat$`75-84_hemato.cancer`[missing.ind] * pr.hema3.age2 + 
  dat$`Age.85+`[missing.ind] * dat$`85+_hemato.cancer`[missing.ind] * pr.hema3.age3



missing.ind = which(is.na(dat$medicare.nonhemo.cat1))
dat$medicare.nonhema.cat1 = dat$medicare.nonhemo.cat1
dat$medicare.nonhema.cat1[missing.ind] = dat$`Age.65-74`[missing.ind] * dat$`65-74_nonhemato.cancer`[missing.ind] * pr.nonhema1.age1 + 
  dat$`Age.75-84`[missing.ind] * dat$`75-84_nonhemato.cancer`[missing.ind] * pr.nonhema1.age2 + 
  dat$`Age.85+`[missing.ind] * dat$`85+_nonhemato.cancer`[missing.ind] * pr.nonhema1.age3

missing.ind = which(is.na(dat$medicare.nonhemo.cat2))
dat$medicare.nonhema.cat2 = dat$medicare.nonhemo.cat2
dat$medicare.nonhema.cat2[missing.ind] = dat$`Age.65-74`[missing.ind] * dat$`65-74_nonhemato.cancer`[missing.ind] * pr.nonhema2.age1 + 
  dat$`Age.75-84`[missing.ind] * dat$`75-84_nonhemato.cancer`[missing.ind] * pr.nonhema2.age2 + 
  dat$`Age.85+`[missing.ind] * dat$`85+_nonhemato.cancer`[missing.ind] * pr.nonhema2.age3

missing.ind = which(is.na(dat$medicare.nonhemo.cat3))
dat$medicare.nonhema.cat3 = dat$medicare.nonhemo.cat3
dat$medicare.nonhema.cat3[missing.ind] = dat$`Age.65-74`[missing.ind] * dat$`65-74_nonhemato.cancer`[missing.ind] * pr.nonhema3.age1 + 
  dat$`Age.75-84`[missing.ind] * dat$`75-84_nonhemato.cancer`[missing.ind] * pr.nonhema3.age2 + 
  dat$`Age.85+`[missing.ind] * dat$`85+_nonhemato.cancer`[missing.ind] * pr.nonhema3.age3

SDI.quintiles = quantile(dat$sdi_score,seq(0,1,by=0.2))[-1]
SDI.quintile = 1 * (dat$sdi_score<SDI.quintiles[1]) + 2 * ((dat$sdi_score>=SDI.quintiles[1])&(dat$sdi_score<SDI.quintiles[2])) + 3 * ((dat$sdi_score>=SDI.quintiles[2])&(dat$sdi_score<SDI.quintiles[3])) + 4 * ((dat$sdi_score>=SDI.quintiles[3])&(dat$sdi_score<SDI.quintiles[4])) + 5 * ((dat$sdi_score>=SDI.quintiles[4])&(dat$sdi_score<=SDI.quintiles[5]))
dat$sdi_quintile = SDI.quintile
dat$sdi1 = ifelse(dat$sdi_quintile==1,1,0)
dat$sdi2 = ifelse(dat$sdi_quintile==2,1,0)
dat$sdi3 = ifelse(dat$sdi_quintile==3,1,0)
dat$sdi4 = ifelse(dat$sdi_quintile==4,1,0)
dat$sdi5 = ifelse(dat$sdi_quintile==5,1,0)

 

covariates = c('Age.15-44','Age.45-54','Age.65-74','Age.75-84','Age.85+',
               'male',paste0('obesity_',c('I','II','III')),'smoking_former','smoking_current',
               'hispanic', 'non_hispanic_black', 'non_hispanic_asian', 'non_hispanic_ai_an',
               paste0('sdi',2:5),'hypertension','copd','asthma', 'chd',
               'diabetes_controlled', 'diabetes_uncontrolled',
               paste0('medicare.nonhema.cat',1:3),paste0('medicare.hema.cat',1:3),'stroke',
               'kidney','rheumatoid') # 34

Covariate_matrix = as.matrix(dat[,covariates])
rs_est = Covariate_matrix %*% t(beta[b,])

risk_score = cbind(dat[,c('state','county','fips','population')],rs_est)
full_output = cbind(dat[,c('state','county','fips','population')],rs_est,dat[,5:ncol(dat)])
colnames(full_output)[5] = colnames(risk_score)[5] = 'rs_est'


# --------------------------------------------------------------------------------------------------
# -------------------------- Calculate variance of risk score for each county ----------------------
# --------------------------------------------------------------------------------------------------
dat.medicare = dat
# -----------------------------------------------------------------------------------------
# --------------------------------- 65+ age group in NHIS ---------------------------------
# -----------------------------------------------------------------------------------------
# ----- Create the covariates matrix using NHIS individual-level data. Don't need to exclude missing rows for now
dat = NHIS
dat = dat[dat$age>=65,]
dat = dat[dat$sampling_weights!=0,]
dat = dat[(!is.na(dat$hematologic_cancer))|(!is.na(dat$non_hematologic_cancer)),] # exclude no rows
covariates = c('Age_15_44','Age_45_54','Age_65_74','Age_75_84','Age_85',
               'male',paste0('obesity',1:3),'smoking_ex','smoking_current',
               'hispanic','black','asian','native', paste0('sdi',2:5),
               'hbp','copd', 'asthma', 'chd', 'diabetes',
               paste0('non_hematologic_cancer',1:3),paste0('hematologic_cancer',1:3),'stroke',
               'kidney_disease','rheumatoid')

# ------- step 1: calculate P (prevalence) i.e. county-level prevalence
# dat.medicare is already defined at the beginning
dat.medicare$diabetes = dat.medicare$diabetes_controlled + dat.medicare$diabetes_uncontrolled
covariates.medicare = c('Age.15-44','Age.45-54','Age.65-74','Age.75-84','Age.85+',
                        'male',paste0('obesity_',c('I','II','III')),'smoking_former','smoking_current',
                        'hispanic', 'non_hispanic_black', 'non_hispanic_asian', 'non_hispanic_ai_an',
                        paste0('sdi',2:5),'hypertension','copd','asthma', 'chd',
                        'diabetes',
                        paste0('medicare.nonhema.cat',1:3),paste0('medicare.hema.cat',1:3),'stroke',
                        'kidney','rheumatoid')
dat.medicare = dat.medicare[,covariates.medicare]


# -------- matrix of marginal prevalence
P = dat.medicare # row: county; column: covariates
# ------- step 2: calculate OR (odds ratio matrix) using NHIS individual-level data
M = length(covariates.medicare)
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

## extract mean risk score
rs.medicare = full_output$rs_est

# ------- step 3: calculate covariance matrix of risk score within each county
multinom.list = list(c(1:5),7:9,10:11,12:15,16:19,25:27,28:30)
multinom.index = unlist(multinom.list)
binomial.index = c(1:M)[-multinom.index]

cdrs_county_dist = function(P,OR,county,Beta,rs.mu.county){
  cov.cdrs = matrix(NA,M,M)
  # variance
  for (i in 1:M){
    cov.cdrs[i,i] = P[county,i]*(1-P[county,i])
  }
  # covariance
  # 1. between all binomial covariates:
  for (i in binomial.index){
    for (j in binomial.index){
      if (i!=j){
        if (OR[i,j] == 'NaN') cov.cdrs[i,j] = cov.cdrs[j,i] = 0
        if (OR[i,j] != 'NaN'){
          p11 = update_p11(P[county,i],P[county,j],OR[i,j],P10star[i,j],P01star[i,j])
          cov.cdrs[i,j] = cov.cdrs[j,i] = p11 - P[county,i] * P[county,j]
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
          p11 = update_p11(P[county,i],P[county,j],OR[i,j],P10star[i,j],P01star[i,j])
          cov.cdrs[i,j] = cov.cdrs[j,i] = p11 - P[county,i] * P[county,j]
        }
      }
    }
    # 2. between binomial and each multinomial category:
    for (i in multinom.list[[k]]){ # multinomial covariates
      for (j in multinom.list[[k]]){ # binomial covariates
        if(i != j){
          if (OR[i,j] == 'NaN') cov.cdrs[i,j] = cov.cdrs[j,i] = 0
          if (OR[i,j] != 'NaN'){
            p11 = update_p11(P[county,i],P[county,j],OR[i,j],P10star[i,j],P01star[i,j])
            cov.cdrs[i,j] = cov.cdrs[j,i] = p11 - P[county,i] * P[county,j]
          }
        }
      }
    }
  }
  # calculate the mean and variance of risk score within each county
  rs.var.county = Beta %*% cov.cdrs %*% Beta
  return(c(mu.rs = rs.mu.county, var.rs = rs.var.county))
}

dist.rs.medicare = t(sapply(1:nrow(dat.medicare),function(x){cdrs_county_dist(P,OR,x,Beta,rs.medicare[x])}))


mean.risks = exp(dist.rs.medicare[,'mu.rs']+0.5*dist.rs.medicare[,'var.rs']) 
Rl = Rmean.b[[b]] #6.098574 # city-level mean from city-level analyses
mean.risks = mean.risks/Rl
mean.risks = data.frame(IER = mean.risks,
                        state = medicare.info$state,
                        county = medicare.info$county,
                        fips = medicare.info$fips,
                        population = medicare.info$population)
IER[[b]] = mean.risks


scenario = c(sample = c('all','deaths'))
for (s in 1:length(scenario)){
  sample = scenario[s]
  # 
  k = 1.2
  highrisk_k2 = t(sapply(1:nrow(medicare.info),function(x){highrisk.size(k, mul1 = dist.rs.medicare[x,'mu.rs'], 
                                                                         sigma2l1 = dist.rs.medicare[x,'var.rs'],
                                                                         population = medicare.info$population[x],
                                                                         sample, city=x)}))
  highrisk_k2[,2] = round(highrisk_k2[,2],0)
  # 
  k = 2
  highrisk_k3 = t(sapply(1:nrow(medicare.info),function(x){highrisk.size(k, mul1 = dist.rs.medicare[x,'mu.rs'], 
                                                                         sigma2l1 = dist.rs.medicare[x,'var.rs'],
                                                                         population = medicare.info$population[x],
                                                                         sample, city=x)}))
  highrisk_k3[,2] = round(highrisk_k3[,2],0)
  # 
  k = 5
  highrisk_k5 = t(sapply(1:nrow(medicare.info),function(x){highrisk.size(k, mul1 = dist.rs.medicare[x,'mu.rs'], 
                                                                         sigma2l1 = dist.rs.medicare[x,'var.rs'],
                                                                         population = medicare.info$population[x],
                                                                         sample, city=x)}))
  highrisk_k5[,2] = round(highrisk_k5[,2],0)
  # 
  k = 10
  highrisk_k10 = t(sapply(1:nrow(medicare.info),function(x){highrisk.size(k, mul1 = dist.rs.medicare[x,'mu.rs'], 
                                                                          sigma2l1 = dist.rs.medicare[x,'var.rs'],
                                                                          population = medicare.info$population[x],
                                                                          sample, city=x)}))
  highrisk_k10[,2] = round(highrisk_k10[,2],0)
  
  highrisk.medicare = cbind(highrisk_k2,highrisk_k3,highrisk_k5,highrisk_k10)
  colnames(highrisk.medicare) = unlist(lapply(c(1.2,2,5,10),function(x){paste0(c('Proportion.k=','N.k='),x)}))
  
  highrisk.medicare = cbind(medicare.info[,c('state', 'county', 'fips', 'population')], highrisk.medicare)
  colnames(highrisk.medicare)
  # -------- output
  if (sample == 'all') highrisk_all[[b]] = highrisk.medicare
  if (sample == 'deaths') highrisk_deaths[[b]] = highrisk.medicare
}

dist.medicare[[b]] = dist.rs.medicare
Rmean[[b]] = Rl # from city-level Bootstrap
print(paste0('Complete Bootstrap ', b))


dist.medicare.b = dist.medicare
IER.b = IER
highrisk_all.b = highrisk_all
highrisk_deaths.b = highrisk_deaths
Rmean.b = Rmean

save(dist.medicare.b,IER.b,highrisk_all.b,highrisk_deaths.b,Rmean.b,file=paste0('data_created/Bootstrap_results_medicare/b',b,'.RData'))


