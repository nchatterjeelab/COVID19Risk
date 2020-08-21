library(SDMTools)
library(openxlsx)
# obtain age prevalence from CDC:
age = read.csv('data/CDC/population_category_wise.csv',header=T)
age = age[age$AGE>=18,]
sum(age[age$AGE>=65,'POPESTIMATE2019'])/sum(age$POPESTIMATE2019) 
# 0.2118267
sum(age[age$AGE>=75,'POPESTIMATE2019'])/sum(age$POPESTIMATE2019) 
# 0.08845924
sum(age[age$AGE>=85,'POPESTIMATE2019'])/sum(age$POPESTIMATE2019) 
# 0.02588146
a1 = sum(age[(age$AGE<45),'POPESTIMATE2019'])/sum(age$POPESTIMATE2019) 
a2 = sum(age[(age$AGE>=45)&(age$AGE<55),'POPESTIMATE2019'])/sum(age$POPESTIMATE2019) 
a3 = sum(age[(age$AGE>=55)&(age$AGE<65),'POPESTIMATE2019'])/sum(age$POPESTIMATE2019) 
a4 = sum(age[(age$AGE>=65)&(age$AGE<75),'POPESTIMATE2019'])/sum(age$POPESTIMATE2019) 
a5 = sum(age[(age$AGE>=75)&(age$AGE<85),'POPESTIMATE2019'])/sum(age$POPESTIMATE2019) 
a6 = sum(age[(age$AGE>=85),'POPESTIMATE2019'])/sum(age$POPESTIMATE2019) 


# Empirical mean in NHIS
# --------------------------- load the US moel ---------------------------
coeffs = read.xlsx('data_created/meta_model.xlsx', sheet = 'coefficients_individual_level')
rownames(coeffs) = coeffs$Variable
coef_name = coeffs$Variable
# --------------------------- load the city-level data ---------------------------
dat = readRDS('data_created/nhis_imputed.rds')
colnames(dat)
covariates = c('Age_15_44','Age_45_54','Age_65_74','Age_75_84','Age_85',
               'male',paste0('obesity',1:3),'smoking_ex','smoking_current',
               'hispanic','black','asian','native',
               paste0('sdi',2:5),'hbp','copd', 'asthma', 'chd', 'diabetes',
               paste0('non_hematologic_cancer',1:3),paste0('hematologic_cancer',1:3),'stroke',
               'kidney_disease','rheumatoid')
p.ages = c(sum(dat[dat$Age_15_44==1,'sampling_weights']),
           sum(dat[dat$Age_45_54==1,'sampling_weights']),
           sum(dat[dat$Age_55_64==1,'sampling_weights']),
           sum(dat[dat$Age_65_74==1,'sampling_weights']),
           sum(dat[dat$Age_75_84==1,'sampling_weights']),
           sum(dat[dat$Age_85==1,'sampling_weights']))/sum(dat$sampling_weights)
dat[dat$Age_15_44==1,'sampling_weights'] = dat[dat$Age_15_44==1,'sampling_weights']*a1/p.ages[1]
dat[dat$Age_45_54==1,'sampling_weights'] = dat[dat$Age_45_54==1,'sampling_weights']*a2/p.ages[2]
dat[dat$Age_55_64==1,'sampling_weights'] = dat[dat$Age_55_64==1,'sampling_weights']*a3/p.ages[3]
dat[dat$Age_65_74==1,'sampling_weights'] = dat[dat$Age_65_74==1,'sampling_weights']*a4/p.ages[4]
dat[dat$Age_75_84==1,'sampling_weights'] = dat[dat$Age_75_84==1,'sampling_weights']*a5/p.ages[5]
dat[dat$Age_85==1,'sampling_weights'] = dat[dat$Age_85==1,'sampling_weights']*a6/p.ages[6]

Covariate_matrix = as.matrix(dat[,covariates])
# risk score (log scale):
rs_est = Covariate_matrix %*% coeffs$estimate
risk_score = cbind(dat[,1:5],rs_est)
colnames(risk_score) = c(colnames(dat)[1:5],'rs')
rs.nhis = cbind(dat[,1:5],rs_est,dat[,6:ncol(dat)])
Rl.nhis = sum(exp(rs.nhis$rs) * rs.nhis$sampling_weights)/sum(rs.nhis$sampling_weights)
Rl.nhis # 7.185669

# https://www.census.gov/popclock/
N.US = 330052960 # 8.1. data
k.candidate = c(1.2,2,5,10)
re = matrix(0,3,4)
colnames(re) = paste0(k.candidate,'-fold')
rownames(re) = c('Overall','15-64','65+')
thr = 65
for (i in 1:length(k.candidate)){
  k = k.candidate[i]
  re[,i] = c(sum(ifelse(exp(rs.nhis$rs_est)>k*Rl.nhis,1,0) * rs.nhis$sampling_weights)/sum(rs.nhis$sampling_weights),
             sum(ifelse(exp(rs.nhis[(rs.nhis$age<thr),'rs_est'])>k*Rl.nhis,1,0) * rs.nhis[(rs.nhis$age<thr),'sampling_weights'])/sum(rs.nhis[(rs.nhis$age<thr),'sampling_weights']),
             sum(ifelse(exp(rs.nhis[(rs.nhis$age>=thr),'rs_est'])>k*Rl.nhis,1,0) * rs.nhis[(rs.nhis$age>=thr),'sampling_weights'])/sum(rs.nhis[(rs.nhis$age>=thr),'sampling_weights'])
  )
}
signif(re,3)
for (i in 1:length(k.candidate)){
  k = k.candidate[i]
  re[,i] = c(sum(ifelse(exp(rs.nhis$rs_est)>k*Rl.nhis,1,0) * rs.nhis$sampling_weights)/sum(rs.nhis$sampling_weights),
             sum(ifelse(exp(rs.nhis[(rs.nhis$age<thr),'rs_est'])>k*Rl.nhis,1,0) * rs.nhis[(rs.nhis$age<thr),'sampling_weights'])/sum(rs.nhis[,'sampling_weights']),
             sum(ifelse(exp(rs.nhis[(rs.nhis$age>=thr),'rs_est'])>k*Rl.nhis,1,0) * rs.nhis[(rs.nhis$age>=thr),'sampling_weights'])/sum(rs.nhis[,'sampling_weights'])
  )
}
round(re*N.US)


# ----------------------------- high risk among deaths -----------------------------
# ----------------------------- Overall -----------------------------
rs = rs.nhis
thr1 = 45; thr2 = 75
Rl = sum(exp(rs$rs_est) * rs$sampling_weights)/sum(rs$sampling_weights)
rs$rs_est = rs$rs_est - log(Rl)
rs1 = rs[rs$age<thr1,]
rs2 = rs[(rs$age>=thr1)&(rs$age<thr2),]
rs3 = rs[rs$age>=thr2,]

RS = lapply(1:nrow(rs),function(x){rep(rs$rs_est[x],rs$sampling_weights[x])})
RS = unlist(RS)
# mixture normal:
mu1 = wt.mean(rs1$rs_est, rs1$sampling_weights)
va1 = wt.var(rs1$rs_est, rs1$sampling_weights)
mu2 = wt.mean(rs2$rs_est, rs2$sampling_weights)
va2 = wt.var(rs2$rs_est, rs2$sampling_weights)
mu3 = wt.mean(rs3$rs_est, rs3$sampling_weights)
va3 = wt.var(rs3$rs_est, rs3$sampling_weights)
mu = wt.mean(rs$rs_est, rs$sampling_weights); va = wt.var(rs$rs_est, rs$sampling_weights)
w1 = sum(ifelse(rs$age<thr1,1,0) * rs$sampling_weights)/sum(rs$sampling_weights) # 
w2 = sum(ifelse((rs$age>=thr1)*(rs$age<thr2),1,0) * rs$sampling_weights)/sum(rs$sampling_weights) # 
w3 = sum(ifelse(rs$age>=thr2,1,0) * rs$sampling_weights)/sum(rs$sampling_weights) # 

res = matrix(NA,4,1)
colnames(res) = c('Proportion among deaths')
rownames(res) = paste0(c(1.2,2,5,10),'-fold')

hr.deaths = function(k, mul1,mul2,mul3, sigma2l1,sigma2l2,sigma2l3, w1,w2,w3){
  # standardized, do not need to divide by Rl
  Pr.k.above = w1 * pnorm(log(k),mul1+sigma2l1,sqrt(sigma2l1),lower.tail = F) * exp(mul1+0.5*sigma2l1) + 
    w2 * pnorm(log(k),mul2+sigma2l2,sqrt(sigma2l2),lower.tail = F) * exp(mul2+0.5*sigma2l2) + 
    w3 * pnorm(log(k),mul3+sigma2l3,sqrt(sigma2l3),lower.tail = F) * exp(mul3+0.5*sigma2l3)
  Pr.k.below = w1 * pnorm(log(k),mul1+sigma2l1,sqrt(sigma2l1),lower.tail = T) * exp(mul1+0.5*sigma2l1) + 
    w2 * pnorm(log(k),mul2+sigma2l2,sqrt(sigma2l2),lower.tail = T) * exp(mul2+0.5*sigma2l2) + 
    w3 * pnorm(log(k),mul3+sigma2l3,sqrt(sigma2l3),lower.tail = T) * exp(mul3+0.5*sigma2l3)
  Pr.k = Pr.k.above/(Pr.k.above + Pr.k.below)
  Pr.k
}
k=1.2
res[1,1] = hr.deaths(k,mu1,mu2,mu3, va1,va2,va3, w1,w2,w3)
k=2
res[2,1] = hr.deaths(k,mu1,mu2,mu3, va1,va2,va3, w1,w2,w3)
k=5
res[3,1] = hr.deaths(k,mu1,mu2,mu3, va1,va2,va3, w1,w2,w3)
k=10
res[4,1] = hr.deaths(k,mu1,mu2,mu3, va1,va2,va3, w1,w2,w3)
signif(res, 3)

# ------------------------- 15-64:
rs = rs.nhis[rs.nhis$age<65,]
Rl = sum(exp(rs.nhis$rs_est) * rs.nhis$sampling_weights)/sum(rs.nhis$sampling_weights)
thr1 = 45
rs$rs_est = rs$rs_est - log(Rl)
rs1 = rs[rs$age<thr1,]
rs2 = rs[rs$age>=thr1,]

RS = lapply(1:nrow(rs),function(x){rep(rs$rs_est[x],rs$sampling_weights[x])})
RS = unlist(RS)
# mixture normal:
mu1 = wt.mean(rs1$rs_est, rs1$sampling_weights)
va1 = wt.var(rs1$rs_est, rs1$sampling_weights)
mu2 = wt.mean(rs2$rs_est, rs2$sampling_weights)
va2 = wt.var(rs2$rs_est, rs2$sampling_weights)
mu = wt.mean(rs$rs_est, rs$sampling_weights); va = wt.var(rs$rs_est, rs$sampling_weights)
w1 = sum(ifelse(rs$age<thr1,1,0) * rs$sampling_weights)/sum(rs$sampling_weights) # 
w2 = sum(ifelse((rs$age>=thr1)*(rs$age<thr2),1,0) * rs$sampling_weights)/sum(rs$sampling_weights) # 

res = matrix(NA,4,1)
colnames(res) = c('Proportion among deaths')
rownames(res) = paste0(c(1.2,2,5,10),'-fold')
hr.deaths = function(k, mul1,mul2, sigma2l1,sigma2l2, w1,w2){
  # standardized, do not need to divide by Rl
  Pr.k.above = w1 * pnorm(log(k),mul1+sigma2l1,sqrt(sigma2l1),lower.tail = F) * exp(mul1+0.5*sigma2l1) + 
    w2 * pnorm(log(k),mul2+sigma2l2,sqrt(sigma2l2),lower.tail = F) * exp(mul2+0.5*sigma2l2)
  Pr.k.below = w1 * pnorm(log(k),mul1+sigma2l1,sqrt(sigma2l1),lower.tail = T) * exp(mul1+0.5*sigma2l1) + 
    w2 * pnorm(log(k),mul2+sigma2l2,sqrt(sigma2l2),lower.tail = T) * exp(mul2+0.5*sigma2l2)
  Pr.k = Pr.k.above/(Pr.k.above + Pr.k.below)
  Pr.k
}
k=1.2
res[1,1] = hr.deaths(k,mu1,mu2, va1,va2, w1,w2)
k=2
res[2,1] = hr.deaths(k,mu1,mu2, va1,va2, w1,w2)
k=5
res[3,1] = hr.deaths(k,mu1,mu2, va1,va2, w1,w2)
k=10
res[4,1] = hr.deaths(k,mu1,mu2, va1,va2, w1,w2)
signif(res, 3)


# ------------------------- 65+:
rs = rs.nhis[rs.nhis$age>=65,]
Rl = sum(exp(rs.nhis$rs_est) * rs.nhis$sampling_weights)/sum(rs.nhis$sampling_weights)
rs$rs_est = rs$rs_est - log(Rl)

RS = lapply(1:nrow(rs),function(x){rep(rs$rs_est[x],rs$sampling_weights[x])})
RS = unlist(RS)
mu = wt.mean(rs$rs_est, rs$sampling_weights); va = wt.var(rs$rs_est, rs$sampling_weights)

res = matrix(NA,4,1)
colnames(res) = c('Empirical','Proportion among deaths')
rownames(res) = paste0(c(1.2,2,5,10),'-fold')
k=1.2
res[1,1] = pnorm(log(k),mu+va,sqrt(va),lower.tail = F)
k=2
res[2,1] = pnorm(log(k),mu+va,sqrt(va),lower.tail = F)
k=5
res[3,1] = pnorm(log(k),mu+va,sqrt(va),lower.tail = F)
k=10
res[4,1] = pnorm(log(k),mu+va,sqrt(va),lower.tail = F)
signif(res, 3)
