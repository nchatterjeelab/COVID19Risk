library(openxlsx)
library(SDMTools)
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


Covariate_matrix = as.matrix(dat[,covariates])
rs_est = Covariate_matrix %*% coeffs$estimate

risk_score = cbind(dat[,1:5],rs_est)
colnames(risk_score) = c(colnames(dat)[1:5],'rs')
rs.nhis = cbind(dat[,1:5],rs_est,dat[,6:ncol(dat)])
Rl.nhis = sum(exp(rs.nhis$rs) * rs.nhis$sampling_weights)/sum(rs.nhis$sampling_weights)


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

hr.calculation = function(sample, t, mul1,mul2,mul3, sigma2l1,sigma2l2,sigma2l3, w1,w2,w3){
  if (sample == 'all'){
    Pr.k = w1 * pnorm(t,mul1,sqrt(sigma2l1),lower.tail = F) + 
      w2 * pnorm(t,mul2,sqrt(sigma2l2),lower.tail = F) + 
      w3 * pnorm(t,mul3,sqrt(sigma2l3),lower.tail = F)
  }
  if (sample == 'cases'){
    # standardized, do not need to divide by Rl
    Pr.k.above = w1 * pnorm(t,mul1+sigma2l1,sqrt(sigma2l1),lower.tail = F) * exp(mul1+0.5*sigma2l1) + 
      w2 * pnorm(t,mul2+sigma2l2,sqrt(sigma2l2),lower.tail = F) * exp(mul2+0.5*sigma2l2) + 
      w3 * pnorm(t,mul3+sigma2l3,sqrt(sigma2l3),lower.tail = F) * exp(mul3+0.5*sigma2l3)
    Pr.k.below = w1 * pnorm(t,mul1+sigma2l1,sqrt(sigma2l1),lower.tail = T) * exp(mul1+0.5*sigma2l1) + 
      w2 * pnorm(t,mul2+sigma2l2,sqrt(sigma2l2),lower.tail = T) * exp(mul2+0.5*sigma2l2) + 
      w3 * pnorm(t,mul3+sigma2l3,sqrt(sigma2l3),lower.tail = T) * exp(mul3+0.5*sigma2l3)
    Pr.k = Pr.k.above/(Pr.k.above + Pr.k.below)
  }
  Pr.k
}

t.candidates = seq(min(rs$rs_est),max(rs$rs_est),by=0.01)
tpr=fpr=numeric()
for (i in 1:length(t.candidates)){
  t = t.candidates[i]
  tpr[i] = hr.calculation('cases',t,mu1,mu2,mu3, va1,va2,va3, w1,w2,w3)
  fpr[i] = hr.calculation('all',t,mu1,mu2,mu3, va1,va2,va3, w1,w2,w3)
}
#plot(fpr,tpr,pch=20,cex=0.5,main='ROC - general US population')
x.segments = sapply(1:(length(fpr)-1),function(x){fpr[length(fpr)-x]-fpr[length(fpr)-x+1]})
y.center.value = sapply(1:(length(tpr)-1),function(x){(tpr[length(tpr)-x]+tpr[length(tpr)-x+1])/2})
AUC = (y.center.value*x.segments)
AUC = sum(AUC[which(x.segments>0)])/(max(fpr)-min(fpr))