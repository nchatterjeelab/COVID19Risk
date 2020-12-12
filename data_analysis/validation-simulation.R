require(grid)
# simulate individual-level data
# Validate the mixture normal/normal approximation using NHIS data
library(dplyr)
library(openxlsx)
library(data.table)
library(ggplot2)
library(scales)
library(ggpubr)
library(ggsci)
library(ggExtra)
library("scales")
#install.packages('https://cran.r-project.org/src/contrib/Archive/SDMTools/SDMTools_1.1-221.2.tar.gz', repos = NULL, type="source")
library(SDMTools)
library(fmsb)
library(pROC)
ks <- function (x) { x = round(x,1) }
coeffs = read.xlsx('data_created/meta_model.xlsx', sheet = 'coefficients_individual_level')
rownames(coeffs) = coeffs$Variable
coef_name = coeffs$Variable
# --------------------------- load the city-level data ---------------------------
dat = readRDS('data_created/nhis_imputed.rds')
colnames(dat)
covariates = c('Age_15_44','Age_45_54','Age_65_74','Age_75_84','Age_85',
               'male',paste0('obesity',1:3),'smoking_ex','smoking_current',
               'hispanic','black','asian','native',
               paste0('IMD',2:5),'hbp','copd', 'asthma', 'chd', 'diabetes',
               paste0('non_hematologic_cancer',1:3),paste0('hematologic_cancer',1:3),'stroke',
               'kidney_disease','rheumatoid')
Covariate_matrix1 = as.matrix(dat[,covariates])
rs_est = Covariate_matrix1 %*% coeffs$estimate
risk_score = cbind(dat[,1:5],rs_est)
colnames(risk_score) = c(colnames(dat)[1:5],'rs')
dat = cbind(dat[,1:5],rs_est,dat[,covariates])
dat = dat[complete.cases(dat),]

ratio = 1/50
te = dat[1,]
dat.individual = te[rep(seq_len(nrow(te)), each = ceiling(dat$sampling_weights[1]*ratio)), ] #matrix(rep(dat[1,],ceiling(dat$sampling_weights[1]*ratio)),ncol=ncol(dat),byrow = T)#t(sapply(1:dat$sampling_weights[1]*ratio,function(x){dat[x,]}))
# 16908
for (i in 1:nrow(dat)){
  te = dat[i,]
  tem = te[rep(seq_len(nrow(te)), each = ceiling(dat$sampling_weights[i]*ratio)), ]
  #matrix(rep(dat[i,],ceiling(dat$sampling_weights[i]*ratio)),ncol=ncol(dat),byrow = T)
  dat.individual =  rbind(dat.individual, tem)
  if (i %% 200 == 0) print(i)
}
save(dat.individual, file = '~/Dropbox/Covid-19/Risk Prediction/code_revision/dat.individual.4m.RData') # 14148

# ----------------------------- simulate outcome
load('~/Dropbox/Covid-19/Risk Prediction/code_revision/dat.individual.4m.RData')

int = - 9.9
R = seq(0.4,1.5,by=0.05)
res = as.data.frame(matrix(0,length(R),2))
colnames(res) = c('R2','AUC')
rownames(res) = paste0('coefficient of IER: ',R)
for (r in c(1:length(R))[-13]){
  dat.individual$p = exp((dat.individual$rs_est * R[r] +int))
  R2.IER = AUC.IER = numeric()
  for (s in 1:5){
    dat.individual$y = sapply(1:nrow(dat.individual),function(x){rbinom(1,1,dat.individual$p[x])})
    sum(dat.individual$y)
    fit = glm(y~rs_est, data=dat.individual, family = binomial(link = "logit"))
    dat.individual$fitted.values = fit$fitted.values
    roc_obj <- roc(as.numeric(dat.individual$y),as.numeric(dat.individual$fitted.values))
    AUC.IER[s] = roc_obj$auc # sum(roc_obj$aucpiece)
  
    # now look at the group level validation:
    set.seed(2020)
    n.group = 10
    size.group = ceiling(nrow(dat.individual)/n.group)
    ind.group = sapply(1:n.group,function(x){((x-1)*size.group + 1):(min(x*size.group,nrow(dat.individual)))})
    dat.group = data.frame(IER = rep(0,n.group), deathrate = rep(0,n.group), population = rep(0,n.group), deaths = rep(0,n.group))
    for (i in 1:n.group){
      dat.group$IER[i] = log(mean(exp(dat.individual[ind.group[[i]],'rs_est'])))
      dat.group$population[i] = log(length(ind.group[[i]]))
      dat.group$deathrate[i] = log(sum(dat.individual$y[ind.group[[i]]])/length(ind.group[[i]]))
      dat.group$deaths[i] = sum(dat.individual$y[ind.group[[i]]])
    }
    dat.group$weights = 1/(dat.group$deathrate)
    dat.group$weights= dat.group$weights/sum(dat.group$weights)
    dat.group = dat.group[dat.group$deathrate>-Inf,]
    fit = lm(deathrate~IER, data=dat.group, weights = dat.group$weights)
    R2.IER[s] = summary(fit)$r.squared
  }
  res[r,'R2'] = mean(R2.IER)
  res[r,'AUC'] = mean(AUC.IER)
  print(paste0('Complete factor ', r,'/',length(R)))
  save(res,file='data_created/R2-AUC2.RData')
}
res

load('data_created/R2-AUC2.RData')
ggplot(res, aes(x = R2, y = AUC)) + 
  geom_point() + 
  xlab(expression('Community-level'~R^2)) + 
  ylab(paste0('Individual-level AUC')) +
  theme(plot.title = element_text(hjust = 0.5,size = 12), 
        axis.text.x = element_text(angle = 0, hjust = 0.5, size=11), 
        axis.text = element_text(size = 11), axis.title = element_text(size = 11),
        legend.position = "none",
        panel.background = element_rect(fill = "white", colour = "white",
                                        size = 0.2, linetype = "solid"),
        panel.grid = element_line(size = 0.2, linetype = 'dashed',
                                  colour = "#cccccc"),
        panel.border = element_blank(),
        axis.line = element_line(colour = "grey30",size=0.3)
  ) + 
  removeGrid(y = FALSE) + 
  geom_point(aes(x=0.154, y=0.895), colour="darkred") +
  geom_segment(aes(x = 0.154, y = 0.93, xend = 0.154, yend = 0.908),
               arrow = arrow(length = unit(0.2, "cm")), col = 'darkred', size=0.65) +
  scale_y_continuous(breaks = seq(0.7,0.95,by=0.05))#limits = c(0.68, 1))



