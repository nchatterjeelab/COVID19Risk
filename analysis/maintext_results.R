# ---------------------------- high risk proportion & size among the whole population ----------------------------
res = read.xlsx('data_created/highrisk_all_mixtureN_overall-mean.xlsx')
sum(res$`N.k=2`) 
sum(res$`N.k=5`) 
sum(res$`N.k=10`) 

# 
quantile(res$`Proportion.k=5`,c(0.01,0.99))
quantile(res$`Proportion.k=10`,c(0.01,0.99))
quantile(res$`Proportion.k=25`,c(0.01,0.99))

# use View(res) to check the rankings etc
res1 = res[res$`Proportion.k=5`>0.05,]
min(res1$`Proportion.k=10`) 
min(res1$`Proportion.k=25`) 

summary(res[res$PlaceName %in% c('Los Angeles', 'Chicago', 'Philadelphia', 'Houston', 'San Antonio', 'Detroit'),'N.k=5'])
summary(res[res$PlaceName %in% c('Los Angeles', 'Chicago', 'Philadelphia', 'Houston', 'San Antonio', 'Detroit'),'N.k=10'])
summary(res[res$PlaceName %in% c('Los Angeles', 'Chicago', 'Philadelphia', 'Houston', 'San Antonio', 'Detroit'),'N.k=25'])

res2 = res[res$`Proportion.k=5`<0.02,]
res3 = res2[res2$population>4e5,]
summary(res3$`N.k=5`)
summary(res3$`N.k=10`)
summary(res3$`N.k=25`)



# ---------------------------- high-risk among cases ----------------------------
res.case = read.xlsx('data_created//highrisk_cases_mixtureN_overall-mean.xlsx')
min(res.case$`Proportion.k=5`); max(res.case$`Proportion.k=5`)
min(res.case$`Proportion.k=10`); max(res.case$`Proportion.k=10`)
min(res.case$`Proportion.k=25`); max(res.case$`Proportion.k=25`)


res4 = res[res$`Proportion.k=5`<0.05,]
res.case2 = res.case[((res.case$PlaceName %in% res4$PlaceName))&(res.case$PlaceFIPS %in% res4$PlaceFIPS),]
sum(res.case2$`Proportion.k=5`>0.25)


# ---------------------------- average risk score for each city ----------------------------
dat.city = readRDS('data_created/full_output_updated.rds')
### mean risk of each city:
dat.city$population = as.character(dat.city$population)
dat.city$population = as.numeric(dat.city$population)
mn.city = read.xlsx('~/Dropbox/NHANES_risk_score/results/highrisk_overall-mean/highrisk_all_mixtureN_overall-mean.xlsx')
dat.city$risk = dat.city$Age_18_39 * exp(mn.city$mu_under40 + 0.5*mn.city$var_under40) + (1-dat.city$Age_18_39) * exp(mn.city$mu_over40 + 0.5*mn.city$var_over40)
max(dat.city$risk)/min(dat.city$risk)
temp = dat.city[,c(1:5,ncol(dat.city))]
temp = cbind(temp,mn.city[,-c(1:4)])
temp = temp[order(-temp$risk),]
temp = cbind(1:nrow(temp),temp)
# View(temp)

dat.city[dat.city$PlaceName=='Detroit','risk'] # 0.9954828
dat.city[dat.city$PlaceName=='Ann Arbor','risk'] # -0.4596561
dat.city[dat.city$PlaceName=='Detroit','risk']/dat.city[dat.city$PlaceName=='Ann Arbor','risk']




# ---------------------------- NHIS ----------------------------
nhis = readRDS('data_created/individual_rs_covariates.rds')
exp(quantile(nhis[nhis$age<40,'rs_est'],0.99)-quantile(nhis[nhis$age<40,'rs_est'],0.01))
exp(quantile(nhis[nhis$age>=40,'rs_est'],0.99)-quantile(nhis[nhis$age>=40,'rs_est'],0.01))


# NYC death by May 2: 24172 (from the reference link in the main manuscript)
d = 24172
d * c(res.case[res.case$PlaceName=='New York','Proportion.k=5'], res.case[res.case$PlaceName=='New York','Proportion.k=10'], res.case[res.case$PlaceName=='New York','Proportion.k=25'])

n = 8550971 # population size in NYC (reference link - 2020 NYC open data)
1e5 * d/n # 282.6813
(res.case[res.case$PlaceName=='New York','Proportion.k=5'] * d)/(res[res$PlaceName=='New York','Proportion.k=5'] * n) * 1e5




