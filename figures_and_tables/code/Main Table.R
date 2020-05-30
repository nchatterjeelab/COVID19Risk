# Empirical mean in NHIS without SDI
rs.nhis = readRDS('data_created/individual_rs_covariates.rds')
Rl.nhis = sum(exp(rs.nhis$rs) * rs.nhis$sampling_weights)/sum(rs.nhis$sampling_weights)
Rl.nhis # 5.342619

k.candidate = c(2,3,5,10,25)
re = matrix(0,7,5)
colnames(re) = paste0(k.candidate,'-fold')
rownames(re) = c('Overall','18-39','40-49','50-59','60-69','70-79','80+')
for (i in 1:length(k.candidate)){
  k = k.candidate[i]
  re[,i] = c(sum(ifelse(exp(rs.nhis$rs_est)>k*Rl.nhis,1,0) * rs.nhis$sampling_weights)/sum(rs.nhis$sampling_weights),
             sum(ifelse(exp(rs.nhis[(rs.nhis$age<40),'rs_est'])>k*Rl.nhis,1,0) * rs.nhis[(rs.nhis$age<40),'sampling_weights'])/sum(rs.nhis[(rs.nhis$age<40),'sampling_weights']),
             sum(ifelse(exp(rs.nhis[((rs.nhis$age<50)&(rs.nhis$age>=40)),'rs_est'])>k*Rl.nhis,1,0) * rs.nhis[((rs.nhis$age<50)&(rs.nhis$age>=40)),'sampling_weights'])/sum(rs.nhis[((rs.nhis$age<50)&(rs.nhis$age>=40)),'sampling_weights']),
             sum(ifelse(exp(rs.nhis[((rs.nhis$age<60)&(rs.nhis$age>=50)),'rs_est'])>k*Rl.nhis,1,0) * rs.nhis[((rs.nhis$age<60)&(rs.nhis$age>=50)),'sampling_weights'])/sum(rs.nhis[((rs.nhis$age<60)&(rs.nhis$age>=50)),'sampling_weights']),
             sum(ifelse(exp(rs.nhis[((rs.nhis$age<70)&(rs.nhis$age>=60)),'rs_est'])>k*Rl.nhis,1,0) * rs.nhis[((rs.nhis$age<70)&(rs.nhis$age>=60)),'sampling_weights'])/sum(rs.nhis[((rs.nhis$age<70)&(rs.nhis$age>=60)),'sampling_weights']),
             sum(ifelse(exp(rs.nhis[((rs.nhis$age<80)&(rs.nhis$age>=70)),'rs_est'])>k*Rl.nhis,1,0) * rs.nhis[((rs.nhis$age<80)&(rs.nhis$age>=70)),'sampling_weights'])/sum(rs.nhis[((rs.nhis$age<80)&(rs.nhis$age>=70)),'sampling_weights']),
             sum(ifelse(exp(rs.nhis[(rs.nhis$age>=80),'rs_est'])>k*Rl.nhis,1,0) * rs.nhis[(rs.nhis$age>=80),'sampling_weights'])/sum(rs.nhis[(rs.nhis$age>=80),'sampling_weights'])
  )
}
# percentage:
re = round(re,5)*100
write.xlsx(re,'figures_and_tables/output/table1.xlsx',row.names=T)

