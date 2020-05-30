# calculate proportion of smoking subcategories
dat = readRDS('data_created/nhis_2017.rds')
dat = dat[dat$smoking_status!='NA',]
dat$smoking_ex = ifelse(dat$smoking_status=='Former',1,0)
dat$smoking_current = ifelse(dat$smoking_status=='Current',1,0)

Pr.smoking_current = sum(dat[dat$smoking_current==1,'sampling_weights'])/sum(dat$sampling_weights)
Pr.smoking_ex = sum(dat[dat$smoking_ex==1,'sampling_weights'])/sum(dat$sampling_weights)
Pr.smoking_never = sum(dat[dat$smoking_status=='Never','sampling_weights'])/sum(dat$sampling_weights)
Pr.smoking_current
Pr.smoking_ex
Pr.smoking_never


# calculate proportion of Obesity subcategories
dat = readRDS('data_created/nhis_2017.rds')
# no missing data for bmi
dat$obesity1 = ifelse((dat$BMI>=30)&(dat$BMI<35),1,0)
dat$obesity2 = ifelse((dat$BMI>=35)&(dat$BMI<40),1,0)
dat$obesity3 = ifelse((dat$BMI>=40),1,0)

Pr.obesity0 = sum(dat[dat$BMI<30,'sampling_weights'])/sum(dat$sampling_weights)
Pr.obesity1 = sum(dat[dat$obesity1==1,'sampling_weights'])/sum(dat$sampling_weights)
Pr.obesity2 = sum(dat[dat$obesity2==1,'sampling_weights'])/sum(dat$sampling_weights)
Pr.obesity3 = sum(dat[dat$obesity3==1,'sampling_weights'])/sum(dat$sampling_weights)
Pr.obesity0 
Pr.obesity1 
Pr.obesity2 
Pr.obesity3 
