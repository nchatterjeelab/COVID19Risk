library(openxlsx)
# --------------------------- load the UK moel ---------------------------
coeffs = read.xlsx('data_created/UK_model.xlsx', sheet = 'coefficients_individual_level')
rownames(coeffs) = coeffs$Variable_Name_UK
coef_name = coeffs$Variable_Name_UK
coef_name
#"Age_18_39"                 "Age_40_49"                 "Age_60_69"                 "Age_70_79"                
#[5] "Age_80_150"                "Sex_M"                     "BMI_obsese1"               "BMI_obsese2"              
#[9] "BMI_obsese3"               "Smoking_ex"                "Smoking_current"           "Hispanic"                 
#[13] "Black"                     "IMD2"                      "IMD3"                      "IMD4"                     
#[17] "IMD5"                      "BP_high"                   "Respiratory_disease"       "Asthma"                   
#[21] "CHD"                       "Diabetes"                  "Cancer_nonhaematological1" "Cancer_nonhaematological2"
#[25] "Cancer_nonhaematological3" "Cancer_haematological1"    "Cancer_haematological2"    "Cancer_haematological3"   
#[29] "Stroke"                    "Kidney"                    "Arthritis"


# --------------------------- load the city-level data ---------------------------
dat = readRDS('data_created/nhis_2017.rds')
colnames(dat)
#[1] "SRVY_YR"                "HHX"                    "FMX"                    "FPX"                    "sampling_weights"      
#[6] "sex"                    "age"                    "BMI"                    "agegroup"               "diabetes"              
#[11] "smoking_status"         "rhemumatoid"            "asthma"                 "stroke"                 "heart_disease"         
#[16] "hypertension"           "resp_ex_asthma"         "kidney_disease"         "liver_disease"          "race_ethnicity"        
#[21] "hematologic_cancer"     "non_hematologic_cancer" "diagnoses_cancer"       "Obesity" 
dat = dat[complete.cases(dat[,-which(colnames(dat) %in% c('diagnoses_cancer'))]),] 
sapply(1:ncol(dat),function(x){sum(is.na(dat[,x]))})
dat$Age_18_39 = ifelse(dat$agegroup=='18_40',1,0)
dat$Age_40_49 = ifelse(dat$agegroup=='40_50',1,0)
dat$Age_50_59 = ifelse(dat$agegroup=='50_60',1,0)
dat$Age_60_69 = ifelse(dat$agegroup=='60_70',1,0)
dat$Age_70_79 = ifelse(dat$agegroup=='70_80',1,0)
dat$Age_80_150 = ifelse(dat$agegroup=='80_and_above',1,0)
# gender
dat$female = ifelse(dat$sex=='Female',1,0)
dat$male = ifelse(dat$sex=='Male',1,0)
# obesity (no missing data for bmi)
dat$obesity1 = ifelse((dat$BMI>=30)&(dat$BMI<35),1,0)
dat$obesity2 = ifelse((dat$BMI>=35)&(dat$BMI<40),1,0)
dat$obesity3 = ifelse((dat$BMI>=40),1,0)

# smoking
dat$smoking_ex = ifelse(dat$smoking_status=='Former',1,0)
dat$smoking_current = ifelse(dat$smoking_status=='Current',1,0)
# ethnicity
dat$white = ifelse(dat$race_ethnicity=='Non_hispanic_white',1,0)
dat$hispanic = ifelse(dat$race_ethnicity=='Hispanic',1,0)
dat$black = ifelse(dat$race_ethnicity=='Black',1,0)
#IMD - missing
dat$IMD2 = 0
dat$IMD3 = 0
dat$IMD4 = 0
dat$IMD5 = 0
# high blood pressure
dat$hbp = ifelse(dat$hypertension=='Hypertension_high_bp',1,0)
# COPD
dat$copd = ifelse(dat$resp_ex_asthma=='Yes',1,0)
# asthma - combine
dat$asthma = ifelse(dat$asthma=='Yes',1,0)
# CHD
dat$chd = ifelse(dat$heart_disease=='Yes',1,0)
# diabetes - combine
dat$diabetes = ifelse(dat$diabetes=='Yes',1,0)
# non_hematologic_cancer
dat$non_hematologic_cancer1 = ifelse((dat$non_hematologic_cancer=='Non_hematological')&(dat$diagnoses_cancer == 'less_than_1_yr'),1,0)
dat$non_hematologic_cancer2 = ifelse((dat$non_hematologic_cancer=='Non_hematological')&(dat$diagnoses_cancer == '1_5yr'),1,0)
dat$non_hematologic_cancer3 = ifelse((dat$non_hematologic_cancer=='Non_hematological')&(dat$diagnoses_cancer == 'greater_than_5_yr'),1,0)
# hematologic_cancer
dat$hematologic_cancer1 = ifelse((dat$hematologic_cancer=='Hematological')&(dat$diagnoses_cancer == 'less_than_1_yr'),1,0)
dat$hematologic_cancer2 = ifelse((dat$hematologic_cancer=='Hematological')&(dat$diagnoses_cancer == '1_5yr'),1,0)
dat$hematologic_cancer3 = ifelse((dat$hematologic_cancer=='Hematological')&(dat$diagnoses_cancer == 'greater_than_5_yr'),1,0)
# remove the ones that do not have diagnosis year
dat = dat[complete.cases(dat[,-which(colnames(dat) %in% c('diagnoses_cancer'))]),] 
sapply(1:ncol(dat),function(x){sum(is.na(dat[,x]))})
# stroke
dat$stroke = ifelse(dat$stroke=='Yes',1,0)
# kidney disease
dat$kidney_disease = ifelse(dat$kidney_disease=='Yes',1,0)
# rhemumatoid
dat$rhemumatoid = ifelse(dat$rhemumatoid=='Yes',1,0)
covariates = c('Age_18_39','Age_40_49','Age_60_69','Age_70_79','Age_80_150',
               'male',paste0('obesity',1:3),'smoking_ex','smoking_current',
               'hispanic','black',
               paste0('IMD',2:5),'hbp','copd', 'asthma', 'chd', 'diabetes',
               paste0('non_hematologic_cancer',1:3),paste0('hematologic_cancer',1:3),'stroke',
               'kidney_disease','rhemumatoid')


Covariate_matrix = as.matrix(dat[,covariates])
## replace NA by 0
sapply(1:ncol(Covariate_matrix),function(x){sum(is.na(Covariate_matrix[,x]))})
Covariate_matrix[is.na(Covariate_matrix)] = 0
# risk score (log hazard ratio):
rs_est = Covariate_matrix %*% coeffs$estimate

risk_score = cbind(dat[,1:5],rs_est)
colnames(risk_score) = c(colnames(dat)[1:5],'rs')
full_output = cbind(dat[,1:5],rs_est,dat[,6:ncol(dat)])

saveRDS(risk_score,file='~/Dropbox/NHANES_risk_score/500cities_data/Analysis/data_created/individual_rs.rds')
saveRDS(full_output,file='~/Dropbox/NHANES_risk_score/500cities_data/Analysis/data_created/individual_rs_covariates.rds')
write.xlsx(risk_score,file='~/Dropbox/NHANES_risk_score/500cities_data/Analysis/data_created/individual_rs.xlsx')
write.xlsx(full_output,file='~/Dropbox/NHANES_risk_score/500cities_data/Analysis/data_created/individual_rs_covariates.xlsx')
