library(openxlsx)
# --------------------------- load the UK moel ---------------------------
coeffs = read.xlsx('data_created/UK_model.xlsx', sheet = 'coefficients')
# load data:
coef_name = coeffs$Variable_Name_UK
coef_name
#[1] "Age_18_39"                 "Age_40_49"                 "Age_60_69"                 "Age_70_79"                
#[5] "Age_80_150"                "Sex_M"                     "BMI_obsese1"               "BMI_obsese2"              
#[9] "BMI_obsese3"               "Smoking_ex"                "Smoking_current"           "Hispanic"                 
#[13] "Black"                     "IMD2"                      "IMD3"                      "IMD4"                     
#[17] "IMD5"                      "BP_high"                   "Respiratory_disease"       "Asthma"                   
#[21] "CHD"                       "Diabetes_controlled"       "Diabetes_uncontrolled"     "Cancer_nonhaematological1"
#[25] "Cancer_nonhaematological2" "Cancer_nonhaematological3" "Cancer_haematological1"    "Cancer_haematological2"   
#[29] "Cancer_haematological3"    "Stroke"                    "Kidney"                    "Arthritis" 


# --------------------------- load the city-level data ---------------------------
dat = readRDS('data_created/combined_updated.rds')
colnames(dat)
covariates = c('Age_18_39','Age_40_49','Age_60_69','Age_70_79','Age_80_150',
               'male_proportion',paste0('obesity',1:3),'smoking_ex_proportion','smoking_current_proportion',
               'proportion_hispanic','proportion_black',
               paste0('IMD',2:5),'BPHIGH_CrudePrev','COPD_CrudePrev',
               'CASTHMA_CrudePrev', 'CHD_CrudePrev',
               'DIABETES_ctrled_CrudePrev', 'DIABETES_unctrled_CrudePrev',
               paste0('non_hematologic_cancer',1:3),paste0('hematologic_cancer',1:3),'STROKE_CrudePrev',
               'KIDNEY_CrudePrev','ARTHRITIS_CrudePrev')

Covariate_matrix = as.matrix(dat[,covariates])
# risk score (log hazard ratio):
rs_est = Covariate_matrix %*% coeffs$estimate

risk_score = cbind(dat[,c('StateAbbr','PlaceName','PlaceFIPS','population')],rs_est)
full_output = cbind(dat[,c('StateAbbr','PlaceName','PlaceFIPS','population')],rs_est,dat[,5:ncol(dat)])

saveRDS(risk_score,file='~/Dropbox/NHANES_risk_score/500cities_data/Analysis/data_created/risk_score_updated.rds')
saveRDS(full_output,file='~/Dropbox/NHANES_risk_score/500cities_data/Analysis/data_created/full_output_updated.rds')

write.xlsx(risk_score,file='~/Dropbox/NHANES_risk_score/500cities_data/Analysis/data_created/risk_score_updated.xlsx')
write.xlsx(full_output,file='~/Dropbox/NHANES_risk_score/500cities_data/Analysis/data_created/full_output_updated.xlsx')
