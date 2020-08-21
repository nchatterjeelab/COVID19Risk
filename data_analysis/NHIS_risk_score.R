library(openxlsx)
# --------------------------- load the US moel ---------------------------
coeffs = read.xlsx('data_created/meta_model.xlsx', sheet = 'coefficients_individual_level')
rownames(coeffs) = coeffs$Variable
coef_name = coeffs$Variable
coef_name
# [1] "Age_15_44"                 "Age_45_54"                 "Age_65_74"                 "Age_75_84"                
# [5] "Age_85"                    "Sex_M"                     "BMI_obsese1"               "BMI_obsese2"              
# [9] "BMI_obsese3"               "Smoking_ex"                "Smoking_current"           "Hispanic"                 
# [13] "Black"                     "Asian"                     "Native_American"           "IMD2"                     
# [17] "IMD3"                      "IMD4"                      "IMD5"                      "BP_high"                  
# [21] "Respiratory_disease"       "Asthma"                    "CHD"                       "Diabetes"                 
# [25] "Cancer_nonhaematological1" "Cancer_nonhaematological2" "Cancer_nonhaematological3" "Cancer_haematological1"   
# [29] "Cancer_haematological2"    "Cancer_haematological3"    "Stroke"                    "Kidney"                   
# [33] "Arthritis"


# --------------------------- load the city-level data ---------------------------
dat = readRDS('data_created/nhis_imputed.rds')
colnames(dat)
covariates = c('Age_15_44','Age_45_54','Age_65_74','Age_75_84','Age_85',
               'male',paste0('obesity',1:3),'smoking_ex','smoking_current',
               'hispanic','black','asian','native',
               paste0('IMD',2:5),'hbp','copd', 'asthma', 'chd', 'diabetes',
               paste0('non_hematologic_cancer',1:3),paste0('hematologic_cancer',1:3),'stroke',
               'kidney_disease','rheumatoid')

Covariate_matrix = as.matrix(dat[,covariates])
sapply(1:ncol(Covariate_matrix),function(x){sum(is.na(Covariate_matrix[,x]))})
Covariate_matrix[is.na(Covariate_matrix)] = 0
# risk score (log scale):
rs_est = Covariate_matrix %*% coeffs$estimate

risk_score = cbind(dat[,1:5],rs_est)
colnames(risk_score) = c(colnames(dat)[1:5],'rs')
full_output = cbind(dat[,1:5],rs_est,dat[,6:ncol(dat)])

saveRDS(risk_score,file='data_created/individual_rs.rds')
saveRDS(full_output,file='data_created/individual_rs_covariates.rds')
write.xlsx(risk_score,file='data_created/individual_rs.xlsx')
write.xlsx(full_output,file='data_created/individual_rs_covariates.xlsx')
