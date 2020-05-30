# load model:
# --------------------------- load the UK moel ---------------------------
coeffs = read.xlsx('data_created/UK_model.xlsx', sheet = 'coefficients')
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
dat.IMD = readRDS('data_created/combined_updated.rds')
dat = as.data.frame(readRDS('data_created/prevalance_age_stratification.rds'))
dat[,9:54] = sapply(dat[,9:54], as.character)
dat[,9:54] = sapply(dat[,9:54], as.numeric)
dat$IMD1 = dat.IMD$IMD1
dat$IMD2 = dat.IMD$IMD2
dat$IMD3 = dat.IMD$IMD3
dat$IMD4 = dat.IMD$IMD4
dat$IMD5 = dat.IMD$IMD5
colnames(dat)

# ------------ Create the covariates matrix. No need to exclude missing rows
# -----------------------------------------------------------------------------------------
# ------------------------------------ 18-39 age group ------------------------------------
# -----------------------------------------------------------------------------------------
covariates_ex_age = c(paste0('prevalance_of_',
                           c('male',paste0('ObesityObese_',c('I','II','III')),
                             'smoking_statusFormer','smoking_statusCurrent',
                             'race_ethnicityHispanic', 'race_ethnicityBlack'),
                           '_given_age_less_than_40'),
                      paste0('IMD',2:5),
                      paste0('prevalance_of_',
                             c('hypertensionhighbp','resp_ex_asthmaYes','asthmaYes','heart_diseaseYes',
                               'diabetesYes_controlled','diabetesYes_uncontrolled',
                               'nonhematoless_than_1_yr','nonhemato1_5yr','nonhematogreater_than_5_yr',
                               'hematoless_than_1_yr','hemato1_5yr','hematogreater_than_5_yr',
                               'strokeYes','kidney_diseaseYes','rhemumatoidYes'),
                             '_given_age_less_than_40'))
Covariate_matrix = dat[,covariates_ex_age]
Covariate_matrix$age1 = 1
Covariate_matrix$age2 = 0
Covariate_matrix$age3 = 0
Covariate_matrix$age4 = 0
Covariate_matrix$age5 = 0
Covariate_matrix = Covariate_matrix[,c((ncol(Covariate_matrix)-4):(ncol(Covariate_matrix)),1:(ncol(Covariate_matrix)-5))]
Covariate_matrix = as.matrix(Covariate_matrix)

# risk score (log hazard ratio):
rs_est = Covariate_matrix %*% matrix(coeffs$estimate,ncol=1)

risk_score = cbind(dat.IMD[,c('StateAbbr','PlaceName','PlaceFIPS','population')],rs_est)
full_output = cbind(dat.IMD[,c('StateAbbr','PlaceName','PlaceFIPS','population')],rs_est,Covariate_matrix)

saveRDS(risk_score,file='~/Dropbox/NHANES_risk_score/500cities_data/Analysis/data_created/risk_score_age_under_40.rds')
saveRDS(full_output,file='~/Dropbox/NHANES_risk_score/500cities_data/Analysis/data_created/full_output_age_under_40.rds')

write.xlsx(risk_score,file='~/Dropbox/NHANES_risk_score/500cities_data/Analysis/data_created/risk_score_age_under_40.xlsx')
write.xlsx(full_output,file='~/Dropbox/NHANES_risk_score/500cities_data/Analysis/data_created/full_output_age_under_40.xlsx')




# ------------ Create the covariates matrix. Don't need to exclude missing rows
# -----------------------------------------------------------------------------------------
# ------------------------------------- 40+ age group -------------------------------------
# -----------------------------------------------------------------------------------------
covariates_ex_age = c(paste0('prevalance_of_',
                             c('male',paste0('ObesityObese_',c('I','II','III')),
                               'smoking_statusFormer','smoking_statusCurrent',
                               'race_ethnicityHispanic','race_ethnicityBlack'),
                             '_given_age_greater_than_40'),
                      paste0('IMD',2:5),
                      paste0('prevalance_of_',
                             c('hypertensionhighbp','resp_ex_asthmaYes','asthmaYes','heart_diseaseYes',
                               'diabetesYes_controlled','diabetesYes_uncontrolled',
                               'nonhematoless_than_1_yr','nonhemato1_5yr','nonhematogreater_than_5_yr',
                               'hematoless_than_1_yr','hemato1_5yr','hematogreater_than_5_yr',
                               'strokeYes','kidney_diseaseYes','rhemumatoidYes'),
                             '_given_age_greater_than_40'))
Covariate_matrix = dat[,covariates_ex_age]
Covariate_matrix$age_over40 = 1 - dat.IMD$Age_18_39
Covariate_matrix$age1 = 0
Covariate_matrix$age2 = dat.IMD$Age_40_49/Covariate_matrix$age_over40
Covariate_matrix$age3 = dat.IMD$Age_60_69/Covariate_matrix$age_over40
Covariate_matrix$age4 = dat.IMD$Age_70_79/Covariate_matrix$age_over40
Covariate_matrix$age5 = dat.IMD$Age_80_150/Covariate_matrix$age_over40
(Covariate_matrix$age2)+(Covariate_matrix$age3)+(Covariate_matrix$age4)+(Covariate_matrix$age5)+dat.IMD$Age_50_59/Covariate_matrix$age_over40
Covariate_matrix = Covariate_matrix[,c((ncol(Covariate_matrix)-4):(ncol(Covariate_matrix)),1:(ncol(Covariate_matrix)-6))] 
Covariate_matrix = as.matrix(Covariate_matrix)

# risk score (log hazard ratio):
rs_est = Covariate_matrix %*% matrix(coeffs$estimate,ncol=1)

risk_score = cbind(dat.IMD[,c('StateAbbr','PlaceName','PlaceFIPS','population')],rs_est)
full_output = cbind(dat.IMD[,c('StateAbbr','PlaceName','PlaceFIPS','population')],rs_est,Covariate_matrix)

saveRDS(risk_score,file='~/Dropbox/NHANES_risk_score/500cities_data/Analysis/data_created/risk_score_age_over_40.rds')
saveRDS(full_output,file='~/Dropbox/NHANES_risk_score/500cities_data/Analysis/data_created/full_output_age_over_40.rds')

write.xlsx(risk_score,file='~/Dropbox/NHANES_risk_score/500cities_data/Analysis/data_created/risk_score_age_over_40.xlsx')
write.xlsx(full_output,file='~/Dropbox/NHANES_risk_score/500cities_data/Analysis/data_created/full_output_age_over_40.xlsx')
