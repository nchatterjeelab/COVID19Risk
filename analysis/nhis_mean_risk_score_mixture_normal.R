update_p11 = function(p1,p2,r){
  a = 1 - r
  b = 1 + (r-1)*(p1+p2)
  c = -r*p1*p2
  (-b+sqrt(b^2-4*a*c))/(2*a)
}

prevalance_brfss_census = readRDS("~/Dropbox/NHANES_risk_score/500cities_data/Analysis/data_created/combined_updated.rds")

nhis_data = readRDS("~/Dropbox/NHANES_risk_score/500cities_data/Analysis/data_created/nhis_2017.rds")

nhis_data  = nhis_data[,c(5:6, 9:24)]

nhis_data= nhis_data %>% mutate(age_mixture_cat = case_when(agegroup == "70_80" | agegroup == "60_70" | agegroup == "40_50" | agegroup == "50_60" | agegroup == "80_and_above" ~ '40_and_above',
                                                            agegroup == "18_40" ~ '18_40'))

#nhis_data_complete = nhis_data[complete.cases(nhis_data), ]

design_matrix_nhis = model.matrix(~ sex + agegroup + diabetes + smoking_status + rhemumatoid + asthma + stroke + heart_disease + hypertension + resp_ex_asthma + kidney_disease + liver_disease + race_ethnicity + hematologic_cancer + non_hematologic_cancer + Obesity , nhis_data)

#----Throughout age_mixture_cat40_and_above is 1 if age is greater than 40

#----Age-Sex---#

data = nhis_data[, c(1,2,19)]
data_complete = data[complete.cases(data), ]
data_design_matrix = model.matrix(~ sex + age_mixture_cat, data_complete)
data_design_matrix = cbind(data_complete$sampling_weights, data_design_matrix[, -1])
colnames(data_design_matrix)[1] = "sampling_weights"
data_design_matrix = as.data.frame(data_design_matrix)
dstrat<-svydesign(ids=~1,weights=~sampling_weights, data = data_design_matrix)
logistic_fit_adj =svyglm(age_mixture_cat40_and_above ~ sexMale, design=dstrat, family = quasibinomial())
r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
p1 = male_p = prevalance_brfss_census$male_proportion
p2 = age_greater_than_40_p = 1 - prevalance_brfss_census$Age_18_39 
p11 = update_p11(p1,p2,r)
c1 = prevalance_of_male_given_age_greater_than_40 = p11/p2 
prevalance_of_male_given_age_less_than_40 = p1/(1-p2) - (c1*p2/(1-p2))

#----Age-Diabetes---#
hbAc1_level = sasxport.get("~/Dropbox/NHANES_risk_score/500cities_data/data/NHANES_Asthma_Diabetes/GHB_J.xpt")
demo = sasxport.get("~/Dropbox/NHANES_risk_score/500cities_data/data/NHANES_Asthma_Diabetes/DEMO_J.xpt")
demo_sampling_weights = demo[, c(1,5,41)]
diabetes_questionnaire = sasxport.get("~/Dropbox/NHANES_risk_score/500cities_data/data/NHANES_Asthma_Diabetes/DIQ_J.xpt")
data = merge(merge(hbAc1_level, demo_sampling_weights, by = "seqn"), diabetes_questionnaire, by = "seqn")
data_diabetes = data[, c(2,3,4,5)]
data_diabetes_greater_18 = data_diabetes[which(data_diabetes$ridageyr>=18), ]
data_diabetes_greater_18 = data_diabetes_greater_18[which(data_diabetes_greater_18$diq010 == 1 | data_diabetes_greater_18$diq010 == 2), ]
data = data_diabetes_greater_18 %>% mutate(diabetes = case_when(diq010 == 2 ~ 'No',
                                                                diq010 == 1 &  lbxgh < 7.4 ~ 'Controlled',
                                                                diq010 == 1 &  lbxgh >= 7.4 ~ 'Uncontrolled'))
data = data %>% mutate(age_mixture_cat = case_when(ridageyr >= 18 & ridageyr < 40 ~ '18_40',
                                                   ridageyr >= 40 ~ '40_and_above'))
data$diabetes[which(data$diabetes == "No")] = "A"
data_complete = data[complete.cases(data), ]
data_design_matrix = model.matrix(~ diabetes + age_mixture_cat, data_complete)
data_design_matrix = cbind(data_complete$wtmec2yr, data_design_matrix[, -1])
colnames(data_design_matrix)[1] = "sampling_weights"
data_design_matrix = as.data.frame(data_design_matrix)
dstrat<-svydesign(ids=~1,weights=~sampling_weights, data = data_design_matrix)
logistic_fit_adj =svyglm(age_mixture_cat40_and_above ~ diabetesControlled , design=dstrat, family = quasibinomial())
r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
p1 = diabetes_controlled_p = (1/(1+0.597411)) * (prevalance_brfss_census$DIABETES_CrudePrev/100)
p2 = age_greater_than_40_p = 1 - prevalance_brfss_census$Age_18_39 
p11 = update_p11(p1,p2,r)
c1 = prevalance_of_diabetesYes_controlled_given_age_greater_than_40 = p11/p2 
prevalance_of_diabetesYes_controlled_given_age_less_than_40 = p1/(1-p2) - (c1*p2/(1-p2))

logistic_fit_adj =svyglm(age_mixture_cat40_and_above ~ diabetesUncontrolled , design=dstrat, family = quasibinomial())
r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
p1 = diabetes_controlled_p = (0.597411/(1+0.597411)) * (prevalance_brfss_census$DIABETES_CrudePrev/100)
p2 = age_greater_than_40_p = 1 - prevalance_brfss_census$Age_18_39 
p11 = update_p11(p1,p2,r)
c1 = prevalance_of_diabetesYes_uncontrolled_given_age_greater_than_40 = p11/p2 
prevalance_of_diabetesYes_uncontrolled_given_age_less_than_40 = p1/(1-p2) - (c1*p2/(1-p2))

#----Age-Smoking---#
data = nhis_data[, c(1,5,19)]
data$smoking_status[which(data$smoking_status == "NA")] = NA
data$smoking_status[which(data$smoking_status == "Never")] = "A"
data_complete = data[complete.cases(data), ]
data_design_matrix = model.matrix(~ smoking_status + age_mixture_cat, data_complete)
data_design_matrix = cbind(data_complete$sampling_weights, data_design_matrix[, -1])
colnames(data_design_matrix)[1] = "sampling_weights"
data_design_matrix = as.data.frame(data_design_matrix)
dstrat<-svydesign(ids=~1,weights=~sampling_weights, data = data_design_matrix)
logistic_fit_adj =svyglm(age_mixture_cat40_and_above ~ smoking_statusCurrent, design=dstrat, family = quasibinomial())
r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
bias = (0.6210029/(1+ 0.6210029))
true = prop.table(table(data$smoking_status))[1]/(prop.table(table(data$smoking_status))[1] + prop.table(table(data$smoking_status))[2])
correction = true/bias
p1 = smoking_p = correction * prevalance_brfss_census$smoking_current_proportion
p2 = age_greater_than_40_p = 1 - prevalance_brfss_census$Age_18_39 
p11 = update_p11(p1,p2,r)
c1 = prevalance_of_smoking_statusCurrent_given_age_greater_than_40 = p11/p2 
prevalance_of_smoking_statusCurrent_given_age_less_than_40 = p1/(1-p2) - (c1*p2/(1-p2))
logistic_fit_adj =svyglm(age_mixture_cat40_and_above ~ smoking_statusFormer, design=dstrat, family = quasibinomial())
r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
bias = (1.610298/(1+1.610298))
true = prop.table(table(data$smoking_status))[2]/(prop.table(table(data$smoking_status))[1] + prop.table(table(data$smoking_status))[2])
correction = true/bias
p1 = smoking_p = correction * prevalance_brfss_census$smoking_ex_proportion
p2 = age_greater_than_40_p = 1 - prevalance_brfss_census$Age_18_39 
p11 = update_p11(p1,p2,r)
c1 = prevalance_of_smoking_statusFormer_given_age_greater_than_40 = p11/p2 
prevalance_of_smoking_statusFormer_given_age_less_than_40 = p1/(1-p2) - (c1*p2/(1-p2))

#----Age-Rheumatoid---#
data = nhis_data[, c(1,6,19)]
data$rhemumatoid[which(data$rhemumatoid == "NA")] = NA
data_complete = data[complete.cases(data), ]
data_design_matrix = model.matrix(~ rhemumatoid + age_mixture_cat, data_complete)
data_design_matrix = cbind(data_complete$sampling_weights, data_design_matrix[, -1])
colnames(data_design_matrix)[1] = "sampling_weights"
data_design_matrix = as.data.frame(data_design_matrix)
dstrat<-svydesign(ids=~1,weights=~sampling_weights, data = data_design_matrix)
logistic_fit_adj =svyglm(age_mixture_cat40_and_above ~ rhemumatoidYes, design=dstrat, family = quasibinomial())
r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
p1 = rhemumatoidYes_p = prevalance_brfss_census$ARTHRITIS_CrudePrev
p2 = age_greater_than_40_p = 1 - prevalance_brfss_census$Age_18_39 
p11 = update_p11(p1,p2,r)
c1 = prevalance_of_rhemumatoidYes_given_age_greater_than_40 = p11/p2 
prevalance_of_rhemumatoidYes_given_age_less_than_40 = p1/(1-p2) - (c1*p2/(1-p2))


#----Age-Asthma---#
data = nhis_data[, c(1,7,19)]
data$asthma[which(data$asthma == "NA")] = NA
data_complete = data[complete.cases(data), ]
data_design_matrix = model.matrix(~ asthma + age_mixture_cat, data_complete)
data_design_matrix = cbind(data_complete$sampling_weights, data_design_matrix[, -1])
colnames(data_design_matrix)[1] = "sampling_weights"
data_design_matrix = as.data.frame(data_design_matrix)
dstrat<-svydesign(ids=~1,weights=~sampling_weights, data = data_design_matrix)
logistic_fit_adj =svyglm(age_mixture_cat40_and_above ~ asthmaYes, design=dstrat, family = quasibinomial())
r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
p1 = male_p = prevalance_brfss_census$CASTHMA_CrudePrev
p2 = age_greater_than_40_p = 1 - prevalance_brfss_census$Age_18_39 
p11 = update_p11(p1,p2,r)
c1 = prevalance_of_asthmaYes_given_age_greater_than_40 = p11/p2 
prevalance_of_asthmaYes_given_age_less_than_40 = p1/(1-p2) - (c1*p2/(1-p2))


#----Age-Stroke---#
data = nhis_data[, c(1,8,19)]
data$stroke[which(data$stroke == "NA")] = NA
data_complete = data[complete.cases(data), ]
data_design_matrix = model.matrix(~ stroke + age_mixture_cat, data_complete)
data_design_matrix = cbind(data_complete$sampling_weights, data_design_matrix[, -1])
colnames(data_design_matrix)[1] = "sampling_weights"
data_design_matrix = as.data.frame(data_design_matrix)
dstrat<-svydesign(ids=~1,weights=~sampling_weights, data = data_design_matrix)
logistic_fit_adj =svyglm(age_mixture_cat40_and_above ~ strokeYes, design=dstrat, family = quasibinomial())
r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
p1 = male_p = prevalance_brfss_census$STROKE_CrudePrev
p2 = age_greater_than_40_p = 1 - prevalance_brfss_census$Age_18_39 
p11 = update_p11(p1,p2,r)
c1 = prevalance_of_strokeYes_given_age_greater_than_40 = p11/p2 
prevalance_of_strokeYes_given_age_less_than_40 = p1/(1-p2) - (c1*p2/(1-p2))

#----Age-Heart---#
data = nhis_data[, c(1,9,19)]
data$heart_disease[which(data$heart_disease == "NA")] = NA
data_complete = data[complete.cases(data), ]
data_design_matrix = model.matrix(~ heart_disease + age_mixture_cat, data_complete)
data_design_matrix = cbind(data_complete$sampling_weights, data_design_matrix[, -1])
colnames(data_design_matrix)[1] = "sampling_weights"
data_design_matrix = as.data.frame(data_design_matrix)
dstrat<-svydesign(ids=~1,weights=~sampling_weights, data = data_design_matrix)
logistic_fit_adj =svyglm(age_mixture_cat40_and_above ~ heart_diseaseYes, design=dstrat, family = quasibinomial())
r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
p1 = male_p = prevalance_brfss_census$CHD_CrudePrev
p2 = age_greater_than_40_p = 1 - prevalance_brfss_census$Age_18_39 
p11 = update_p11(p1,p2,r)
c1 = prevalance_of_heart_diseaseYes_given_age_greater_than_40 = p11/p2 
prevalance_of_heart_diseaseYes_given_age_less_than_40 = p1/(1-p2) - (c1*p2/(1-p2))

#----Age-Hypertension---#
data = nhis_data[, c(1,10,19)]
data$hypertension[which(data$hypertension == "NA")] = NA
data_complete = data[complete.cases(data), ]
data_design_matrix = model.matrix(~ hypertension + age_mixture_cat, data_complete)
data_design_matrix = cbind(data_complete$sampling_weights, data_design_matrix[, -1])
colnames(data_design_matrix)[1] = "sampling_weights"
data_design_matrix = as.data.frame(data_design_matrix)
dstrat<-svydesign(ids=~1,weights=~sampling_weights, data = data_design_matrix)
logistic_fit_adj =svyglm(age_mixture_cat40_and_above ~ hypertensionNormal, design=dstrat, family = quasibinomial())
r = odds_ratio = exp(-logistic_fit_adj$coefficients[2])
p1 = male_p = prevalance_brfss_census$BPHIGH_CrudePrev
p2 = age_greater_than_40_p = 1 - prevalance_brfss_census$Age_18_39 
p11 = update_p11(p1,p2,r)
c1 = prevalance_of_hypertensionhighbp_given_age_greater_than_40 = p11/p2 
prevalance_of_hypertensionhighbp_given_age_less_than_40 = p1/(1-p2) - (c1*p2/(1-p2))

#----Age-Resp_ex_asthma---#
data = nhis_data[, c(1,11,19)]
data$resp_ex_asthma[which(data$resp_ex_asthma == "NA")] = NA
data_complete = data[complete.cases(data), ]
data_design_matrix = model.matrix(~ resp_ex_asthma + age_mixture_cat, data_complete)
data_design_matrix = cbind(data_complete$sampling_weights, data_design_matrix[, -1])
colnames(data_design_matrix)[1] = "sampling_weights"
data_design_matrix = as.data.frame(data_design_matrix)
dstrat<-svydesign(ids=~1,weights=~sampling_weights, data = data_design_matrix)
logistic_fit_adj =svyglm(age_mixture_cat40_and_above ~ resp_ex_asthmaYes, design=dstrat, family = quasibinomial())
r = odds_ratio = exp(-logistic_fit_adj$coefficients[2])
p1 = male_p = prevalance_brfss_census$COPD_CrudePrev
p2 = age_greater_than_40_p = 1 - prevalance_brfss_census$Age_18_39 
p11 = update_p11(p1,p2,r)
c1 = prevalance_of_resp_ex_asthmaYes_given_age_greater_than_40 = p11/p2 
prevalance_of_resp_ex_asthmaYes_given_age_less_than_40 = p1/(1-p2) - (c1*p2/(1-p2))

#----Age-Kidney---#
data = nhis_data[, c(1,12,19)]
data$kidney_disease[which(data$kidney_disease == "NA")] = NA
data_complete = data[complete.cases(data), ]
data_design_matrix = model.matrix(~ kidney_disease + age_mixture_cat, data_complete)
data_design_matrix = cbind(data_complete$sampling_weights, data_design_matrix[, -1])
colnames(data_design_matrix)[1] = "sampling_weights"
data_design_matrix = as.data.frame(data_design_matrix)
dstrat<-svydesign(ids=~1,weights=~sampling_weights, data = data_design_matrix)
logistic_fit_adj =svyglm(age_mixture_cat40_and_above ~ kidney_diseaseYes, design=dstrat, family = quasibinomial())
r = odds_ratio = exp(-logistic_fit_adj$coefficients[2])
p1 = male_p = prevalance_brfss_census$KIDNEY_CrudePrev
p2 = age_greater_than_40_p = 1 - prevalance_brfss_census$Age_18_39 
p11 = update_p11(p1,p2,r)
c1 = prevalance_of_kidney_diseaseYes_given_age_greater_than_40 = p11/p2 
prevalance_of_kidney_diseaseYes_given_age_less_than_40 = p1/(1-p2) - (c1*p2/(1-p2))

#----Age-Liver---#
data = nhis_data[, c(1,13,19)]
data$liver_disease[which(data$liver_disease == "NA")] = NA
data_complete = data[complete.cases(data), ]
data_design_matrix = model.matrix(~ liver_disease + age_mixture_cat, data_complete)
data_design_matrix = cbind(data_complete$sampling_weights, data_design_matrix[, -1])
colnames(data_design_matrix)[1] = "sampling_weights"
data_design_matrix = as.data.frame(data_design_matrix)
dstrat<-svydesign(ids=~1,weights=~sampling_weights, data = data_design_matrix)
logistic_fit_adj =svyglm(age_mixture_cat40_and_above ~ liver_diseaseYes, design=dstrat, family = quasibinomial())
r = odds_ratio = exp(-logistic_fit_adj$coefficients[2])
p1 = male_p = prevalance_brfss_census$male_proportion
p2 = age_greater_than_40_p = 1 - prevalance_brfss_census$Age_18_39 
p11 = update_p11(p1,p2,r)
c1 = prevalance_of_liver_diseaseYes_given_age_greater_than_40 = p11/p2 
prevalance_of_liver_diseaseYes_given_age_less_than_40 = p1/(1-p2) - (c1*p2/(1-p2))

#----Age-Race_ethnicity---#
data = nhis_data[, c(1,14,19)]
#data$race_ethnicity[which(data$race_ethnicity == "NA")] = NA
data_complete = data[complete.cases(data), ]
data_complete$race_ethnicity[which(data$race_ethnicity == "Non_hispanic_white")] = "A"
data_design_matrix = model.matrix(~ race_ethnicity + age_mixture_cat, data_complete)
data_design_matrix = cbind(data_complete$sampling_weights, data_design_matrix[, -1])
colnames(data_design_matrix)[1] = "sampling_weights"
data_design_matrix = as.data.frame(data_design_matrix)
dstrat<-svydesign(ids=~1,weights=~sampling_weights, data = data_design_matrix)
logistic_fit_adj =svyglm(age_mixture_cat40_and_above ~ race_ethnicityBlack, design=dstrat, family = quasibinomial())
r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
p1 = male_p = prevalance_brfss_census$proportion_black
p2 = age_greater_than_40_p = 1 - prevalance_brfss_census$Age_18_39 
p11 = update_p11(p1,p2,r)
c1 = prevalance_of_race_ethnicityBlack_given_age_greater_than_40 = p11/p2 
prevalance_of_race_ethnicityBlack_given_age_less_than_40  = p1/(1-p2) - (c1*p2/(1-p2))
logistic_fit_adj =svyglm(age_mixture_cat40_and_above ~ race_ethnicityHispanic, design=dstrat, family = quasibinomial())
r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
p1 = male_p = prevalance_brfss_census$proportion_hispanic
p2 = age_greater_than_40_p = 1 - prevalance_brfss_census$Age_18_39 
p11 = update_p11(p1,p2,r)
c1 = prevalance_of_race_ethnicityHispanic_given_age_greater_than_40 = p11/p2 
prevalance_of_race_ethnicityHispanic_given_age_less_than_40 = p1/(1-p2) - (c1*p2/(1-p2))
# logistic_fit_adj =svyglm(age_mixture_cat40_and_above ~ race_ethnicityMixed, design=dstrat, family = quasibinomial())
# r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
# p1 = male_p = prevalance_brfss_census$proportion_mixed
# p2 = age_greater_than_40_p = 1 - prevalance_brfss_census$Age_18_39 
# p11 = update_p11(p1,p2,r)
# c1 = prevalance_of_race_ethnicityMixed_given_age_greater_than_40 = p11/p2 
# prevalance_of_race_ethnicityMixed_given_age_less_than_40 = p1/(1-p2) - (c1*p2/(1-p2))
# logistic_fit_adj =svyglm(age_mixture_cat40_and_above ~ race_ethnicityOthers, design=dstrat, family = quasibinomial())
# r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
# p1 = male_p = prevalance_brfss_census$proportion_others
# p2 = age_greater_than_40_p = 1 - prevalance_brfss_census$Age_18_39 
# p11 = update_p11(p1,p2,r)
# c1 = prevalance_of_race_ethnicityOthers_given_age_greater_than_40 = p11/p2 
# prevalance_of_race_ethnicityOthers_given_age_less_than_40 = p1/(1-p2) - (c1*p2/(1-p2))
# logistic_fit_adj =svyglm(age_mixture_cat40_and_above ~ race_ethnicityAsian, design=dstrat, family = quasibinomial())
# r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
# p1 = male_p = prevalance_brfss_census$proportion_asian
# p2 = age_greater_than_40_p = 1 - prevalance_brfss_census$Age_18_39 
# p11 = update_p11(p1,p2,r)
# c1 = prevalance_of_race_ethnicityAsian_given_age_greater_than_40 = p11/p2 
# prevalance_of_race_ethnicityAsian_given_age_less_than_40 = p1/(1-p2) - (c1*p2/(1-p2))

#----Age-Hematologic---#
data = nhis_data[, c(1,15,17, 19)]
data = data  %>% mutate(hemato = case_when(hematologic_cancer == "None" ~ 'No',
                                                             hematologic_cancer == "Hematological" &  diagnoses_cancer == "greater_than_5_yr" ~ 'greater_than_5_yr',
                                                             hematologic_cancer == "Hematological" &  diagnoses_cancer == "1_5yr" ~ '1_5yr',
                                                             hematologic_cancer == "Hematological" &  diagnoses_cancer == "less_than_1_yr" ~ 'less_than_1_yr'))
data_hemato = data[, c(1,4,5)]
data_complete = data_hemato[complete.cases(data_hemato), ]
data_complete$hemato[which(data_complete$hemato == "No")] = "0yr"
data_design_matrix = model.matrix(~ age_mixture_cat + hemato, data_complete)
data_design_matrix = cbind(data_complete$sampling_weights, data_design_matrix[, -1])
colnames(data_design_matrix)[1] = "sampling_weights"
data_design_matrix = as.data.frame(data_design_matrix)
dstrat<-svydesign(ids=~1,weights=~sampling_weights, data = data_design_matrix)
logistic_fit_adj =svyglm(age_mixture_cat40_and_above ~ hematoless_than_1_yr, design=dstrat, family = quasibinomial())
r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
p1 = hemato_cat1_p = prevalance_brfss_census$hematologic_cancer1
p2 = age_greater_than_40_p = 1 - prevalance_brfss_census$Age_18_39 
p11 = update_p11(p1,p2,r)
c1 = prevalance_of_hematoless_than_1_yr_given_age_greater_than_40 = p11/p2 
prevalance_of_hematoless_than_1_yr_given_age_less_than_40 = p1/(1-p2) - (c1*p2/(1-p2))

logistic_fit_adj =svyglm(age_mixture_cat40_and_above ~ hemato1_5yr, design=dstrat, family = quasibinomial())
r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
p1 = hemato_cat2_p = prevalance_brfss_census$hematologic_cancer2
p2 = age_greater_than_40_p = 1 - prevalance_brfss_census$Age_18_39 
p11 = update_p11(p1,p2,r)
c1 = prevalance_of_hemato1_5yr_given_age_greater_than_40 = p11/p2 
prevalance_of_hemato1_5yr_given_age_less_than_40 = p1/(1-p2) - (c1*p2/(1-p2))

logistic_fit_adj =svyglm(age_mixture_cat40_and_above ~ hematogreater_than_5_yr, design=dstrat, family = quasibinomial())
r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
p1 = hemato_cat3_p = prevalance_brfss_census$hematologic_cancer3
p2 = age_greater_than_40_p = 1 - prevalance_brfss_census$Age_18_39 
p11 = update_p11(p1,p2,r)
c1 = prevalance_of_hematogreater_than_5_yr_given_age_greater_than_40 = p11/p2 
prevalance_of_hematogreater_than_5_yr_given_age_less_than_40 = p1/(1-p2) - (c1*p2/(1-p2))



#----Age-non_hematologic---#
data = nhis_data[, c(1,16,17,19)]
data = data  %>% mutate(nonhemato = case_when(non_hematologic_cancer == "None" ~ 'No',
                                           non_hematologic_cancer == "Non_hematological" &  diagnoses_cancer == "greater_than_5_yr" ~ 'greater_than_5_yr',
                                           non_hematologic_cancer == "Non_hematological" &  diagnoses_cancer == "1_5yr" ~ '1_5yr',
                                           non_hematologic_cancer == "Non_hematological" &  diagnoses_cancer == "less_than_1_yr" ~ 'less_than_1_yr'))
data_nonhemato = data[, c(1,4,5)]
data_complete = data_nonhemato[complete.cases(data_nonhemato), ]
data_complete$nonhemato[which(data_complete$nonhemato == "No")] = "0yr"
data_design_matrix = model.matrix(~ age_mixture_cat + nonhemato, data_complete)
data_design_matrix = cbind(data_complete$sampling_weights, data_design_matrix[, -1])
colnames(data_design_matrix)[1] = "sampling_weights"
data_design_matrix = as.data.frame(data_design_matrix)
dstrat<-svydesign(ids=~1,weights=~sampling_weights, data = data_design_matrix)
logistic_fit_adj =svyglm(age_mixture_cat40_and_above ~ nonhematoless_than_1_yr, design=dstrat, family = quasibinomial())
r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
p1 = non_hemato_cancer1_p = prevalance_brfss_census$non_hematologic_cancer1
p2 = age_greater_than_40_p = 1 - prevalance_brfss_census$Age_18_39 
p11 = update_p11(p1,p2,r)
c1 = prevalance_of_nonhematoless_than_1_yr_given_age_greater_than_40 = p11/p2 
prevalance_of_nonhematoless_than_1_yr_given_age_less_than_40 = p1/(1-p2) - (c1*p2/(1-p2))

logistic_fit_adj =svyglm(age_mixture_cat40_and_above ~ nonhemato1_5yr, design=dstrat, family = quasibinomial())
r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
p1 = non_hemato_cancer2_p = prevalance_brfss_census$non_hematologic_cancer2
p2 = age_greater_than_40_p = 1 - prevalance_brfss_census$Age_18_39 
p11 = update_p11(p1,p2,r)
c1 = prevalance_of_nonhemato1_5yr_given_age_greater_than_40 = p11/p2 
prevalance_of_nonhemato1_5yr_given_age_less_than_40 = p1/(1-p2) - (c1*p2/(1-p2))

logistic_fit_adj =svyglm(age_mixture_cat40_and_above ~ nonhematogreater_than_5_yr, design=dstrat, family = quasibinomial())
r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
p1 = non_hemato_cancer3_p = prevalance_brfss_census$non_hematologic_cancer3
p2 = age_greater_than_40_p = 1 - prevalance_brfss_census$Age_18_39 
p11 = update_p11(p1,p2,r)
c1 = prevalance_of_nonhematogreater_than_5_yr_given_age_greater_than_40 = p11/p2 
prevalance_of_nonhematogreater_than_5_yr_given_age_less_than_40 = p1/(1-p2) - (c1*p2/(1-p2))



#----Age-Obesity---#
data = nhis_data[, c(1,18,19)]
data$Obesity[which(data$Obesity == "NA")] = NA
data_complete = data[complete.cases(data), ]
data_design_matrix = model.matrix(~ Obesity + age_mixture_cat, data_complete)
data_design_matrix = cbind(data_complete$sampling_weights, data_design_matrix[, -1])
colnames(data_design_matrix)[1] = "sampling_weights"
data_design_matrix = as.data.frame(data_design_matrix)
dstrat<-svydesign(ids=~1,weights=~sampling_weights, data = data_design_matrix)
logistic_fit_adj =svyglm(age_mixture_cat40_and_above ~ ObesityObese_I, design=dstrat, family = quasibinomial())
r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
p1 = Obesity_I_p = (prop.table(table(data_complete$Obesity))[2]/(sum(prop.table(table(data_complete$Obesity))[2:4])))*prevalance_brfss_census$OBESITY_CrudePrev
p2 = age_greater_than_40_p = 1 - prevalance_brfss_census$Age_18_39 
p11 = update_p11(p1,p2,r)
c1 = prevalance_of_ObesityObese_I_given_age_greater_than_40 = p11/p2 
prevalance_of_ObesityObese_I_given_age_less_than_40 = p1/(1-p2) - (c1*p2/(1-p2))

logistic_fit_adj =svyglm(age_mixture_cat40_and_above ~ ObesityObese_II, design=dstrat, family = quasibinomial())
r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
p1 = Obesity_II_p = (prop.table(table(data_complete$Obesity))[3]/(sum(prop.table(table(data_complete$Obesity))[2:4])))*prevalance_brfss_census$OBESITY_CrudePrev

p2 = age_greater_than_40_p = 1 - prevalance_brfss_census$Age_18_39 
p11 = update_p11(p1,p2,r)
c1 = prevalance_of_ObesityObese_II_given_age_greater_than_40 = p11/p2 
prevalance_of_ObesityObese_II_given_age_less_than_40 = p1/(1-p2) - (c1*p2/(1-p2))

logistic_fit_adj =svyglm(age_mixture_cat40_and_above ~ ObesityObese_III, design=dstrat, family = quasibinomial())
r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
p1 = Obesity_III_p = (prop.table(table(data_complete$Obesity))[4]/(sum(prop.table(table(data_complete$Obesity))[2:4])))*prevalance_brfss_census$OBESITY_CrudePrev
p2 = age_greater_than_40_p = 1 - prevalance_brfss_census$Age_18_39 
p11 = update_p11(p1,p2,r)
c1 = prevalance_of_ObesityObese_III_given_age_greater_than_40 = p11/p2 
prevalance_of_ObesityObese_III_given_age_less_than_40 = p1/(1-p2) - (c1*p2/(1-p2))


prevalance_age_stratification  = cbind(prevalance_brfss_census$StateAbbr,
                                       prevalance_brfss_census$PlaceName,
                                       prevalance_brfss_census$PlaceFIPS,
                                       prevalance_brfss_census$Geographic.Area.Name,
                                       prevalance_brfss_census$Geolocation,
                                       prevalance_brfss_census$lat,
                                       prevalance_brfss_census$lon,
                                       prevalance_brfss_census$county,
                                       prevalance_of_male_given_age_greater_than_40,
                                       prevalance_of_male_given_age_less_than_40,
                                       prevalance_of_diabetesYes_controlled_given_age_greater_than_40, 
                                       prevalance_of_diabetesYes_controlled_given_age_less_than_40,
prevalance_of_diabetesYes_uncontrolled_given_age_greater_than_40,
prevalance_of_diabetesYes_uncontrolled_given_age_less_than_40,
prevalance_of_smoking_statusCurrent_given_age_greater_than_40,
prevalance_of_smoking_statusCurrent_given_age_less_than_40 ,
prevalance_of_smoking_statusFormer_given_age_greater_than_40,
prevalance_of_smoking_statusFormer_given_age_less_than_40,
prevalance_of_rhemumatoidYes_given_age_greater_than_40 ,
prevalance_of_rhemumatoidYes_given_age_less_than_40,
prevalance_of_asthmaYes_given_age_greater_than_40,
prevalance_of_asthmaYes_given_age_less_than_40,
prevalance_of_strokeYes_given_age_greater_than_40,
prevalance_of_strokeYes_given_age_less_than_40,
prevalance_of_heart_diseaseYes_given_age_greater_than_40,
prevalance_of_heart_diseaseYes_given_age_less_than_40,
prevalance_of_hypertensionhighbp_given_age_greater_than_40, 
prevalance_of_hypertensionhighbp_given_age_less_than_40 ,
prevalance_of_resp_ex_asthmaYes_given_age_greater_than_40,
prevalance_of_resp_ex_asthmaYes_given_age_less_than_40,
prevalance_of_race_ethnicityBlack_given_age_greater_than_40,
prevalance_of_race_ethnicityBlack_given_age_less_than_40 ,
prevalance_of_race_ethnicityHispanic_given_age_greater_than_40,
prevalance_of_race_ethnicityHispanic_given_age_less_than_40,
prevalance_of_hematoless_than_1_yr_given_age_greater_than_40,  
prevalance_of_hematoless_than_1_yr_given_age_less_than_40,
prevalance_of_hemato1_5yr_given_age_greater_than_40 ,
prevalance_of_hemato1_5yr_given_age_less_than_40 ,
prevalance_of_hematogreater_than_5_yr_given_age_greater_than_40,
prevalance_of_hematogreater_than_5_yr_given_age_less_than_40,
prevalance_of_nonhematoless_than_1_yr_given_age_greater_than_40,
prevalance_of_nonhematoless_than_1_yr_given_age_less_than_40,
prevalance_of_nonhemato1_5yr_given_age_greater_than_40 ,
prevalance_of_nonhemato1_5yr_given_age_less_than_40 ,
prevalance_of_nonhematogreater_than_5_yr_given_age_greater_than_40,
prevalance_of_nonhematogreater_than_5_yr_given_age_less_than_40,
prevalance_of_ObesityObese_I_given_age_greater_than_40 ,
prevalance_of_ObesityObese_I_given_age_less_than_40,
prevalance_of_ObesityObese_II_given_age_greater_than_40,
prevalance_of_ObesityObese_II_given_age_less_than_40,
prevalance_of_ObesityObese_III_given_age_greater_than_40, 
prevalance_of_ObesityObese_III_given_age_less_than_40,
prevalance_of_kidney_diseaseYes_given_age_greater_than_40, 
prevalance_of_kidney_diseaseYes_given_age_less_than_40)

colnames(prevalance_age_stratification)[1:8] = c("StateAbbr", "PlaceName", "PlaceFIPS", "Geographic.Area.Name", "Geolocation", "lat",
                                                 "lon",
                                                 "county")
saveRDS(prevalance_age_stratification, "~/Dropbox/NHANES_risk_score/500cities_data/Analysis/data_created/prevalance_age_stratification.rds")
