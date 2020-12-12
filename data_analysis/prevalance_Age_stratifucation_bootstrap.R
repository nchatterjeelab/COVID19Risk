library(fastDummies)
library(Hmisc)
update_p11 = function(p1,p2,r){
  a = 1 - r
  b = 1 + (r-1)*(p1+p2)
  c = -r*p1*p2
  (-b+sqrt(b^2-4*a*c))/(2*a)
}

prevalance_brfss_census = load("data_created/Bootstrap_samples/combined_updated_bootstrap.RData")
#prevalance_brfss_census = readRDS('~/Dropbox/NHANES_risk_score/Nature Medicine Revision/Github_revision/COVID19Risk/data_created/combined_updated.rds')
#prevalance_brfss_census = prevalance_brfss_census %>% mutate("Age_45_74" = Age_45_54 + Age_55_64 + Age_65_74, "Age_75+" = Age_75_84 + Age_85)
#nhis_data = readRDS('~/Dropbox/NHANES_risk_score/Nature Medicine Revision/Github_revision/COVID19Risk/data_created/nhis_2017.rds')
nhis_data_bs = readRDS('~/Dropbox/NHANES_risk_score/Nature Medicine Revision/Github_revision/COVID19Risk/data_created/Bootstrap_samples/NHIS_imputed_bootstrap.rds')


age_stratification_prevalance = list()

for(i in 1:1000)
{
  prevalance_brfss_census = citydata[[i]]
  prevalance_brfss_census = prevalance_brfss_census %>% mutate(Age_45_74 = Age_45_54 + Age_55_64 + Age_65_74) %>%
    mutate("Age_75+" = Age_75_84 + Age_85)
    
  nhis_data = nhis_data_bs[[i]] %>% mutate(agegroup.cdc = case_when(age >= 15  & age < 45 ~ '15_45',
                                                                    age >= 45  & age < 55 ~ '45_54',
                                                                    age >= 55  & age < 65 ~ '55_64',
                                                                    age >= 65  & age < 75 ~ '65_74',
                                                                    age >= 75  & age < 85 ~ '75_84',
                                                                    age >= 85  & age < 100 ~ '85+'))
  
  additional_weights = nhis_data[, c(25:26)]
  nhis_data  = nhis_data[,c(5:6, 8:9, 66, 10:18,23,24,20, 21,22)]
  nhis_data = nhis_data %>% mutate(age_mixture_cat = case_when(agegroup.cdc == "45_54" | agegroup.cdc == "55_64" | agegroup.cdc == "65_74" ~ '45_74',
                                                               agegroup.cdc == "75_84" | agegroup.cdc == "85+" ~ '75above',
                                                               agegroup.cdc == "15_45" ~ '15_44'))
  nhis_data = cbind(nhis_data, additional_weights)
  
  # additional_weights = nhis_data[, c(37:38)]
  # nhis_data  = nhis_data[,c(6:7, 9:24,34,35,36, 30,31)]
  # nhis_data= nhis_data %>% mutate(age_mixture_cat = case_when(agegroup.cdc == "45-54" | agegroup.cdc == "55_64" | agegroup.cdc == "65_74" ~ '45_74',
  #                                                             agegroup.cdc == "75-84" | agegroup.cdc == "85+" ~ '75above',
  #                                                             agegroup.cdc == "15_45" ~ '15_44'))
  # nhis_data = cbind(nhis_data, additional_weights)
  
  #----Throughout age_mixture_cat40_and_above is 1 if age is greater than 40
  
  #----Age-Sex---#
  
  data = nhis_data[, c(1,2,20, 21:22)]
  data_complete = data[complete.cases(data), ]
  data_dummy = dummy_cols(data, select_columns = c("age_mixture_cat", "sex"))
  colnames(data_dummy)[1] = "sampling_weights"
  dstrat<-svydesign(ids=~1,weights=~sampling_weights, data = data_dummy)
  dstrat<-svydesign(id=~PPSU, strata=~PSTRAT,
                    nest = TRUE,
                    weights=~sampling_weights, data = data_dummy)
  
  
  logistic_fit_adj =svyglm(age_mixture_cat_45_74 ~ sex_Male, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = male_p = prevalance_brfss_census$male_proportion
  p2 = age_mixture_cat45_74 = prevalance_brfss_census$Age_45_74 
  p11 = update_p11(p1,p2,r)
  prevalance_of_male_given_age_45_74 = p11/p2 
  
  logistic_fit_adj =svyglm(age_mixture_cat_75above ~ sex_Male, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = male_p = prevalance_brfss_census$male_proportion
  p2 = age_mixture_cat45_74 = prevalance_brfss_census$`Age_75+`
  p11 = update_p11(p1,p2,r)
  prevalance_of_male_given_age_75above = p11/p2 
  
  logistic_fit_adj =svyglm(age_mixture_cat_15_44 ~ sex_Male, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = male_p = prevalance_brfss_census$male_proportion
  p2 = age_mixture_cat15_44 = prevalance_brfss_census$Age_15_44
  p11 = update_p11(p1,p2,r)
  prevalance_of_male_given_age_15_44 = p11/p2
  
  
  #----Age-Diabetes---#
  data = readRDS("data_created/Bootstrap_samples/NHANES_individual_level_bootstrap.rds")
  data = data[[i]]
  data = data %>% mutate(diabetes_Controlled = 1 - uncontrolled)
  colnames(data)[c(3,8)] = c("sampling_weights", "diabetes_Uncontrolled" )
  # data$diabetes[which(data$diabetes == "No")] = "A"
  # data  = data[-which(is.na(data$diabetes) == TRUE),] 
  data_dummy = dummy_cols(data, select_columns = c("age_mixture_cat"))
  # colnames(data_dummy)[3] = "sampling_weights"
  # data_dummy = data_dummy[-which(data_dummy$sampling_weights == 0), ]
  # dstrat<-svydesign(ids=~1,weights=~sampling_weights, data = data_dummy)
  dstrat<-svydesign(id=~sdmvpsu, strata=~sdmvstra,
                    nest = TRUE,
                    weights=~sampling_weights, data = data_dummy)
  
  dsrtat = subset(dstrat, inAnalysis)
  logistic_fit_adj =svyglm(age_mixture_cat_45_74 ~ diabetes_Controlled , design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = diabetes_controlled_p = prevalance_brfss_census$DIABETES_ctrled_CrudePrev
  p2 = age_mixture_cat45_74 = prevalance_brfss_census$Age_45_74  
  p11 = update_p11(p1,p2,r)
  prevalance_of_diabetesYes_controlled_given_age_45_74 = p11/p2 
  
  logistic_fit_adj =svyglm(age_mixture_cat_75above ~ diabetes_Controlled , design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = diabetes_controlled_p = prevalance_brfss_census$DIABETES_ctrled_CrudePrev
  p2 = age_mixture_cat75_above = prevalance_brfss_census$`Age_75+`  
  p11 = update_p11(p1,p2,r)
  prevalance_of_diabetesYes_controlled_given_age_75above = p11/p2 
  
  
  logistic_fit_adj =svyglm(age_mixture_cat_15_44 ~ diabetes_Controlled , design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = diabetes_controlled_p = prevalance_brfss_census$DIABETES_ctrled_CrudePrev
  p2 = age_mixture_cat15_44 = prevalance_brfss_census$Age_15_44  
  p11 = update_p11(p1,p2,r)
  prevalance_of_diabetesYes_controlled_given_age_15_44 = p11/p2 
  
  
  #Uncontrolled
  logistic_fit_adj =svyglm(age_mixture_cat_45_74 ~ diabetes_Uncontrolled , design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = diabetes_uncontrolled_p = prevalance_brfss_census$DIABETES_unctrled_CrudePrev
  p2 = age_mixture_cat45_74 = prevalance_brfss_census$Age_45_74  
  p11 = update_p11(p1,p2,r)
  prevalance_of_diabetesYes_uncontrolled_given_age_45_74 = p11/p2 
  
  logistic_fit_adj =svyglm(age_mixture_cat_75above ~ diabetes_Uncontrolled , design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = diabetes_uncontrolled_p = prevalance_brfss_census$DIABETES_unctrled_CrudePrev
  p2 = age_mixture_cat75_above = prevalance_brfss_census$`Age_75+`  
  p11 = update_p11(p1,p2,r)
  prevalance_of_diabetesYes_uncontrolled_given_age_75above = p11/p2 
  
  
  logistic_fit_adj =svyglm(age_mixture_cat_15_44 ~ diabetes_Uncontrolled , design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = diabetes_uncontrolled_p = prevalance_brfss_census$DIABETES_unctrled_CrudePrev
  p2 = age_mixture_cat15_44 = prevalance_brfss_census$Age_15_44  
  p11 = update_p11(p1,p2,r)
  prevalance_of_diabetesYes_uncontrolled_given_age_15_44 = p11/p2 
  
  
  #----Age-Smoking---#
  data = nhis_data[, c(1,7,20, 21:22)]
  data$smoking_status[which(data$smoking_status == "NA")] = NA
  data$smoking_status[which(data$smoking_status == "Never")] = "A"
  data_complete = data[complete.cases(data), ]
  data_dummy = dummy_cols(data, select_columns = c("age_mixture_cat", "smoking_status"))
  dstrat<-svydesign(id=~PPSU, strata=~PSTRAT,
                    nest = TRUE,
                    weights=~sampling_weights, data = data_dummy)
  
  logistic_fit_adj =svyglm(age_mixture_cat_45_74 ~ smoking_status_Current, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = smoking_p = prevalance_brfss_census$smoking_current_proportion
  p2 = prevalance_brfss_census$Age_45_74
  p11 = update_p11(p1,p2,r)
  prevalance_of_smoking_statusCurrent_given_age_45_74 = p11/p2 
  
  logistic_fit_adj =svyglm(age_mixture_cat_75above ~ smoking_status_Current, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = smoking_p = prevalance_brfss_census$smoking_current_proportion
  p2 = prevalance_brfss_census$`Age_75+`
  p11 = update_p11(p1,p2,r)
  prevalance_of_smoking_statusCurrent_given_age_75above = p11/p2 
  
  logistic_fit_adj =svyglm(age_mixture_cat_15_44 ~ smoking_status_Current, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = smoking_p = prevalance_brfss_census$smoking_current_proportion
  p2 = prevalance_brfss_census$Age_15_44
  p11 = update_p11(p1,p2,r)
  prevalance_of_smoking_statusCurrent_given_age_15_44 = p11/p2 
  
  
  
  logistic_fit_adj =svyglm(age_mixture_cat_45_74 ~ smoking_status_Former, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = smoking_p = prevalance_brfss_census$smoking_ex_proportion
  p2 = prevalance_brfss_census$Age_45_74
  p11 = update_p11(p1,p2,r)
  prevalance_of_smoking_statusFormer_given_age_45_74 = p11/p2 
  
  logistic_fit_adj =svyglm(age_mixture_cat_75above ~ smoking_status_Former, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = smoking_p = prevalance_brfss_census$smoking_ex_proportion
  p2 = prevalance_brfss_census$`Age_75+`
  p11 = update_p11(p1,p2,r)
  prevalance_of_smoking_statusFormer_given_age_75above = p11/p2 
  
  logistic_fit_adj =svyglm(age_mixture_cat_15_44 ~ smoking_status_Former, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = smoking_p = prevalance_brfss_census$smoking_ex_proportion
  p2 = prevalance_brfss_census$Age_15_44
  p11 = update_p11(p1,p2,r)
  prevalance_of_smoking_statusFormer_given_age_15_44 = p11/p2
  
  #----Age-Rheumatoid---#
  data = nhis_data[, c(1,8,20, 21:22)]
  data$rheumatoid[which(data$rheumatoid == "NA")] = NA
  data_complete = data[complete.cases(data), ]
  data_dummy = dummy_cols(data, select_columns = c("age_mixture_cat", "rheumatoid"))
  dstrat<-svydesign(ids=~1,weights=~sampling_weights, data = data_dummy)
  dstrat<-svydesign(id=~PPSU, strata=~PSTRAT,
                    nest = TRUE,
                    weights=~sampling_weights, data = data_dummy)
  logistic_fit_adj =svyglm(age_mixture_cat_15_44 ~ rheumatoid_1, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = rheumatoidYes_p = prevalance_brfss_census$rheumatoid
  
  p2 = prevalance_brfss_census$Age_15_44 
  p11 = update_p11(p1,p2,r)
  prevalance_of_rheumatoidYes_given_age_15_44 = p11/p2 
  
  logistic_fit_adj =svyglm(age_mixture_cat_45_74 ~ rheumatoid_1, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = rheumatoidYes_p = prevalance_brfss_census$rheumatoid
  
  p2 = prevalance_brfss_census$Age_45_74 
  p11 = update_p11(p1,p2,r)
  prevalance_of_rheumatoidYes_given_age_45_74 = p11/p2 
  
  logistic_fit_adj =svyglm(age_mixture_cat_75above ~ rheumatoid_1, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = rheumatoidYes_p = prevalance_brfss_census$rheumatoid
  
  p2 = prevalance_brfss_census$`Age_75+` 
  p11 = update_p11(p1,p2,r)
  prevalance_of_rheumatoidYes_given_age_75above = p11/p2 
  
  
  #----Age-Asthma---#
  data = nhis_data[, c(1,9,20, 21:22)]
  data$asthma[which(data$asthma == "NA")] = NA
  data_complete = data[complete.cases(data), ]
  data_dummy = dummy_cols(data, select_columns = c("age_mixture_cat", "asthma"))
  dstrat<-svydesign(ids=~1,weights=~sampling_weights, data = data_dummy)
  dstrat<-svydesign(id=~PPSU, strata=~PSTRAT,
                    nest = TRUE,
                    weights=~sampling_weights, data = data_dummy)
  logistic_fit_adj =svyglm(age_mixture_cat_15_44 ~ asthma_1, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = asthmaYes_p = prevalance_brfss_census$CASTHMA_CrudePrev
  
  p2 = prevalance_brfss_census$Age_15_44 
  p11 = update_p11(p1,p2,r)
  prevalance_of_asthmaYes_given_age_15_44 = p11/p2 
  
  logistic_fit_adj =svyglm(age_mixture_cat_45_74 ~ asthma_1, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = asthmaYes_p = prevalance_brfss_census$CASTHMA_CrudePrev
  
  p2 = prevalance_brfss_census$Age_45_74 
  p11 = update_p11(p1,p2,r)
  prevalance_of_asthmaYes_given_age_45_74 = p11/p2 
  
  logistic_fit_adj =svyglm(age_mixture_cat_75above ~ asthma_1, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = asthmaYes_p = prevalance_brfss_census$CASTHMA_CrudePrev
  
  p2 = prevalance_brfss_census$`Age_75+` 
  p11 = update_p11(p1,p2,r)
  prevalance_of_asthmaYes_given_age_75above = p11/p2 
  
  
  
  #----Age-Stroke---#
  data = nhis_data[, c(1,10,20, 21:22)]
  data$stroke[which(data$stroke == "NA")] = NA
  data_complete = data[complete.cases(data), ]
  data_dummy = dummy_cols(data, select_columns = c("age_mixture_cat", "stroke"))
  dstrat<-svydesign(ids=~1,weights=~sampling_weights, data = data_dummy)
  dstrat<-svydesign(id=~PPSU, strata=~PSTRAT,
                    nest = TRUE,
                    weights=~sampling_weights, data = data_dummy)
  logistic_fit_adj =svyglm(age_mixture_cat_15_44 ~ stroke_1, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = strokeYes_p = prevalance_brfss_census$STROKE_CrudePrev
  
  p2 = prevalance_brfss_census$Age_15_44 
  p11 = update_p11(p1,p2,r)
  prevalance_of_strokeYes_given_age_15_44 = p11/p2 
  
  logistic_fit_adj =svyglm(age_mixture_cat_45_74 ~ stroke_1, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = strokeYes_p = prevalance_brfss_census$STROKE_CrudePrev
  
  p2 = prevalance_brfss_census$Age_45_74 
  p11 = update_p11(p1,p2,r)
  prevalance_of_strokeYes_given_age_45_74 = p11/p2 
  
  logistic_fit_adj =svyglm(age_mixture_cat_75above ~ stroke_1, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = strokeYes_p = prevalance_brfss_census$STROKE_CrudePrev
  
  p2 = prevalance_brfss_census$`Age_75+` 
  p11 = update_p11(p1,p2,r)
  prevalance_of_strokeYes_given_age_75above = p11/p2 
  
  #----Age-Heart---#
  data = nhis_data[, c(1,11,20, 21:22)]
  data$heart_disease[which(data$heart_disease == "NA")] = NA
  data_complete = data[complete.cases(data), ]
  data_dummy = dummy_cols(data, select_columns = c("age_mixture_cat", "heart_disease"))
  dstrat<-svydesign(ids=~1,weights=~sampling_weights, data = data_dummy)
  dstrat<-svydesign(id=~PPSU, strata=~PSTRAT,
                    nest = TRUE,
                    weights=~sampling_weights, data = data_dummy)
  logistic_fit_adj =svyglm(age_mixture_cat_15_44 ~ heart_disease_Yes, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = heart_diseaseYes_p = prevalance_brfss_census$CHD_CrudePrev
  
  p2 = prevalance_brfss_census$Age_15_44 
  p11 = update_p11(p1,p2,r)
  prevalance_of_heart_diseaseYes_given_age_15_44 = p11/p2 
  
  logistic_fit_adj =svyglm(age_mixture_cat_45_74 ~ heart_disease_Yes, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = heart_diseaseYes_p = prevalance_brfss_census$CHD_CrudePrev
  
  p2 = prevalance_brfss_census$Age_45_74 
  p11 = update_p11(p1,p2,r)
  prevalance_of_heart_diseaseYes_given_age_45_74 = p11/p2 
  
  logistic_fit_adj =svyglm(age_mixture_cat_75above ~ heart_disease_Yes, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = heart_diseaseYes_p = prevalance_brfss_census$CHD_CrudePrev
  
  p2 = prevalance_brfss_census$`Age_75+` 
  p11 = update_p11(p1,p2,r)
  prevalance_of_heart_diseaseYes_given_age_75above = p11/p2 
  
  
  #----Age-Hypertension---#
  data = nhis_data[, c(1,12,20, 21:22)]
  data$hypertension[which(data$hypertension == "NA")] = NA
  data_complete = data[complete.cases(data), ]
  data_dummy = dummy_cols(data, select_columns = c("age_mixture_cat", "hypertension"))
  dstrat<-svydesign(ids=~1,weights=~sampling_weights, data = data_dummy)
  dstrat<-svydesign(id=~PPSU, strata=~PSTRAT,
                    nest = TRUE,
                    weights=~sampling_weights, data = data_dummy)
  logistic_fit_adj =svyglm(age_mixture_cat_15_44 ~ hypertension_Hypertension_high_bp, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = hypertensionYes_p = prevalance_brfss_census$BPHIGH_CrudePrev
  
  p2 = prevalance_brfss_census$Age_15_44 
  p11 = update_p11(p1,p2,r)
  prevalance_of_hypertensionYes_given_age_15_44 = p11/p2 
  
  logistic_fit_adj =svyglm(age_mixture_cat_45_74 ~ hypertension_Hypertension_high_bp, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = hypertensionYes_p = prevalance_brfss_census$BPHIGH_CrudePrev
  
  p2 = prevalance_brfss_census$Age_45_74 
  p11 = update_p11(p1,p2,r)
  prevalance_of_hypertensionYes_given_age_45_74 = p11/p2 
  
  logistic_fit_adj =svyglm(age_mixture_cat_75above ~ hypertension_Hypertension_high_bp, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = hypertensionYes_p = prevalance_brfss_census$BPHIGH_CrudePrev
  
  p2 = prevalance_brfss_census$`Age_75+` 
  p11 = update_p11(p1,p2,r)
  prevalance_of_hypertensionYes_given_age_75above = p11/p2 
  
  #----Age-Resp_ex_asthma---#
  data = nhis_data[, c(1,13,20, 21:22)]
  data$resp_ex_asthma[which(data$resp_ex_asthma == "NA")] = NA
  data_complete = data[complete.cases(data), ]
  data_dummy = dummy_cols(data, select_columns = c("age_mixture_cat", "resp_ex_asthma"))
  dstrat<-svydesign(ids=~1,weights=~sampling_weights, data = data_dummy)
  dstrat<-svydesign(id=~PPSU, strata=~PSTRAT,
                    nest = TRUE,
                    weights=~sampling_weights, data = data_dummy)
  logistic_fit_adj =svyglm(age_mixture_cat_15_44 ~ resp_ex_asthma_Yes, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = resp_ex_asthmaYes_p = prevalance_brfss_census$COPD_CrudePrev
  
  p2 = prevalance_brfss_census$Age_15_44 
  p11 = update_p11(p1,p2,r)
  prevalance_of_resp_ex_asthmaYes_given_age_15_44 = p11/p2 
  
  logistic_fit_adj =svyglm(age_mixture_cat_45_74 ~ resp_ex_asthma_Yes, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = resp_ex_asthmaYes_p = prevalance_brfss_census$COPD_CrudePrev
  
  p2 = prevalance_brfss_census$Age_45_74 
  p11 = update_p11(p1,p2,r)
  prevalance_of_resp_ex_asthmaYes_given_age_45_74 = p11/p2 
  
  logistic_fit_adj =svyglm(age_mixture_cat_75above ~ resp_ex_asthma_Yes, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = resp_ex_asthmaYes_p = prevalance_brfss_census$COPD_CrudePrev
  
  p2 = prevalance_brfss_census$`Age_75+` 
  p11 = update_p11(p1,p2,r)
  prevalance_of_resp_ex_asthmaYes_given_age_75above = p11/p2 
  
  
  #----Age-Kidney---#
  data = nhis_data[, c(1,14,20, 21:22)]
  data$kidney_disease[which(data$kidney_disease == "NA")] = NA
  data_complete = data[complete.cases(data), ]
  data_dummy = dummy_cols(data, select_columns = c("age_mixture_cat", "kidney_disease"))
  dstrat<-svydesign(ids=~1,weights=~sampling_weights, data = data_dummy)
  dstrat<-svydesign(id=~PPSU, strata=~PSTRAT,
                    nest = TRUE,
                    weights=~sampling_weights, data = data_dummy)
  
  logistic_fit_adj =svyglm(age_mixture_cat_15_44 ~ kidney_disease_1, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = kidney_diseaseYes_p = prevalance_brfss_census$KIDNEY_CrudePrev
  
  p2 = prevalance_brfss_census$Age_15_44 
  p11 = update_p11(p1,p2,r)
  prevalance_of_kidney_diseaseYes_given_age_15_44 = p11/p2 
  
  logistic_fit_adj =svyglm(age_mixture_cat_45_74 ~ kidney_disease_1, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = kidney_diseaseYes_p = prevalance_brfss_census$KIDNEY_CrudePrev
  
  p2 = prevalance_brfss_census$Age_45_74 
  p11 = update_p11(p1,p2,r)
  prevalance_of_kidney_diseaseYes_given_age_45_74 = p11/p2 
  
  logistic_fit_adj =svyglm(age_mixture_cat_75above ~ kidney_disease_1, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = kidney_diseaseYes_p = prevalance_brfss_census$KIDNEY_CrudePrev
  
  p2 = prevalance_brfss_census$`Age_75+` 
  p11 = update_p11(p1,p2,r)
  prevalance_of_kidney_diseaseYes_given_age_75above = p11/p2 
  
  
  #----Age-Race_ethnicity---#
  data = nhis_data[, c(1,17,20, 21:22)]
  #data$race_ethnicity[which(data$race_ethnicity == "NA")] = NA
  data_complete = data[complete.cases(data), ]
  data_dummy = dummy_cols(data, select_columns = c("age_mixture_cat", "race_ethnicity.cdc"))
  dstrat<-svydesign(ids=~1,weights=~sampling_weights, data = data_dummy)
  dstrat<-svydesign(id=~PPSU, strata=~PSTRAT,
                    nest = TRUE,
                    weights=~sampling_weights, data = data_dummy)
  
  logistic_fit_adj =svyglm(age_mixture_cat_15_44 ~ race_ethnicity.cdc_Non_hispanic_Black, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = black_p = prevalance_brfss_census$proportion_black
  
  p2 = prevalance_brfss_census$Age_15_44 
  p11 = update_p11(p1,p2,r)
  prevalance_of_black_given_age_15_44 = p11/p2 
  
  logistic_fit_adj =svyglm(age_mixture_cat_45_74 ~ race_ethnicity.cdc_Non_hispanic_Black, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = black_p = prevalance_brfss_census$proportion_black
  
  p2 = prevalance_brfss_census$Age_45_74 
  p11 = update_p11(p1,p2,r)
  prevalance_of_black_given_age_45_74 = p11/p2 
  
  logistic_fit_adj =svyglm(age_mixture_cat_75above ~ race_ethnicity.cdc_Non_hispanic_Black, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = black_p = prevalance_brfss_census$proportion_black
  
  p2 = prevalance_brfss_census$`Age_75+` 
  p11 = update_p11(p1,p2,r)
  prevalance_of_black_given_age_75above = p11/p2 
  
  
  #Asian
  logistic_fit_adj =svyglm(age_mixture_cat_15_44 ~ race_ethnicity.cdc_Non_hispanic_asian, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = asian_p = prevalance_brfss_census$proportion_asian
  
  p2 = prevalance_brfss_census$Age_15_44 
  p11 = update_p11(p1,p2,r)
  prevalance_of_asian_given_age_15_44 = p11/p2 
  
  logistic_fit_adj =svyglm(age_mixture_cat_45_74 ~ race_ethnicity.cdc_Non_hispanic_asian, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = asian_p = prevalance_brfss_census$proportion_asian
  
  p2 = prevalance_brfss_census$Age_45_74 
  p11 = update_p11(p1,p2,r)
  prevalance_of_asian_given_age_45_74 = p11/p2 
  
  logistic_fit_adj =svyglm(age_mixture_cat_75above ~ race_ethnicity.cdc_Non_hispanic_asian, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = asian_p = prevalance_brfss_census$proportion_asian
  
  p2 = prevalance_brfss_census$`Age_75+` 
  p11 = update_p11(p1,p2,r)
  prevalance_of_asian_given_age_75above = p11/p2 
  
  
  
  #American-Indian
  logistic_fit_adj =svyglm(age_mixture_cat_15_44 ~ race_ethnicity.cdc_Non_hispanic_american_Indian, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = aian_p = prevalance_brfss_census$proportion_american_indian_alaska_native
  
  p2 = prevalance_brfss_census$Age_15_44 
  p11 = update_p11(p1,p2,r)
  prevalance_of_aian_given_age_15_44 = p11/p2 
  
  logistic_fit_adj =svyglm(age_mixture_cat_45_74 ~ race_ethnicity.cdc_Non_hispanic_american_Indian, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = aian_p = prevalance_brfss_census$proportion_american_indian_alaska_native
  
  p2 = prevalance_brfss_census$Age_45_74 
  p11 = update_p11(p1,p2,r)
  prevalance_of_aian_given_age_45_74 = p11/p2 
  
  logistic_fit_adj =svyglm(age_mixture_cat_75above ~ race_ethnicity.cdc_Non_hispanic_american_Indian, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = aian_p = prevalance_brfss_census$proportion_american_indian_alaska_native
  
  p2 = prevalance_brfss_census$`Age_75+` 
  p11 = update_p11(p1,p2,r)
  prevalance_of_aian_given_age_75above = p11/p2 
  
  
  
  #Hispanic
  logistic_fit_adj =svyglm(age_mixture_cat_15_44 ~ race_ethnicity.cdc_Hispanic, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = hispanic_p = prevalance_brfss_census$proportion_hispanic
  
  p2 = prevalance_brfss_census$Age_15_44 
  p11 = update_p11(p1,p2,r)
  prevalance_of_hispanic_given_age_15_44 = p11/p2 
  
  logistic_fit_adj =svyglm(age_mixture_cat_45_74 ~ race_ethnicity.cdc_Hispanic, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = hispanic_p = prevalance_brfss_census$proportion_hispanic
  
  p2 = prevalance_brfss_census$Age_45_74 
  p11 = update_p11(p1,p2,r)
  prevalance_of_hispanic_given_age_45_74 = p11/p2 
  
  logistic_fit_adj =svyglm(age_mixture_cat_75above ~ race_ethnicity.cdc_Hispanic, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = hispanic_p = prevalance_brfss_census$proportion_hispanic
  
  p2 = prevalance_brfss_census$`Age_75+` 
  p11 = update_p11(p1,p2,r)
  prevalance_of_hispanic_given_age_75above = p11/p2 
  
  
  
  
  
  
  #----Age-Hematologic---#
  data = nhis_data[, c(1,15,18,20, 21:22)]
  data = data  %>% mutate(hemato = case_when(hematologic_cancer == "None" ~ 'No',
                                             hematologic_cancer == "Hematological" &  diagnoses_cancer == "greater_than_5_yr" ~ 'greater_than_5_yr',
                                             hematologic_cancer == "Hematological" &  diagnoses_cancer == "1_5yr" ~ '1_5yr',
                                             hematologic_cancer == "Hematological" &  diagnoses_cancer == "less_than_1_yr" ~ 'less_than_1_yr'))
  data_hemato = data[, c(1,4,5,6,7)]
  data_complete = data_hemato[complete.cases(data_hemato), ]
  data_hemato$hemato[which(data_hemato$hemato == "No")] = "0yr"
  
  
  
  data_dummy = dummy_cols(data_hemato, select_columns = c("age_mixture_cat", "hemato"))
  dstrat<-svydesign(ids=~1,weights=~sampling_weights, data = data_dummy)
  dstrat<-svydesign(id=~PPSU, strata=~PSTRAT,
                    nest = TRUE,
                    weights=~sampling_weights, data = data_dummy)
  logistic_fit_adj =svyglm(age_mixture_cat_15_44 ~ hemato_less_than_1_yr, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = hemato_less_than_1_yr_p = prevalance_brfss_census$hematologic_cancer1
  
  p2 = prevalance_brfss_census$Age_15_44 
  p11 = update_p11(p1,p2,r)
  prevalance_of_hemato_less_than_1_yr_given_age_15_44 = p11/p2 
  
  logistic_fit_adj =svyglm(age_mixture_cat_45_74 ~ hemato_less_than_1_yr, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = hemato_less_than_1_yr_p = prevalance_brfss_census$hematologic_cancer1
  
  p2 = prevalance_brfss_census$Age_45_74 
  p11 = update_p11(p1,p2,r)
  prevalance_of_hemato_less_than_1_yr_given_age_45_74 = p11/p2 
  
  logistic_fit_adj =svyglm(age_mixture_cat_75above ~ hemato_less_than_1_yr, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = hemato_less_than_1_yr_p = prevalance_brfss_census$hematologic_cancer1
  
  p2 = prevalance_brfss_census$`Age_75+` 
  p11 = update_p11(p1,p2,r)
  prevalance_of_hemato_less_than_1_yr_given_age_75above = p11/p2 
  
  logistic_fit_adj =svyglm(age_mixture_cat_15_44 ~ hemato_1_5yr, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = hemato_1_5yr_p = prevalance_brfss_census$hematologic_cancer2
  
  p2 = prevalance_brfss_census$Age_15_44 
  p11 = update_p11(p1,p2,r)
  prevalance_of_hemato_1_5yr_given_age_15_44 = p11/p2 
  
  logistic_fit_adj =svyglm(age_mixture_cat_45_74 ~ hemato_1_5yr, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = hemato_1_5yr_p = prevalance_brfss_census$hematologic_cancer2
  
  p2 = prevalance_brfss_census$Age_45_74 
  p11 = update_p11(p1,p2,r)
  prevalance_of_hemato_1_5yr_given_age_45_74 = p11/p2 
  
  logistic_fit_adj =svyglm(age_mixture_cat_75above ~ hemato_1_5yr, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = hemato_1_5yr_p = prevalance_brfss_census$hematologic_cancer2
  
  p2 = prevalance_brfss_census$`Age_75+` 
  p11 = update_p11(p1,p2,r)
  prevalance_of_hemato_1_5yr_given_age_75above = p11/p2 
  
  logistic_fit_adj =svyglm(age_mixture_cat_15_44 ~ hemato_greater_than_5_yr, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = hemato_greater_than_5_yr_p = prevalance_brfss_census$hematologic_cancer3
  
  p2 = prevalance_brfss_census$Age_15_44 
  p11 = update_p11(p1,p2,r)
  prevalance_of_hemato_greater_than_5_yr_given_age_15_44 = p11/p2 
  
  logistic_fit_adj =svyglm(age_mixture_cat_45_74 ~ hemato_greater_than_5_yr, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = hemato_greater_than_5_yr_p = prevalance_brfss_census$hematologic_cancer3
  
  p2 = prevalance_brfss_census$Age_45_74 
  p11 = update_p11(p1,p2,r)
  prevalance_of_hemato_greater_than_5_yr_given_age_45_74 = p11/p2 
  
  logistic_fit_adj =svyglm(age_mixture_cat_75above ~ hemato_greater_than_5_yr, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = hemato_greater_than_5_yr_p = prevalance_brfss_census$hematologic_cancer3
  
  p2 = prevalance_brfss_census$`Age_75+` 
  p11 = update_p11(p1,p2,r)
  prevalance_of_hemato_greater_than_5_yr_given_age_75above = p11/p2 
  
  
  
  
  #----Age-non_hematologic---#
  data = nhis_data[, c(1,15,19,20, 21:22)]
  data = data  %>% mutate(nonhemato = case_when(non_hematologic_cancer == "None" ~ 'No',
                                                non_hematologic_cancer == "Non_hematological" &  diagnoses_cancer == "greater_than_5_yr" ~ 'greater_than_5_yr',
                                                non_hematologic_cancer == "Non_hematological" &  diagnoses_cancer == "1_5yr" ~ '1_5yr',
                                                non_hematologic_cancer == "Non_hematological" &  diagnoses_cancer == "less_than_1_yr" ~ 'less_than_1_yr'))
  data_nonhemato = data[, c(1,4,5,6,7)]
  data_complete = data_nonhemato[complete.cases(data_nonhemato), ]
  data_dummy = dummy_cols(data_nonhemato, select_columns = c("age_mixture_cat", "nonhemato"))
  dstrat<-svydesign(ids=~1,weights=~sampling_weights, data = data_dummy)
  dstrat<-svydesign(id=~PPSU, strata=~PSTRAT,
                    nest = TRUE,
                    weights=~sampling_weights, data = data_dummy)
  
  logistic_fit_adj =svyglm(age_mixture_cat_15_44 ~ nonhemato_less_than_1_yr, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = nonhemato_less_than_1_yr_p = prevalance_brfss_census$non_hematologic_cancer1
  
  p2 = prevalance_brfss_census$Age_15_44 
  p11 = update_p11(p1,p2,r)
  prevalance_of_nonhemato_less_than_1_yr_given_age_15_44 = p11/p2 
  
  logistic_fit_adj =svyglm(age_mixture_cat_45_74 ~ nonhemato_less_than_1_yr, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = nonhemato_less_than_1_yr_p = prevalance_brfss_census$non_hematologic_cancer1
  
  p2 = prevalance_brfss_census$Age_45_74 
  p11 = update_p11(p1,p2,r)
  prevalance_of_nonhemato_less_than_1_yr_given_age_45_74 = p11/p2 
  
  logistic_fit_adj =svyglm(age_mixture_cat_75above ~ nonhemato_less_than_1_yr, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = nonhemato_less_than_1_yr_p = prevalance_brfss_census$non_hematologic_cancer1
  
  p2 = prevalance_brfss_census$`Age_75+` 
  p11 = update_p11(p1,p2,r)
  prevalance_of_nonhemato_less_than_1_yr_given_age_75above = p11/p2 
  
  logistic_fit_adj =svyglm(age_mixture_cat_15_44 ~ nonhemato_1_5yr, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = nonhemato_1_5yr_p = prevalance_brfss_census$non_hematologic_cancer2
  
  p2 = prevalance_brfss_census$Age_15_44 
  p11 = update_p11(p1,p2,r)
  prevalance_of_nonhemato_1_5yr_given_age_15_44 = p11/p2 
  
  logistic_fit_adj =svyglm(age_mixture_cat_45_74 ~ nonhemato_1_5yr, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = nonhemato_1_5yr_p = prevalance_brfss_census$non_hematologic_cancer2
  
  p2 = prevalance_brfss_census$Age_45_74 
  p11 = update_p11(p1,p2,r)
  prevalance_of_nonhemato_1_5yr_given_age_45_74 = p11/p2 
  
  logistic_fit_adj =svyglm(age_mixture_cat_75above ~ nonhemato_1_5yr, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = nonhemato_1_5yr_p = prevalance_brfss_census$non_hematologic_cancer2
  
  p2 = prevalance_brfss_census$`Age_75+` 
  p11 = update_p11(p1,p2,r)
  prevalance_of_nonhemato_1_5yr_given_age_75above = p11/p2 
  
  logistic_fit_adj =svyglm(age_mixture_cat_15_44 ~ nonhemato_greater_than_5_yr, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = nonhemato_greater_than_5_yr_p = prevalance_brfss_census$non_hematologic_cancer3
  
  p2 = prevalance_brfss_census$Age_15_44 
  p11 = update_p11(p1,p2,r)
  prevalance_of_nonhemato_greater_than_5_yr_given_age_15_44 = p11/p2 
  
  logistic_fit_adj =svyglm(age_mixture_cat_45_74 ~ nonhemato_greater_than_5_yr, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = nonhemato_greater_than_5_yr_p = prevalance_brfss_census$non_hematologic_cancer3
  
  p2 = prevalance_brfss_census$Age_45_74 
  p11 = update_p11(p1,p2,r)
  prevalance_of_nonhemato_greater_than_5_yr_given_age_45_74 = p11/p2 
  
  logistic_fit_adj =svyglm(age_mixture_cat_75above ~ nonhemato_greater_than_5_yr, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = nonhemato_greater_than_5_yr_p = prevalance_brfss_census$non_hematologic_cancer3
  
  p2 = prevalance_brfss_census$`Age_75+` 
  p11 = update_p11(p1,p2,r)
  prevalance_of_nonhemato_greater_than_5_yr_given_age_75above = p11/p2 
  
  
  #----Age-Obesity---#
  data = nhis_data[, c(1,16,20,21,22)]
  data$Obesity[which(data$Obesity == "NA")] = NA
  data_complete = data[complete.cases(data), ]
  
  data_dummy = dummy_cols(data, select_columns = c("age_mixture_cat", "Obesity"))
  dstrat<-svydesign(ids=~1,weights=~sampling_weights, data = data_dummy)
  dstrat<-svydesign(id=~PPSU, strata=~PSTRAT,
                    nest = TRUE,
                    weights=~sampling_weights, data = data_dummy)
  
  logistic_fit_adj =svyglm(age_mixture_cat_15_44 ~ Obesity_Obese_I, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = Obesity_Obese_I_p = prevalance_brfss_census$obesity1
  
  p2 = prevalance_brfss_census$Age_15_44 
  p11 = update_p11(p1,p2,r)
  prevalance_of_Obesity_Obese_I_given_age_15_44 = p11/p2 
  
  logistic_fit_adj =svyglm(age_mixture_cat_45_74 ~ Obesity_Obese_I, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = Obesity_Obese_I_p = prevalance_brfss_census$obesity1
  
  p2 = prevalance_brfss_census$Age_45_74 
  p11 = update_p11(p1,p2,r)
  prevalance_of_Obesity_Obese_I_given_age_45_74 = p11/p2 
  
  logistic_fit_adj =svyglm(age_mixture_cat_75above ~ Obesity_Obese_I, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = Obesity_Obese_I_p = prevalance_brfss_census$obesity1
  
  p2 = prevalance_brfss_census$`Age_75+` 
  p11 = update_p11(p1,p2,r)
  prevalance_of_Obesity_Obese_I_given_age_75above = p11/p2 
  
  logistic_fit_adj =svyglm(age_mixture_cat_15_44 ~ Obesity_Obese_II, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = Obesity_Obese_II_p = prevalance_brfss_census$obesity2
  
  p2 = prevalance_brfss_census$Age_15_44 
  p11 = update_p11(p1,p2,r)
  prevalance_of_Obesity_Obese_II_given_age_15_44 = p11/p2 
  
  logistic_fit_adj =svyglm(age_mixture_cat_45_74 ~ Obesity_Obese_II, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = Obesity_Obese_II_p = prevalance_brfss_census$obesity2
  
  p2 = prevalance_brfss_census$Age_45_74 
  p11 = update_p11(p1,p2,r)
  prevalance_of_Obesity_Obese_II_given_age_45_74 = p11/p2 
  
  logistic_fit_adj =svyglm(age_mixture_cat_75above ~ Obesity_Obese_II, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = Obesity_Obese_II_p = prevalance_brfss_census$obesity2
  
  p2 = prevalance_brfss_census$`Age_75+` 
  p11 = update_p11(p1,p2,r)
  prevalance_of_Obesity_Obese_II_given_age_75above = p11/p2 
  
  
  logistic_fit_adj =svyglm(age_mixture_cat_15_44 ~ Obesity_Obese_III, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = Obesity_Obese_III_p = prevalance_brfss_census$obesity3
  
  p2 = prevalance_brfss_census$Age_15_44 
  p11 = update_p11(p1,p2,r)
  prevalance_of_Obesity_Obese_III_given_age_15_44 = p11/p2 
  
  logistic_fit_adj =svyglm(age_mixture_cat_45_74 ~ Obesity_Obese_III, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = Obesity_Obese_III_p = prevalance_brfss_census$obesity3
  
  p2 = prevalance_brfss_census$Age_45_74 
  p11 = update_p11(p1,p2,r)
  prevalance_of_Obesity_Obese_III_given_age_45_74 = p11/p2 
  
  logistic_fit_adj =svyglm(age_mixture_cat_75above ~ Obesity_Obese_III, design=dstrat, family = quasibinomial())
  r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
  p1 = Obesity_Obese_III_p = prevalance_brfss_census$obesity3
  
  p2 = prevalance_brfss_census$`Age_75+` 
  p11 = update_p11(p1,p2,r)
  prevalance_of_Obesity_Obese_III_given_age_75above = p11/p2 
  
  #print(i)
  
  prevalance_age_stratification  = data.frame(cbind(
                                                    prevalance_brfss_census$PlaceFIPS,
                                            
                                                    prevalance_of_male_given_age_15_44,
                                                    prevalance_of_male_given_age_45_74,
                                                    prevalance_of_male_given_age_75above,
                                                    prevalance_of_diabetesYes_controlled_given_age_15_44,
                                                    prevalance_of_diabetesYes_controlled_given_age_45_74,
                                                    prevalance_of_diabetesYes_controlled_given_age_75above,
                                                    prevalance_of_diabetesYes_uncontrolled_given_age_15_44,
                                                    prevalance_of_diabetesYes_uncontrolled_given_age_45_74,
                                                    prevalance_of_diabetesYes_uncontrolled_given_age_75above,
                                                    prevalance_of_smoking_statusCurrent_given_age_15_44,
                                                    prevalance_of_smoking_statusCurrent_given_age_45_74,
                                                    prevalance_of_smoking_statusCurrent_given_age_75above,
                                                    prevalance_of_smoking_statusFormer_given_age_15_44,
                                                    prevalance_of_smoking_statusFormer_given_age_45_74,
                                                    prevalance_of_smoking_statusFormer_given_age_75above,
                                                    prevalance_of_rheumatoidYes_given_age_15_44,
                                                    prevalance_of_rheumatoidYes_given_age_45_74,
                                                    prevalance_of_rheumatoidYes_given_age_75above,
                                                    prevalance_of_asthmaYes_given_age_15_44,
                                                    prevalance_of_asthmaYes_given_age_45_74,
                                                    prevalance_of_asthmaYes_given_age_75above,
                                                    prevalance_of_strokeYes_given_age_15_44,
                                                    prevalance_of_strokeYes_given_age_45_74,
                                                    prevalance_of_strokeYes_given_age_75above,
                                                    prevalance_of_heart_diseaseYes_given_age_15_44,
                                                    prevalance_of_heart_diseaseYes_given_age_45_74,
                                                    prevalance_of_heart_diseaseYes_given_age_75above,
                                                    prevalance_of_hypertensionYes_given_age_15_44,
                                                    prevalance_of_hypertensionYes_given_age_45_74,
                                                    prevalance_of_hypertensionYes_given_age_75above,
                                                    prevalance_of_resp_ex_asthmaYes_given_age_15_44,
                                                    prevalance_of_resp_ex_asthmaYes_given_age_45_74,
                                                    prevalance_of_resp_ex_asthmaYes_given_age_75above,
                                                    prevalance_of_kidney_diseaseYes_given_age_15_44,
                                                    prevalance_of_kidney_diseaseYes_given_age_45_74,
                                                    prevalance_of_kidney_diseaseYes_given_age_75above,
                                                    prevalance_of_Obesity_Obese_I_given_age_15_44,
                                                    prevalance_of_Obesity_Obese_I_given_age_45_74,
                                                    prevalance_of_Obesity_Obese_I_given_age_75above,
                                                    prevalance_of_Obesity_Obese_II_given_age_15_44,
                                                    prevalance_of_Obesity_Obese_II_given_age_45_74,
                                                    prevalance_of_Obesity_Obese_II_given_age_75above,
                                                    prevalance_of_Obesity_Obese_III_given_age_15_44,
                                                    prevalance_of_Obesity_Obese_III_given_age_45_74,
                                                    prevalance_of_Obesity_Obese_III_given_age_75above,
                                                    prevalance_of_black_given_age_15_44,
                                                    prevalance_of_black_given_age_45_74,
                                                    prevalance_of_black_given_age_75above,
                                                    prevalance_of_asian_given_age_15_44,
                                                    prevalance_of_asian_given_age_45_74,
                                                    prevalance_of_asian_given_age_75above,
                                                    prevalance_of_aian_given_age_15_44,
                                                    prevalance_of_aian_given_age_45_74,
                                                    prevalance_of_aian_given_age_75above,
                                                    prevalance_of_hispanic_given_age_15_44,
                                                    prevalance_of_hispanic_given_age_45_74,
                                                    prevalance_of_hispanic_given_age_75above,
                                                    prevalance_of_hemato_1_5yr_given_age_15_44,
                                                    prevalance_of_hemato_1_5yr_given_age_45_74,
                                                    prevalance_of_hemato_1_5yr_given_age_75above,
                                                    prevalance_of_hemato_less_than_1_yr_given_age_15_44,
                                                    prevalance_of_hemato_less_than_1_yr_given_age_45_74,
                                                    prevalance_of_hemato_less_than_1_yr_given_age_75above,
                                                    prevalance_of_hemato_greater_than_5_yr_given_age_15_44,
                                                    prevalance_of_hemato_greater_than_5_yr_given_age_45_74,
                                                    prevalance_of_hemato_greater_than_5_yr_given_age_75above,
                                                    prevalance_of_nonhemato_1_5yr_given_age_15_44,
                                                    prevalance_of_nonhemato_1_5yr_given_age_45_74,
                                                    prevalance_of_nonhemato_1_5yr_given_age_75above,
                                                    prevalance_of_nonhemato_less_than_1_yr_given_age_15_44,
                                                    prevalance_of_nonhemato_less_than_1_yr_given_age_45_74,
                                                    prevalance_of_nonhemato_less_than_1_yr_given_age_75above,
                                                    prevalance_of_nonhemato_greater_than_5_yr_given_age_15_44,
                                                    prevalance_of_nonhemato_greater_than_5_yr_given_age_45_74,
                                                    prevalance_of_nonhemato_greater_than_5_yr_given_age_75above,
                                                    prevalance_brfss_census$IMD1,
                                                    prevalance_brfss_census$IMD2,
                                                    prevalance_brfss_census$IMD3,
                                                    prevalance_brfss_census$IMD4,
                                                    prevalance_brfss_census$IMD5,
                                                    prevalance_brfss_census$Age_15_44,
                                                    prevalance_brfss_census$Age_45_54,
                                                    prevalance_brfss_census$Age_55_64,
                                                    prevalance_brfss_census$Age_65_74,
                                                    prevalance_brfss_census$Age_75_84,
                                                    prevalance_brfss_census$Age_85
  ))
  colnames(prevalance_age_stratification)[c(1, 77:87)] = c("PlaceFIPS",
                                                              "IMD1", "IMD2", "IMD3", "IMD4", "IMD5",
                                                             "Age_15_44", "Age_45_54", "Age_55_64", "Age_65_74", "Age_75_84", "Age_85")
  
  age_stratification_prevalance[[i]] = prevalance_age_stratification
  print(i)
}




saveRDS(age_stratification_prevalance, 'data_created/Bootstrap_samples/prevalance_age_stratification_bootstrap.rds')
