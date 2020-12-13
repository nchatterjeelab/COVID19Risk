state_brfss = readRDS("~/Dropbox/NHANES_risk_score/Nature Medicine Revision/Github_revision/COVID19Risk/data_created/State data/US_51_State_0928.rds")


state_brfss = state_brfss %>% mutate(Age_15_44 = Age_15_44_Prev) %>%
                              mutate(Age_45_64 = 1 - Age_15_44_Prev  - Age_65_74_Prev - Age_75_84_Prev - Age_85_Prev) %>%
                              mutate(Age_65_above = Age_65_74_Prev + Age_75_84_Prev + Age_85_Prev)

nhis_data = readRDS('~/Dropbox/NHANES_risk_score/Nature Medicine Revision/Github_revision/COVID19Risk/data_created/nhis_2017.rds')

additional_weights = nhis_data[, c(37:38)]
nhis_data  = nhis_data[,c(6:7, 9:24,34,35,36, 30,31)]
nhis_data= nhis_data %>% mutate(age_mixture_cat = case_when(agegroup.cdc == "45_54" | agegroup.cdc == "55_64"  ~ '45_64',
                                                            agegroup.cdc == "75_84" | agegroup.cdc == "85+" | agegroup.cdc == "65_74" ~ '65above',
                                                            agegroup.cdc == "15_45" ~ '15_44'))
nhis_data = cbind(nhis_data, additional_weights)

#----Age-Hematologic---#
data = nhis_data[, c(1,19,22,24, 25, 26)]
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
p1 = hemato_less_than_1_yr_p = state_brfss$hemo_cat1

p2 = state_brfss$Age_15_44 
p11 = update_p11(p1,p2,r)
prevalance_of_hemato_less_than_1_yr_given_age_15_44 = p11/p2 

logistic_fit_adj =svyglm(age_mixture_cat_45_64 ~ hemato_less_than_1_yr, design=dstrat, family = quasibinomial())
r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
p1 = hemato_less_than_1_yr_p = state_brfss$hemo_cat1

p2 = state_brfss$Age_45_64 
p11 = update_p11(p1,p2,r)
prevalance_of_hemato_less_than_1_yr_given_age_45_64 = p11/p2 

logistic_fit_adj =svyglm(age_mixture_cat_65above ~ hemato_less_than_1_yr, design=dstrat, family = quasibinomial())
r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
p1 = hemato_less_than_1_yr_p = state_brfss$hemo_cat1

p2 = state_brfss$Age_65_above 
p11 = update_p11(p1,p2,r)
prevalance_of_hemato_less_than_1_yr_given_age_65above = p11/p2 

logistic_fit_adj =svyglm(age_mixture_cat_15_44 ~ hemato_1_5yr, design=dstrat, family = quasibinomial())
r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
p1 = hemato_1_5yr_p = state_brfss$hemo_cat2

p2 = state_brfss$Age_15_44 
p11 = update_p11(p1,p2,r)
prevalance_of_hemato_1_5yr_given_age_15_44 = p11/p2 

logistic_fit_adj =svyglm(age_mixture_cat_45_64 ~ hemato_1_5yr, design=dstrat, family = quasibinomial())
r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
p1 = hemato_1_5yr_p = state_brfss$hemo_cat2

p2 = state_brfss$Age_45_64 
p11 = update_p11(p1,p2,r)
prevalance_of_hemato_1_5yr_given_age_45_64 = p11/p2 

logistic_fit_adj =svyglm(age_mixture_cat_65above ~ hemato_1_5yr, design=dstrat, family = quasibinomial())
r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
p1 = hemato_1_5yr_p = state_brfss$hemo_cat2

p2 = state_brfss$Age_65_above
p11 = update_p11(p1,p2,r)
prevalance_of_hemato_1_5yr_given_age_65above = p11/p2 

logistic_fit_adj =svyglm(age_mixture_cat_15_44 ~ hemato_greater_than_5_yr, design=dstrat, family = quasibinomial())
r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
p1 = hemato_greater_than_5_yr_p = state_brfss$hemo_cat3

p2 = state_brfss$Age_15_44 
p11 = update_p11(p1,p2,r)
prevalance_of_hemato_greater_than_5_yr_given_age_15_44 = p11/p2 

logistic_fit_adj =svyglm(age_mixture_cat_45_64 ~ hemato_greater_than_5_yr, design=dstrat, family = quasibinomial())
r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
p1 = hemato_greater_than_5_yr_p = state_brfss$hemo_cat3

p2 = state_brfss$Age_45_64 
p11 = update_p11(p1,p2,r)
prevalance_of_hemato_greater_than_5_yr_given_age_45_64 = p11/p2 

logistic_fit_adj =svyglm(age_mixture_cat_65above ~ hemato_greater_than_5_yr, design=dstrat, family = quasibinomial())
r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
p1 = hemato_greater_than_5_yr_p = state_brfss$hemo_cat3

p2 = state_brfss$Age_65_above
p11 = update_p11(p1,p2,r)
prevalance_of_hemato_greater_than_5_yr_given_age_65above = p11/p2 




#----Age-non_hematologic---#
data = nhis_data[, c(1,19,23,24, 25,26)]
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
p1 = nonhemato_less_than_1_yr_p = state_brfss$non_hemo_cat1

p2 = state_brfss$Age_15_44
p11 = update_p11(p1,p2,r)
prevalance_of_nonhemato_less_than_1_yr_given_age_15_44 = p11/p2 

logistic_fit_adj =svyglm(age_mixture_cat_45_64 ~ nonhemato_less_than_1_yr, design=dstrat, family = quasibinomial())
r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
p1 = nonhemato_less_than_1_yr_p = state_brfss$non_hemo_cat1

p2 = state_brfss$Age_45_64 
p11 = update_p11(p1,p2,r)
prevalance_of_nonhemato_less_than_1_yr_given_age_45_64 = p11/p2 

logistic_fit_adj =svyglm(age_mixture_cat_65above ~ nonhemato_less_than_1_yr, design=dstrat, family = quasibinomial())
r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
p1 = nonhemato_less_than_1_yr_p = state_brfss$non_hemo_cat1

p2 = state_brfss$Age_65_above
p11 = update_p11(p1,p2,r)
prevalance_of_nonhemato_less_than_1_yr_given_age_65above = p11/p2 

logistic_fit_adj =svyglm(age_mixture_cat_15_44 ~ nonhemato_1_5yr, design=dstrat, family = quasibinomial())
r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
p1 = nonhemato_1_5yr_p = state_brfss$non_hemo_cat2

p2 = state_brfss$Age_15_44
p11 = update_p11(p1,p2,r)
prevalance_of_nonhemato_1_5yr_given_age_15_44 = p11/p2 

logistic_fit_adj =svyglm(age_mixture_cat_45_64 ~ nonhemato_1_5yr, design=dstrat, family = quasibinomial())
r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
p1 = nonhemato_1_5yr_p = state_brfss$non_hemo_cat2

p2 = state_brfss$Age_45_64 
p11 = update_p11(p1,p2,r)
prevalance_of_nonhemato_1_5yr_given_age_45_64 = p11/p2 

logistic_fit_adj =svyglm(age_mixture_cat_65above ~ nonhemato_1_5yr, design=dstrat, family = quasibinomial())
r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
p1 = nonhemato_1_5yr_p = state_brfss$non_hemo_cat2

p2 = state_brfss$Age_65_above 
p11 = update_p11(p1,p2,r)
prevalance_of_nonhemato_1_5yr_given_age_65above = p11/p2 

logistic_fit_adj =svyglm(age_mixture_cat_15_44 ~ nonhemato_greater_than_5_yr, design=dstrat, family = quasibinomial())
r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
p1 = nonhemato_greater_than_5_yr_p = state_brfss$non_hemo_cat3

p2 = state_brfss$Age_15_44 
p11 = update_p11(p1,p2,r)
prevalance_of_nonhemato_greater_than_5_yr_given_age_15_44 = p11/p2 

logistic_fit_adj =svyglm(age_mixture_cat_45_64 ~ nonhemato_greater_than_5_yr, design=dstrat, family = quasibinomial())
r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
p1 = nonhemato_greater_than_5_yr_p = state_brfss$non_hemo_cat3

p2 = state_brfss$Age_45_64
p11 = update_p11(p1,p2,r)
prevalance_of_nonhemato_greater_than_5_yr_given_age_45_64 = p11/p2 

logistic_fit_adj =svyglm(age_mixture_cat_65above ~ nonhemato_greater_than_5_yr, design=dstrat, family = quasibinomial())
r = odds_ratio = exp(logistic_fit_adj$coefficients[2])
p1 = nonhemato_greater_than_5_yr_p = state_brfss$non_hemo_cat3

p2 = state_brfss$Age_65_above 
p11 = update_p11(p1,p2,r)
prevalance_of_nonhemato_greater_than_5_yr_given_age_65above = p11/p2 

append_data = as.data.frame(cbind( prevalance_of_hemato_1_5yr_given_age_15_44,
                                   prevalance_of_hemato_1_5yr_given_age_45_64,
                                   prevalance_of_hemato_1_5yr_given_age_65above,
                                   prevalance_of_hemato_less_than_1_yr_given_age_15_44,
                                   prevalance_of_hemato_less_than_1_yr_given_age_45_64,
                                   prevalance_of_hemato_less_than_1_yr_given_age_65above,
                                   prevalance_of_hemato_greater_than_5_yr_given_age_15_44,
                                   prevalance_of_hemato_greater_than_5_yr_given_age_45_64,
                                   prevalance_of_hemato_greater_than_5_yr_given_age_65above,
                                   prevalance_of_nonhemato_1_5yr_given_age_15_44,
                                   prevalance_of_nonhemato_1_5yr_given_age_45_64,
                                   prevalance_of_nonhemato_1_5yr_given_age_65above,
                                   prevalance_of_nonhemato_less_than_1_yr_given_age_15_44,
                                   prevalance_of_nonhemato_less_than_1_yr_given_age_45_64,
                                   prevalance_of_nonhemato_less_than_1_yr_given_age_65above,
                                   prevalance_of_nonhemato_greater_than_5_yr_given_age_15_44,
                                   prevalance_of_nonhemato_greater_than_5_yr_given_age_45_64,
                                   prevalance_of_nonhemato_greater_than_5_yr_given_age_65above))

merge_data = cbind(state_brfss, append_data)

saveRDS(merge_data, "~/Dropbox/NHANES_risk_score/Nature Medicine Revision/Github_revision/COVID19Risk/data_created/State data/US_51_State_0928_appended_cancer.rds")
