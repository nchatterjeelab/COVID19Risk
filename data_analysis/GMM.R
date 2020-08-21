ascii_link = "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/2017/samadult.zip"
program_code = "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Program_Code/NHIS/2017/SAMADULT.sas"
NHIS.17.samadult.df  = read.SAScii (ascii_link , program_code , zipped = T )
gender_var = "SEX"
race_var = c("HISPAN_I", "RACERPI2", "MRACBPI2")
age_var = "AGE_P"
bmi_var = "BMI"
smoking_var = "SMKSTAT2"
cancer_var = c("CANEV", paste0("CNKIND", seq(1, 31,1)))
age_cancer_var = c(paste0("CANAGE", seq(1,30,1)))
diabetes_var = c("ALCHRC10", "DIBEV1", "DIBPRE2", "DIBTYPE")
liver_codition_var = "LIVYR"
bp_var = c("ALCHRC9", "HYPEV", "HYBPCKNO", "HYPYR1", "HYBPLEV")
stroke_var = c("STREV", "ALCHRC8")
heart_var = c("ALCHRC7", "MIEV", "HRTEV", "CHDEV", "ANGEV")
rheumatoid_var = c("ARTH1")
resp_ex_asthma_var = c("COPDEV", "CBRCHYR")
asthma_var = c("AASMEV", "AASSTILL")
kidney_var = "KIDWKYR"
nhis_2017_data = NHIS.17.samadult.df[, c(1,2,3,6,7,8,9,which(colnames(NHIS.17.samadult.df) %in% c(gender_var, race_var, age_var, bmi_var, smoking_var, cancer_var, age_cancer_var, diabetes_var, liver_codition_var, bp_var, stroke_var, heart_var, rheumatoid_var, resp_ex_asthma_var, asthma_var, kidney_var) == T))]

#---- Male = 1, Female = 0 (2 = Female coded in NHIS)---#
nhis_2017_data$SEX[nhis_2017_data$SEX == 2] = "Female"
nhis_2017_data$SEX[nhis_2017_data$SEX == 1] = "Male"

nhis_2017_data = nhis_2017_data %>% mutate(agegroup = case_when(AGE_P >= 18  & AGE_P < 40 ~ '18_40',
                                                                AGE_P >= 40  & AGE_P < 50 ~ '40_50',
                                                                AGE_P >= 50  & AGE_P < 60 ~ '50_60',
                                                                AGE_P >= 60  & AGE_P < 70 ~ '60_70',
                                                                AGE_P >= 70  & AGE_P < 80 ~ '70_80',
                                                                AGE_P >= 80  & AGE_P < 100 ~ '80_and_above'))
nhis_2017_data = nhis_2017_data %>% mutate(agegroup.cdc = case_when(AGE_P >= 15  & AGE_P < 45 ~ '15_45',
                                                                AGE_P >= 45  & AGE_P < 55 ~ '45_54',
                                                                AGE_P >= 55  & AGE_P < 65 ~ '55_64',
                                                                AGE_P >= 65  & AGE_P < 75 ~ '65_74',
                                                                AGE_P >= 75  & AGE_P < 85 ~ '75_84',
                                                                AGE_P >= 85  & AGE_P < 100 ~ '85+'))
nhis_2017_data = nhis_2017_data %>% mutate(diabetes = case_when(DIBEV1 == 1 ~ 'Yes',
                                                                DIBEV1 == 2 | DIBEV1 == 3 ~ 'No',
                                                                DIBEV1 > 3 ~ 'NA'))
nhis_2017_data = nhis_2017_data %>% mutate(smoking_status = case_when(SMKSTAT2 == 3 ~ 'Former',
                                                                      SMKSTAT2 == 1 | SMKSTAT2 == 2 ~ 'Current',
                                                                      SMKSTAT2 == 4 ~ 'Never',
                                                                      SMKSTAT2 > 4 ~ 'NA'))
nhis_2017_data = nhis_2017_data %>% mutate(rheumatoid = case_when(ARTH1 == 1 ~ 'Yes',
                                                                   ARTH1 == 2 ~ 'No',
                                                                   ARTH1 > 2 ~ 'NA'))
nhis_2017_data = nhis_2017_data %>% mutate(asthma = case_when(AASSTILL == 1 & AASMEV == 1 ~ 'Yes',
                                                              AASSTILL == 2 & AASMEV == 1 ~ 'No',
                                                              AASMEV  == 2 ~ 'No'))
nhis_2017_data = nhis_2017_data %>% mutate(stroke = case_when(STREV == 1 ~ 'Yes',
                                                              STREV == 2 ~ 'No',
                                                              STREV > 2 ~ 'NA'))
nhis_2017_data = nhis_2017_data %>% mutate(heart_disease1 = case_when(MIEV == 1 ~ 'Yes',
                                                                      MIEV == 2 ~ 'No',
                                                                      MIEV > 2 ~ 'NA'))
nhis_2017_data = nhis_2017_data %>% mutate(heart_disease2 = case_when(ALCHRC7 == 1 & HRTEV == 1 ~ 'Yes',
                                                                      ALCHRC7 == 2 | HRTEV == 2 ~ 'No',
                                                                      HRTEV > 2 ~ 'NA'))
nhis_2017_data = nhis_2017_data %>% mutate(heart_disease3 = case_when(CHDEV == 1 ~ 'Yes',
                                                                      CHDEV == 2 ~ 'No',
                                                                      CHDEV > 2 ~ 'NA'))
nhis_2017_data = nhis_2017_data %>% mutate(heart_disease4 = case_when(ANGEV == 1 ~ 'Yes',
                                                                      ANGEV == 2 ~ 'No',
                                                                      ANGEV > 2 ~ 'NA'))
nhis_2017_data = nhis_2017_data %>% mutate(heart_disease = case_when(heart_disease2 == "Yes" | heart_disease3 == "Yes" | heart_disease4 == "Yes" ~ 'Yes',
                                                                     heart_disease2 == "No" & heart_disease3 == "No" & heart_disease4 == "No" ~ 'No'))
nhis_2017_data = nhis_2017_data %>% mutate(hypertension = case_when(HYPYR1 == 1 | HYBPLEV == 2 ~ 'Hypertension_high_bp',
                                                                    HYBPLEV == 3 ~ 'Normal'))
nhis_2017_data = nhis_2017_data %>% mutate(resp_ex_asthma = case_when(COPDEV == 1 | CBRCHYR == 1 ~ 'Yes',
                                                                      COPDEV == 2 & CBRCHYR == 2 ~ 'No'))
nhis_2017_data = nhis_2017_data %>% mutate(kidney_disease = case_when(KIDWKYR == 1 ~ 'Yes',
                                                                      KIDWKYR == 2 ~ 'No'))
nhis_2017_data = nhis_2017_data %>% mutate(liver_disease = case_when(LIVYR == 1 ~ 'Yes',
                                                                     LIVYR == 2 ~ 'No'))
nhis_2017_data = nhis_2017_data %>% mutate(id_created = seq(1, nrow(nhis_2017_data), 1))
nhis_2017_data = nhis_2017_data %>% mutate(ethnicity = case_when(HISPAN_I < 10 ~ 'Hispanic',
                                                                 HISPAN_I == 12 ~ 'Not_hispanic'))
nhis_2017_data = nhis_2017_data %>% mutate(race = case_when(MRACBPI2 == 1 ~ 'White',
                                                            MRACBPI2 == 2 ~ 'Black',
                                                            MRACBPI2 == 6 |  MRACBPI2 == 7 |  MRACBPI2 == 12  ~ 'Asian',
                                                            MRACBPI2 == 3 |  MRACBPI2 == 16  ~ 'Others',
                                                            MRACBPI2 == 17 ~ 'Mixed'))

nhis_2017_data = nhis_2017_data %>% mutate(race.cdc = case_when(MRACBPI2 == 1 ~ 'White',
                                                            MRACBPI2 == 2 ~ 'Black',
                                                            MRACBPI2 == 6 |  MRACBPI2 == 7 |  MRACBPI2 == 12  ~ 'Asian',
                                                            MRACBPI2 == 3  ~ 'American_Indian',
                                                            MRACBPI2 == 17 ~ 'More_than_one_race'))

nhis_2017_data = nhis_2017_data %>% mutate(race_ethnicity = case_when(ethnicity == "Not_hispanic" & race == "White" ~ 'Non_hispanic_white',
                                                                      ethnicity == "Not_hispanic" & race == "Black" ~ 'Black',
                                                                      ethnicity == "Not_hispanic" & race == "Asian" ~ 'Asian',
                                                                      ethnicity == "Not_hispanic" & race == "Others"  ~ 'Others',
                                                                      ethnicity == "Not_hispanic" & race == "Mixed" ~ 'Mixed',
                                                                      ethnicity == "Hispanic" ~ 'Hispanic'))
hematologic_code = c("CNKIND2", "CNKIND3", "CNKIND12", "CNKIND15")
paste0("mm$CNKIND", setdiff(seq(1,31,1), c(1,3,12,15)), " == 1", collapse = "|")
nhis_2017_data = nhis_2017_data %>% mutate(hematologic_cancer = case_when(CNKIND2 == 1 | CNKIND3 == 1 | CNKIND12 == 1 | CNKIND15 == 1 ~ 'Hematological',
                                                                          CNKIND1 == 2 & CNKIND2 == 2 & CNKIND3 == 2 & CNKIND4 == 2 & CNKIND5 == 2 & CNKIND6 == 2 & CNKIND7 == 2 & CNKIND8 == 2 & CNKIND9 == 2 & CNKIND10 == 2 & CNKIND11 == 2 & CNKIND12 == 2 & CNKIND13 == 2 & CNKIND14 == 2 & CNKIND15 == 2 & CNKIND16 == 2 & CNKIND17 == 2 & CNKIND18 == 2 & CNKIND19 == 2 & CNKIND20 == 2 & CNKIND21 == 2 & CNKIND22 == 2 & CNKIND23 == 2 & CNKIND24 == 2 & CNKIND25 == 2 & CNKIND26 == 2 & CNKIND27 == 2 & CNKIND28 == 2 & CNKIND29 == 2 & CNKIND30 == 2 & CNKIND31 == 2 ~ 'None'))
nhis_2017_data = nhis_2017_data %>% mutate(non_hematologic_cancer = case_when(CNKIND1 == 1 | CNKIND4 == 1 | CNKIND5 == 1 | CNKIND6 == 1 | CNKIND7 == 1 | CNKIND8 == 1 | CNKIND9 == 1 | CNKIND10 == 1 | CNKIND11 == 1 | CNKIND13 == 1 | CNKIND14 == 1 | CNKIND16 == 1 | CNKIND17 == 1 | CNKIND18 == 1 | CNKIND19 == 1 | CNKIND20 == 1 | CNKIND21 == 1 | CNKIND22 == 1 | CNKIND23 == 1 | CNKIND24 == 1 | CNKIND25 == 1 | CNKIND26 == 1 | CNKIND27 == 1 | CNKIND28 == 1 | CNKIND29 == 1  ~ 'Non_hematological',
                                                                              CNKIND1 == 2 & CNKIND2 == 2 & CNKIND3 == 2 & CNKIND4 == 2 & CNKIND5 == 2 & CNKIND6 == 2 & CNKIND7 == 2 & CNKIND8 == 2 & CNKIND9 == 2 & CNKIND10 == 2 & CNKIND11 == 2 & CNKIND12 == 2 & CNKIND13 == 2 & CNKIND14 == 2 & CNKIND15 == 2 & CNKIND16 == 2 & CNKIND17 == 2 & CNKIND18 == 2 & CNKIND19 == 2 & CNKIND20 == 2 & CNKIND21 == 2 & CNKIND22 == 2 & CNKIND23 == 2 & CNKIND24 == 2 & CNKIND25 == 2 & CNKIND26 == 2 & CNKIND27 == 2 & CNKIND28 == 2 & CNKIND29 == 2 & CNKIND30 == 2 & CNKIND31 == 2 ~ 'None'))
nhis_2017_data$hematologic_cancer[which(nhis_2017_data$CANEV == 2)] = "None"
nhis_2017_data$non_hematologic_cancer[which(nhis_2017_data$CANEV == 2)] = "None"
nhis_2017_data$hematologic_cancer[which(nhis_2017_data$CNKIND31 == 1)] = NA
nhis_2017_data$non_hematologic_cancer[which(nhis_2017_data$CNKIND31 == 1)] = NA

nhis_2017_data = nhis_2017_data %>% mutate(age_cancer = apply(nhis_2017_data[, 57:86], 1, FUN = function(x){if(sum(!is.na(x)) == 0){return(NA)}else{return(min(x, na.rm = T))}})) 
nhis_2017_data = nhis_2017_data %>% mutate(age_diff = AGE_P-age_cancer) 
nhis_2017_data$age_diff[which(nhis_2017_data$age_diff < 0)] = NA


nhis_2017_data = nhis_2017_data %>% mutate(diagnoses_cancer = case_when(age_diff >= 0 & age_diff < 1 ~ 'less_than_1_yr',
                                                                        age_diff >= 1 & age_diff < 5 ~ '1_5yr',
                                                                        age_diff >= 5 ~ 'greater_than_5_yr'))

nhis_2017_data = nhis_2017_data %>% mutate(Obesity = case_when(BMI >= 30  & BMI < 35 ~ 'Obese_I',
                                                               BMI >= 35  & BMI < 40 ~ 'Obese_II',
                                                               BMI >= 40 ~ 'Obese_III',
                                                               BMI < 30 ~ 'Not_obese'))


nhis_2017_data$hematologic_cancer[which(is.na(nhis_2017_data$hematologic_cancer) == T & nhis_2017_data$non_hematologic_cancer == "Non_hematological")] = "99"
nhis_2017_data$non_hematologic_cancer[nhis_2017_data$hematologic_cancer == "Hematological" & is.na(nhis_2017_data$non_hematologic_cancer) == T] = "99"
colnames(nhis_2017_data)[c(7:8, 12)] = c("sampling_weights", "sex", "age")




# nhis_2017_data$hematologic_cancer[which(is.na(nhis_2017_data$hematologic_cancer) == T & nhis_2017_data$non_hematologic_cancer == "Non_hematological")] = "None"
# nhis_2017_data$non_hematologic_cancer[nhis_2017_data$hematologic_cancer == "Hematological" & is.na(nhis_2017_data$non_hematologic_cancer) == T] = "None"

nhis_2017_data$hematologic_cancer[which(is.na(nhis_2017_data$hematologic_cancer) == T & nhis_2017_data$non_hematologic_cancer == "Non_hematological")] = "99"
nhis_2017_data$non_hematologic_cancer[nhis_2017_data$hematologic_cancer == "Hematological" & is.na(nhis_2017_data$non_hematologic_cancer) == T] = "99"

#nhis_2017_data = nhis_2017_data %>% mutate(hematologic_cancer = case_when(hematologic_cancer == "None" ~ 0,
 #                                                                         hematologic_cancer == "Hematological" & age_diff < 1 ~ 1,
  #                                                                        hematologic_cancer == "Hematological" & age_diff >= 1 & age_diff < 5 ~ 2,
   #                                                                       hematologic_cancer == "Hematological" & age_diff >= 5 ~ 3))

#nhis_2017_data = nhis_2017_data %>% mutate(non_hematologic_cancer = case_when(non_hematologic_cancer == "None" ~ 0,
 #                                                                               non_hematologic_cancer == "Non_hematological" & age_diff < 1 ~ 1,
  #                                                                              non_hematologic_cancer == "Non_hematological" & age_diff >= 1 & age_diff < 5 ~ 2,
   #                                                                             non_hematologic_cancer == "Non_hematological" & age_diff >= 5 ~ 3))

nhis_2017_data$rheumatoid[which(nhis_2017_data$rheumatoid == "NA")] = NA
nhis_2017_data$smoking_status[which(nhis_2017_data$smoking_status == "NA")] = NA
nhis_2017_data$diabetes[which(nhis_2017_data$diabetes == "NA")] = NA
nhis_2017_data$stroke[which(nhis_2017_data$stroke == "NA")] = NA

nhis_2017_data = nhis_2017_data %>% mutate(race_ethnicity.cdc = case_when(ethnicity == "Not_hispanic" & race.cdc == "White" ~ 'Non_hispanic_white',
                                                                                            ethnicity == "Not_hispanic" & race.cdc == "Black" ~ 'Non_hispanic_Black',
                                                                                            ethnicity == "Not_hispanic" & race.cdc == "Asian" ~ 'Non_hispanic_asian',
                                                                                            ethnicity == "Not_hispanic" & race.cdc == "American_Indian"  ~ 'Non_hispanic_american_Indian',
                                                                                            ethnicity == "Not_hispanic" & race.cdc == "More_than_one_race" ~ 'Non_hispanic_more_than_one_race',
                                                                                            ethnicity == "Hispanic" ~ 'Hispanic'))


nhis_2017_data = nhis_2017_data[, c(2:6,7,8,12, 99:115, 117:127)]

saveRDS(nhis_2017_data, '~/Dropbox/NHANES_risk_score/500cities_data/Updated_July_06_2020/data_created/nhis_2017.rds')

nhis_2017_data = nhis_2017_data %>% mutate(eid = seq(1,dim(nhis_2017_data)[1],1))



reference_data_full_model = nhis_2017_data[, c(2:5,7,8,12, 99, 101,102:106,111:115, 117,119, 121:122, 126, 127)]
reference_data_full_model = reference_data_full_model %>% mutate(race_ethnicity = case_when(ethnicity == "Not_hispanic" & race.cdc == "White" ~ 'Non_hispanic_white',
                                                                                            ethnicity == "Not_hispanic" & race.cdc == "Black" ~ 'Non_hispanic_Black',
                                                                                            ethnicity == "Not_hispanic" & race.cdc == "Asian" ~ 'Non_hispanic_asian',
                                                                                            ethnicity == "Not_hispanic" & race.cdc == "American_Indian"  ~ 'Non_hispanic_american_Indian',
                                                                                            ethnicity == "Not_hispanic" & race.cdc == "More_than_one_race" ~ 'Non_hispanic_more_than_one_race',
                                                                                            ethnicity == "Hispanic" ~ 'Hispanic'))

reference_data_full_model = reference_data_full_model[, c(5,9,26,6,24, 11,16,17,13, 15, 10, 23,22,14, 18, 12, 19, 25)]
reference_data_full_model_complete = reference_data_full_model[complete.cases(reference_data_full_model), ]
eid_inc = reference_data_full_model_complete$eid

reference_data_uk = nhis_2017_data[, c(2:5,7,8,12, 99:100,102:106,111:115, 120, 121:122, 126, 127)]
eid_common = intersect(reference_data_uk$eid, eid_inc)

reference_data_full_model_complete = reference_data_full_model_complete[which(reference_data_full_model_complete$eid %in% eid_common == T), ]

sampling_weights = reference_data_full_model_complete$WTFA_SA
reference_data_full_model_complete = reference_data_full_model_complete[, -c(1,18)]
reference_data_full_model_complete$agegroup.cdc = as.factor(reference_data_full_model_complete$agegroup.cdc)
reference_data_full_model_complete$agegroup.cdc = relevel(reference_data_full_model_complete$agegroup.cdc, ref = "55_64")
reference_data_full_model_complete$SEX = as.factor(reference_data_full_model_complete$SEX)
reference_data_full_model_complete$SEX = relevel(reference_data_full_model_complete$SEX, ref = "Female")
reference_data_full_model_complete$smoking_status = as.factor(reference_data_full_model_complete$smoking_status)
reference_data_full_model_complete$smoking_status = relevel(reference_data_full_model_complete$smoking_status, ref = "Never")
reference_data_full_model_complete$race_ethnicity = as.factor(reference_data_full_model_complete$race_ethnicity)
reference_data_full_model_complete$race_ethnicity = relevel(reference_data_full_model_complete$race_ethnicity, ref = "Non_hispanic_white")
reference_data_full_model_complete$hypertension = as.factor(reference_data_full_model_complete$hypertension)
reference_data_full_model_complete$hypertension = relevel(reference_data_full_model_complete$hypertension, ref = "Normal")
reference_data_full_model_complete$hematologic_cancer = as.factor(reference_data_full_model_complete$hematologic_cancer)
reference_data_full_model_complete$non_hematologic_cancer = as.factor(reference_data_full_model_complete$non_hematologic_cancer)

reference_data_full_model_complete_model_matrix = model.matrix(as.formula(paste0("~", paste0(colnames(reference_data_full_model_complete), collapse = "+"))), data = reference_data_full_model_complete)

beta_initial = c(-9.367273, as.numeric(CDC_theta[-1]))



reference_data_cdc = nhis_2017_data[, c(7,101,117,119,8,127)]
reference_data_cdc = reference_data_cdc[which(reference_data_cdc$eid %in% eid_common == T), ]
reference_data_cdc = reference_data_cdc[, -6]
reference_data_cdc = reference_data_cdc %>% mutate(race_ethnicity = case_when(ethnicity == "Not_hispanic" & race.cdc == "White" ~ 'Non_hispanic_white',
                                                                      ethnicity == "Not_hispanic" & race.cdc == "Black" ~ 'Non_hispanic_Black',
                                                                      ethnicity == "Not_hispanic" & race.cdc == "Asian" ~ 'Non_hispanic_asian',
                                                                      ethnicity == "Not_hispanic" & race.cdc == "American_Indian"  ~ 'Non_hispanic_american_Indian',
                                                                      ethnicity == "Not_hispanic" & race.cdc == "More_than_one_race" ~ 'Non_hispanic_more_than_one_race',
                                                                      ethnicity == "Hispanic" ~ 'Hispanic'))
reference_data_cdc = reference_data_cdc[, c(2,6)]
reference_data_cdc = reference_data_cdc[complete.cases(reference_data_cdc), ]
reference_data_cdc$race_ethnicity = as.factor(reference_data_cdc$race_ethnicity)
reference_data_cdc$race_ethnicity = relevel(reference_data_cdc$race_ethnicity, ref = "Non_hispanic_white")
reference_data_cdc$agegroup.cdc = as.factor(reference_data_cdc$agegroup.cdc)
reference_data_cdc$agegroup.cdc = relevel(reference_data_cdc$agegroup.cdc, ref = "55_64")
reference_data_cdc_model_matrix = model.matrix(as.formula(paste0("~", paste0(colnames(reference_data_cdc), collapse = "+"))), data = reference_data_cdc)


reference_data_uk = nhis_2017_data[, c(2:5,7,8,12, 99:100,102:106,111:115, 120, 121:122, 126, 127)]
#reference_data_uk = reference_data_uk[which(reference_data_uk$race_ethnicity == "Non_hispanic_white" | reference_data_uk$race_ethnicity == "Black"), ]
eid_common = intersect(reference_data_uk$eid, eid_inc)
reference_data_uk = reference_data_uk[which(reference_data_uk$eid %in% eid_common == T), ]
reference_data_uk = reference_data_uk[, -24]
reference_data_uk = reference_data_uk[, c(9,6,23, 11, 20, 16, 17, 13, 15, 10, 22, 21, 14, 18, 12, 19)]
reference_data_uk_complete = reference_data_uk[complete.cases(reference_data_uk), ]
reference_data_uk_complete$agegroup = as.factor(reference_data_uk_complete$agegroup)
reference_data_uk_complete$agegroup = relevel(reference_data_uk_complete$agegroup, ref = "50_60")
reference_data_uk_complete$SEX = as.factor(reference_data_uk_complete$SEX)
reference_data_uk_complete$SEX = relevel(reference_data_uk_complete$SEX, ref = "Female")
reference_data_uk_complete$smoking_status = as.factor(reference_data_uk_complete$smoking_status)
reference_data_uk_complete$smoking_status = relevel(reference_data_uk_complete$smoking_status, ref = "Never")
reference_data_uk_complete$race_ethnicity = as.factor(reference_data_uk_complete$race_ethnicity)
reference_data_uk_complete$race_ethnicity = relevel(reference_data_uk_complete$race_ethnicity, ref = "Non_hispanic_white")
reference_data_uk_complete$hypertension = as.factor(reference_data_uk_complete$hypertension)
reference_data_uk_complete$hypertension = relevel(reference_data_uk_complete$hypertension, ref = "Normal")
reference_data_uk_complete_model_matrix = model.matrix(as.formula(paste0("~", paste0(colnames(reference_data_uk_complete), collapse = "+"))), data = reference_data_uk_complete)


#nhis_imputed = readRDS("~/Dropbox/NHANES_risk_score/Github/COVID19Risk/data_created/individual_rs_covariates_imputed_sdi.rds")
UK_beta = read.xlsx("~/Dropbox/NHANES_risk_score/Github/COVID19Risk/data_created/UK_model.xlsx")
UK_beta = UK_beta[-c(12,14:17, 23), ]
weights = c(489297, 1043176, 195243)/17425445
diabetes_coeff = sum(weights * log(c(1.47, 2.23, 1.91)))
UK_beta[17,2] =  diabetes_coeff
UK_beta$Variable_Name_UK[17] = "Diabetes"
temp = UK_beta$estimate[11]
UK_beta$estimate[11] = UK_beta$estimate[10]
UK_beta$estimate[10] = temp
UK_beta$Variable_Name_UK[10] = "Smoking_current"
UK_beta$Variable_Name_UK[11] = "Smoking_ex"
UK_beta = c(UK_beta$estimate, log(1.61))


CDC_theta = poisson.model$coefficients[1:11]
theta_CDC = as.numeric(CDC_theta)
X_CDC = reference_data_cdc_model_matrix
X_UK =  reference_data_uk_complete_model_matrix
X_full = reference_data_full_model_complete_model_matrix

X_CDC_wt = (sampling_weights/(sum(sampling_weights))) * reference_data_cdc_model_matrix
X_UK_wt =  (sampling_weights/(sum(sampling_weights))) *  reference_data_uk_complete_model_matrix
X_full_wt = (sampling_weights/(sum(sampling_weights))) * reference_data_full_model_complete_model_matrix

theta_UK = as.numeric(UK_beta)
names(theta_UK) = c("agegroup18_40", "agegroup40_50","agegroup60_70", "agegroup70_80", "agegroup80+", "Male","Obese_I", "Obese_II", "Obese_III", "Smk_current", "Smk_ex", "Black", "Hypertension", "Resp_ex_asthma","Asthma", "CHD", "Diabetes", "Non_hema_1", "Non_hema_2","Non_hema_3", "Hema_1","Hema_2", "Hema_3", "Stroke", "Kidney", "Arthritis", "Liver")
Q = function(beta)
{
  U = as.numeric(t(X_CDC_wt) %*% as.numeric(expit(X_full[,1:11] %*% beta + X_full[,12:32] %*% theta_UK[-c(1:5,12)]) - expit(X_CDC %*% theta_CDC)))
  C = diag(length(U))
  return(as.numeric(U^T %*% C %*% U))
}

fit.optim = optim(as.numeric(beta_initial), Q, method = c("L-BFGS-B"), control = list(maxit=50))
adj_est = exp(fit.optim$par)
names(adj_est) = colnames(X_CDC)







nhis_imputed = readRDS("~/Dropbox/NHANES_risk_score/500cities_data/Updated_July_06_2020/data_created/nhis_imputed.rds")

nhis_imputed = nhis_imputed %>% mutate(agegroup.cdc = case_when(age >= 15  & age < 45 ~ '15_45',
                                                                age >= 45  & age < 55 ~ '45_54',
                                                                age >= 55  & age < 65 ~ '55_64',
                                                                age >= 65  & age < 75 ~ '65_74',
                                                                age >= 75  & age < 85 ~ '75_84',
                                                                age >= 85  & age < 100 ~ '85+'))
reference_data_cdc = nhis_imputed[, c(62,20)]
reference_data_cdc = reference_data_cdc[complete.cases(reference_data_cdc), ]
reference_data_cdc$race_ethnicity.cdc = as.factor(reference_data_cdc$race_ethnicity.cdc)
reference_data_cdc$race_ethnicity.cdc = relevel(reference_data_cdc$race_ethnicity.cdc, ref = "Non_hispanic_white")
reference_data_cdc$agegroup.cdc = as.factor(reference_data_cdc$agegroup.cdc)
reference_data_cdc$agegroup.cdc = relevel(reference_data_cdc$agegroup.cdc, ref = "55_64")
reference_data_cdc_model_matrix = model.matrix(as.formula(paste0("~", paste0(colnames(reference_data_cdc), collapse = "+"))), data = reference_data_cdc)


reference_data_full_model = nhis_imputed[, c(5,62, 20, 6,33:35,37:36, 47, 17, 13, 49,10, 50:55,14, 18, 12,43:46)]
reference_data_full_model_complete = reference_data_full_model[complete.cases(reference_data_full_model), ]
sampling_weights = reference_data_full_model_complete$sampling_weights
reference_data_full_model_complete = reference_data_full_model_complete[, -1]
reference_data_full_model_complete$agegroup.cdc = as.factor(reference_data_full_model_complete$agegroup.cdc)
reference_data_full_model_complete$agegroup.cdc = relevel(reference_data_full_model_complete$agegroup.cdc, ref = "55_64")
reference_data_full_model_complete$sex = as.factor(reference_data_full_model_complete$sex)
reference_data_full_model_complete$sex = relevel(reference_data_full_model_complete$sex, ref = "Female")
reference_data_full_model_complete$race_ethnicity.cdc = as.factor(reference_data_full_model_complete$race_ethnicity.cdc)
reference_data_full_model_complete$race_ethnicity.cdc = relevel(reference_data_full_model_complete$race_ethnicity.cdc, ref = "Non_hispanic_white")

reference_data_full_model_complete_model_matrix = model.matrix(as.formula(paste0("~", paste0(colnames(reference_data_full_model_complete), collapse = "+"))), data = reference_data_full_model_complete)


UK_beta = read.xlsx("~/Dropbox/NHANES_risk_score/Github/COVID19Risk/data_created/UK_model.xlsx")
UK_beta = UK_beta[c(1:11, 13,18:22, 24:32, 14:17), ]
weights = c(489297, 1043176)/sum(c(489297, 1043176))
diabetes_coeff = log(sum(weights * c(1.47, 2.23)))
UK_beta[17,2] =  diabetes_coeff
UK_beta$Variable_Name_UK[17] = "Diabetes"
temp = UK_beta$estimate[11]
UK_beta$estimate[11] = UK_beta$estimate[10]
UK_beta$estimate[10] = temp
UK_beta$Variable_Name_UK[10] = "Smoking_current"
UK_beta$Variable_Name_UK[11] = "Smoking_ex"
UK_beta = c(UK_beta$estimate)


CDC_theta = poisson.model$coefficients[1:10]
theta_CDC = as.numeric(CDC_theta)
X_CDC = reference_data_cdc_model_matrix
X_full = reference_data_full_model_complete_model_matrix

X_CDC_wt = (sampling_weights/(sum(sampling_weights))) * X_CDC
X_full_wt = (sampling_weights/(sum(sampling_weights))) * X_full

beta_initial = c(-9.367273, as.numeric(CDC_theta[-1]))

theta_UK = as.numeric(UK_beta)
names(theta_UK) = c("agegroup18_40", "agegroup40_50","agegroup60_70", "agegroup70_80", "agegroup80+", "Male","Obese_I", "Obese_II", "Obese_III", "Smk_current", "Smk_ex", "Black", "Hypertension", "Resp_ex_asthma","Asthma", "CHD", "Diabetes", "Non_hema_1", "Non_hema_2","Non_hema_3", "Hema_1","Hema_2", "Hema_3", "Stroke", "Kidney", "Arthritis", "IMD2", "IMD3", "IMD4", "IMD5")
Q = function(beta)
{
  U = as.numeric(t(X_CDC_wt) %*% as.numeric(expit(X_full[,1:10] %*% beta + X_full[,11:34] %*% theta_UK[-c(1:5,12)]) - expit(X_CDC %*% theta_CDC)))
  C = diag(length(U))
  return(as.numeric(U^T %*% C %*% U))
}

fit.optim = optim(as.numeric(beta_initial), Q, method = c("L-BFGS-B"), control = list(maxit=50))
adj_est = exp(fit.optim$par)
names(adj_est) = colnames(X_CDC)














census_18_race = read.csv('~/Dropbox/NHANES_risk_score/Github/COVID19Risk/data/Census/RACE_ETHNICITY_2018/ACSDT1Y2018.C03002_data_with_overlays_2020-05-10T211828.csv', skip = 1, header = T)
required_columns = c(1,2,3,7,9,11,13,15,17,19,25)
census_18_race = census_18_race[, required_columns]
census_18_race = census_18_race[-which(census_18_race$Estimate..Total == "null"),]

census_18_race = census_18_race %>% mutate(proportion_non_hispanic_white = as.numeric(Estimate..Total..Not.Hispanic.or.Latino..White.alone)/as.numeric(Estimate..Total),
                                           proportion_non_hispanic_asian = as.numeric(Estimate..Total..Not.Hispanic.or.Latino..Asian.alone)/as.numeric(Estimate..Total),
                                           proportion_black = as.numeric(census_18_race$Estimate..Total..Not.Hispanic.or.Latino..Black.or.African.American.alone)/as.numeric(Estimate..Total),
                                           proportion_two_more = as.numeric(Estimate..Total..Not.Hispanic.or.Latino..Two.or.more.races)/as.numeric(Estimate..Total),
                                           proportion_hispanic = as.numeric(Estimate..Total..Hispanic.or.Latino)/as.numeric(Estimate..Total))

census_18_race = census_18_race[, c(1,2,12:16)]
census_18_race = census_18_race %>% mutate(PlaceFIPS = str_sub(census_18_race$id, -7,-1))
saveRDS(census_18_race, file="/Users/prosenjitkundu/Dropbox/NHANES_risk_score/500cities_data/Updated_July_06_2020/data_created/census_18_race.rds")



temp <- tempfile()
download.file("http://download.geonames.org/export/zip/US.zip",temp)
con <- unz(temp, "US.txt")
US <- read.delim(con, header=FALSE)
unlink(temp)

colnames(US)[c(3,5,6)] <- c("city","state","county")
US$city <- tolower(US$city)
colnames(US)[c(10,11)] = c('lat','lon')

# ------------------------- merge city ID with county ID & SDI data -------------------------
rawSDI=read.xlsx('~/Dropbox/NHANES_risk_score/Github/COVID19Risk/data_created/SDI_quintile.xlsx')

brfss = readRDS("~/Dropbox/NHANES_risk_score/Github/COVID19Risk/data_created/BRFSS.rds")
cityloc = t(sapply(1:nrow(brfss),function(x){as.numeric(c(substr(strsplit(brfss$Geolocation[x],',')[[1]][1],2,11),substr(strsplit(brfss$Geolocation[x],',')[[1]][2],1,11)))}))
brfss$lat = as.numeric(cityloc[,1])
brfss$lon = as.numeric(cityloc[,2])

geocode = read.xlsx('~/Dropbox/NHANES_risk_score/Github/COVID19Risk/data/Geocodes/all-geocodes-2017.xlsx')
# select the counties that have the same state-county ID as 500 cities:
colnames(geocode) = geocode[3,]
geocode = geocode[-c(1:3),]
geocode$scID = paste0(geocode$`State Code (FIPS)`,geocode$`County Code (FIPS)`)
geocode$`Area Name (including legal/statistical area description)` = tolower(geocode$`Area Name (including legal/statistical area description)`)

rawSDI$PlaceName = NA
brfss$county = NA
brfss$sdi = NA
rawUScounty = list()
county.length = succeed = numeric()
for (i in 1:nrow(brfss)){
  # find the county of the city
  brfss_city = tolower(brfss$PlaceName[i])
  if (brfss_city == 'san buenaventura (ventura)') brfss_city = 'ventura'
  if (brfss_city == 'port st. lucie') brfss_city = 'port saint lucie'
  if (brfss_city == 'st. petersburg') brfss_city = 'saint petersburg'
  if (brfss_city == 'boise city') brfss_city = 'boise'
  if (brfss_city == 'farmington hills') brfss_city = 'farmington'
  if (brfss_city == 'rochester hills') brfss_city = 'rochester'
  if (brfss_city == 'st. paul') brfss_city = 'saint paul'
  if (brfss_city == "lee's summit") brfss_city = 'lees summit'
  if (brfss_city == "o'fallon") brfss_city = 'o fallon'
  if (brfss_city == 'st. joseph') brfss_city = 'saint joseph'
  if (brfss_city == 'st. louis') brfss_city = 'saint louis'
  if (brfss_city == 'st. george') brfss_city = 'saint george'
  if (brfss_city == 'spokane valley') brfss_city = 'spokane'
  brfss_state = brfss$StateAbbr[i]
  brfss_PlaceFIPS = brfss$PlaceFIPS[i]
  brfss_stateID = substr(brfss$PlaceFIPS[i],1,2)
  if (brfss_city == 'honolulu') brfss_stateID ='15'
  brfss_lat = brfss$lat[i]
  brfss_lon = brfss$lon[i]
  
  candidate_county = US[(US$city == brfss_city)&(US$state == brfss_state),]
  # calculate the distance between the candidates counties and the city:
  dist.county.city = sqrt((candidate_county$lat-brfss_lat)^2 + (candidate_county$lon-brfss_lon)^2)
  # select the county that has the smallest distance to the city:
  rawUScounty[[i]] = brfss_county =  as.character(candidate_county[which.min(dist.county.city),'county'])
  # check if only one county is selected
  county.length[i] = length(brfss_county)
  # Capital letter -> small letter
  brfss_county = tolower(brfss_county)
  # remove white space:
  if (county.length[i] == 1){ # if unique county info
    # remove "(city)":
    if (grepl('(city)', brfss_county, fixed=T)){
      brfss_county = gsub('[(]','',brfss_county)
      brfss_county = gsub('[)]','',brfss_county)
    }
    # remove "city of":
    if (grepl('city of', brfss_county, fixed=T)){
      brfss_county = gsub('city of','',brfss_county)
      brfss_county = sub(' ','',brfss_county)
      brfss_county = paste0(c(brfss_county,' city'),collapse='')
    }
    # remove "city and county of":
    if (grepl('city and county of', brfss_county, fixed=T)){
      brfss_county = gsub('city and county of','',brfss_county)
      brfss_county = trimws(brfss_county, 'l')
    }
    if (nchar(brfss_county) <= 6){
      brfss_county = paste0(brfss_county,' ',collapse = '')
    }
    if (brfss_county == 'saint charles') brfss_county = 'st. charles'
    which.geocode.row = which((grepl(brfss_county, geocode$`Area Name (including legal/statistical area description)`, fixed=T))&(geocode$`State Code (FIPS)` == brfss_stateID)&(geocode$`Summary Level` == '050'))
    if (length(which.geocode.row>1)){
      geocode_first_word = sapply(1:length(which.geocode.row), function(x){strsplit(geocode$`Area Name (including legal/statistical area description)`[which.geocode.row[x]],' ')[[1]][1]})
      brfss_first_word = strsplit(brfss_county,' ')[[1]][1]
      which.geocode.row = which.geocode.row[which(geocode_first_word == brfss_first_word)]
    }
    if (length(which.geocode.row) == 1){
      geocode_info = geocode[which.geocode.row,]
      # write placeFIPS info to SDI file:
      which.sdi.row = which(rawSDI$county == geocode_info$scID)
      if (length(which.sdi.row) == 1){
        brfss$county[i] = geocode_info$scID
        brfss$sdi[i] = rawSDI[which.sdi.row,'sdi_quintile']
        succeed[i] = 1
        print(i)
      }
    }
  }
}

specialcity = tolower(brfss$PlaceName[which(is.na(succeed))])
missing_county = list(c('Jefferson','Shelby'), 'Arapahoe', 'Jefferson', 'Broward', 'Broward', 'Broward',
                      'Broward', 'Fulton', 'Fulton', 'Hennepin', 'Hennepin', 'Hennepin', 'Cuyahoga')
names(missing_county) = specialcity[1:length(missing_county)]

l=1
for (i in which(is.na(succeed))){
  # find the county of the city
  brfss_city = tolower(brfss$PlaceName[i])
  brfss_state = brfss$StateAbbr[i]
  brfss_PlaceFIPS = brfss$PlaceFIPS[i]
  brfss_stateID = substr(brfss$PlaceFIPS[i],1,2)
  brfss_lat = brfss$lat[i]
  brfss_lon = brfss$lon[i]
  
  candidate_county = US[(US$county %in% missing_county[[l]]),]
  # calculate the distance between the candidates counties and the city:
  dist.county.city = sqrt((candidate_county$lat-brfss_lat)^2 + (candidate_county$lon-brfss_lon)^2)
  brfss_county =  candidate_county[which.min(dist.county.city),'county']
  brfss_county = as.character(brfss_county)
  rawUScounty[[i]] = brfss_county =  as.character(candidate_county[which.min(dist.county.city),'county'])
  county.length[i] = length(brfss_county)
  brfss_county = tolower(brfss_county)
  if (county.length[i] == 1){ # if unique county info
    # remove "(city)":
    if (grepl('(city)', brfss_county, fixed=T)){
      brfss_county = gsub('[(]','',brfss_county)
      brfss_county = gsub('[)]','',brfss_county)
    }
    # remove "city of":
    if (grepl('city of', brfss_county, fixed=T)){
      brfss_county = gsub('city of','',brfss_county)
      brfss_county = sub(' ','',brfss_county)
      brfss_county = paste0(c(brfss_county,' city'),collapse='')
    }
    # remove "city and county of":
    if (grepl('city and county of', brfss_county, fixed=T)){
      brfss_county = gsub('city and county of','',brfss_county)
      brfss_county = trimws(brfss_county, 'l')
    }
    if (nchar(brfss_county) <= 6){
      brfss_county = paste0(brfss_county,' ',collapse = '')
    }
    if (brfss_county == 'saint charles') brfss_county = 'st. charles'
    which.geocode.row = which((grepl(brfss_county, geocode$`Area Name (including legal/statistical area description)`, fixed=T))&(geocode$`State Code (FIPS)` == brfss_stateID)&(geocode$`Summary Level` == '050'))
    if (length(which.geocode.row>1)){
      geocode_first_word = sapply(1:length(which.geocode.row), function(x){strsplit(geocode$`Area Name (including legal/statistical area description)`[which.geocode.row[x]],' ')[[1]][1]})
      brfss_first_word = strsplit(brfss_county,' ')[[1]][1]
      which.geocode.row = which.geocode.row[which(geocode_first_word == brfss_first_word)]
    }
    if (length(which.geocode.row) == 1){
      geocode_info = geocode[which.geocode.row,]
      # write placeFIPS info to SDI file:
      which.sdi.row = which(rawSDI$county == geocode_info$scID)
      if (length(which.sdi.row) == 1){
        brfss$county[i] = geocode_info$scID
        brfss$sdi[i] = rawSDI[which.sdi.row,'sdi_quintile']
        succeed[i] = 1
        print(i)
      }
    }
  }
  l = l + 1
}

# remove irrelavent columns that have missing value:
missing_column = which(sapply(1:ncol(brfss),function(x){sum(complete.cases(brfss[,x]))})<500)
brfss = brfss[,-missing_column]

### output: city-level SDI
placefips_county = brfss[,c('PlaceFIPS','county')]
saveRDS(placefips_county,file='data_created/placefips_county.rds')
saveRDS(brfss,file='data_created/brfss_sdi.rds')



# ------------------------- merge with cancer data -------------------------
sdi_city = brfss
cancerraw = readRDS('~/Dropbox/NHANES_risk_score/Github/COVID19Risk/data_created/cancer_2017.rds')
# this data set contains info for the total prevalence of 3 hematological/non-hematological categories
cancerall = readRDS('~/Dropbox/NHANES_risk_score/500cities_data/Analysis/data_created/combined_hispanic.rds')
cancer = data.frame(county = sdi_city$county, PlaceFIPS = sdi_city$PlaceFIPS,
                    hematologic_cancer1 = NA, hematologic_cancer2 = NA, hematologic_cancer3 = NA,
                    non_hematologic_cancer1 = NA, non_hematologic_cancer2 = NA, non_hematologic_cancer3 = NA)
for (i in 1:nrow(cancer)){
  if ((cancer$PlaceFIPS[i] %in% cancerall$PlaceFIPS)&(sum(cancerraw$State_county_fips == cancer$county[i])==1)){
    tem = cancerraw[cancerraw$State_county_fips == cancer$county[i],]
    cancer[i,paste0('hematologic_cancer',1:3)] = tem[paste0('hemo_cat',1:3)]
    cancer[i,paste0('non_hematologic_cancer',1:3)] = tem[paste0('non_hemo_cat',1:3)]
  }
}

cancer = cancer[complete.cases(cancer),]
cancer = cancer[,-which(colnames(cancer) == 'county')]
cancer$PlaceFIPS = as.character(cancer$PlaceFIPS)
sdi_cancer_citylevel = sdi_city %>% inner_join(cancer, by = 'PlaceFIPS') 
sdi_cancer_citylevel = sdi_cancer_citylevel[complete.cases(sdi_cancer_citylevel),] # 442


# ------------------- merge with city-level age, gender and ethnicity from census data  -------------------
agesex = readRDS('~/Dropbox/NHANES_risk_score/Github/COVID19Risk/data_created/census_age_sex_2.rds')
agesex = agesex[-which(agesex[,4] == 'N'),]
tm = agesex[,-c(1,2,ncol(agesex))]
tm <- as.data.frame(sapply(tm, as.numeric))
total.population = tm$Total..Estimate..Total.population - tm$Male..Estimate..AGE..Under.5.years - tm$Male..Estimate..AGE..5.to.9.years - tm$Male..Estimate..AGE..10.to.14.years - 
  tm$Male..Estimate..AGE..15.to.19.years - tm$Female..Estimate..AGE..Under.5.years - tm$Female..Estimate..AGE..5.to.9.years - tm$Female..Estimate..AGE..10.to.14.years - tm$Female..Estimate..AGE..15.to.19.years

# update gender
tm$female_proportion = tm$Female..Estimate..AGE..20.to.24.years + tm$Female..Estimate..AGE..25.to.29.years + tm$Male..Estimate..AGE..30.to.34.years + tm$Female..Estimate..AGE..35.to.39.years + 
  tm$Female..Estimate..AGE..40.to.44.years + tm$Female..Estimate..AGE..45.to.49.years + tm$Female..Estimate..AGE..50.to.54.years + tm$Female..Estimate..AGE..55.to.59.years + 
  tm$Female..Estimate..AGE..60.to.64.years + tm$Female..Estimate..AGE..65.to.69.years + tm$Female..Estimate..AGE..70.to.74.years + tm$Female..Estimate..AGE..75.to.79.years + 
  tm$Female..Estimate..AGE..80.to.84.years + tm$Female..Estimate..AGE..85.years.and.over
tm$female_proportion = tm$female_proportion/total.population
tm$male_proportion = tm$Male..Estimate..AGE..20.to.24.years + tm$Male..Estimate..AGE..25.to.29.years + tm$Male..Estimate..AGE..30.to.34.years + tm$Male..Estimate..AGE..35.to.39.years + 
  tm$Male..Estimate..AGE..40.to.44.years + tm$Male..Estimate..AGE..45.to.49.years + tm$Male..Estimate..AGE..50.to.54.years + tm$Male..Estimate..AGE..55.to.59.years + 
  tm$Male..Estimate..AGE..60.to.64.years + tm$Male..Estimate..AGE..65.to.69.years + tm$Male..Estimate..AGE..70.to.74.years + tm$Male..Estimate..AGE..75.to.79.years + 
  tm$Male..Estimate..AGE..80.to.84.years + tm$Male..Estimate..AGE..85.years.and.over
tm$male_proportion = tm$male_proportion/total.population

# update age
tm$Age_18_39 = tm$Male..Estimate..AGE..20.to.24.years + tm$Male..Estimate..AGE..25.to.29.years + tm$Male..Estimate..AGE..30.to.34.years + tm$Male..Estimate..AGE..35.to.39.years +
  tm$Female..Estimate..AGE..20.to.24.years + tm$Female..Estimate..AGE..25.to.29.years + tm$Female..Estimate..AGE..30.to.34.years + tm$Female..Estimate..AGE..35.to.39.years
tm$Age_40_49 = tm$Male..Estimate..AGE..40.to.44.years + tm$Male..Estimate..AGE..45.to.49.years + 
  tm$Female..Estimate..AGE..40.to.44.years + tm$Female..Estimate..AGE..45.to.49.years
tm$Age_50_59 = tm$Male..Estimate..AGE..50.to.54.years + tm$Male..Estimate..AGE..55.to.59.years + 
  tm$Female..Estimate..AGE..50.to.54.years + tm$Female..Estimate..AGE..55.to.59.years
tm$Age_60_69 = tm$Male..Estimate..AGE..60.to.64.years + tm$Male..Estimate..AGE..65.to.69.years + 
  tm$Female..Estimate..AGE..60.to.64.years + tm$Female..Estimate..AGE..65.to.69.years
tm$Age_70_79 = tm$Male..Estimate..AGE..70.to.74.years + tm$Male..Estimate..AGE..75.to.79.years + 
  tm$Female..Estimate..AGE..70.to.74.years + tm$Female..Estimate..AGE..75.to.79.years
tm$Age_80_150 = tm$Male..Estimate..AGE..80.to.84.years + tm$Male..Estimate..AGE..85.years.and.over + 
  tm$Female..Estimate..AGE..80.to.84.years + tm$Female..Estimate..AGE..85.years.and.over

tm$Age_18_39 = tm$Age_18_39/total.population
tm$Age_40_49 = tm$Age_40_49/total.population
tm$Age_50_59 = tm$Age_50_59/total.population
tm$Age_60_69 = tm$Age_60_69/total.population
tm$Age_70_79 = tm$Age_70_79/total.population
tm$Age_80_150 = tm$Age_80_150/total.population

agesex = data.frame(id = agesex$id, Geographic.Area.Name = agesex$Geographic.Area.Name,
                    population = agesex$Total..Estimate..Total.population,
                    female_proportion = tm$female_proportion, male_proportion = tm$male_proportion,
                    Age_18_39 = tm$Age_18_39, Age_40_49 = tm$Age_40_49, Age_50_59 = tm$Age_50_59,
                    Age_60_69 = tm$Age_60_69, Age_70_79 = tm$Age_70_79, Age_80_150 = tm$Age_80_150,
                    PlaceFIPS = agesex$PlaceFIPS)
agesex$PlaceFIPS = as.character(agesex$PlaceFIPS)
# add hispanic:
#race = readRDS('~/Dropbox/NHANES_risk_score/Github/COVID19Risk/data_created/census_18_race.rds')
race = census_18_race
race = race[,-c(1,2)]
combined = sdi_cancer_citylevel %>% inner_join(agesex, by = 'PlaceFIPS') 
combined = combined %>% inner_join(race, by = 'PlaceFIPS') 
sum(complete.cases(combined)) # 442
combined = combined[complete.cases(combined),]

# update diabetes
ratio.unctrl.ctrl = 0.6133612 # 0.597411
pr.unctrl = ratio.unctrl.ctrl/(1+ratio.unctrl.ctrl)
combined$DIABETES_unctrled_CrudePrev = combined$DIABETES_CrudePrev * pr.unctrl
combined$DIABETES_ctrled_CrudePrev = combined$DIABETES_CrudePrev * (1-pr.unctrl)

# asthma - due to the absense of US data for asthma,
# we are using the prevalence for different categories of asthma from the UK study

# update IMD
combined$IMD1 = ifelse(combined$sdi==1,1,0)
combined$IMD2 = ifelse(combined$sdi==2,1,0)
combined$IMD3 = ifelse(combined$sdi==3,1,0)
combined$IMD4 = ifelse(combined$sdi==4,1,0)
combined$IMD5 = ifelse(combined$sdi==5,1,0)

# update smoking:
Pr.smoking_current = 0.1395412
Pr.smoking_ex = 0.2263748
combined$smoking_ex_proportion = combined$CSMOKING_CrudePrev * (Pr.smoking_ex)/(Pr.smoking_current+Pr.smoking_ex)/100
combined$smoking_current_proportion = combined$CSMOKING_CrudePrev * (Pr.smoking_current)/(Pr.smoking_current+Pr.smoking_ex)/100

# update obesity:
Pr.obesity0 = 0.6662283
Pr.obesity1 = 0.1792105
Pr.obesity2 = 0.07423196
Pr.obesity3 = 0.08032927
Pr.obesity = Pr.obesity1 + Pr.obesity2 + Pr.obesity3
combined$obesity1 = combined$OBESITY_CrudePrev * Pr.obesity1/(Pr.obesity*100)
combined$obesity2 = combined$OBESITY_CrudePrev * Pr.obesity2/(Pr.obesity*100)
combined$obesity3 = combined$OBESITY_CrudePrev * Pr.obesity3/(Pr.obesity*100)

# change to percentages:
combined[,c('OBESITY_CrudePrev','BPHIGH_CrudePrev','COPD_CrudePrev','CASTHMA_CrudePrev',
            'CHD_CrudePrev', 'DIABETES_unctrled_CrudePrev','DIABETES_ctrled_CrudePrev', 
            'STROKE_CrudePrev','KIDNEY_CrudePrev','ARTHRITIS_CrudePrev')] = 
  combined[,c('OBESITY_CrudePrev','BPHIGH_CrudePrev','COPD_CrudePrev','CASTHMA_CrudePrev', 
              'CHD_CrudePrev', 'DIABETES_unctrled_CrudePrev','DIABETES_ctrled_CrudePrev', 
              'STROKE_CrudePrev','KIDNEY_CrudePrev','ARTHRITIS_CrudePrev')]/100

# output: city-level data
saveRDS(combined,file='data_created/combined_updated.rds')















#------------NO-model assumption for imputing SDI-------#
nhis = readRDS("~/Dropbox/NHANES_risk_score/500cities_data/Analysis/data_created/individual_rs_covariates.rds")
data = readRDS("~/Dropbox/NHANES_risk_score/500cities_data/Analysis/data_created/full_output_updated.rds")
data$population = as.numeric(as.character(data$population))
nhis.black = subset(nhis,race_ethnicity=="Black")
nhis.white = subset(nhis,race_ethnicity=="Non_hispanic_white")
nhis.hispanic = subset(nhis,race_ethnicity=="Hispanic")

#Black
city.size = as.numeric(as.character(data$population))/sum(as.numeric(as.character(data$population)))
prob.black = sum(data$proportion_black  * as.numeric(as.character(data$population)))/sum(as.numeric(as.character(data$population)))
prob.cities.given.black = (data$proportion_black * city.size)/prob.black
data.black = data.frame(data$sdi, prob.cities.given.black)
colnames(data.black) = c("sdi", "prob.cities.given.black")
theta = log(c(1, 1.19, 1.26, 1.53, 1.70))
data.black = data.black %>% mutate(coefficients = case_when(sdi == 1 ~ theta[1],
                                                            sdi == 2 ~ theta[2],
                                                            sdi == 3 ~ theta[3],
                                                            sdi == 4 ~ theta[4],
                                                            sdi == 5 ~ theta[5]))
nhis.black.risk.sdi = sum(data.black$prob.cities.given.black*exp(data.black$coefficients))
nhis.black.risk.sdi1 = sum(data.black$prob.cities.given.black*data.black$coefficients)
prev.sdi.black = sapply(1:5,function(x){sum(data.black[data.black$sdi == x,'prob.cities.given.black'])})


#Non_hispanic_white
city.size = as.numeric(as.character(data$population))/sum(as.numeric(as.character(data$population)))
prob.white = sum(data$proportion_non_hispanic_white_asian  * as.numeric(as.character(data$population)))/sum(as.numeric(as.character(data$population)))
prob.cities.given.white = (data$proportion_non_hispanic_white_asian * city.size)/prob.white
data.white = data.frame(data$sdi, prob.cities.given.white)
colnames(data.white) = c("sdi", "prob.cities.given.white")
theta = log(c(1, 1.19, 1.26, 1.53, 1.70))
data.white = data.white %>% mutate(coefficients = case_when(sdi == 1 ~ theta[1],
                                                            sdi == 2 ~ theta[2],
                                                            sdi == 3 ~ theta[3],
                                                            sdi == 4 ~ theta[4],
                                                            sdi == 5 ~ theta[5]))
nhis.white.risk.sdi = sum(data.white$prob.cities.given.white*exp(data.white$coefficients))
nhis.white.risk.sdi1 = sum(data.white$prob.cities.given.white*data.white$coefficients)
prev.sdi.white = sapply(1:5,function(x){sum(data.white[data.white$sdi == x,'prob.cities.given.white'])})

#Hispanic
city.size = as.numeric(as.character(data$population))/sum(as.numeric(as.character(data$population)))
prob.hispanic = sum(data$proportion_hispanic  * as.numeric(as.character(data$population)))/sum(as.numeric(as.character(data$population)))
prob.cities.given.hispanic = (data$proportion_hispanic * city.size)/prob.hispanic
data.hispanic = data.frame(data$sdi, prob.cities.given.hispanic)
colnames(data.hispanic) = c("sdi", "prob.cities.given.hispanic")
theta = log(c(1, 1.19, 1.26, 1.53, 1.70))
data.hispanic = data.hispanic %>% mutate(coefficients = case_when(sdi == 1 ~ theta[1],
                                                                  sdi == 2 ~ theta[2],
                                                                  sdi == 3 ~ theta[3],
                                                                  sdi == 4 ~ theta[4],
                                                                  sdi == 5 ~ theta[5]))
nhis.hispanic.risk.sdi = sum(data.hispanic$prob.cities.given.hispanic*exp(data.hispanic$coefficients))
nhis.hispanic.risk.sdi1 = sum(data.hispanic$prob.cities.given.hispanic*data.hispanic$coefficients)
prev.sdi.hispanic = sapply(1:5,function(x){sum(data.hispanic[data.hispanic$sdi == x,'prob.cities.given.hispanic'])})



###########
nhis = nhis %>% mutate(sdi_rs_est = case_when(race_ethnicity == "Black" ~ nhis.black.risk.sdi1,
                                              race_ethnicity == "Hispanic" ~ nhis.hispanic.risk.sdi1,
                                              race_ethnicity == "Non_hispanic_white" ~ nhis.white.risk.sdi1))




dat = readRDS('~/Dropbox/NHANES_risk_score/Github/COVID19Risk/data_created/nhis_2017.rds')
#dat = dat[complete.cases(dat[,-which(colnames(dat) %in% c('diagnoses_cancer'))]),]
dat$non_hematologic_cancer1 = ifelse((dat$non_hematologic_cancer=='Non_hematological')&(dat$diagnoses_cancer == 'less_than_1_yr'),1,0)
dat$non_hematologic_cancer1[which(dat$non_hematologic_cancer=='Non_hematological' & is.na(dat$diagnoses_cancer) == T)] = NA
dat$non_hematologic_cancer2 = ifelse((dat$non_hematologic_cancer=='Non_hematological')&(dat$diagnoses_cancer == '1_5yr'),1,0)
dat$non_hematologic_cancer2[which(dat$non_hematologic_cancer=='Non_hematological' & is.na(dat$diagnoses_cancer) == T)] = NA

dat$non_hematologic_cancer3 = ifelse((dat$non_hematologic_cancer=='Non_hematological')&(dat$diagnoses_cancer == 'greater_than_5_yr'),1,0)
dat$non_hematologic_cancer3[which(dat$non_hematologic_cancer=='Non_hematological' & is.na(dat$diagnoses_cancer) == T)] = NA

# hematologic_cancer
dat$hematologic_cancer1 = ifelse((dat$hematologic_cancer=='Hematological')&(dat$diagnoses_cancer == 'less_than_1_yr'),1,0)
dat$hematologic_cancer1[which(dat$hematologic_cancer=='Hematological' & is.na(dat$diagnoses_cancer) == T)] = NA

dat$hematologic_cancer2 = ifelse((dat$hematologic_cancer=='Hematological')&(dat$diagnoses_cancer == '1_5yr'),1,0)
dat$hematologic_cancer2[which(dat$hematologic_cancer=='Hematological' & is.na(dat$diagnoses_cancer) == T)] = NA

dat$hematologic_cancer3 = ifelse((dat$hematologic_cancer=='Hematological')&(dat$diagnoses_cancer == 'greater_than_5_yr'),1,0)
dat$hematologic_cancer3[which(dat$hematologic_cancer=='Hematological' & is.na(dat$diagnoses_cancer) == T)] = NA
dat$rhemumatoid[which(dat$rhemumatoid== "NA")] = NA
dat$diabetes[which(dat$diabetes == "NA")] = NA
dat$stroke[which(dat$stroke== "NA")] = NA
dat$smoking_status[which(dat$smoking_status == "NA")] = NA

dat.comp = dat[complete.cases(dat), -c(21:23)]
