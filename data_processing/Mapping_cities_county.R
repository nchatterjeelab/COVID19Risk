geocodes_data = openxlsx::read.xlsx('data/Geocodes/all-geocodes-2017.xlsx', startRow = 5, colNames = TRUE)
geocodes_data_state = openxlsx::read.xlsx('data/Geocodes/all-geocodes-2017.xlsx', sheet=2, colNames = TRUE)
geocodes_data = geocodes_data[-1,]
geocodes_data = geocodes_data %>% mutate(State = geocodes_data_state$STATE[match(as.numeric(geocodes_data$`State.Code.(FIPS)`), geocodes_data_state$FIPS)])

#county_place = fread("ftp://ftp2.census.gov/geo/docs/reference/codes/PLACElist.txt")

#Removing duplicated rows
geocodes_data_sub = geocodes_data[!duplicated(geocodes_data), ]

#----Creating a Place FIPS code representing 7 digits (2(state) + 5(place)) in geocodes_data_sub---#
geocodes_data_sub = geocodes_data_sub %>% 
                    mutate(PlaceFIPS = paste0(geocodes_data_sub$`State.Code.(FIPS)`, geocodes_data_sub$`Place.Code.(FIPS)`)) %>%
                    mutate(State_county_fips = paste0(geocodes_data_sub$`State.Code.(FIPS)`, geocodes_data_sub$`County.Code.(FIPS)`))

geocodes_data_sub$PlaceFIPS =  sprintf("%07d", as.numeric(geocodes_data_sub$PlaceFIPS))

saveRDS(geocodes_data_sub, file = 'data/mapping.rds')

#--- Reading BRFSS data
BRFSS_data = read.csv('data/BRFSS/500_Cities__City-level_Data__GIS_Friendly_Format___2019_release.csv')
BRFSS_data$PlaceFIPS =  sprintf("%07d",BRFSS_data$PlaceFIPS)
saveRDS(BRFSS_data, file = '/data/BRFSS.rds')


#---- Census Data
#--Age-Sex
census_17_age_sex = read.csv('data/Census/AGE_SEX_2017/ACSST1Y2017.S0101_data_with_overlays_2020-05-09T173530.csv', skip = 1, header = T)
odd_percent_female = seq(309,343,2)
odd_percent_male = seq(157,191,2)
census_17_age_sex = census_17_age_sex[, c(1,2,3, odd_percent_male, odd_percent_female)]
# census_17_age_sex = census_17_age_sex %>% mutate(female_proportion = as.numeric(Female..Estimate..Total.population)/as.numeric(Total..Estimate..Total.population), male_proportion = as.numeric(Male..Estimate..Total.population)/as.numeric(Total..Estimate..Total.population))
# census_17_age_sex = census_17_age_sex[, c(1,2,odd_percent_age, 459,460)]
# census_17_age_sex[, 3:20] = as.matrix(sapply(census_17_age_sex[, 3:20], as.numeric))/100
# colnames(census_17_age_sex)[3:20] = gsub("Percent__Estimate", "proportion", gsub("\\.", "_",colnames(census_17_age_sex)[3:20])) 
census_17_age_sex = census_17_age_sex %>% mutate(PlaceFIPS = str_sub(census_17_age_sex$id, -7,-1))
saveRDS(census_17_age_sex, file = 'data/census_age_sex.rds')

#--Race
census_18_race = read.csv('data/Census/RACE_ETHNICITY_2018/ACSDT1Y2018.C03002_data_with_overlays_2020-05-10T211828.csv', skip = 1, header = T)
required_columns = c(1,2,3,7,9,11,13,15,17,19,25)
census_18_race = census_18_race[, required_columns]
census_18_race = census_18_race[-which(census_18_race$Estimate..Total == "null"),]

total_est =  rowSums(cbind(as.numeric(census_18_race$Estimate..Total..Not.Hispanic.or.Latino..White.alone), as.numeric(census_18_race$Estimate..Total..Not.Hispanic.or.Latino..Black.or.African.American.alone),as.numeric(census_18_race$Estimate..Total..Not.Hispanic.or.Latino..Asian.alone), as.numeric(census_18_race$Estimate..Total..Hispanic.or.Latino)), na.rm=TRUE)

total_non_hispanic_white_asian = rowSums(cbind(as.numeric(census_18_race$Estimate..Total..Not.Hispanic.or.Latino..White.alone), as.numeric(census_18_race$Estimate..Total..Not.Hispanic.or.Latino..Asian.alone)), na.rm = T)

census_18_race = census_18_race %>% mutate(proportion_non_hispanic_white_asian = total_non_hispanic_white_asian/total_est,
                                           proportion_black = as.numeric(census_18_race$Estimate..Total..Not.Hispanic.or.Latino..Black.or.African.American.alone)/total_est,
                                           proportion_hispanic = as.numeric(census_18_race$Estimate..Total..Hispanic.or.Latino)/total_est)

census_18_race = census_18_race[, c(1,2,12:14)]
census_18_race = census_18_race %>% mutate(PlaceFIPS = str_sub(census_18_race$id, -7,-1))
saveRDS(census_18_race, file = '/data/census_18_race.rds')

#-----Cancer Data
cancer_data = fread("data/Cancer/USCS-1999-2016-ASCII/BYAREA_COUNTY.TXT", header = T) %>%
  filter(RACE == "All Races", SEX == "Male and Female") %>%
  mutate(State_county_fips = substring( unlist(str_extract_all(AREA, "\\([^()]+\\)")), 2, nchar( unlist(str_extract_all(AREA, "\\([^()]+\\)")))-1)) %>%
  filter(POPULATION != 0) %>%
  select(State_county_fips, COUNT, POPULATION, SITE, EVENT_TYPE) %>%
  filter(EVENT_TYPE == "Incidence")
cancer_data$COUNT = gsub("~", NA, cancer_data$COUNT)

cancer_data = cancer_data %>% select(COUNT, POPULATION, SITE) %>%
  group_by(SITE) %>% summarise(incidence = sum(as.numeric(COUNT), na.rm = T)/sum(as.numeric(POPULATION), na.rm = T))
survival_rate_5_years = read.csv('data/Cancer/USCS_SurvivalAllCancers.csv', header = T) %>%
  select(CancerType, X5.yearRelativeSurvival.) %>%
  mutate(survival_5_year_rate = as.numeric(gsub("'","",X5.yearRelativeSurvival.))/100) %>%
  mutate(SITE = gsub("'","",CancerType)) %>%
  select(SITE, survival_5_year_rate)
survival_rate_5_years$SITE[21] = c("Corpus and Uterus, NOS")
survival_rate_5_years = survival_rate_5_years[-22,]
incidence_survival = merge(cancer_data, survival_rate_5_years, by = "SITE", all= T)

total = sum(incidence_survival$incidence[c(7,11,18,19)])
survival_5year_hemo = sum(incidence_survival$incidence[c(7,11,18,19)]*incidence_survival$survival_5_year_rate[c(7,11,18,19)])/total
relative_incidence_hemo = sum(incidence_survival$incidence[c(7,11,18,19)])/incidence_survival$incidence[1]
survival_5year_nonhemo = (incidence_survival$survival_5_year_rate[1] - relative_incidence_hemo*survival_5year_hemo)/(1 - relative_incidence_hemo)
survival_1year_hemo = survival_5year_hemo/5
survival_1year_nonhemo = survival_5year_nonhemo/5


cancer_data = fread("data/Cancer/USCS-1999-2016-ASCII/BYAREA_COUNTY.TXT", header = T) %>%
  filter(RACE == "All Races", SEX == "Male and Female") %>%
  mutate(State_county_fips = substring( unlist(str_extract_all(AREA, "\\([^()]+\\)")), 2, nchar( unlist(str_extract_all(AREA, "\\([^()]+\\)")))-1)) %>%
  filter(POPULATION != 0) %>%
  select(State_county_fips, COUNT, POPULATION, SITE, EVENT_TYPE) 
cancer_data$COUNT = gsub("~", NA, cancer_data$COUNT)
#cancer_data = cancer_data[which(cancer_data$SITE!= "All Cancer Sites Combined"), ]

hemato_cancer = function(x)
{
  if(x == "Hodgkin Lymphoma" | x ==  "Non-Hodgkin Lymphoma" | x == "Leukemias" | x == "Myeloma"){
    return("hematologic_cancer")}else{
      return(x)
    }
  
  
  
}
cancer_data = cancer_data %>% mutate(cancer_type = sapply(cancer_data$SITE, hemato_cancer))
cancer_data = cancer_data[which(cancer_data$cancer_type == "hematologic_cancer" | cancer_data$cancer_type == "All Cancer Sites Combined"), ]
cancer_data = cancer_data %>% group_by(State_county_fips,cancer_type, EVENT_TYPE) %>% mutate(COUNT = sum(as.numeric(COUNT), na.rm = T)) %>% 
  select(State_county_fips, COUNT, POPULATION, cancer_type, EVENT_TYPE) %>% distinct() %>% filter(POPULATION != 0)
cancer_data$COUNT[which(cancer_data$COUNT == 0)] = NA

non_hematologic_cancer_incidence = rep(NA, 3044)
i = 1
for(k in 1:3044)
{
  if(is.na(cancer_data$COUNT[i+2]) == T)
  {
    non_hematologic_cancer_incidence[k] =  cancer_data$COUNT[i]
  }else{
    non_hematologic_cancer_incidence[k] = cancer_data$COUNT[i] - cancer_data$COUNT[i+2]
  }
  i = i+4
}

non_hematologic_cancer_mortality = rep(NA, 3044)
i = 2
for(k in 1:3044)
{
  if(is.na(cancer_data$COUNT[i+2]) == T)
  {
    non_hematologic_cancer_mortality[k] =  cancer_data$COUNT[i]
  }else{
    non_hematologic_cancer_mortality[k] = cancer_data$COUNT[i] - cancer_data$COUNT[i+2]
  }
  i = i+4
}

odd_numbers = seq(1,12176,4)
cancer_data$COUNT[odd_numbers] = non_hematologic_cancer_incidence
odd_numbers = seq(2,12176,4)
cancer_data$COUNT[odd_numbers] = non_hematologic_cancer_mortality
cancer_data$cancer_type[which(cancer_data$cancer_type == "All Cancer Sites Combined")] = "non_hematologic_cancer"
cancer_data = cancer_data %>% mutate(proportion = COUNT/POPULATION, annual_proportion = proportion/5)
cancer_data = cancer_data %>% select(State_county_fips, cancer_type, EVENT_TYPE, annual_proportion)
cancer_data = spread(cancer_data, cancer_type, annual_proportion)
cancer_data = cancer_data[which(cancer_data$EVENT_TYPE == "Incidence"), ]
cancer_data = cancer_data %>% mutate(hemo_one_yr_survival = survival_1year_hemo, nonhemo_one_yr_survival = survival_1year_nonhemo)
cancer_data = cancer_data %>% mutate(hemo_cat1 = hematologic_cancer*hemo_one_yr_survival/2, hemo_cat1 = hematologic_cancer*hemo_one_yr_survival/2, hemo_cat2 = 8*hematologic_cancer*hemo_one_yr_survival, hemo_cat3 = 15* 7.5* hematologic_cancer*hemo_one_yr_survival, hemo_cat1 = hematologic_cancer*hemo_one_yr_survival/2, non_hemo_cat1 = non_hematologic_cancer*nonhemo_one_yr_survival/2, non_hemo_cat2 = 8*non_hematologic_cancer*nonhemo_one_yr_survival, non_hemo_cat3 = 15* 7.5* non_hematologic_cancer*nonhemo_one_yr_survival)
cancer_data = cancer_data %>% ungroup() %>% select(State_county_fips, hemo_cat1, hemo_cat2, hemo_cat3, non_hemo_cat1, non_hemo_cat2, non_hemo_cat3)
saveRDS(cancer_data,'data_created/cancer_2016.rds')

# non_hemato_cancer_incidence = cancer_data[which(cancer_data$cancer_type == "non_hematologic_cancer"), ]
# hemato_cancer_incidence = cancer_data[which(cancer_data$cancer_type == "hematologic_cancer"), ]
# hemato_cancer_incidence_US = sum(hemato_cancer_incidence$COUNT, na.rm = T)/sum(hemato_cancer_incidence$POPULATION, na.rm = T)
# non_hemato_cancer_incidence_US = sum(non_hemato_cancer_incidence$COUNT, na.rm = T)/sum(non_hemato_cancer_incidence$POPULATION, na.rm = T)
# cancer_prevalance_US =  read.csv("~/Downloads/USCS_Prevalence.csv", header = T) %>%
#                         filter(Duration == "'15-year Limited Duration'") %>% 
#                         select(CancerType, Count)
# cancer_prevalance_US$Count = gsub("'", "", cancer_prevalance_US$Count)
# cancer_prevalance_US = cancer_prevalance_US %>% group_by(CancerType) %>% mutate(Count = sum(as.numeric(Count))) %>%
#                        distinct()
# cancer_prevalance_US_hematologic = sum(cancer_prevalance_US$Count[c(8,17,18,27)])/323100000
# cancer_prevalance_US_non_hematologic = (cancer_prevalance_US$Count[1] - sum(cancer_prevalance_US$Count[c(8,17,18,27)]))/323100000
# 
# ratio_hemato = cancer_prevalance_US_hematologic/hemato_cancer_incidence_US
# ratio_non_hemato = cancer_prevalance_US_non_hematologic/non_hemato_cancer_incidence_US
# 
# cancer_data$incidence[which(cancer_data$cancer_type == "non_hematologic_cancer")] =  cancer_data$incidence[which(cancer_data$cancer_type == "non_hematologic_cancer")] * ratio_non_hemato
# cancer_data$incidence[which(cancer_data$cancer_type == "hematologic_cancer")] =  cancer_data$incidence[which(cancer_data$cancer_type == "hematologic_cancer")] * ratio_hemato
# colnames(cancer_data)[5]  = "prevalence"
# cancer_data = cancer_data %>% select(State_county_fips, cancer_type, prevalence)
# cancer_data = spread(cancer_data, cancer_type, prevalence)
# saveRDS(cancer_data,"~/Dropbox/NHANES_risk_score/500cities_data/Analysis/data_created/cancer_2016.rds" )
# 


#-----NHANES----#
#Diabetes
hbAc1_level = sasxport.get('data/NHANES_Asthma_Diabetes/GHB_J.xpt')
demo = sasxport.get('data/NHANES_Asthma_Diabetes/DEMO_J.xpt')
demo_sampling_weights = demo[which(demo$ridreth3 != 7 & demo$ridageyr>=18), c(1,5,8,41)]
diabetes_questionnaire = sasxport.get('data/NHANES_Asthma_Diabetes/DIQ_J.xpt')
seqno_diabetes_yes = diabetes_questionnaire$seqn[which(diabetes_questionnaire$diq010 == 1)]
hbAc1_level_diabetes_yes = hbAc1_level[which(hbAc1_level$seqn %in% intersect(seqno_diabetes_yes, hbAc1_level$seqn) == T), ]
sampling_weights = demo_sampling_weights[which(demo_sampling_weights$seqn %in% intersect(seqno_diabetes_yes, hbAc1_level$seqn) == T), ]
hbAc1_level_diabetes_yes_sampling_weighs = merge(sampling_weights, hbAc1_level_diabetes_yes, by = "seqn")
count_controlled_diabetes = sum(hbAc1_level_diabetes_yes_sampling_weighs$wtmec2yr[which(as.numeric(hbAc1_level_diabetes_yes_sampling_weighs$lbxgh) < 7.4)])
count_uncontrolled_diabetes = sum(hbAc1_level_diabetes_yes_sampling_weighs$wtmec2yr[which(as.numeric(hbAc1_level_diabetes_yes_sampling_weighs$lbxgh) >= 7.4)])
ratio_uncontrolled_to_controlled = count_uncontrolled_diabetes/count_controlled_diabetes
# ratio_uncontrolled_to_controlled = 0.6133612

#Asthma
asthma = sasxport.get('data/NHANES_Asthma_Diabetes/MCQ_J.xpt')
asthma = asthma[which(asthma$mcq010 == 1), c(1,2)]
demo_sampling_weights = demo[, c(1,40)]
drug_prescription = sasxport.get('data/NHANES_Asthma_Diabetes/RXQ_DRUG.xpt')
drug_medicine = sasxport.get('data/NHANES_Asthma_Diabetes/RXQ_RX_I_2015_2016.XPT')

drug_code_OCS = c("d00760", "d00761", "d01296", "d04276", "d04611", "d04795", "d05262", "d05465", "d07660", "d08100", "d08666")


#----NHIS----#
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
nhis_2017_data = nhis_2017_data %>% mutate(diabetes = case_when(DIBEV1 == 1 ~ 'Yes',
                                                                DIBEV1 == 2 | DIBEV1 == 3 ~ 'No',
                                                                DIBEV1 > 3 ~ 'NA'))
nhis_2017_data = nhis_2017_data %>% mutate(smoking_status = case_when(SMKSTAT2 == 3 ~ 'Former',
                                                                SMKSTAT2 == 1 | SMKSTAT2 == 2 ~ 'Current',
                                                                SMKSTAT2 == 4 ~ 'Never',
                                                                SMKSTAT2 > 4 ~ 'NA'))
nhis_2017_data = nhis_2017_data %>% mutate(rhemumatoid = case_when(ARTH1 == 1 ~ 'Yes',
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

nhis_2017_data = nhis_2017_data %>% mutate(race_ethnicity = case_when(ethnicity == "Not_hispanic" & race == "White" ~ 'Non_hispanic_white',
                                                                      ethnicity == "Not_hispanic" & race == "Black" ~ 'Black',
                                                                      ethnicity == "Not_hispanic" & race == "Asian" ~ 'Non_hispanic_white',
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
nhis_2017_data = nhis_2017_data[, c(2:5,7,8,12, 99:105,110:114, 118:120, 123:124)]
nhis_2017_data$hematologic_cancer[which(is.na(nhis_2017_data$hematologic_cancer) == T & nhis_2017_data$non_hematologic_cancer == "Non_hematological")] = "99"
nhis_2017_data$non_hematologic_cancer[nhis_2017_data$hematologic_cancer == "Hematological" & is.na(nhis_2017_data$non_hematologic_cancer) == T] = "99"
colnames(nhis_2017_data)[5:7] = c("sampling_weights", "sex", "age")
nhis_2017_data = nhis_2017_data[which(nhis_2017_data$race_ethnicity == "Non_hispanic_white" | nhis_2017_data$race_ethnicity == "Hispanic" | nhis_2017_data$race_ethnicity == "Black"),]
saveRDS(nhis_2017_data, 'data_created/nhis_2017.rds')
