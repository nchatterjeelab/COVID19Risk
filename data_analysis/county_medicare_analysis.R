library(dplyr)
library(Hmisc)
library(openxlsx)
demo.data.age.sex.race = read.csv("~/Dropbox/NHANES_risk_score/500cities_data/Updated_July_06_2020/medicare_data/data/age_sex_race.csv")


demo.data.age.sex.race.year2019 = demo.data.age.sex.race %>% filter(YEAR == 12)
demo.data.age.sex.race.year2019$COUNTY[which(nchar(demo.data.age.sex.race.year2019$COUNTY) == 1)] = paste0("00", demo.data.age.sex.race.year2019$COUNTY[which(nchar(demo.data.age.sex.race.year2019$COUNTY) == 1)])
demo.data.age.sex.race.year2019$COUNTY[which(nchar(demo.data.age.sex.race.year2019$COUNTY) == 2)] = paste0("0", demo.data.age.sex.race.year2019$COUNTY[which(nchar(demo.data.age.sex.race.year2019$COUNTY) == 2)])
demo.data.age.sex.race.year2019$STATE[which(nchar(demo.data.age.sex.race.year2019$STATE) == 1)] = paste0("0", demo.data.age.sex.race.year2019$STATE[which(nchar(demo.data.age.sex.race.year2019$STATE) == 1)])
demo.data.age.sex.race.year2019 = demo.data.age.sex.race.year2019 %>% mutate(STATE_COUNTY_FIPS = paste0(STATE, COUNTY))


demo.data.age.year2019 = demo.data.age.sex.race.year2019 %>%  filter(AGEGRP >= 14) %>% 
  group_by(STATE_COUNTY_FIPS, CTYNAME, STNAME) %>% summarise(TOT_POP_AGE_GREATER_65 = sum(TOT_POP))

demo.data.age.year2019.all.age = demo.data.age.sex.race.year2019 %>% group_by(STATE_COUNTY_FIPS, CTYNAME, STNAME) %>%
  summarise(TOT_POP = sum(TOT_POP))

demo.data.age.prevalence.greater.65 = merge(demo.data.age.year2019, demo.data.age.year2019.all.age, by = c("STATE_COUNTY_FIPS", "CTYNAME", "STNAME"))
demo.data.age.prevalence.greater.65 = demo.data.age.prevalence.greater.65 %>% mutate(age_prevalance = TOT_POP_AGE_GREATER_65/TOT_POP)


demo.data.age.sex.race.year2019.age.group.greater.than.65 = demo.data.age.sex.race.year2019 %>% filter(AGEGRP >= 14)


demo.data.age.sex.race.year2019.age.group.greater.than.65 = demo.data.age.sex.race.year2019.age.group.greater.than.65 %>% group_by(STATE_COUNTY_FIPS, CTYNAME, STNAME) %>%
  summarise(TOT_POP = sum(TOT_POP), TOT_MALE = sum(TOT_MALE), TOT_FEMALE = sum(TOT_FEMALE), TOT_HISPANIC = sum(H_MALE, H_FEMALE), TOT_NH_WHITE = sum(NHWA_MALE + NHWA_FEMALE), TOT_NH_BLACK = sum(NHBA_MALE + NHBA_FEMALE), TOT_NH_ASIAN = sum(NHAA_MALE + NHAA_FEMALE), TOT_NH_AMERICAN_INDIAN_ALASKAN_NATIVE = sum(NHIA_MALE + NHIA_FEMALE))

demo.data.age.sex.race.year2019.age.group.greater.than.65 = demo.data.age.sex.race.year2019.age.group.greater.than.65 %>%
  mutate(male = TOT_MALE/TOT_POP, female = TOT_FEMALE/TOT_POP, hispanic = TOT_HISPANIC/TOT_POP, non_hispanic_white = TOT_NH_WHITE/TOT_POP, non_hispanic_black = TOT_NH_BLACK/TOT_POP, non_hispanic_asian = TOT_NH_ASIAN/TOT_POP, non_hispanic_ai_an = TOT_NH_AMERICAN_INDIAN_ALASKAN_NATIVE/TOT_POP)


demo.data.age.greater.65 = merge(demo.data.age.prevalence.greater.65, demo.data.age.sex.race.year2019.age.group.greater.than.65, by = c("STATE_COUNTY_FIPS", "CTYNAME", "STNAME"))

demo.data.age.greater.65 = demo.data.age.greater.65[, c(1:3, 6, 15:21)]

#saveRDS(demo.data.age.greater.65, file = "~/Dropbox/NHANES_risk_score/500cities_data/Updated_July_06_2020/medicare_data/data/age_greater_than_65_sex_race.rds")



#CMS data
data.cms = readRDS("~/Dropbox/NHANES_risk_score/500cities_data/Updated_July_06_2020/medicare_data/data/CMS_Medicare_Data_0709.rds")
data.cms.order = data.cms[order(data.cms$fips), ]
data.cms.order$CHD_CrudePrev = data.cms.order$CHD_CrudePrev/100
colnames(data.cms.order)[1] = "STATE_COUNTY_FIPS"

demo.data.age.sex.race.year2019 = demo.data.age.sex.race %>% filter(YEAR == 12)
demo.data.age.sex.race.year2019$COUNTY[which(nchar(demo.data.age.sex.race.year2019$COUNTY) == 1)] = paste0("00", demo.data.age.sex.race.year2019$COUNTY[which(nchar(demo.data.age.sex.race.year2019$COUNTY) == 1)])
demo.data.age.sex.race.year2019$COUNTY[which(nchar(demo.data.age.sex.race.year2019$COUNTY) == 2)] = paste0("0", demo.data.age.sex.race.year2019$COUNTY[which(nchar(demo.data.age.sex.race.year2019$COUNTY) == 2)])
demo.data.age.sex.race.year2019$STATE[which(nchar(demo.data.age.sex.race.year2019$STATE) == 1)] = paste0("0", demo.data.age.sex.race.year2019$STATE[which(nchar(demo.data.age.sex.race.year2019$STATE) == 1)])
demo.data.age.sex.race.year2019 = demo.data.age.sex.race.year2019 %>% mutate(STATE_COUNTY_FIPS = paste0(STATE, COUNTY))
demo.data.age.sex.race.year2019 = demo.data.age.sex.race.year2019 %>% filter(AGEGRP >= 14) %>% mutate(medicare_age_grps = case_when(AGEGRP == 14 | AGEGRP == 15 ~ '65-74',
                                                                                                           AGEGRP == 16 | AGEGRP == 17 ~ '75-84',
                                                                                                           AGEGRP == 18 ~ '85+'))
demo.data.age.census.2019 = demo.data.age.sex.race.year2019 %>% group_by(STATE_COUNTY_FIPS, CTYNAME, STNAME, medicare_age_grps) %>%
  summarise(TOTAL = sum(TOT_POP))
colnames(demo.data.age.census.2019)[4] = "Age"

medicare_merge = inner_join(data.cms.order, demo.data.age.census.2019, by = c("STATE_COUNTY_FIPS", "Age"))

medicare_merge_wt  = medicare_merge %>% group_by(STATE_COUNTY_FIPS, STNAME, CTYNAME) %>%
        summarise(arthritis = weighted.mean(ARTHRITIS_CrudePrev, w = TOTAL),
                  asthma = weighted.mean(CASTHMA_CrudePrev, w = TOTAL),
                  copd = weighted.mean(COPD_CrudePrev, w = TOTAL),
                  hypertension = weighted.mean(BPHIGH_CrudePrev, w = TOTAL),
                  diabetes = weighted.mean(DIABETES_CrudePrev, w = TOTAL),
                  kidney = weighted.mean(KIDNEY_CrudePrev, w = TOTAL),
                  obesity = weighted.mean(OBESITY_CrudePrev, w = TOTAL),
                  stroke = weighted.mean(STROKE_CrudePrev, w = TOTAL),
                  smoking = weighted.mean(`CSMOKING_CrudePre
v`, w = TOTAL),
                  hemato_cancer = weighted.mean(`Leukemias and Lymphomas`, w = TOTAL),
                  non_hemato_cancer = weighted.mean(`Cancer, Colorectal, Breast, Prostate, Lung`, w = TOTAL),
                  chd = weighted.mean(CHD_CrudePrev, w = TOTAL))


medicare.data.age.greater.65 = merge(demo.data.age.greater.65, medicare_merge_wt, by = c("STATE_COUNTY_FIPS", "CTYNAME", "STNAME"))



nhis_data_2017 = readRDS("~/Dropbox/NHANES_risk_score/500cities_data/Updated_July_06_2020/data_created/nhis_2017.rds")
nhis_data_2017_age_greater_65 = nhis_data_2017 %>% filter(age >= 65)
# Not_obese   Obese_I  Obese_II Obese_III 
# 5026      1340       462       506

# Current  Former   Never 
# 658    2864    3776



#Diabetes
hbAc1_level = sasxport.get('~/Dropbox/NHANES_risk_score/Github/COVID19Risk/data/NHANES_Diabetes/GHB_J.xpt')
demo = sasxport.get('~/Dropbox/NHANES_risk_score/Github/COVID19Risk/data/NHANES_Diabetes/DEMO_J.xpt')
demo_sampling_weights = demo[which(demo$ridreth3 != 7 & demo$ridageyr>=65), c(1,5,8,41)]
common_seq = intersect(hbAc1_level$seqn, demo_sampling_weights$seqn)
hbAc1_level = hbAc1_level[which(hbAc1_level$seqn %in% common_seq == T), ]
diabetes_questionnaire = sasxport.get('~/Dropbox/NHANES_risk_score/Github/COVID19Risk/data/NHANES_Diabetes/DIQ_J.xpt')
seqno_diabetes_yes = diabetes_questionnaire$seqn[which(diabetes_questionnaire$diq010 == 1)]
seqno_diabetes_yes = intersect(seqno_diabetes_yes, common_seq)
hbAc1_level_diabetes_yes = hbAc1_level[which(hbAc1_level$seqn %in% intersect(seqno_diabetes_yes, hbAc1_level$seqn) == T), ]
sampling_weights = demo_sampling_weights[which(demo_sampling_weights$seqn %in% intersect(seqno_diabetes_yes, hbAc1_level$seqn) == T), ]
hbAc1_level_diabetes_yes_sampling_weighs = merge(sampling_weights, hbAc1_level_diabetes_yes, by = "seqn")
count_controlled_diabetes = sum(hbAc1_level_diabetes_yes_sampling_weighs$wtmec2yr[which(as.numeric(hbAc1_level_diabetes_yes_sampling_weighs$lbxgh) < 7.4)])
count_uncontrolled_diabetes = sum(hbAc1_level_diabetes_yes_sampling_weighs$wtmec2yr[which(as.numeric(hbAc1_level_diabetes_yes_sampling_weighs$lbxgh) >= 7.4)])
ratio_uncontrolled_to_controlled = count_uncontrolled_diabetes/count_controlled_diabetes
# ratio_uncontrolled_to_controlled = 0.4856145


#Arthritis
arthritis = sasxport.get('~/Dropbox/NHANES_risk_score/Github/COVID19Risk/data/NHANES_Diabetes/MCQ_J.xpt')
#mcq195, mcq160a
arthritis = arthritis %>% select(seqn, mcq160a, mcq195)
arthritis_yes = arthritis[which(arthritis$mcq160a == 1), ]
demo = sasxport.get('~/Dropbox/NHANES_risk_score/Github/COVID19Risk/data/NHANES_Diabetes/DEMO_J.xpt')
demo_sampling_weights = demo[which(demo$ridreth3 != 7 & demo$ridageyr>=65), c(1,5,8,40)]
common_seq = intersect(arthritis_yes$seqn, demo_sampling_weights$seqn)
arthritis_yes = arthritis_yes[which(arthritis_yes$seqn %in% common_seq == T), ]
join_data = inner_join(demo_sampling_weights, arthritis_yes, by = "seqn")
join_data = join_data %>% filter(mcq195 !=7) %>% filter(mcq195 !=9)
rheumatoid = sum(join_data$wtint2yr[which(join_data$mcq195 == 2 |join_data$mcq195 == 3 )])
non_rheumatoid = sum(join_data$wtint2yr[which(join_data$mcq195 == 1 |join_data$mcq195 == 4 )])
# ratio_rheumatoid_to_non_rheumatoid =  0.2037804

medicare.data.age.greater.65  = medicare.data.age.greater.65 %>% mutate(obesity_I = (1340/7334)*obesity, obesity_II = (462/7334)*obesity, obesity_III = (506/7334)*obesity,
                                                                        smoking_former = (2864/7298)*smoking, smoking_current = (658/7298)*smoking,
                                                                        rheumatoid = (0.2037804/(1+0.2037804)*arthritis),
                                                                        diabetes_uncontrolled = (0.4856145/(1+0.4856145))*diabetes, diabetes_controlled = (1/(1+0.4856145))*diabetes)
data.sdi = read.xlsx("~/Dropbox/NHANES_risk_score/500cities_data/Updated_July_06_2020/medicare_data/data/ACS2015_countyallvars.xlsx")
colnames(data.sdi)[1] = "STATE_COUNTY_FIPS"
data.sdi = data.sdi[, c(1:3)]
medicare.data.age.greater.65 = merge(medicare.data.age.greater.65, data.sdi, by = "STATE_COUNTY_FIPS")
colnames(medicare.data.age.greater.65)[1:4] = c("fips", "county", "state", "age")
medicare.data.age.greater.65 = medicare.data.age.greater.65[, -c(16,18,20)]

cancer.data = readRDS("~/Dropbox/NHANES_risk_score/Github/COVID19Risk/data_created/cancer_2017.rds")
colnames(cancer.data)[1] = "fips"
#medicare.data.age.greater.65 = readRDS("~/Dropbox/NHANES_risk_score/500cities_data/Updated_July_06_2020/medicare_data/data/medicare_data_combined_age_greater_65_sex_race.rds")
medicare.data.age.greater.65 = merge(medicare.data.age.greater.65, cancer.data, by = "fips", all.x = T)
###---- ignore the creating the cancer sub categories
medicare.data.age.greater.65 = medicare.data.age.greater.65 %>% mutate(medicare.hemo.cat1 = (hemo_cat1)*hemato_cancer,
                                                                       medicare.hemo.cat2 = (hemo_cat2)*hemato_cancer,
                                                                       medicare.hemo.cat3 = (hemo_cat3)*hemato_cancer,
                                                                       medicare.nonhemo.cat1 = (non_hemo_cat1)*non_hemato_cancer,
                                                                       medicare.nonhemo.cat2 = (non_hemo_cat2)*non_hemato_cancer,
                                                                       medicare.nonhemo.cat3 = (non_hemo_cat3)*non_hemato_cancer)

medicare.data.age.greater.65 = medicare.data.age.greater.65[, -c(31:36)]




demo.data.age.sex.race = read.csv("~/Dropbox/NHANES_risk_score/500cities_data/Updated_July_06_2020/medicare_data/data/age_sex_race.csv")


demo.data.age.sex.race.year2019 = demo.data.age.sex.race %>% filter(YEAR == 12)
demo.data.age.sex.race.year2019$COUNTY[which(nchar(demo.data.age.sex.race.year2019$COUNTY) == 1)] = paste0("00", demo.data.age.sex.race.year2019$COUNTY[which(nchar(demo.data.age.sex.race.year2019$COUNTY) == 1)])
demo.data.age.sex.race.year2019$COUNTY[which(nchar(demo.data.age.sex.race.year2019$COUNTY) == 2)] = paste0("0", demo.data.age.sex.race.year2019$COUNTY[which(nchar(demo.data.age.sex.race.year2019$COUNTY) == 2)])
demo.data.age.sex.race.year2019$STATE[which(nchar(demo.data.age.sex.race.year2019$STATE) == 1)] = paste0("0", demo.data.age.sex.race.year2019$STATE[which(nchar(demo.data.age.sex.race.year2019$STATE) == 1)])
demo.data.age.sex.race.year2019 = demo.data.age.sex.race.year2019 %>% mutate(STATE_COUNTY_FIPS = paste0(STATE, COUNTY))
demo.data.age.year2019 = demo.data.age.sex.race.year2019 %>%  mutate(AGEGRP >= 14) %>%
  group_by(STATE_COUNTY_FIPS, CTYNAME, STNAME) %>% summarise(TOT_POP_AGE_GREATER_65 = sum(TOT_POP))

demo.data.age.year2019.all.age = demo.data.age.sex.race.year2019 %>% group_by(STATE_COUNTY_FIPS, CTYNAME, STNAME) %>%
  summarise(TOT_POP = sum(TOT_POP))




demo.data.age.sex.race = read.csv("~/Dropbox/NHANES_risk_score/500cities_data/Updated_July_06_2020/medicare_data/data/age_sex_race.csv")

demo.data.age.sex.race.year2019 = demo.data.age.sex.race %>% filter(YEAR == 12)
demo.data.age.sex.race.year2019$COUNTY[which(nchar(demo.data.age.sex.race.year2019$COUNTY) == 1)] = paste0("00", demo.data.age.sex.race.year2019$COUNTY[which(nchar(demo.data.age.sex.race.year2019$COUNTY) == 1)])
demo.data.age.sex.race.year2019$COUNTY[which(nchar(demo.data.age.sex.race.year2019$COUNTY) == 2)] = paste0("0", demo.data.age.sex.race.year2019$COUNTY[which(nchar(demo.data.age.sex.race.year2019$COUNTY) == 2)])
demo.data.age.sex.race.year2019$STATE[which(nchar(demo.data.age.sex.race.year2019$STATE) == 1)] = paste0("0", demo.data.age.sex.race.year2019$STATE[which(nchar(demo.data.age.sex.race.year2019$STATE) == 1)])
demo.data.age.sex.race.year2019 = demo.data.age.sex.race.year2019 %>% mutate(STATE_COUNTY_FIPS = paste0(STATE, COUNTY))


demo.data.age.year2019 = demo.data.age.sex.race.year2019 %>%  mutate(medicare_age_grps = case_when(AGEGRP == 14 | AGEGRP == 15 ~ '65-74',
                                                                                                   AGEGRP == 16 | AGEGRP == 17 ~ '75-84',
                                                                                                   AGEGRP == 18 ~ '85+',
                                                                                                   AGEGRP < 14 ~ '65-'))
demo.data.age.year2019 = demo.data.age.year2019 %>%
  group_by(STATE_COUNTY_FIPS, CTYNAME, STNAME, medicare_age_grps) %>% summarise(TOT_POP = sum(TOT_POP))
demo.data.age.year2019 = spread(demo.data.age.year2019, medicare_age_grps, TOT_POP)
colnames(demo.data.age.year2019)[4:7] = paste0("age.", colnames(demo.data.age.year2019)[4:7])
demo.data.age.year2019 = demo.data.age.year2019 %>% mutate("Age.65-74" = `age.65-74`/sum(`age.65-`, `age.65-74`, `age.75-84`, `age.85+`),
                                                           "Age.75-84" = `age.75-84`/sum(`age.65-`, `age.65-74`, `age.75-84`, `age.85+`),
                                                           "Age.85+" = `age.85+`/sum(`age.65-`, `age.65-74`, `age.75-84`, `age.85+`))
demo.data.age.year2019 = demo.data.age.year2019[,-c(4:7)]
colnames(demo.data.age.year2019)[1:3] = c("fips", "county", "state")


#medicare.data.age.greater.65 = readRDS("~/Dropbox/NHANES_risk_score/500cities_data/Updated_July_06_2020/medicare_data/data/medicare_data_combined_age_greater_65_sex_race.rds")

medicare.data.age.greater.65 = merge(medicare.data.age.greater.65,demo.data.age.year2019, by = c("fips", "county", "state"))

saveRDS(medicare.data.age.greater.65, file = "~/Dropbox/NHANES_risk_score/500cities_data/Updated_July_06_2020/medicare_data/data/medicare_data_combined_age_greater_65_sex_race.rds")
