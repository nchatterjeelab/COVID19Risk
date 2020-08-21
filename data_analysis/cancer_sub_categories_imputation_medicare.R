library(dplyr)
library(Hmisc)
library(openxlsx)
demo.data.age.sex.race = read.csv("~/Dropbox/NHANES_risk_score/500cities_data/Updated_July_06_2020/medicare_data/data/age_sex_race.csv")


demo.data.age.sex.race.year2019 = demo.data.age.sex.race %>% filter(YEAR == 12)
demo.data.age.sex.race.year2019$COUNTY[which(nchar(demo.data.age.sex.race.year2019$COUNTY) == 1)] = paste0("00", demo.data.age.sex.race.year2019$COUNTY[which(nchar(demo.data.age.sex.race.year2019$COUNTY) == 1)])
demo.data.age.sex.race.year2019$COUNTY[which(nchar(demo.data.age.sex.race.year2019$COUNTY) == 2)] = paste0("0", demo.data.age.sex.race.year2019$COUNTY[which(nchar(demo.data.age.sex.race.year2019$COUNTY) == 2)])
demo.data.age.sex.race.year2019$STATE[which(nchar(demo.data.age.sex.race.year2019$STATE) == 1)] = paste0("0", demo.data.age.sex.race.year2019$STATE[which(nchar(demo.data.age.sex.race.year2019$STATE) == 1)])
demo.data.age.sex.race.year2019 = demo.data.age.sex.race.year2019 %>% mutate(STATE_COUNTY_FIPS = paste0(STATE, COUNTY))

demo.data.age.sex.race.year2019 = demo.data.age.sex.race.year2019 %>% filter(AGEGRP >=14) %>% dplyr::select(STATE_COUNTY_FIPS, STATE, COUNTY, STNAME, CTYNAME, TOT_POP, AGEGRP, H_MALE, H_FEMALE, NHWA_MALE, NHWA_FEMALE, NHBA_MALE, NHBA_FEMALE, NHAA_MALE, NHAA_FEMALE, NHIA_MALE, NHIA_FEMALE, NHNA_MALE, NHNA_FEMALE)
demo.data.age.sex.race.year2019 = demo.data.age.sex.race.year2019 %>% mutate(NHAA_FEMALE = NHAA_FEMALE + NHNA_FEMALE, NHAA_MALE = NHAA_MALE + NHNA_MALE)

demo.data.age.year2019.all.age = demo.data.age.sex.race.year2019 %>% group_by(STATE_COUNTY_FIPS, CTYNAME, STNAME) %>%
  summarise(TOT_POP_COUNTY_AGE_GREATER_65 = sum(TOT_POP))


demo.data.age.sex.race.year2019 = demo.data.age.sex.race.year2019 %>% mutate(Age = case_when(AGEGRP == 14 | AGEGRP == 15 ~ "65-74",
                                                                                             AGEGRP == 16 | AGEGRP == 17 ~ "75-84",
                                                                                             AGEGRP == 18 ~ "85+"))
demo.data.age.sex.race.year2019 = demo.data.age.sex.race.year2019 %>% dplyr::select(-AGEGRP)

demo.data.age.sex.race.year2019 = demo.data.age.sex.race.year2019 %>% group_by(STATE_COUNTY_FIPS, CTYNAME, STNAME, Age) %>%
  summarise(NHWA_MALE = sum(NHWA_MALE), NHWA_FEMALE = sum(NHWA_FEMALE), NHBA_MALE = sum(NHBA_MALE), NHBA_FEMALE = sum(NHBA_FEMALE), NHAA_MALE = sum(NHAA_MALE), NHAA_FEMALE = sum(NHAA_FEMALE), NHIA_MALE = sum(NHIA_MALE), NHIA_FEMALE = sum(NHIA_FEMALE), H_MALE = sum(H_MALE), H_FEMALE = sum(H_FEMALE))


demo.data.age.prevalence.greater.65 = merge(demo.data.age.sex.race.year2019, demo.data.age.year2019.all.age, by = c("STATE_COUNTY_FIPS", "CTYNAME", "STNAME"))
demo.data.age.prevalence.greater.65 = demo.data.age.prevalence.greater.65 %>% 
  mutate(non_hispanic_white_female = NHWA_FEMALE/TOT_POP_COUNTY_AGE_GREATER_65, non_hispanic_white_male = NHWA_MALE/TOT_POP_COUNTY_AGE_GREATER_65,
         non_hispanic_black_female = NHBA_FEMALE/TOT_POP_COUNTY_AGE_GREATER_65, non_hispanic_black_male = NHBA_MALE/TOT_POP_COUNTY_AGE_GREATER_65,
         non_hispanic_asian_pi_female = NHAA_FEMALE/TOT_POP_COUNTY_AGE_GREATER_65, non_hispanic_asian_pi_male = NHAA_MALE/TOT_POP_COUNTY_AGE_GREATER_65,
         non_hispanic_aaian_female = NHIA_FEMALE/TOT_POP_COUNTY_AGE_GREATER_65, non_hispanic_aaian_male = NHIA_MALE/TOT_POP_COUNTY_AGE_GREATER_65,
         hispanic_female = H_FEMALE/TOT_POP_COUNTY_AGE_GREATER_65, hispanic_male = H_MALE/TOT_POP_COUNTY_AGE_GREATER_65)


demo.data.age.prevalence.greater.65 = demo.data.age.prevalence.greater.65[, c(1:4, 16:25)]

demo.data.age.prevalence.greater.65 = demo.data.age.prevalence.greater.65 %>%
  pivot_wider(names_from = Age, values_from = c(non_hispanic_white_female, non_hispanic_white_male, non_hispanic_asian_pi_female, non_hispanic_asian_pi_male, non_hispanic_aaian_female, non_hispanic_aaian_male, non_hispanic_black_female, non_hispanic_black_male, hispanic_female, hispanic_male))

colnames(demo.data.age.prevalence.greater.65)[1:2] = c("fips", "county")
demo.data.age.prevalence.greater.65$county = as.character(demo.data.age.prevalence.greater.65$county)
#saveRDS(demo.data.age.greater.65, file = "~/Dropbox/NHANES_risk_score/500cities_data/Updated_July_06_2020/medicare_data/data/age_greater_than_65_sex_race.rds")



#CMS data
data.cms = readRDS("~/Dropbox/NHANES_risk_score/500cities_data/Updated_July_06_2020/medicare_data/data/CMS_Medicare_Data_36_Groups_17_Cate_0711.rds")
data.cms.order = data.cms[order(data.cms$fips), ]
data.cms = data.cms[, c(1,4,13,19,20,21,22)]
colnames(data.cms)[2:3] = c("nonhemato.cancer", "hemato.cancer")
data.cms = data.cms %>% filter(Race != "Other")
data.cms$Race[which(data.cms$Race == "American Indian/Alaska native")] = "aian"
data.cms$Race[which(data.cms$Race == "Asian/Pacific islander")] = "asian_pi"

data.cms = data.cms %>% pivot_wider(names_from = c(Age, Sex, Race), values_from = c(nonhemato.cancer, hemato.cancer)) %>% unique()

#saveRDS(medicare.data.age.greater.65, file = "~/Dropbox/NHANES_risk_score/500cities_data/Updated_July_06_2020/medicare_data/data/medicare_data_combined_age_greater_65_sex_race.rds")

medicare_cancer_imputed_data = inner_join(demo.data.age.prevalence.greater.65, data.cms, by=c("fips", "county"))
saveRDS(medicare_cancer_imputed_data, file = "~/Dropbox/NHANES_risk_score/500cities_data/Updated_July_06_2020/medicare_data/data/medicare_cancer_imputed_data.rds")
