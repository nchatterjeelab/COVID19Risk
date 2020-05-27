library(dplyr)
# merge SDI with BRFSS
#### US state, county, city info
prevalence_cancer = function(IK, DK, total_prev){
  prev1 = IK-DK/2
  prev2 = 4*IK-DK*2
  prev3 = total_prev - prev1 - prev2
  c(prev1,prev2,prev3)
}
### merge with cancer data:
sdi_city = readRDS('~/Dropbox/NHANES_risk_score/500cities_data/Analysis/data_created/brfss_sdi.rds')
cancerraw = readRDS('~/Dropbox/NHANES_risk_score/500cities_data/Analysis/data_created/cancer_2016.rds')
# this data set contains info for the total prevalence of 3 hematological/non-hematological categories
cancerall = readRDS('~/Dropbox/NHANES_risk_score/500cities_data/Analysis/data_created/combined_hispanic.rds')
cancer = data.frame(county = sdi_city$county, PlaceFIPS = sdi_city$PlaceFIPS,
                    hematologic_cancer1 = NA, hematologic_cancer2 = NA, hematologic_cancer3 = NA,
                    non_hematologic_cancer1 = NA, non_hematologic_cancer2 = NA, non_hematologic_cancer3 = NA)
for (i in 1:nrow(cancer)){
  if ((cancer$PlaceFIPS[i] %in% cancerall$PlaceFIPS)&(sum(cancerraw$State_county_fips == cancer$county[i])==1)){
    tem = cancerraw[cancerraw$State_county_fips == cancer$county[i],]
    #tem_incidence = tem[tem$EVENT_TYPE=='Incidence',]
    #tem_mortality = tem[tem$EVENT_TYPE=='Mortality',]
    # Hematologic cancer
    #total_prevalence = cancerall[cancerall$PlaceFIPS == cancer$PlaceFIPS[i],'hematologic_cancer']
    #cancer[i,paste0('hematologic_cancer',1:3)] = prevalence_cancer(IK = as.numeric(tem_incidence[,'hematologic_cancer']), 
    #                                                               DK = as.numeric(tem_mortality[,'hematologic_cancer']),
    #                                                               total_prev = total_prevalence)
    # Non-hematologic cancer
    #total_prevalence = cancerall[cancerall$PlaceFIPS == cancer$PlaceFIPS[i],'non_hematologic_cancer']
    #cancer[i,paste0('non_hematologic_cancer',1:3)] = prevalence_cancer(IK = as.numeric(tem_incidence[,'non_hematologic_cancer']), 
    #                                                                   DK = as.numeric(tem_mortality[,'non_hematologic_cancer']),
    #                                                                   total_prev = total_prevalence)
    cancer[i,paste0('hematologic_cancer',1:3)] = tem[paste0('hemo_cat',1:3)]
    cancer[i,paste0('non_hematologic_cancer',1:3)] = tem[paste0('non_hemo_cat',1:3)]
  }
}

sum(complete.cases(cancer)) # 442
cancer = cancer[complete.cases(cancer),]
cancer = cancer[,-which(colnames(cancer) == 'county')]
cancer$PlaceFIPS = as.character(cancer$PlaceFIPS)
#colnames(cancer)[1] = 'county'
sdi_cancer_citylevel = sdi_city %>% inner_join(cancer, by = 'PlaceFIPS') 
sdi_cancer_citylevel = sdi_cancer_citylevel[complete.cases(sdi_cancer_citylevel),] # 442
length(unique(sdi_cancer_citylevel$county)) # 246
length(unique(sdi_city$county)) # 285

# merge with census data:
agesex = readRDS('~/Dropbox/NHANES_risk_score/500cities_data/Analysis/data_created/census_age_sex_2.rds')
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
race = readRDS('~/Dropbox/NHANES_risk_score/500cities_data/Analysis/data_created/census_18_race.rds')
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

# update asthma - no need, we are using the prevalence in UK model instead
#pr.OCS = (1.7)/(1.7+14.2) # from UK data
#combined$CASTHMA_OCS_CrudePrev = combined$CASTHMA_CrudePrev * pr.OCS
#combined$CASTHMA_noOCS_CrudePrev = combined$CASTHMA_CrudePrev * (1-pr.OCS)

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
Pr.obesity0 = 0.6662283 # 0.6692845
Pr.obesity1 = 0.1792105 # 0.1772857
Pr.obesity2 = 0.07423196 # 0.07338
Pr.obesity3 = 0.08032927 # 0.0800498
Pr.obesity = Pr.obesity1 + Pr.obesity2 + Pr.obesity3
combined$obesity1 = combined$OBESITY_CrudePrev * Pr.obesity1/(Pr.obesity*100)
combined$obesity2 = combined$OBESITY_CrudePrev * Pr.obesity2/(Pr.obesity*100)
combined$obesity3 = combined$OBESITY_CrudePrev * Pr.obesity3/(Pr.obesity*100)

# Transform to percentage: exclude smoking from this
combined[,c('OBESITY_CrudePrev','BPHIGH_CrudePrev','COPD_CrudePrev','CASTHMA_CrudePrev',
            'CHD_CrudePrev', 'DIABETES_unctrled_CrudePrev','DIABETES_ctrled_CrudePrev', 
            'STROKE_CrudePrev','KIDNEY_CrudePrev','ARTHRITIS_CrudePrev')] = 
  combined[,c('OBESITY_CrudePrev','BPHIGH_CrudePrev','COPD_CrudePrev','CASTHMA_CrudePrev', 
              'CHD_CrudePrev', 'DIABETES_unctrled_CrudePrev','DIABETES_ctrled_CrudePrev', 
              'STROKE_CrudePrev','KIDNEY_CrudePrev','ARTHRITIS_CrudePrev')]/100


saveRDS(combined,file='~/Dropbox/NHANES_risk_score/500cities_data/Analysis/data_created/combined_updated.rds')




# sum(rawSDI$county %in% geocode$`Place Code (FIPS)`)
# [1] 275
# sum(rawSDI$county %in% geocode$scID)
# [1] 3142
# sum(rawSDI$county %in% geocode$`County Subdivision Code (FIPS)`)
# [1] 3142
# length(unique(geocode$scID))
# 3273
# Salt Lake City <-> Salt Lake City city
