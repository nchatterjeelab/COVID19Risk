library(dplyr)
library(openxlsx)
# merge SDI with BRFSS
#### US state, county, city info
temp <- tempfile()
download.file("http://download.geonames.org/export/zip/US.zip",temp)
con <- unz(temp, "US.txt")
US <- read.delim(con, header=FALSE)
unlink(temp)

colnames(US)[c(3,5,6)] <- c("city","state","county")
US$city <- tolower(US$city)
colnames(US)[c(10,11)] = c('lat','lon')

# ------------------------- merge city ID with county ID & SDI data -------------------------
rawSDI=read.xlsx('data_create/SDI_quintile.xlsx')
brfss = readRDS("data_created/BRFSS.rds")
cityloc = t(sapply(1:nrow(brfss),function(x){as.numeric(c(substr(strsplit(brfss$Geolocation[x],',')[[1]][1],2,11),substr(strsplit(brfss$Geolocation[x],',')[[1]][2],1,11)))}))
brfss$lat = as.numeric(cityloc[,1])
brfss$lon = as.numeric(cityloc[,2])

geocode = read.xlsx('data/Geocodes/all-geocodes-2017.xlsx')
# select the counties that have the same state-county ID as the 500 cities:
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
brfss = brfss[,-missing_column] # 500

### output: city-level SDI
placefips_county = brfss[,c('PlaceFIPS','county')]
saveRDS(placefips_county,file='data_created/placefips_county.rds')
saveRDS(brfss,file='data_created/brfss_sdi.rds')



# ------------------------- merge with cancer data -------------------------
sdi_city = readRDS('data_created/brfss_sdi.rds')
# ---- updated 071020:
cancer = readRDS('data_created/cancer_2017.rds')
colnames(cancer)[1] = 'county'
kansas_counties = unique(sdi_city[sdi_city$StateAbbr == 'KS','county'])
minnesota_counties = unique(sdi_city[sdi_city$StateAbbr == 'MN','county'])
kansas_imputed_cancer = cbind(kansas_counties, cancer[cancer$county == '20999',2:7])
colnames(kansas_imputed_cancer)[1] = 'county'
minnesota_imputed_cancer = cbind(minnesota_counties, cancer[cancer$county == '27999',2:7])
colnames(minnesota_imputed_cancer)[1] = 'county'

cancer = rbind(cancer, kansas_imputed_cancer, minnesota_imputed_cancer)

b = sdi_city %>% inner_join(cancer, by = 'county') # 500
b = b[complete.cases(b),] # 500
length(unique(cancer$county)) # 2964, all unique
length(unique(sdi_city$county)) # 285
cancerraw = b

cancer = data.frame(county = sdi_city$county, PlaceFIPS = sdi_city$PlaceFIPS,
                    hematologic_cancer1 = NA, hematologic_cancer2 = NA, hematologic_cancer3 = NA,
                    non_hematologic_cancer1 = NA, non_hematologic_cancer2 = NA, non_hematologic_cancer3 = NA)
l=0
for (i in 1:nrow(cancer)){
  tem = cancerraw[(cancerraw$county == cancer$county[i])&(cancerraw$PlaceFIPS == cancer$PlaceFIPS[i]),]
  if (nrow(tem) == 1){
    l = l + 1
    cancer[i,paste0('hematologic_cancer',1:3)] = tem[paste0('hemo_cat',1:3)]
    cancer[i,paste0('non_hematologic_cancer',1:3)] = tem[paste0('non_hemo_cat',1:3)]
  }
}


cancer = cancer[complete.cases(cancer),]
cancer = cancer[,-which(colnames(cancer) == 'county')]
cancer$PlaceFIPS = as.character(cancer$PlaceFIPS)
sdi_cancer_citylevel = sdi_city %>% inner_join(cancer, by = 'PlaceFIPS') 
sdi_cancer_citylevel = sdi_cancer_citylevel[complete.cases(sdi_cancer_citylevel),]


# ------------------- merge with age, gender and ethnicity from census data  -------------------
agesex = readRDS('data_created/census_age_sex_2.rds')
agesex = agesex[-which(agesex[,4] == 'N'),]
tm = agesex[,-c(1,2,ncol(agesex))]
tm <- as.data.frame(sapply(tm, as.numeric))
total.population = tm$Total..Estimate..Total.population - tm$Male..Estimate..AGE..Under.5.years - tm$Male..Estimate..AGE..5.to.9.years - tm$Male..Estimate..AGE..10.to.14.years - 
  tm$Female..Estimate..AGE..Under.5.years - tm$Female..Estimate..AGE..5.to.9.years - tm$Female..Estimate..AGE..10.to.14.years

# update gender
tm$female_proportion = tm$Female..Estimate..AGE..15.to.19.years +
  tm$Female..Estimate..AGE..20.to.24.years + tm$Female..Estimate..AGE..25.to.29.years + tm$Male..Estimate..AGE..30.to.34.years + tm$Female..Estimate..AGE..35.to.39.years + 
  tm$Female..Estimate..AGE..40.to.44.years + tm$Female..Estimate..AGE..45.to.49.years + tm$Female..Estimate..AGE..50.to.54.years + tm$Female..Estimate..AGE..55.to.59.years + 
  tm$Female..Estimate..AGE..60.to.64.years + tm$Female..Estimate..AGE..65.to.69.years + tm$Female..Estimate..AGE..70.to.74.years + tm$Female..Estimate..AGE..75.to.79.years + 
  tm$Female..Estimate..AGE..80.to.84.years + tm$Female..Estimate..AGE..85.years.and.over
tm$female_proportion = tm$female_proportion/total.population
tm$male_proportion = tm$Male..Estimate..AGE..15.to.19.years +
  tm$Male..Estimate..AGE..20.to.24.years + tm$Male..Estimate..AGE..25.to.29.years + tm$Male..Estimate..AGE..30.to.34.years + tm$Male..Estimate..AGE..35.to.39.years + 
  tm$Male..Estimate..AGE..40.to.44.years + tm$Male..Estimate..AGE..45.to.49.years + tm$Male..Estimate..AGE..50.to.54.years + tm$Male..Estimate..AGE..55.to.59.years + 
  tm$Male..Estimate..AGE..60.to.64.years + tm$Male..Estimate..AGE..65.to.69.years + tm$Male..Estimate..AGE..70.to.74.years + tm$Male..Estimate..AGE..75.to.79.years + 
  tm$Male..Estimate..AGE..80.to.84.years + tm$Male..Estimate..AGE..85.years.and.over
tm$male_proportion = tm$male_proportion/total.population

# update age
tm$Age_15_44 = tm$Male..Estimate..AGE..15.to.19.years + tm$Female..Estimate..AGE..15.to.19.years + 
  tm$Male..Estimate..AGE..40.to.44.years + tm$Female..Estimate..AGE..40.to.44.years + 
  tm$Male..Estimate..AGE..20.to.24.years + tm$Male..Estimate..AGE..25.to.29.years + tm$Male..Estimate..AGE..30.to.34.years + tm$Male..Estimate..AGE..35.to.39.years +
  tm$Female..Estimate..AGE..20.to.24.years + tm$Female..Estimate..AGE..25.to.29.years + tm$Female..Estimate..AGE..30.to.34.years + tm$Female..Estimate..AGE..35.to.39.years
tm$Age_45_54 = tm$Male..Estimate..AGE..45.to.49.years + tm$Male..Estimate..AGE..50.to.54.years + 
  tm$Female..Estimate..AGE..45.to.49.years + tm$Female..Estimate..AGE..50.to.54.years
tm$Age_55_64 = tm$Male..Estimate..AGE..55.to.59.years + tm$Male..Estimate..AGE..60.to.64.years + 
  tm$Female..Estimate..AGE..55.to.59.years + tm$Female..Estimate..AGE..60.to.64.years
tm$Age_65_74 = tm$Male..Estimate..AGE..65.to.69.years + tm$Male..Estimate..AGE..70.to.74.years + 
  tm$Female..Estimate..AGE..65.to.69.years + tm$Female..Estimate..AGE..70.to.74.years
tm$Age_75_84 = tm$Male..Estimate..AGE..75.to.79.years + tm$Male..Estimate..AGE..80.to.84.years + 
  tm$Female..Estimate..AGE..75.to.79.years + tm$Female..Estimate..AGE..80.to.84.years
tm$Age_85 = tm$Male..Estimate..AGE..85.years.and.over + 
  tm$Female..Estimate..AGE..85.years.and.over

tm$Age_15_39 = tm$Male..Estimate..AGE..15.to.19.years + tm$Female..Estimate..AGE..15.to.19.years + 
  tm$Male..Estimate..AGE..20.to.24.years + tm$Male..Estimate..AGE..25.to.29.years + tm$Male..Estimate..AGE..30.to.34.years + tm$Male..Estimate..AGE..35.to.39.years +
  tm$Female..Estimate..AGE..20.to.24.years + tm$Female..Estimate..AGE..25.to.29.years + tm$Female..Estimate..AGE..30.to.34.years + tm$Female..Estimate..AGE..35.to.39.years
tm$Age_40above = tm$Male..Estimate..AGE..40.to.44.years + tm$Female..Estimate..AGE..40.to.44.years + 
  tm$Male..Estimate..AGE..45.to.49.years + tm$Male..Estimate..AGE..45.to.49.years + 
  tm$Male..Estimate..AGE..50.to.54.years + tm$Male..Estimate..AGE..50.to.54.years + 
  tm$Male..Estimate..AGE..55.to.59.years + tm$Male..Estimate..AGE..55.to.59.years +
  tm$Male..Estimate..AGE..60.to.64.years + tm$Male..Estimate..AGE..60.to.64.years +
  tm$Male..Estimate..AGE..65.to.69.years + tm$Male..Estimate..AGE..65.to.69.years +
  tm$Male..Estimate..AGE..70.to.74.years + tm$Male..Estimate..AGE..70.to.74.years +
  tm$Male..Estimate..AGE..75.to.79.years + tm$Male..Estimate..AGE..75.to.79.years +
  tm$Male..Estimate..AGE..80.to.84.years + tm$Male..Estimate..AGE..80.to.84.years +
  tm$Male..Estimate..AGE..85.years.and.over + tm$Male..Estimate..AGE..85.years.and.over

tm$Age_15_44 = tm$Age_15_44/total.population
tm$Age_45_54 = tm$Age_45_54/total.population
tm$Age_55_64 = tm$Age_55_64/total.population
tm$Age_65_74 = tm$Age_65_74/total.population
tm$Age_75_84 = tm$Age_75_84/total.population
tm$Age_85 = tm$Age_85/total.population
tm$Age_15_39 = tm$Age_15_39/total.population
tm$Age_40above = tm$Age_40above/total.population

agesex = data.frame(id = agesex$id, Geographic.Area.Name = agesex$Geographic.Area.Name,
                    population = agesex$Total..Estimate..Total.population,
                    female_proportion = tm$female_proportion, male_proportion = tm$male_proportion,
                    Age_15_44 = tm$Age_15_44, Age_45_54 = tm$Age_45_54, Age_55_64 = tm$Age_55_64,
                    Age_65_74 = tm$Age_65_74, Age_75_84 = tm$Age_75_84, Age_85 = tm$Age_85,
                    Age_15_39 = tm$Age_15_39, Age_40above = tm$Age_40above,
                    PlaceFIPS = agesex$PlaceFIPS)
agesex$PlaceFIPS = as.character(agesex$PlaceFIPS)

race = readRDS('data_created/census_18_race.rds')
race = race[,-c(1,2)]
combined = sdi_cancer_citylevel %>% inner_join(agesex, by = 'PlaceFIPS')
combined = combined %>% inner_join(race, by = 'PlaceFIPS')
sum(complete.cases(combined))
combined = combined[complete.cases(combined),]

# update diabetes subcategories
ratio.unctrl.ctrl = 0.6133612
pr.unctrl = ratio.unctrl.ctrl/(1+ratio.unctrl.ctrl)
combined$DIABETES_unctrled_CrudePrev = combined$DIABETES_CrudePrev * pr.unctrl
combined$DIABETES_ctrled_CrudePrev = combined$DIABETES_CrudePrev * (1-pr.unctrl)

# here IMD quntiles are defined as SDI quintiles
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

# update arthiritis:
# Among arthritis, the ratio_rheumatoid_to_non_rheumatoid =  0.269098
ratio.rheumatoid.arthritis = 0.269098/(1+0.269098)
combined$rheumatoid = combined$ARTHRITIS_CrudePrev * ratio.rheumatoid.arthritis

# output: city-level data
saveRDS(combined,file='data_created/combined_updated.rds')




