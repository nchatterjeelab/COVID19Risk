library(dplyr)
library(ggplot2)


########----------set the working directory to CDC data folder
deaths = read.csv("Deaths_involving_coronavirus_disease_2019__COVID-19__by_race_and_Hispanic_origin_group_and_age__by_state.csv", header = T)
population = read.csv("population_category_wise.csv")
#Check US population is ~ 308 million in 2010

####-------- Unadjusted RR for age-groups
deaths = deaths %>% filter(State != "United States") %>% 
  filter(Age.group != "1-4 years") %>% 
  filter(Age.group != "5-14 years") %>% 
  filter(Age.group != "Under 1 year") %>%
  filter(Age.group != "All Ages") %>% 
  filter(Race.and.Hispanic.Origin.Group != "Total Deaths") %>% 
  filter(Race.and.Hispanic.Origin.Group != "Unknown")
deaths$State[which(deaths$State == "New York City")] = "New York"
deaths = deaths %>% mutate(Race = Race.and.Hispanic.Origin.Group) %>% 
  mutate(agegroup = case_when(Age.group == "15-24 years" | Age.group == "25-34 years" | Age.group == "35-44 years"~ '15-44',
                              Age.group == "45-54 years" ~ '45-54',
                              Age.group == "55-64 years" ~ '55-64',
                              Age.group == "65-74 years" ~ '65-74',
                              Age.group == "75-84 years" ~ '75-84',
                              Age.group == "85 years and over" ~ '85+')) %>%
  select(State, COVID.19.Deaths, agegroup, Race)

deaths_race = deaths %>% group_by(State,agegroup,Race) %>% summarise(covid_deaths = sum(COVID.19.Deaths, na.rm = T)) %>% ungroup()

population = population %>% filter(ORIGIN != 0) %>% filter(SEX != 0) %>% filter(AGE >=15)
population = population %>% mutate(agegroup = case_when(AGE >= 15  & AGE < 45 ~ '15-44',
                                                        AGE >= 45  & AGE < 55 ~ '45-54',
                                                        AGE >= 55  & AGE < 65 ~ '55-64',
                                                        AGE >= 65  & AGE < 75 ~ '65-74',
                                                        AGE >= 70  & AGE < 85 ~ '75-84',
                                                        AGE >= 80  & AGE < 100 ~ '85+')) %>%
  mutate(Race = case_when(RACE == 1  & ORIGIN == 1 ~ 'Non-Hispanic White',
                          RACE == 2  & ORIGIN == 1 ~ 'Non-Hispanic Black',
                          RACE == 3  & ORIGIN == 1 ~ 'Non-Hispanic American Indian or Alaska Native',
                          RACE == 4  & ORIGIN == 1 ~ 'Non-Hispanic Asian',
                          RACE == 5  & ORIGIN == 1 ~ 'Non-Hispanic Native Hawaiian or Other Pacific Islander',
                          RACE == 6  & ORIGIN == 1 ~ 'Non-Hispanic More than one race',
                          ORIGIN == 2 ~ 'Hispanic or Latino'))

population = population %>% select(NAME, agegroup, Race, POPESTIMATE2019)
colnames(population)[1] = "State"

population_race = population %>% group_by(State, agegroup,Race) %>% summarise(population = sum(POPESTIMATE2019)) %>% ungroup()

data_race = merge(population_race, deaths_race, by = c("State", "agegroup", "Race"))

data_race = data_race  %>% mutate(death.rate = covid_deaths/population)

data_race$covid_deaths = as.numeric(data_race$covid_deaths)
data_race$agegroup = as.factor(data_race$agegroup)
data_race$Race = as.factor(data_race$Race)
data_race$agegroup = relevel(data_race$agegroup, ref = "55-64")
data_race$Race = relevel(data_race$Race, ref = "Non-Hispanic White")
data_race$State = as.factor(data_race$State)
data_race = data_race[-which(data_race$population==0), ]
###               State agegroup                                                   Race population covid_deaths death.rate
###1931       Vermont      85+ Non-Hispanic Native Hawaiian or Other Pacific Islander          0            0        NaN
###2057 West Virginia      85+ Non-Hispanic Native Hawaiian or Other Pacific Islander          0            0        NaN

#####---------- poisson model for race adjusted for age and state
race.model <- glm(covid_deaths~agegroup + Race + State,offset=log(population),family=poisson(link = "log"),data = data_race)
summary(race.model)

####-----create the plots= data for race
deathrate_race_cdc = data.frame(race=c("Hispanic or Latino",
                                       "Non-Hispanic American Indian or \n Alaska Native",
                                       "Non-Hispanic Asian",
                                       "Non-Hispanic White",
                                       "Non-Hispanic Black",
                                       "Non-Hispanic More than \n one race",
                                       "Non-Hispanic Native Hawaiian or Other Pacific Islander"),
                                logRR=c(race.model$coefficients[7:9],0,race.model$coefficients[10:12]))
rownames(deathrate_race_cdc) = NULL

deathrate_race_uk = data.frame(race=c("Asian or Asian British","Black","White"),
                               logRR=c(0.668,0.775,0))

deathrate = bind_rows(deathrate_race_cdc,deathrate_race_uk,.id="id")
deathrate$id = as.factor(deathrate$id)
levels(deathrate$id) = c("US","UK")
deathrate$race = as.factor(deathrate$race)
deathrate = subset(deathrate,!deathrate$race %in% c("Non-Hispanic Native Hawaiian or Other Pacific Islander",
                                                    "Non-Hispanic More than \n one race"))
deathrate$race = ordered(deathrate$race,levels=c("Asian or Asian British",
                                                 "Non-Hispanic Asian","Black",
                                                 "Non-Hispanic Black",
                                                 "White",
                                                 "Non-Hispanic White",
                                                 "Hispanic or Latino",
                                                 "Non-Hispanic American Indian or \n Alaska Native"))

####-------save the race deathrate data for plot
saveRDS(deathrate,"CDC_race_deathrate.rds")

#####---------- poisson model for age adjusted for state
deaths_age = deaths %>% group_by(State,agegroup) %>% summarise(covid_deaths = sum(COVID.19.Deaths, na.rm = T)) %>% ungroup()
population_age = population %>% group_by(State, agegroup) %>% summarise(population = sum(POPESTIMATE2019)) %>% ungroup()
data_age = merge(population_age, deaths_age, by = c("State", "agegroup"))

data_age = data_age  %>% mutate(death.rate = covid_deaths/population)

data_age$covid_deaths = as.numeric(data_age$covid_deaths)
data_age$agegroup = as.factor(data_age$agegroup)
data_age$agegroup = relevel(data_age$agegroup, ref = "55-64")
data_age$State = as.factor(data_age$State)

age.model <- glm(covid_deaths ~ agegroup + State,offset=log(population),family=poisson(link = "log"),data = data_age)
summary(age.model)

####-----create plot data for age
deathrate_age_cdc = data.frame(age.group=c("15-44","45-54","55-64","65-74","75-84","85+"),
                               logRR=c(age.model$coefficients[2:3],0,age.model$coefficients[4:6]))
rownames(deathrate_age_cdc) = NULL

deathrate_age_uk = data.frame(age.group=c("18-39","40-49","50-59","60-69","70-79","80+"),
                              logRR=c(-2.996,-1.309,0,0.959,2.029,3.268))

deathrate = bind_rows(deathrate_age_cdc,deathrate_age_uk,.id="id")
deathrate$id = as.factor(deathrate$id)
levels(deathrate$id) = c("US","UK")
deathrate$age.group = as.factor(deathrate$age.group)
deathrate$age.group = relevel(deathrate$age.group,"18-39")

####-------save the age deathrate data for plot
saveRDS(deathrate,"CDC_age_deathrate.rds")

