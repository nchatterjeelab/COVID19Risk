library(dplyr)
library(MASS)
deaths = read.csv("data/CDC/Deaths_involving_coronavirus_disease_2019__COVID-19__by_race_and_Hispanic_origin_group_and_age__by_state.csv", header = T)
population = read.csv("data/CDC/population_category_wise.csv")
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
  dplyr::select(State, COVID.19.Deaths, agegroup, Race)

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

population = population %>% dplyr::select(NAME, agegroup, Race, POPESTIMATE2019)
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

data_race = data_race %>% filter(Race != "Non-Hispanic More than one race") %>% filter(Race != "Non-Hispanic Native Hawaiian or Other Pacific Islander")
race.model <- glm(covid_deaths~agegroup + Race + State,offset=log(population),family=poisson(link = "log"),data = data_race)

race.model.nb <- glm.nb(covid_deaths~agegroup + Race + State + offset(log(population)) ,data = data_race, maxit = 100)


race_model_coefficients_comparision  = as.data.frame(cbind(race.model.nb$coefficients[1:10], race.model$coefficients[1:10]))
colnames(race_model_coefficients_comparision) = c("Negative_binomial", "Poisson")
#write.csv(race_model_coefficients_comparision,"~/Dropbox/NHANES_risk_score/Nature Medicine Revision/Intermediate results/race_model_coefficients_comparision.csv")


#-----Age-model-------#
deaths_age = deaths %>% group_by(State,agegroup) %>% summarise(covid_deaths = sum(COVID.19.Deaths, na.rm = T)) %>% ungroup()
population_age = population %>% group_by(State, agegroup) %>% summarise(population = sum(POPESTIMATE2019)) %>% ungroup()
data_age = merge(population_age, deaths_age, by = c("State", "agegroup"))

data_age = data_age  %>% mutate(death.rate = covid_deaths/population)

data_age$covid_deaths = as.numeric(data_age$covid_deaths)
data_age$agegroup = as.factor(data_age$agegroup)
data_age$agegroup = relevel(data_age$agegroup, ref = "55-64")
data_age$State = as.factor(data_age$State)
#data_age = data_age[-which(data_age$population==0), ]

age.model <- glm(covid_deaths ~ agegroup + State,offset=log(population),family=poisson(link = "log"),data = data_age)
summary(age.model)
age.model.nb <- glm.nb(covid_deaths ~ agegroup + State + offset(log(population)), data = data_age, maxit=100)
summary(age.model.nb)

age_model_coefficients_comparision  = as.data.frame(cbind(age.model.nb$coefficients[1:6], age.model$coefficients[1:6]))
colnames(age_model_coefficients_comparision) = c("Negative_binomial", "Poisson")
#write.csv(age_model_coefficients_comparision,"~/Dropbox/NHANES_risk_score/Nature Medicine Revision/Intermediate results/age_model_coefficients_comparision.csv")

