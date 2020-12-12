# Validation after adjusting for the prevalence of age 65+ 
library(openxlsx)
library(survey)
library(ggpubr)
library(gghighlight)
library(ggExtra)
library(ggplot2)
library(data.table)
library(gapminder)
library(tidyverse)
library(grid)
library(gridExtra)
library(ggrepel)
coeffs = read.xlsx('data_created/meta_model.xlsx', sheet = 'coefficients')
coef_name = coeffs$Variable
Beta = coeffs$estimate
names(Beta) = rownames(coeffs)


dat = readRDS('data_created/full_output_medicare.rds')
dat$`Age.55-64` = 1 - dat$`Age.15-44` - dat$`Age.45-54` - dat$`Age.65-74` - dat$`Age.75-84` - dat$`Age.85+`
covariates = c('Age.15-44','Age.45-54','Age.65-74','Age.75-84','Age.85+',
               'male',paste0('obesity_',c('I','II','III')),'smoking_former','smoking_current',
               'hispanic', 'non_hispanic_black', 'non_hispanic_asian', 'non_hispanic_ai_an',
               paste0('sdi',2:5),'hypertension','copd','asthma', 'chd',
               'diabetes_controlled', 'diabetes_uncontrolled',
               paste0('medicare.nonhema.cat',1:3),paste0('medicare.hema.cat',1:3),'stroke',
               'kidney','rheumatoid') # 34

Covariate_matrix = as.matrix(dat[,covariates[1:5]])
risk = Covariate_matrix %*% exp(c(-2.9087259, -0.8986471, 0.8953923, 1.8209374, 2.8679086)) + dat$`Age.55-64`


# --------- Calculate Index of Excess Risk (IER) of each city ---------
full.city = cbind(dat[,c('state','county','fips','population')],risk,dat[,6:ncol(dat)])
full.city$population = as.numeric(as.character(full.city$population))

mean.city.risks = full.city$risk
Rl = 1.107206 #sum(mean.city.risks * as.numeric(as.character(full.city$population)))/sum(as.numeric(as.character(full.city$population)))
Rl # 5.40851

# Index of Excess Risk (iER)
mean.city.risks = mean.city.risks/Rl
mean.city.risks = data.frame(IER.age = mean.city.risks,
                             #state = full.city$StateAbbr,
                             #city = full.city$PlaceName,
                             fips = full.city$fips)
#population = full.city$population)
IER.age = mean.city.risks
IER.age = cbind(IER.age,full.city[,c('Age.15-44','Age.45-54','Age.55-64','Age.65-74','Age.75-84','Age.85+','agesum')])


# Validation of the risk model using recent county-level deaths in the US
library(openxlsx)
library(chron)
library(ggplot2)
library(SDMTools)
library(gridExtra)
library(gtable)
library(grid)
library(ggpubr)
library(RColorBrewer)
library(hrbrthemes)
library(MASS)
library(gtable)
library(grid)
percent <- function(x, digits = 1, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}
### Case data
projections_link = "https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv?raw=true"
deaths = read.csv(projections_link, na.strings = c("NA"), header = T)
#View(deaths)
deaths$FIPS = ifelse(nchar(deaths$FIPS)==4, paste0('0',deaths$FIPS), deaths$FIPS)
# add population density
popdens = readRDS('~/Dropbox/Covid-19/Risk Prediction/code_updated/population_density_covid.rds')
additional.counties = c('Baltimore city', 'Buchanan', 'Greene', 'St. Charles', 'Washoe')
for (i in 1:nrow(popdens)){
  tem = strsplit(popdens$county[i],' ')[[1]]
  if ((tem[length(tem)] != 'County')&(tem[length(tem)] != 'county')&(tem[length(tem)] != 'city')&(tem[length(tem)] != 'City'))
  {
    popdens$county[i] = 'NO'#paste(tem[-length(tem)],collapse = ' ') #paste(tem,collapse = ' ')
  }
  if ((tem[length(tem)] == 'County')|(tem[length(tem)] == 'county'))
  {
    popdens$county[i] = paste(tem[-length(tem)],collapse = ' ')
  }
  if (popdens$county[i] == 'Baltimore city') popdens$county[i] = 'Baltimore City'
  if (popdens$county[i] == 'St. Louis city') popdens$county[i] = 'St. Louis City'
  if (popdens$county[i] == 'Fairfax city') popdens$county[i] = 'Fairfax City'
  if (popdens$county[i] == 'Franklin city') popdens$county[i] = 'Franklin City'
  if (popdens$county[i] == 'Richmond city') popdens$county[i] = 'Richmond City'
  if (popdens$county[i] == 'Roanoke city') popdens$county[i] = 'Roanoke City'
}
deaths$popdens = NA
for (i in 1:nrow(deaths)){
  tem = popdens[((popdens$state == deaths$Province_State[i])&(popdens$county == deaths$Admin2[i])),'pop_den']
  if (length(tem) == 1){
    deaths$popdens[i] = tem
  }
  if (length(tem) > 1) print(i) # no such error
}
deaths = deaths[,c(1:11,ncol(deaths),12:(ncol(deaths)-1))]
deaths = deaths[!is.na(deaths$popdens),]

brfss = readRDS('data_created/brfss_sdi.rds')
codes = readRDS('data_created/placefips_county.rds')

IER = read.xlsx(paste0('data_created/IER-medicare.xlsx'))
IER = merge(IER, IER.age, by='fips')
colnames(IER)[which(colnames(IER)=='fips')] = 'FIPS'
IER = IER[,c('IER','state','FIPS','population','IER.age','Age.15-44','Age.45-54','Age.55-64','Age.65-74','Age.75-84','Age.85+','agesum')]



combined = merge(IER, deaths, by='FIPS')
colnames(combined) = c(colnames(combined)[1:23],
                       substr(colnames(combined)[24:ncol(combined)],2,20))

deathrate = cbind(combined[,1:23],pmax(sapply(24:(ncol(combined)-1),function(x){combined[,x+1]-combined[,x]}),0))
colnames(deathrate) = c(colnames(combined)[c(1:23,25:ncol(combined))])
infectionrate = deathrate
infectionrate = infectionrate[,c(1:23,(which(colnames(infectionrate) == '6.7.20')):ncol(infectionrate))]
colnames(infectionrate)[which(colnames(infectionrate)=='population')] = 'Population'




# Download county-level death data
projections_link = "https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv?raw=true"

deaths = read.csv(projections_link, na.strings = c("NA"), header = T)
deaths$FIPS = ifelse(nchar(deaths$FIPS)==4, paste0('0',deaths$FIPS), deaths$FIPS)
popdens = readRDS('~/Dropbox/Covid-19/Risk Prediction/code_updated/population_density_covid.rds')
additional.counties = c('Baltimore city', 'Buchanan', 'Greene', 'St. Charles', 'Washoe')
for (i in 1:nrow(popdens)){
  tem = strsplit(popdens$county[i],' ')[[1]]
  if ((tem[length(tem)] != 'County')&(tem[length(tem)] != 'county')&(tem[length(tem)] != 'city')&(tem[length(tem)] != 'City'))
  {
    popdens$county[i] = 'NO'#paste(tem[-length(tem)],collapse = ' ') #paste(tem,collapse = ' ')
  }
  if ((tem[length(tem)] == 'County')|(tem[length(tem)] == 'county'))
  {
    popdens$county[i] = paste(tem[-length(tem)],collapse = ' ')
  }
  if (popdens$county[i] == 'Baltimore city') popdens$county[i] = 'Baltimore City'
  if (popdens$county[i] == 'St. Louis city') popdens$county[i] = 'St. Louis City'
  if (popdens$county[i] == 'Fairfax city') popdens$county[i] = 'Fairfax City'
  if (popdens$county[i] == 'Franklin city') popdens$county[i] = 'Franklin City'
  if (popdens$county[i] == 'Richmond city') popdens$county[i] = 'Richmond City'
  if (popdens$county[i] == 'Roanoke city') popdens$county[i] = 'Roanoke City'
}
deaths$popdens = NA
for (i in 1:nrow(deaths)){
  tem = popdens[((popdens$state == deaths$Province_State[i])&(popdens$county == deaths$Admin2[i])),'pop_den']
  if (length(tem) == 1){
    deaths$popdens[i] = tem
  }
  if (length(tem) > 1) print(length(tem)) # no such error
}
deaths = deaths[,c(1:12,ncol(deaths),13:(ncol(deaths)-1))]
deaths = deaths[!is.na(deaths$popdens),]
deaths = deaths[,-which(colnames(deaths) == 'Population')]

brfss = readRDS('data_created/brfss_sdi.rds')
codes = readRDS('data_created/placefips_county.rds')

IER = read.xlsx(paste0('data_created/IER-medicare.xlsx'))
IER = merge(IER, IER.age, by='fips')
colnames(IER)[which(colnames(IER)=='fips')] = 'FIPS'
IER = IER[,c('IER','state','FIPS','population','IER.age','Age.15-44','Age.45-54','Age.55-64','Age.65-74','Age.75-84','Age.85+','agesum')]

combined = merge(IER, deaths, by='FIPS')
colnames(combined) = c(colnames(combined)[1:23],
                       substr(colnames(combined)[24:ncol(combined)],2,20))
deathrate = cbind(combined[,1:23],pmax(sapply(24:(ncol(combined)-1),function(x){combined[,x+1]-combined[,x]}),0))
colnames(deathrate) = c(colnames(combined)[c(1:23,25:ncol(combined))])
deathrate = deathrate[,c(1:23,(which(colnames(deathrate) == '6.7.20')):ncol(deathrate))]
colnames(deathrate)[which(colnames(deathrate)=='population')] = 'Population'


infectionrate = infectionrate[,1:which(colnames(infectionrate) == "10.1.20")]
deathrate = deathrate[,1:which(colnames(deathrate) == "10.1.20")]


factor.names = c('IER', 'Population density', '3-week prior infection rate\n(in 2-week window)')
unconditional.name = 'No'
conditional.name = 'Yes'
linewidth = 0.9
linewidth2 = 0.9
legend.border.col = "white"
legend.fill.col = "snow2"
legend.text.size = 9
fig.lab = c('a','b')
stratify = 'no'
len = 25
t.death = 21

state.abbr = read.csv('data/Others/50_us_states_all_data.csv',header=F)
state.abbr$V3 = as.character(state.abbr$V3)
n.periods = as.numeric(as.Date(colnames(deathrate)[ncol(deathrate)], format = '%m.%d.%y') - (as.Date('2020-06-07')) + 2 - len)
time.periods = lapply(1:n.periods,function(x){c(as.Date(colnames(deathrate)[ncol(deathrate)], format = '%m.%d.%y')-n.periods+1 - len + x, as.Date(colnames(deathrate)[ncol(deathrate)], format = '%m.%d.%y') - n.periods + x)})
dates = as.Date(colnames(deathrate), format = '%m.%d.%y')


png(paste0('figures_and_tables/output/Validation-medicare-agegroup-adjusted.png'),
    width = 1200*4, height = 600*2, res = 200*2)
par(mfrow=c(1,4))
set.seed(2020)
randomj = seq(93-25*4,93,by=25)[-1]
randomj = sort(randomj)
for (j in randomj){
  which.cols = which(dates == time.periods[[j]][1]):which(dates == time.periods[[j]][2])
  dat = data.frame(state = deathrate$Province_State,
                   death_rate = log(rowSums(deathrate[,which.cols])/deathrate$Population),
                   deaths = rowSums(deathrate[,which.cols]),
                   popdens = log(deathrate$popdens),
                   IER = log(deathrate[,'IER']*deathrate$agesum),
                   InfectionRate = log(rowSums(infectionrate[,which.cols])/infectionrate$Population),
                   popsize = deathrate$Population,
                   population = log(deathrate$Population),
                   IER.age = log(deathrate[,'IER.age']*deathrate$agesum))
  dat$weights = 1/(dat$death_rate)
  dat$weights= dat$weights/sum(dat$weights)
  
  dat = dat[dat[,2]>-Inf,]
  dat$state = as.character(dat$state)
  
  dat$region = NA
  dat$region[dat$state %in% c('Connecticut', 'Maine', 'Massachusetts', 'New Hampshire',
                              'Rhode Island', 'Vermont', 
                              'New Jersey', 'New York', 'Pennsylvania')] = 'Northeast'
  dat$region[dat$state %in% c('Illinois', 'Indiana', 'Michigan', 'Ohio', 'Wisconsin',
                              'Iowa', 'Kansas', 'Minnesota', 'Missouri', 'Nebraska', 
                              'North Dakota', 'South Dakota')] = 'Midwest'
  dat$region[dat$state %in% c('Delaware', 'Florida', 'Georgia', 'Maryland', 'North Carolina', 
                              'South Carolina', 'Virginia', 'District of Columbia', 'West Virginia',
                              'Alabama', 'Kentucky', 'Mississippi', 'Tennessee', 
                              'Arkansas', 'Louisiana', 'Oklahoma', 'Texas')] = 'South'
  dat$region[dat$state %in% c('Arizona', 'Colorado', 'Idaho', 'Montana', 'Nevada', 'New Mexico',
                              'Utah', 'Wyoming', 'Alaska', 'California', 'Hawaii', 'Oregon',
                              'Washington')] = 'West'
  regions = c("Northeast", "Midwest", "South", "West")
  dat$region = factor(dat$region, levels = regions)
  
  fit = lm(death_rate~IER, data=dat, weights = dat$weights)
  R2.IER = summary(fit)$r.squared
  int = (summary(fit)$coef['(Intercept)','Estimate'])
  
  fit = glm.nb(deaths~IER + offset(population), data=dat)
  slp = (summary(fit)$coef['IER','Estimate'])
  pvalue = summary(fit)$coef['IER','Pr(>|z|)']
  
  loessfit <- loess(death_rate ~ IER, data=dat, span=0.8, weights = dat$weights) # 30% smoothing span
  smoothed <- predict(loessfit) 
  
  plot(dat$IER,dat$death_rate,pch=20,ylab='Log of Death Rate',xlab='Log of IER',
       main=paste(time.periods[[j]][1],'to',time.periods[[j]][2],collapse = ''),
       cex=0.5,xlim=c(-2.66,0),ylim=c(-12.5,-3.5))
  abline(a=int,b=slp,lwd=2.5, col='red3')
  points(sort(dat$IER),sort(smoothed),col="orange1",type='l',lwd=2.5) # lty='dashed',
  text(x=-1, y = -11.8, labels = bquote(paste(R^2," = ",.(format(signif(R2.IER, 2), nsmall = 2)), 
                                              " (p-value = ",.(formatC(signif(pvalue, 2), format = "e", digits = 1)),")")))
}
dev.off()