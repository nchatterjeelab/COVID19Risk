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
library(ggExtra)
percent <- function(x, digits = 1, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

### Case data
projections_link = "https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv?raw=true"
deaths = read.csv(projections_link, na.strings = c("NA"), header = T)
deaths$FIPS = ifelse(nchar(deaths$FIPS)==4, paste0('0',deaths$FIPS), deaths$FIPS)
# add population density
popdens = readRDS('data/Census/population_density_covid.rds')
additional.counties = c('Baltimore city', 'Buchanan', 'Greene', 'St. Charles', 'Washoe')
for (i in 1:nrow(popdens)){
  tem = strsplit(popdens$county[i],' ')[[1]]
  if ((tem[length(tem)] != 'County')&(tem[length(tem)] != 'county')&(tem[length(tem)] != 'city')&(tem[length(tem)] != 'City'))
  {
    popdens$county[i] = 'NO'
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

IER = read.xlsx(paste0('data_created/IER-city.xlsx'))
IER = merge(IER, codes, by='PlaceFIPS')
colnames(IER)[which(colnames(IER)=='county')] = 'FIPS'

counties = unique(IER$FIPS)
i=1
tem = IER[IER$FIPS == counties[i],]
tem$population = as.numeric(tem$population)
IER_county = data.frame(IER = sum(tem$IER * tem$population)/sum(tem$population),
                        state = tem$state[1],
                        FIPS = tem$FIPS[1])#,
for (i in 2:length(counties)){
  tem = IER[IER$FIPS == counties[i],]
  tem$population = as.numeric(tem$population)
  te = data.frame(IER = sum(tem$IER * tem$population)/sum(tem$population),
                  state = tem$state[1],
                  FIPS = tem$FIPS[1])#,
  IER_county = rbind(IER_county, te)
}
IER = IER_county; rm(IER_county)

combined = merge(IER, deaths, by='FIPS')
colnames(combined) = c(colnames(combined)[1:14],
                       substr(colnames(combined)[15:ncol(combined)],2,20))

deathrate = cbind(combined[,1:14],pmax(sapply(15:(ncol(combined)-1),function(x){combined[,x+1]-combined[,x]}),0))
colnames(deathrate) = c(colnames(combined)[c(1:14,16:ncol(combined))])
infectionrate = deathrate
infectionrate = infectionrate[,c(1:14,(which(colnames(infectionrate) == '6.7.20')):ncol(infectionrate))]




# Download county-level death data
projections_link = "https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv?raw=true"
deaths = read.csv(projections_link, na.strings = c("NA"), header = T)
deaths$FIPS = ifelse(nchar(deaths$FIPS)==4, paste0('0',deaths$FIPS), deaths$FIPS)
popdens = readRDS('data/Census/population_density_covid.rds')
additional.counties = c('Baltimore city', 'Buchanan', 'Greene', 'St. Charles', 'Washoe')
for (i in 1:nrow(popdens)){
  tem = strsplit(popdens$county[i],' ')[[1]]
  if ((tem[length(tem)] != 'County')&(tem[length(tem)] != 'county')&(tem[length(tem)] != 'city')&(tem[length(tem)] != 'City'))
  {
    popdens$county[i] = 'NO'
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

brfss = readRDS('data_created/brfss_sdi.rds')
codes = readRDS('data_created/placefips_county.rds')

IER = read.xlsx(paste0('data_created/IER-city.xlsx'))
IER = merge(IER, codes, by='PlaceFIPS')
colnames(IER)[which(colnames(IER)=='county')] = 'FIPS'

counties = unique(IER$FIPS)
i=1
tem = IER[IER$FIPS == counties[i],]
tem$population = as.numeric(tem$population)
IER_county = data.frame(IER = sum(tem$IER * tem$population)/sum(tem$population),
                        state = tem$state[1],
                        FIPS = tem$FIPS[1])
for (i in 2:length(counties)){
  tem = IER[IER$FIPS == counties[i],]
  tem$population = as.numeric(tem$population)
  te = data.frame(IER = sum(tem$IER * tem$population)/sum(tem$population),
                  state = tem$state[1],
                  FIPS = tem$FIPS[1])
  IER_county = rbind(IER_county, te)
}
IER = IER_county; rm(IER_county)
combined = merge(IER, deaths, by='FIPS')
colnames(combined) = c(colnames(combined)[1:15],
                       substr(colnames(combined)[16:ncol(combined)],2,20))
deathrate = cbind(combined[,1:15],pmax(sapply(16:(ncol(combined)-1),function(x){combined[,x+1]-combined[,x]}),0))
colnames(deathrate) = c(colnames(combined)[c(1:15,17:ncol(combined))])
deathrate = deathrate[,c(1:15,(which(colnames(deathrate) == '6.7.20')):ncol(deathrate))]
population.size = deathrate[,c('FIPS','Population')]
infectionrate = merge(infectionrate, population.size, by='FIPS')
infectionrate = cbind(infectionrate[,1:14],infectionrate[,ncol(infectionrate)],infectionrate[,15:(ncol(infectionrate)-1)])
colnames(infectionrate)[15] = 'Population'
infectionrate = infectionrate[,1:which(colnames(infectionrate) == "10.1.20")]
deathrate = deathrate[,1:which(colnames(deathrate) == "10.1.20")]
D0 = deathrate
I0 = infectionrate 

set.seed(2020)
boot.indx = lapply(1:1000,function(x){sample(1:nrow(D0),size=nrow(D0),replace = T)})

out = list()
for (b in 1:1000){
  infectionrate = I0[boot.indx[[b]],]
  deathrate = D0[boot.indx[[b]],]
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
  len = 14
  t.death = 21
  
  state.abbr = read.csv('data/Others/50_us_states_all_data.csv',header=F)
  state.abbr$V3 = as.character(state.abbr$V3)
  n.periods = as.numeric(as.Date(colnames(deathrate)[ncol(deathrate)], format = '%m.%d.%y') - (as.Date('2020-06-07')) + 2 - len)
  time.periods = lapply(1:n.periods,function(x){c(as.Date(colnames(deathrate)[ncol(deathrate)], format = '%m.%d.%y')-n.periods+1 - len + x, as.Date(colnames(deathrate)[ncol(deathrate)], format = '%m.%d.%y') - n.periods + x)})
  dates = as.Date(colnames(deathrate), format = '%m.%d.%y')
  r2 = as.data.frame(matrix(NA,n.periods,3+1))
  colnames(r2) = c('IER','popdens','InfectionRate', 'Total')
  pval = as.data.frame(matrix(NA,n.periods,3*2))
  temp = c('IER','popdens','InfectionRate')
  colnames(pval) = as.vector(sapply(1:length(temp), 
                                    function(x){c(paste0(temp[x],'.coef'),c(paste0(temp[x],'.pvalue')))}))
  ci = as.data.frame(matrix(NA,n.periods,2))
  colnames(ci) = c("IER.lb", "IER.ub")
  
  for (j in 1:n.periods){
    which.cols = which(dates == time.periods[[j]][1]):which(dates == time.periods[[j]][2])
    dat = data.frame(state = deathrate$Province_State,
                     death_rate = log(rowSums(deathrate[,which.cols])/deathrate$Population),
                     deaths = rowSums(deathrate[,which.cols]),
                     popdens = log(deathrate$popdens),
                     IER = log(deathrate[,'IER']),
                     InfectionRate = log(rowSums(infectionrate[,which.cols])/infectionrate$Population),
                     popsize = deathrate$Population,
                     population = log(deathrate$Population))
    dat$weights = 1/(dat$death_rate)
    dat$weights= dat$weights/sum(dat$weights)
    
    dat = dat[dat[,2]>-Inf,]
    #dat = dat[dat[,2]>-10,]
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
    
    
    if (nrow(dat) > 15){
      fit = lm(death_rate~IER, data=dat, weights = dat$weights)
      r2[j,'IER'] = summary(fit)$r.squared
      fit = lm(death_rate~popdens, data=dat, weights = dat$weights)
      r2[j,'popdens'] = summary(fit)$r.squared
      fit = lm(death_rate~InfectionRate, data=dat, weights = dat$weights)
      r2[j,'InfectionRate'] = summary(fit)$r.squared
      fit = lm(death_rate~InfectionRate+IER+popdens, data=dat, weights = dat$weights)
      r2[j,'Total'] = summary(fit)$r.squared
      
      # coef for IER and p-values
      fit = glm.nb(deaths~IER + offset(population), data=dat)
      pval[j,'IER.coef'] = (summary(fit)$coef['IER','Estimate'])
      pval[j,'IER.pvalue'] = summary(fit)$coef['IER','Pr(>|z|)']
      ci[j,'IER.lb'] = summary(fit)$coef['IER','Estimate'] - summary(fit)$coef['IER','Std. Error']* 1.96
      ci[j,'IER.ub'] = summary(fit)$coef['IER','Estimate'] + summary(fit)$coef['IER','Std. Error']* 1.96
    }
    rownames(r2)[j] = rownames(pval)[j] = as.character(time.periods[[j]][1])
  }
  
  start.period = which(complete.cases(r2))[1]
  time.periods = time.periods[start.period:length(time.periods)]
  r2 = r2[start.period:nrow(r2),]
  pval = pval[start.period:nrow(pval),]
  
  results = as.data.frame(rbind(cbind(1:length(time.periods), r2[,'IER'], pval[,'IER.coef'], pval[,'IER.pvalue'], factor.names[1]), 
                                cbind(1:length(time.periods), r2[,'popdens'], pval[,'popdens.coef'], pval[,'popdens.pvalue'], factor.names[2]),
                                cbind(1:length(time.periods), r2[,'InfectionRate'], pval[,'InfectionRate.coef'], pval[,'InfectionRate.pvalue'], factor.names[3])))
  colnames(results) = c('Time_period', 'R2', 'Coefficient', 'P-value', 'Variable')
  results$Variable = factor(results$Variable, levels = factor.names)
  results$Time_period = as.numeric(as.character(results$Time_period))
  results$R2 = as.numeric(as.character(results$R2))
  results$`P-value` = as.numeric(as.character(results$`P-value`))
  
  colnames(deathrate)[which(dates == time.periods[[start.period]][1])]
  begin <- chron(dates. = dates(colnames(deathrate)[which(dates == time.periods[[1]][1])], format='m.d.y'))
  plot.length = as.numeric(as.Date(colnames(deathrate)[ncol(deathrate)], format = '%m.%d.%y') - as.Date(begin, format = '%m/%d/%y')) - len
  n.x.grid = 4
  chron_ls <- chron((begin+len-1):(begin + ceiling((plot.length+len)/n.x.grid)*n.x.grid))
  x.breaks = seq(1, ceiling(length(chron_ls)/n.x.grid)*n.x.grid+1, by = ceiling(length(chron_ls)/n.x.grid))
  chron_ls <- chron((begin+len-1):(begin +len-1 + max(x.breaks)-1))
  x.labels <- chron((begin+len-1):(begin +len-1 + max(x.breaks)-1), out.format = 'Month day')
  x.breaks = seq(1,1+20*5,by=20) 
  x.index = x.breaks
  y.breaks = seq(0, 0.5, by = 0.1)
  y.labels = percent(y.breaks, digits = 0)
  
  
  output = cbind(as.Date(rownames(pval))+len-1, pval$IER.coef, ci[,c('IER.lb','IER.ub')],
                 pval$IER.pvalue, r2$IER, r2$popdens, r2$InfectionRate,
                 r2$Total)
  colnames(output)[1] = 'End date of the 2-week period'
  colnames(output)[2] = 'Coefficient of IER'
  colnames(output)[3] = '95%CI Upper Bound of IER'
  colnames(output)[4] = '95%CI Lower Bound of IER'
  colnames(output)[5] = 'Pvalue of IER'
  colnames(output)[6] = 'R2 of IER'
  colnames(output)[7] = 'R2 of population density'
  colnames(output)[8] = 'R2 of 3-week prior infection rate over 2-week period'
  colnames(output)[9] = 'Total R2'
  rownames(output) = NULL
  output[,-1] = signif(output[,-1],3)
  out[[b]] = output
  print(b)
  save(out,file='data_created/validation_uncertainty.RData')
}

a=b=c=numeric()
for (i in 1:1000){
  a=c(a,mean(out[[i]]$`R2 of IER`))
  b=c(b,mean(out[[i]]$`R2 of population density`))
  c=c(c,mean(out[[i]]$`R2 of 3-week prior infection rate over 2-week period`))
  print(i)
}
quantile(a,c(0.025,0.975))
quantile(b,c(0.025,0.975))
quantile(c,c(0.025,0.975))


