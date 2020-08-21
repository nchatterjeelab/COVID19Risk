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
infectionrate = infectionrate[,1:which(colnames(infectionrate) == "8.1.20")]
deathrate = deathrate[,1:which(colnames(deathrate) == "8.1.20")]



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
x.labels <- chron((begin+len-1):(begin +len-1 + max(x.breaks)-1), out.format = 'month day')
x.breaks = which(as.character(x.labels) %in% c('June 20 ', 'June 30 ', 'July 10 ', 'July 20 ', 'July 30 '))
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
#write.xlsx(output, file=paste0('figures_and_tables/output/Unconditional_model_validation.xlsx'))


# ------------ Add conditional analysis:
results.unconditional = results
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
  dat = dat[dat[,'InfectionRate']>-8,]
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
    fit = lm(death_rate~region, data=dat, weights = dat$weights)
    dat$death_rate = fit$residuals
    fit = lm(death_rate~IER, data=dat, weights = dat$weights)
    r2[j,'IER'] = summary(fit)$r.squared
    fit = lm(death_rate~popdens, data=dat, weights = dat$weights)
    r2[j,'popdens'] = summary(fit)$r.squared
    fit = lm(death_rate~InfectionRate, data=dat, weights = dat$weights)
    r2[j,'InfectionRate'] = summary(fit)$r.squared
    fit = lm(death_rate~InfectionRate+IER+popdens, data=dat, weights = dat$weights)
    r2[j,'Total'] = summary(fit)$r.squared
    
    # coef for IER and p-values
    fit = glm.nb(deaths~IER + region + offset(population), data=dat)
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
#write.xlsx(output, file=paste0('figures_and_tables/output/Model_validation_conditional_on_regions.xlsx'))


results.unconditional$conditional = 'Unconditional analysis'
results$conditional = 'Adjusting for regional differences'
results = rbind(results.unconditional,results)
results$conditional = factor(results$conditional, 
                          levels = c('Unconditional analysis','Adjusting for regional differences'))


# ---------- 1. plot for unconditional
res.uncndl = results[results$conditional=='Unconditional analysis',-ncol(results)]
mycols = c("#E7B800", "#A6CEE3", "#1F78B4")
plot1.1 <- ggplot(data=res.uncndl, aes(x=Time_period, y=R2, group=Variable)) +
  geom_line(aes(color=Variable), linetype = 'solid', size=linewidth) + 
  labs(title = fig.lab[1], Position="left", 
       x = "End date of the 2-week period", y = paste0('Explained R2 in death rate\nover 2-week period'), #expression(paste("Explained ", R^2, " in Linear Regression")),
       color = '') + 
  theme(plot.title = element_text(hjust = -0.11,size = 15, face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5, size = 11), 
        axis.text.x = element_text(angle = 0, hjust = 0.5, size=11), 
        axis.text = element_text(size = 11), axis.title = element_text(size = 11),
        legend.position = "right",  legend.text = element_text(size = legend.text.size),
        legend.key=element_blank(), legend.key.width=unit(1,"cm"),
        legend.key.size = unit(0.5, "cm"),
        legend.title.align=0.5, legend.title = element_blank(),
        legend.background = element_rect(fill='white', size=0.3, linetype="solid", colour = legend.border.col),
        panel.background = element_rect(fill = "white", colour = "white",
                                        size = 0.2, linetype = "solid"),
        panel.grid = element_line(size = 0.2, linetype = 'dashed',
                                  colour = "#cccccc"),
        panel.border = element_blank(),
        axis.line = element_line(colour = "grey30",size=0.3)
  ) + 
  removeGrid(y = FALSE) + 
  scale_x_continuous(labels = x.labels[x.breaks], breaks = x.index)  +
  scale_y_continuous(labels = y.labels, breaks = y.breaks, expand = c(0, 0.005),limits=c(0,0.425))  +
  scale_color_manual(values = mycols)


# ---------- Extract the legend - unconditional
leg1 <- gtable_filter(ggplot_gtable(ggplot_build(plot1.1)), "guide-box") 


# ---------- 2. plot for conditional
res.cndl = results
mycols = c("#E7B800", "#A6CEE3", "#1F78B4")
plot1.2 <- ggplot(data=res.cndl, aes(x=Time_period, y=R2, group=conditional)) +
  geom_line(aes(linetype=conditional), color = 'grey30', size=linewidth) + 
  labs(title = fig.lab[1], Position="left", 
       x = "End date of the 2-week period", y = paste0('Explained variation in death rate\nover 2-week period'), #expression(paste("Explained ", R^2, " in Linear Regression")),
       color = '') + 
  theme(plot.title = element_text(hjust = -0.11,size = 15, face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5, size = 11), 
        axis.text.x = element_text(angle = 0, hjust = 0.5, size=11), 
        axis.text = element_text(size = 11), axis.title = element_text(size = 11),
        legend.position = "right",  legend.text = element_text(size = legend.text.size),
        legend.key=element_blank(), legend.key.width=unit(1,"cm"),
        legend.key.size = unit(0.5, "cm"),
        legend.title.align=0.5, legend.title = element_blank(),
        legend.background = element_rect(fill="white", size=0.3, linetype="solid", colour = legend.border.col),
        panel.background = element_rect(fill = "white", colour = "white",
                                        size = 0.2, linetype = "solid"),
        panel.grid = element_line(size = 0.2, linetype = 'dashed',
                                  colour = "#cccccc"),
        panel.border = element_blank(),
        axis.line = element_line(colour = "grey30",size=0.3)
  ) + 
  removeGrid(y = FALSE) + 
  scale_x_continuous(labels = x.labels[x.breaks], breaks = x.index)  +
  scale_y_continuous(labels = y.labels, breaks = y.breaks, expand = c(0, 0.005),limits=c(0,0.425))  +
  scale_linetype_manual(values=c("solid", "dashed"))
  


# ----------- Extract the legend - unconditional
leg2 <- gtable_filter(ggplot_gtable(ggplot_build(plot1.2)), "guide-box") 





# ----------- 3. plot with no legend
mycols = c("#E7B800", "#A6CEE3", "#1F78B4", "#E7B800", "#A6CEE3", "#1F78B4")

plot1 <- ggplot(data=results, aes(x=Time_period, y=R2, group=interaction(Variable,conditional))) +
  geom_line(aes(color=Variable, linetype = conditional, size=conditional)) + 
  labs(title = fig.lab[1], Position="left", 
       x = "End date of the 2-week period", y = paste("Explained R2 in death rate over 2-week\nperiod (moving window)"),
       color = "") + 
  theme(plot.title = element_text(hjust = -0.11,size = 15, face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5, size = 11), 
        axis.text.x = element_text(angle = 0, hjust = 0.5, size=11), 
        axis.text = element_text(size = 11), axis.title = element_text(size = 11),
        legend.position = "none",
        panel.background = element_rect(fill = "white", colour = "white",
                                        size = 0.2, linetype = "solid"),
        panel.grid = element_line(size = 0.2, linetype = 'dashed',
                                  colour = "#cccccc"),
        panel.border = element_blank(),
        axis.line = element_line(colour = "grey30",size=0.3)
  ) + 
  removeGrid(y = FALSE) + 
  scale_x_continuous(labels = x.labels[x.breaks], breaks = x.index,expand = c(0, 0))  +
  scale_y_continuous(labels = y.labels, breaks = y.breaks, expand = c(0, 0.005),limits=c(0,0.5))  +
  scale_color_manual(values = mycols[1:3]) +
  scale_linetype_manual(values=c("solid", "dashed")) +
  scale_size_manual(values=c(linewidth, linewidth2))



# --- Figure 4
# Arrange the three components (plot1, leg1, leg2)
plotNew <- plot1 + 
  annotation_custom(grob = leg1, xmin = 7.5, xmax = 9.5, ymin = 0.4, ymax = 0.4)
plotNew <- plotNew + 
  annotation_custom(grob = leg2, xmin = 22.5, xmax = 24.5, ymin = 0.42, ymax = 0.42)

daily.deaths = rep(0,n.periods)
names(daily.deaths) = as.Date(do.call("c", lapply(1:n.periods,function(x){time.periods[[x]][2]})))
for (j in 1:n.periods){
  which.cols = which(dates == time.periods[[j]][1]):which(dates == time.periods[[j]][2])
  daily.deaths[j] = ceiling(sum(rowSums(deathrate[,which.cols])))
}

temp = daily.deaths[c((which(as.Date(names(daily.deaths)) == as.Date(chron_ls[1]))):(length(daily.deaths)))]
dr = data.frame(x = 1:length(temp), 
                death_rate = temp)
dr$death = dr$death_rate

dr.breaks = seq(0, 8000, by = 2000)

bar.width = 0.6
plot2 <- ggplot(data=dr, aes(x=x, y=death)) +
  geom_bar(aes(y=death),stat="identity", size=.2, 
           fill='paleturquoise4', color="white", alpha= 0.9,
           position=position_dodge(width=0), width=bar.width+0.05) +
  labs(title = fig.lab[2], x = "End date of the 2-week period",
       y = "Total confirmed COVID-19 deaths within\n2-week periods (moving window)",
       color = "") + 
  theme(plot.title = element_text(hjust = -0.11,size = 15, face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5, size = 11), 
        axis.text.x = element_text(angle = 0, hjust = 0.5, size=11), 
        axis.text = element_text(size = 11), axis.title = element_text(size = 11),
        legend.position = "None",  legend.text = element_text(size = 11),
        panel.background = element_rect(fill = "white", colour = "white",
                                        size = 0.2, linetype = "solid"),
        panel.grid = element_line(size = 0.2, linetype = 'dashed',
                                  colour = "#cccccc"),
        panel.border = element_blank(),
        axis.line = element_line(colour = "grey30",size=0.3)
  ) + 
  removeGrid(y = FALSE) + 
  scale_x_continuous(labels = x.labels[x.breaks], breaks = x.index,expand = c(0, 0))  +
  scale_y_continuous(labels = dr.breaks, breaks = dr.breaks, limits = c(0,8500),
                     expand = c(0, 0))

png(paste0('figures_and_tables/output/Figure 4.png'),
    width = 1600*4, height = 800*8, res = 200*4.5)
library(gtable)
library(grid)
g1 <- ggplotGrob(plotNew)
g2 <- ggplotGrob(plot2)
g <- rbind(g1, g2, size = "first")
g$widths <- unit.pmax(g1$widths, g2$widths)
grid.draw(g)
dev.off()







