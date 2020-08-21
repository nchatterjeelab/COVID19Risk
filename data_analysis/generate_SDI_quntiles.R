library(openxlsx)
## load raw SDI county data https://www.graham-center.org/rgc/maps-data-tools/sdi/social-deprivation-index.html

# Methodology: The SDI is a composite measure of seven demographic characteristics collected in the 
# American Community Survey (ACS): 
# percent living in poverty, 
# percent with less than 12 years of education, 
# percent single parent household, 
# percent living in rented housing unit, 
# percent living in overcrowded housing unit, 
# percent of households without a car, and 
# percent non-employed adults under 65 years of age.

# The output from the factor analysis model is a factor loading for each variable. 
# The factor loading represents the strength of correlation between the variables that comprise the factor 
# and the factor itself.
# The factor loadings can be interpreted as though they are regression coefficients; 
# the higher the factor loading score, the greater the variation that is explained by that variable.

# To simplify the model, we restricted it to only include measures with factor loadings greater than 0.60, 
# thus excluding percent black and percent in high-needs age groups.

# We constructed a final SDI measure based on weighted factor loading scores for each measure. 
# We examined the difference between models that included either non-employed (those not seeking work) 
# or unemployed (those actively seeking work). 
# The factor loading for the model using non-employed was slightly higher than for the model using unemployed. 
# These initial results were reproduced for each period and for each geography.

# ref:
# Measures of social deprivation that predict health care access and need within a rational area of primary care service delivery.
rawSDI = read.xlsx('data/SDI/ACS2015_countyallvars.xlsx')
digits = nchar(rawSDI$county)
add0 = ifelse(digits == 4,'0','')
rawSDI$county = paste0(add0,rawSDI$county)
rawSDI$stateID = substr(rawSDI$county,1,2)
rawSDI$countyID = substr(rawSDI$county,3,5)
rawSDI.quintiles = quantile(rawSDI$sdi_score,seq(0,1,by=0.2))[-1] # seq(20,100,by=20)
rawSDI.quintile = 1 * (rawSDI$sdi_score<rawSDI.quintiles[1]) + 2 * ((rawSDI$sdi_score>=rawSDI.quintiles[1])&(rawSDI$sdi_score<rawSDI.quintiles[2])) + 3 * ((rawSDI$sdi_score>=rawSDI.quintiles[2])&(rawSDI$sdi_score<rawSDI.quintiles[3])) + 4 * ((rawSDI$sdi_score>=rawSDI.quintiles[3])&(rawSDI$sdi_score<rawSDI.quintiles[4])) + 5 * ((rawSDI$sdi_score>=rawSDI.quintiles[4])&(rawSDI$sdi_score<=rawSDI.quintiles[5]))
rawSDI$sdi_quintile = rawSDI.quintile

write.xlsx(rawSDI,'SDI/SDI_quintile.xlsx')



