# Covid-19 Risk Prediction

## Description
Source code for the paper:

[Estimating the Size of High-risk Populations for COVID-19 Mortality across 442 US Cities](https://www.medrxiv.org/content/10.1101/2020.05.27.20115170v1)




## Contents

#### data

This folder contains the raw data downloaded from various publicly accessible sites to obtain prevalence data on various risk factors.

> `BRFSS`

      This folder contains the raw data downloaded from CDC. The BRFSS “500 Cities: Local Data for Better Health, 2019 release provides information on estimates of crude prevalence for covariates related to unhealthy behaviors and health outcomes among adult US residents for 500 cities in the year 2017. The risk factors are BMI, smoking status, hypertension, diabetes, respiratory disease other than asthma, chronic heart disease, stroke, kidney disease, arthritis, asthma.

> `Census`

      This folder contains raw files on demographic variables, age, sex and ethnicity at city level downloaded from the publicly available American Community Survey (ACS) data.

> `Cancer`
      
      The USCS data is a combined cancer data source from Centers for Disease Control and Prevention (CDC) and the National Cancer Institute (NCI) that provides statistics on the cancer incidence (newly diagonsed cases) across different cancer sites at county level. This folder contains information on cancer incidence rates. We adjust it with survival rates to obtain cancer prevalence for Haematological and Non-Haematological cancers.
      
> `SDI`

      Information of Social Deprivation Index is obtained from Robert Graham Center and American Community Survey. This folder contains information on county level SDI quintile, which is a prooxy for IMD used in UK analysis.
      
> `Geocodes`

      This folder contains data for mapping counties and cities.
      
      
> `NHANES_Diabetes`

      This folder is required to categorize diabetes prevalences into controlled and uncontrolled group.
      
#### data_processing

    Mapping_cities_county and merge_city_data`: code for merging city- and county-level data.

#### data_created

    This folder contains all the intermediate datasets created.


#### analysis

    This folder contains all codes for the analysis in the paper, including calculating the mean risk in each city, risk for the NHIS individuals, the proportion and size of populations/cases that exceed various high-risk thresholds, etc.


#### figures_and_tables

> `code`

    This folder contains all codes to create the final set of figures and tables in the paper.

> `output`

    This folder contains the final set of figures and tables.

	
<br/>


## Authors
* Jin Jin, Department of Biostatistics, Johns Hopkins Bloomberg School of Public Health
* Prosenjit Kundu, Department of Biostatistics, Johns Hopkins Bloomberg School of Public Health
* Neha Agarwala, Department of Mathematics and Statistics, University of Maryland, Baltimore County
* [Nilanjan Chatterjee](https://nilanjanchatterjee.org/), Department of Biostatistics, Johns Hopkins Bloomberg School of Public Health
 

