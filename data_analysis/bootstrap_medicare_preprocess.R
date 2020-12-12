rm(list=ls())
library(openxlsx)
library(survey)
library(data.table)
library(gapminder)
library(tidyverse)
library(grid)
###### construct Bootstrap samples
betaraw = readRDS('data_created/Bootstrap_samples/beta_bootstrap_nature.rds')
beta = betaraw[,c("agegroup.cdc15_45", "agegroup.cdc45_54", "agegroup.cdc65_74", "agegroup.cdc75_84", 
                  "agegroup.cdc85+", "Male", "Obese_I", "Obese_II", "Obese_III", "Smk_ex", "Smk_current",
                  "race_ethnicity.cdcHispanic", "race_ethnicity.cdcNon_hispanic_Black", "race_ethnicity.cdcNon_hispanic_asian",
                  "race_ethnicity.cdcNon_hispanic_american_Indian", paste0("IMD",2:5), "Hypertension",
                  "Resp_ex_asthma", "Asthma", "CHD", "Diabetes_unctrl","Diabetes_ctrl", paste0("Non_hema_", 1:3),paste0("Hema_", 1:3),
                  "Stroke", "Kidney", "Arthritis")]
nhisdata = readRDS('data_created/Bootstrap_samples/NHIS_imputed_bootstrap.rds')
print(paste0('Complete loading data'))


n.boot = 1000
for (b in 1:n.boot){
  # log hazard ratio of diabetes:
  Beta = beta[b,]
  NHIS = nhisdata[[b]]
  NHIS = NHIS[NHIS$sampling_weights != 0,]
  print(paste0('Complete Bootstrap ', b))
  save(Beta, NHIS, file=paste0('data_created/Bootstrap_samples/medicare/bdata/',b,'.RData'))
}
