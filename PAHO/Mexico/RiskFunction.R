###This function will group your ages into age groups if you do not have an agegrp column
### You do not have to run it individually, it is part of the calculateRisks function
ageConverter <- function(age) {
  if(age < 40) {return(1)}
  if(age < 50) {return(2)}
  if(age < 60) {return(3)}
  if(age < 70) {return(4)}
  if(age < 80) {return(5)}
  return(6)
}

###This is for if your BMI's are not categorized, again the same as above
BMIConverter <- function(bmi) {
  if(bmi < 30) {return(1)}
  if(bmi < 35) {return(2)}
  if(bmi < 40) {return(3)}
  return(4)
}


### This function calculates the risks on individual level basis, and groups them into thresholds
### Params: filename should be a string containing the path to the csv file
### kFold should be a vector of 5 numbers to indicate the thresholds of risk you are interested in, i.e. c(1.2, 2, 5, 10, 25)
###Returns a data frame containing sizes of each threshold, as well as creating a picture plot file and new csv with risks and thresholds
### Column names: 
### agegrp represents which age group the individual is in, will also work with just an age column
### bmicat is the BMI category, but will also work with just a BMI column
### sex is explanatory, 1 for male and 2 for female
### smokes is 1 for both current and former smokers
### hbp for high blood pressure and hypertension
### kidney for kidney disease
### diabetes for having diabetes
### Rheu for rheumatoid arthritis
### ImmSupp for an immunosuppresive condition
### RespxAsthma for a respiratory disease other than asthma
### Asthma for asthma
### stroke for stroke
### liver for liver disease
### hemoCancer for haematological cancer
### nonHemoCancer for non-haematological cancer
### postweight for weights
### Only age (agegrp), bmi (bmicat), and sex are required
calculateRisks <-function(filename, kFold) {
  library(reshape2)
  library(dplyr)
  library(ggplot2)
  library(ggpubr)
  library(gridExtra)
  library(readr)
  library(plotrix)
  
  risksdf <- read.csv(filename)
  
  ###If there is age, but no agegrp
  if(!("agegrp" %in% colnames(risksdf))) {
    risksdf$agegrp = sapply(risksdf$age, ageConverter)
  }
  
  ### Age dummy variables
  risksdf <- mutate(risksdf, age1 = ifelse(agegrp == 1, 1, 0))
  risksdf <- mutate(risksdf, age2 = ifelse(agegrp == 2, 1, 0))
  risksdf <- mutate(risksdf, age3 = ifelse(agegrp == 3, 1, 0))
  risksdf <- mutate(risksdf, age4 = ifelse(agegrp == 4, 1, 0))
  risksdf <- mutate(risksdf, age5 = ifelse(agegrp == 5, 1, 0))
  risksdf <- mutate(risksdf, age6 = ifelse(agegrp == 6, 1, 0))
  
  
  ###If there isn't a male column
  if(!("male" %in% colnames(risksdf))) {
    risksdf <- mutate(risksdf, male = 2-sex)
  }
  
  ###If there is BMI, but no bmicat
  if(!("bmicat" %in% colnames(risksdf))) {
    risksdf$bmicat = sapply(risksdf$bmi, BMIConverter)
  }
  
  ###BMI dummy variables
  risksdf <- mutate(risksdf, bmi1 = ifelse(bmicat == 1, 1, 0))
  risksdf <- mutate(risksdf, bmi2 = ifelse(bmicat == 2, 1, 0))
  risksdf <- mutate(risksdf, bmi3 = ifelse(bmicat == 3, 1, 0))
  risksdf <- mutate(risksdf, bmi4 = ifelse(bmicat == 4, 1, 0))
  
  
  ###All of these are optional columns, and it checks if they are present or not
  if(!("postweight" %in% colnames(risksdf))) {
    risksdf <- mutate(risksdf, postweight = 1)
  }
  if(!("smokes" %in% colnames(risksdf))) {
    risksdf <- mutate(risksdf, smokes = 0)
  }
  
  if(!("hbp" %in% colnames(risksdf))) {
    risksdf <- mutate(risksdf, hbp = 0)
  }
  
  if(!("cvd" %in% colnames(risksdf))) {
    risksdf <- mutate(risksdf, cvd = 0)
  }
  if(!("stroke" %in% colnames(risksdf))) {
    risksdf <- mutate(risksdf, stroke = 0)
  }
  if(!("kidney" %in% colnames(risksdf))) {
    risksdf <- mutate(risksdf, kidney = 0)
  }
  if(!("diabetes" %in% colnames(risksdf))) {
    risksdf <- mutate(risksdf, diabetes = 0)
  }
  if(!("Rheu" %in% colnames(risksdf))) {
    risksdf <- mutate(risksdf, Rheu = 0)
  }
  if(!("ImmSupp" %in% colnames(risksdf))) {
    risksdf <- mutate(risksdf, ImmSupp = 0)
  }
  if(!("RespxAsthma" %in% colnames(risksdf))) {
    risksdf <- mutate(risksdf, RespxAsthma = 0)
  }
  if(!("Asthma" %in% colnames(risksdf))) {
    risksdf <- mutate(risksdf, Asthma = 0)
  }
  if(!("liver" %in% colnames(risksdf))) {
    risksdf <- mutate(risksdf, liver = 0)
  }
  
  if(!("hemoCancer" %in% colnames(risksdf))) {
    risksdf <- mutate(risksdf, hemoCancer = 0)
  }
  if(!("nonHemoCancer" %in% colnames(risksdf))) {
    risksdf <- mutate(risksdf, nonHemoCancer = 0)
  }
  
  
  
  
  ###The actual risk score
  risksdf <- risksdf %>%
    mutate(raw_risk = -2.526*age1 - 1.204*age2 + .728*age4 + 1.571*age5 + 2.488*age6 + .657*male + .247*bmi2 + .470*bmi3 + 0.824*bmi4 + .174*smokes - 0.030*hbp + .239*cvd + .560*stroke + .678*diabetes + .880*hemoCancer + .122*nonHemoCancer +0.476*liver + 0.565*kidney + 0.157*Rheu + 0.507*ImmSupp + 0.582*RespxAsthma + 0.18*Asthma)
  
  ###Turns risk scores into relative risks
  risksdf <- risksdf %>%
    mutate(exp_risk = exp(raw_risk))
  
  ###Much more helpful plot of raw risks
  rawRiskPlot = ggplot(data = risksdf, aes(x=raw_risk, y = ..density.., weight = postweight)) +
    geom_histogram(bins = 30, fill = "#6A6599FF",alpha=0.9, color = "black") + 
    xlab("Raw Risk Score") + ylab("Relative Amount of People")+
    theme_bw()+   
    theme(legend.position = "none",axis.text=element_text(size=12,colour="black"),
          axis.title=element_text(size=16)) 
  
  ###Place people into thresholds
  mean_risk <- weighted.mean(risksdf$exp_risk, risksdf$postweight)
  risksdf <- mutate(risksdf, overFirstThreshold = ifelse(exp_risk > kFold[1]*mean_risk, 1, 0))
  risksdf <- mutate(risksdf, overSecondThreshold = ifelse(exp_risk > kFold[2]*mean_risk, 1, 0))
  risksdf <- mutate(risksdf, overThirdThreshold = ifelse(exp_risk > kFold[3]*mean_risk, 1, 0))
  risksdf <- mutate(risksdf, overFourthThreshold = ifelse(exp_risk > kFold[4]*mean_risk, 1, 0))
  risksdf <- mutate(risksdf, overFifthThreshold = ifelse(exp_risk > kFold[5]*mean_risk, 1, 0))
  
  mean_risk <- round(mean_risk, digits = 2)
  
  
  mean_risk_score <- log(mean_risk)
  first_score <- log(kFold[1] * mean_risk)
  second_score <- log(kFold[2]* mean_risk)
  third_score <- log(kFold[3]* mean_risk)
  fourth_score <- log(kFold[4]* mean_risk)
  fifth_score <- log(kFold[5]* mean_risk)
  kFoldRisks <- c(mean_risk_score, first_score, second_score, third_score, fourth_score, fifth_score) 
  
  rawRiskPlot <- rawRiskPlot + geom_vline(xintercept=kFoldRisks, linetype="dashed", size = 0.5)
  testRiskPlot <- rawRiskPlot + annotate(geom="text", x = kFoldRisks, y = 0.35, label = c(paste("Mean Risk = ", as.character(mean_risk)), paste("Over", as.character(kFold[1])), paste("Over", as.character(kFold[2])), paste("Over", as.character(kFold[3])), paste("Over", as.character(kFold[4])), paste("Over", as.character(kFold[5]))), vjust = -0.5, angle = 90, size = 8)
  rawRiskPlot <- testRiskPlot
  
  ###THe final csv
  write_csv(risksdf, "risks.csv")
  
  ###Picture with risk plot, including mean risk
  ggsave("riskPlot.png", rawRiskPlot, width = 17, height = 11, dpi = 300)
  
  
  ###This is all to calculate number of ppl in each threshold
  numOver25 <- risksdf %>%
    group_by(overFifthThreshold) %>%
    summarize(counts = sum(postweight)) %>%
    mutate(pop = sum(counts)) %>%
    mutate(proportion = counts / pop) %>%
    filter(overFifthThreshold == 1) %>%
    rename(threshold = overFifthThreshold) %>%
    select(-pop)
  numOver25$threshold <- as.factor(numOver25$threshold)
  levels(numOver25$threshold) = c("over25")
  
  numOver10 <- risksdf %>%
    group_by(overFourthThreshold) %>%
    summarize(counts = sum(postweight)) %>%
    mutate(pop = sum(counts)) %>%
    mutate(proportion = counts / pop) %>%
    filter(overFourthThreshold == 1) %>%
    rename(threshold = overFourthThreshold) %>%
    select(-pop)
  numOver10$threshold <- as.factor(numOver10$threshold)
  levels(numOver10$threshold) = c("over10")
  
  numOver5 <- risksdf %>%
    group_by(overThirdThreshold) %>%
    summarize(counts = sum(postweight)) %>%
    mutate(pop = sum(counts)) %>%
    mutate(proportion = counts / pop) %>%
    filter(overThirdThreshold == 1) %>%
    rename(threshold = overThirdThreshold) %>%
    select(-pop)
  numOver5$threshold <- as.factor(numOver5$threshold)
  levels(numOver5$threshold) = c("over5")
  
  
  numOver2 <- risksdf %>%
    group_by(overSecondThreshold) %>%
    summarize(counts = sum(postweight)) %>%
    mutate(pop = sum(counts)) %>%
    mutate(proportion = counts / pop) %>%
    filter(overSecondThreshold == 1) %>%
    rename(threshold = overSecondThreshold) %>%
    select(-pop)
  numOver2$threshold <- as.factor(numOver2$threshold)
  levels(numOver2$threshold) = c("over2")
  
  numOver1.2 <- risksdf %>%
    group_by(overFirstThreshold) %>%
    summarize(counts = sum(postweight)) %>%
    mutate(pop = sum(counts)) %>%
    mutate(proportion = counts / pop) %>%
    filter(overFirstThreshold == 1) %>%
    rename(threshold = overFirstThreshold) %>%
    select(-pop)
  numOver1.2$threshold <- as.factor(numOver1.2$threshold)
  levels(numOver1.2$threshold) = c("over1.2")
  
  
  threshold_counts <- numOver1.2 %>%
    bind_rows(numOver2) %>%
    bind_rows(numOver5) %>%
    bind_rows(numOver10) %>%
    bind_rows(numOver25) %>%
    mutate(Number = counts, Proportion = 100 * proportion) %>%
    select(Number, Proportion)
  threshold_counts$Number <- round(threshold_counts$Number, digits = 0)
  rownames(threshold_counts) = paste0(c(kFold[1],kFold[2],kFold[3],kFold[4],kFold[5]),'-fold')
  
  
  return(threshold_counts)
  
}