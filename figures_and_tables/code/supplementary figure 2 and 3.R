library(ggplot2)
library(dplyr)
library(ggpubr)
library(questionr)

### supplementary figures 2 and 3

### NHIS risk without SDI using mean risk without SDI

#--------------- Load the NHIS individual level data  ---------------------
# use the relevant path for the data
individual_rs_covariates = readRDS('data_created/individual_rs_covariates.rds')

# ---- mean risk from NHIS data
mean_risk = 6.687769 
individual_rs_covariates_2fold = subset(individual_rs_covariates,exp(rs_est)>2*mean_risk)
individual_rs_covariates_5fold = subset(individual_rs_covariates,exp(rs_est)>5*mean_risk)
individual_rs_covariates_10fold = subset(individual_rs_covariates,exp(rs_est)>10*mean_risk)

df = bind_rows(individual_rs_covariates,individual_rs_covariates_2fold,
               individual_rs_covariates_5fold,individual_rs_covariates_10fold,.id="Id")
df$Id = as.factor(df$Id)
levels(df$Id) = c("NHIS population","> 2-fold Risk","> 5-fold Risk","> 10-fold Risk")

#### Age

df1 = as.data.frame(wtd.table(x = df$agegroup,y=df$Id,weights = df$sampling_weights))
df2 = df1 %>% group_by(Var2) %>% 
              mutate(percent = Freq/sum(Freq)) %>% 
              ungroup()
levels(df2$Var1) = c(sub("_", "-", levels(df2$Var1)[-length(levels(df2$Var1))]),"80+")

p1=ggplot(data=df2,aes(x=Var2,y=percent,fill=Var1)) + 
  geom_bar(stat="identity",position=position_dodge(),alpha=0.9,na.rm=TRUE) +
  scale_fill_manual(values=c("#374E55FF","#DF8F44FF","#00A1D5FF", "#B24745FF","#79AF97FF","#6A6599FF"))+
  labs(title="", Position="center", x = "Age", y = "Prevalence") + 
  theme_bw() + 
  theme(axis.text=element_text(size=12,colour = "black"),
        axis.title=element_text(size=16))+
  scale_y_continuous(expand = c(0,0),limits = c(0,max(df2$percent)+0.05))+
  theme(legend.key.size=unit(0.5, "cm"),legend.direction = "horizontal",
        legend.position = c(0.23, 0.78),legend.title = element_blank(),
        legend.text=element_text(colour="black", size=14,face="bold"))


#### Ethnicity
df1 = as.data.frame(ftable(wtd.table(x = df$race_ethnicity,y=df$Id, 
                                     weights = df$sampling_weights)))
df2 = df1 %>% group_by(Var2) %>% 
              mutate(percent = Freq/sum(Freq)) %>% 
              ungroup()
levels(df2$Var1) = sub("Non_hispanic_white", "White", levels(df2$Var1))

p3 = ggplot(data=df2,aes(x=Var2,y=percent,fill=Var1)) + 
  geom_bar(stat="identity",width=0.8,position=position_dodge(),alpha=0.9,na.rm=TRUE) +
  scale_fill_manual(values = c("#B24745FF","#79AF97FF","#6A6599FF"))+    
  labs(title="", Position="center", x = "Ethnicity", y = "Prevalence") + 
  theme_bw() +   
  theme(axis.text=element_text(size=12,colour = "black"),
        axis.title=element_text(size=16))+
  scale_y_continuous(expand = c(0,0),limits = c(0,max(df2$percent)+0.05))+
  theme(legend.key.size=unit(0.5, "cm"),legend.position = c(0.0875, 0.65),
        legend.title = element_blank(),legend.text=element_text(colour="black",size=14,face="bold"))

#### BMI

df1 = as.data.frame(wtd.table(x = df$Obesity,y=df$Id, weights = df$sampling_weights))
df2 = df1 %>% group_by(Var2) %>% 
              mutate(percent = Freq/sum(Freq)) %>% ungroup()
levels(df2$Var1) = c("None",sub("_", "-",levels(df2$Var1)[-1]))

p4 = ggplot(data=df2,aes(x=Var2,y=percent,fill=Var1)) + 
  geom_bar(stat="identity",position=position_dodge(),alpha=0.9,na.rm=TRUE) +
  scale_fill_manual(values = c("#00A1D5FF","#B24745FF","#79AF97FF","#6A6599FF"))+    
  labs(title="", Position="center", x = "BMI", y = "Prevalence") + 
  theme_bw() + 
  theme(axis.text=element_text(size=12,colour = "black"),
        axis.title=element_text(size=16))+
  scale_y_continuous(expand = c(0,0),limits = c(0,max(df2$percent)+0.05))+
  #theme(legend.position = "None")
  theme(legend.key.size=unit(0.5, "cm"),legend.direction = "horizontal",
        legend.position = c(0.65, 0.9),legend.title = element_blank(),
        legend.text=element_text(colour="black", size=14,face="bold"))

#### Smoking Status

df1 = as.data.frame(wtd.table(x = df$smoking_status,y=df$Id, weights = df$sampling_weights))
df2 = df1 %>% group_by(Var2) %>% 
              mutate(percent = Freq/sum(Freq)) %>% 
              ungroup()
df3 = df2[-which(as.character(df2$Var1) %in% c("NA")),]

p5 = ggplot(data=df3,aes(x=Var2,y=percent,fill=Var1)) + 
  geom_bar(stat="identity",width=0.8,position=position_dodge(),alpha=0.9,na.rm=TRUE) +
  scale_fill_manual(values = c("#B24745FF","#79AF97FF","#6A6599FF"))+    
  labs(title="", Position="center", x = "Smoking", y = "Prevalence") + 
  theme_bw() +
  theme(axis.text=element_text(size=12,colour = "black"),
        axis.title=element_text(size=16))+
  scale_y_continuous(expand = c(0,0),limits = c(0,max(df3$percent)+0.05))+
  theme(legend.key.size=unit(0.5, "cm"),
        legend.position = c(0.0875, 0.65),legend.title = element_blank(),
        legend.text=element_text(colour="black", size=14,face="bold"))

#### Haematological_cancer
df_c_none = df[-which(df$hematologic_cancer != "99" & df$non_hematologic_cancer == "Non_hematological"),]
df_hc_none = subset(df_c_none,hematologic_cancer %in% c("Hematological","None"))
df1 = as.data.frame(wtd.table(x = df_hc_none$diagnoses_cancer,y=df_hc_none$Id, weights = df_hc_none$sampling_weights, na.show = T))
df1$Var1 = addNA(df1$Var1)
levels(df1$Var1) = c("1-4.9 years ago","≥5 years ago","< 1 year ago","None")
df1 = df1[complete.cases(df1),]
df2 = df1 %>% 
      mutate(Var1= if_else(as.character(Var1)=="1-4.9 years ago" | as.character(Var1)=="< 1 year ago","< 5 years ago",as.character(Var1)))
df3 = df2 %>% 
      group_by(Var1,Var2) %>% 
      mutate(Freq = sum(Freq)) %>% 
      distinct()
df3 = df3 %>% 
      group_by(Var2) %>% 
      mutate(percent = Freq/sum(Freq)) %>% 
     ungroup()
df3$Var1 <- as.factor(df3$Var1)

p15 = ggplot(data=df3,aes(x=Var2,y=percent,fill=Var1)) + 
  geom_bar(stat="identity",width=0.8,position=position_dodge(),alpha=0.9,na.rm=TRUE) +
  scale_fill_manual(values = c("#79AF97FF","#B24745FF","#6A6599FF"))+    
  labs(title="", Position="center", x = "Haematological malignancy diagnosis", y = "Prevalence") + 
  theme_bw() + 
  theme(axis.text=element_text(size=12,colour = "black"),
        axis.title=element_text(size=16))+
  scale_y_continuous(expand = c(0,0),limits = c(0,max(df3$percent)+0.35))+
  theme(legend.key.size=unit(0.5, "cm"),legend.position = c(0.7, 0.85),
        legend.title = element_blank(),legend.direction = "horizontal",
        legend.text=element_text(colour="black", size=14,face="bold"))

#### non-haematological cancer

df_nhc_none = subset(df_c_none,non_hematologic_cancer %in% c("Non_hematological","None"))
df1 = as.data.frame(wtd.table(x = df_nhc_none$diagnoses_cancer,y=df_nhc_none$Id, weights = df_nhc_none$sampling_weights, na.show = T))
df1$Var1 = addNA(df1$Var1)
levels(df1$Var1) = c("1-4.9 years ago","≥5 years ago","< 1 year ago","None")
df1 = df1[complete.cases(df1),]
df2 = df1 %>% 
      mutate(Var1= if_else(as.character(Var1)=="1-4.9 years ago" | as.character(Var1)=="< 1 year ago","< 5 years ago",as.character(Var1)))
df3 = df2 %>% 
      group_by(Var1,Var2) %>% 
      mutate(Freq = sum(Freq)) %>% 
      distinct()
df3 = df3 %>% 
      group_by(Var2) %>% mutate(percent = Freq/sum(Freq)) %>% ungroup()
df3$Var1 <- as.factor(df3$Var1)

p16 = ggplot(data=df3,aes(x=Var2,y=percent,fill=Var1)) + 
  geom_bar(stat="identity",width=0.8,position=position_dodge(),alpha=0.9,na.rm=TRUE) +
  scale_fill_manual(values = c("#79AF97FF","#B24745FF","#6A6599FF"))+    
  labs(title="", Position="center", x = "Non-haematological cancer diagnosis", y = "Prevalence") + 
  theme_bw() + 
  theme(axis.text=element_text(size=12,colour = "black"),
        axis.title=element_text(size=16))+
  scale_y_continuous(expand = c(0,0),limits = c(0,max(df3$percent)+0.05))+
  theme(legend.key.size=unit(0.5, "cm"),legend.position = c(0.65, 0.88),
        legend.title = element_blank(),legend.direction = "horizontal",
        legend.text=element_text(colour="black", size=14,face="bold"))


supp_fig2 = ggarrange(p1,p5,p3,p4,p15,p16,ncol=2,nrow=3)
ggsave('figures_and_tables\output\supp_fig2.png', supp_fig2, width = 17,height=11, dpi = 300)


#### Sex
df1 = as.data.frame(wtd.table(x = df$sex,y=df$Id, weights = df$sampling_weights))
df2 = df1 %>% 
      group_by(Var2) %>% 
      mutate(percent = Freq/sum(Freq)) %>% 
      ungroup()

p2 = ggplot(data=df2,aes(x=Var2,y=percent,fill=Var1)) + 
  geom_bar(stat="identity",width=0.6,position=position_dodge(),alpha=0.9,na.rm=TRUE) +
  scale_fill_manual(values = c("#79AF97FF","#6A6599FF"))+   
  labs(title="", Position="center", x = "Sex", y = "Prevalence") + 
  theme_bw() + 
  theme(axis.text=element_text(size=12,colour = "black"),
        axis.title=element_text(size=16))+
  scale_y_continuous(expand = c(0,0),limits = c(0,max(df2$percent)+0.05))+
  theme(legend.key.size=unit(0.5, "cm"),legend.position = c(0.27, 0.85),
        legend.title = element_blank(),
        legend.text=element_text(colour="black", size=14,face="bold"))

#### Hypertension

df1 = as.data.frame(wtd.table(x = df$hypertension,y=df$Id, weights = df$sampling_weights))
df2 = df1 %>% 
      group_by(Var2) %>% 
      mutate(percent = Freq/sum(Freq)) %>% 
      ungroup()
levels(df2$Var1) = c(sub("Hypertension_high_bp", "High", levels(df2$Var1)))

p6 = ggplot(data=df2,aes(x=Var2,y=percent,fill=Var1)) + 
  geom_bar(stat="identity",width=0.6,position=position_dodge(),alpha=0.9,na.rm=TRUE) +
  scale_fill_manual(values = c("#79AF97FF","#6A6599FF"))+    
  labs(title="", Position="center", x = "Blood pressure", y = "Prevalence") + 
  theme_bw() + 
  theme(axis.text=element_text(size=12,colour = "black"),
        axis.title=element_text(size=16))+
  scale_y_continuous(expand = c(0,0),limits = c(0,max(df2$percent)+0.05))+
  theme(legend.key.size=unit(0.5, "cm"),legend.direction = "horizontal",
        legend.position = c(0.42, 0.92),legend.title = element_blank(),
        legend.text=element_text(colour="black", size=14,face="bold"))

#### Asthma

df1 = as.data.frame(wtd.table(x = df$asthma,y=df$Id, weights = df$sampling_weights))
df2 = df1 %>% 
      group_by(Var2) %>% 
      mutate(percent = Freq/sum(Freq)) %>% 
      ungroup()
levels(df2$Var1) = c("No","Yes")

p7 = ggplot(data=df2,aes(x=Var2,y=percent,fill=Var1)) + 
  geom_bar(stat="identity",width=0.6,position=position_dodge(),alpha=0.9,na.rm=TRUE) +
  scale_fill_manual(values = c("#79AF97FF","#6A6599FF"))+    
  labs(title="", Position="center", x = "Asthma", y = "Prevalence") + 
  theme_bw() + 
  theme(axis.text=element_text(size=12,colour = "black"),
        axis.title=element_text(size=16))+
  scale_y_continuous(expand = c(0,0),limits = c(0,max(df2$percent)+0.05))+
  theme(legend.key.size=unit(0.5, "cm"),
        legend.position = c(0.23, 0.75),legend.title = element_blank(),
        legend.text=element_text(colour="black", size=14,face="bold"))

#### Diabetes

df1 = as.data.frame(wtd.table(x = df$diabetes,y=df$Id, weights = df$sampling_weights))
df2 = df1 %>% 
      group_by(Var2) %>% 
      mutate(percent = Freq/sum(Freq)) %>% 
      ungroup()
levels(df2$Var1) = c("No","Yes")

p8 = ggplot(data=df2,aes(x=Var2,y=percent,fill=Var1)) + 
  geom_bar(stat="identity",width=0.6,position=position_dodge(),alpha=0.9,na.rm=TRUE) +
  scale_fill_manual(values = c("#79AF97FF","#6A6599FF"))+    
  labs(title="", Position="center", x = "Diabetes", y = "Prevalence") + 
  theme_bw() + 
  theme(axis.text=element_text(size=12,colour = "black"),
        axis.title=element_text(size=16))+
  scale_y_continuous(expand = c(0,0),limits = c(0,max(df2$percent)+0.05))+
  theme(legend.key.size=unit(0.5, "cm"),
        legend.position = c(0.75, 0.85),legend.title = element_blank(),
        legend.text=element_text(colour="black", size=14,face="bold"))

#### Stroke

df1 = as.data.frame(wtd.table(x = df$stroke,y=df$Id, weights = df$sampling_weights))
df2 = df1 %>% 
      group_by(Var2) %>% 
      mutate(percent = Freq/sum(Freq)) %>% 
      ungroup()
levels(df2$Var1) = c("No","Yes")

p9 = ggplot(data=df2,aes(x=Var2,y=percent,fill=Var1)) + 
  geom_bar(stat="identity",width=0.6,position=position_dodge(),alpha=0.9,na.rm=TRUE) +
  scale_fill_manual(values = c("#79AF97FF","#6A6599FF"))+    
  labs(title="", Position="center", x = "Stroke", y = "Prevalence") + 
  theme_bw() + 
  theme(axis.text=element_text(size=12,colour = "black"),
        axis.title=element_text(size=16))+
  scale_y_continuous(expand = c(0,0),limits = c(0,max(df2$percent)+0.05))+
  theme(legend.key.size=unit(0.5, "cm"),
        legend.position = c(0.75, 0.85),legend.title = element_blank(),
        legend.text=element_text(colour="black", size=14,face="bold"))

#### Rhemumatoid

df1 = as.data.frame(wtd.table(x = df$rhemumatoid,y=df$Id, weights = df$sampling_weights))
df2 = df1 %>% 
      group_by(Var2) %>% 
      mutate(percent = Freq/sum(Freq)) %>% 
      ungroup()
levels(df2$Var1) = c("No","Yes")

p10 = ggplot(data=df2,aes(x=Var2,y=percent,fill=Var1)) + 
  geom_bar(stat="identity",width=0.6,position=position_dodge(),alpha=0.9,na.rm=TRUE) +
  scale_fill_manual(values = c("#79AF97FF","#6A6599FF"))+    
  labs(title="", Position="center", x = "Arthritis", y = "Prevalence") + 
  theme_bw() + 
  theme(axis.text=element_text(size=12,colour = "black"),
        axis.title=element_text(size=16))+
  scale_y_continuous(expand = c(0,0),limits = c(0,max(df2$percent)+0.05))+
  theme(legend.key.size=unit(0.5, "cm"),
        legend.position = c(0.25, 0.85),legend.title = element_blank(),
        legend.text=element_text(colour="black", size=14,face="bold"))

#### heart disease

df1 = as.data.frame(wtd.table(x = df$heart_disease,y=df$Id, weights = df$sampling_weights))
df2 = df1 %>% 
      group_by(Var2) %>% 
      mutate(percent = Freq/sum(Freq)) %>% 
      ungroup()

p11 = ggplot(data=df2,aes(x=Var2,y=percent,fill=Var1)) + 
  geom_bar(stat="identity",width=0.6,position=position_dodge(),alpha=0.9,na.rm=TRUE) +
  scale_fill_manual(values = c("#79AF97FF","#6A6599FF"))+    
  labs(title="", Position="center", x = "Chronic heart disease", y = "Prevalence") + 
  theme_bw() + 
  theme(axis.text=element_text(size=12,colour = "black"),
        axis.title=element_text(size=16))+
  scale_y_continuous(expand = c(0,0),limits = c(0,max(df2$percent)+0.05))+
  theme(legend.key.size=unit(0.5, "cm"),
        legend.position = c(0.75, 0.85),legend.title = element_blank(),
        legend.text=element_text(colour="black", size=14,face="bold"))

#### kidney_disease

df1 = as.data.frame(wtd.table(x = df$kidney_disease,y=df$Id, weights = df$sampling_weights))
df2 = df1 %>% 
      group_by(Var2) %>% 
      mutate(percent = Freq/sum(Freq)) %>% 
      ungroup()
levels(df2$Var1) = c("No","Yes")

p12 = ggplot(data=df2,aes(x=Var2,y=percent,fill=Var1)) + 
  geom_bar(stat="identity",width=0.6,position=position_dodge(),alpha=0.9,na.rm=TRUE) +
  scale_fill_manual(values = c("#79AF97FF","#6A6599FF"))+    
  labs(title="", Position="center", x = "Kidney disease", y = "Prevalence") + 
  theme_bw() + 
  theme(axis.text=element_text(size=12,colour = "black"),
        axis.title=element_text(size=16))+
  scale_y_continuous(expand = c(0,0),limits = c(0,max(df2$percent)+0.05))+
  theme(legend.key.size=unit(0.5, "cm"),
        legend.position = c(0.75, 0.85),legend.title = element_blank(),
        legend.text=element_text(colour="black", size=14,face="bold"))

#### resp_ex_asthma

df1 = as.data.frame(wtd.table(x = df$resp_ex_asthma,y=df$Id, weights = df$sampling_weights))
df2 = df1 %>% 
      group_by(Var2) %>% 
      mutate(percent = Freq/sum(Freq)) %>% 
      ungroup()

p13 = ggplot(data=df2,aes(x=Var2,y=percent,fill=Var1)) + 
  geom_bar(stat="identity",width=0.6,position=position_dodge(),alpha=0.9,na.rm=TRUE) +
  scale_fill_manual(values = c("#79AF97FF","#6A6599FF"))+    
  labs(title="", Position="center", x = "Respiratory disease (excluding asthma)", y = "Prevalence") + 
  theme_bw() + 
  theme(axis.text=element_text(size=12,colour = "black"),
        axis.title=element_text(size=16))+
  scale_y_continuous(expand = c(0,0),limits = c(0,max(df2$percent)+0.05))+
  theme(legend.key.size=unit(0.5, "cm"),
        legend.position = c(0.75, 0.85),legend.title = element_blank(),
        legend.text=element_text(colour="black", size=14,face="bold"))

supp_fig3 = ggarrange(p2,p6,p7,p8,p9,p10,p11,p12,p13,ncol=3,nrow=3)
ggsave('figures_and_tables\output\supp_fig3.png', supp_fig3, width = 17,height=11, dpi = 300)
