library(reshape2)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(gridExtra)

#---------- Supplementary figure 5 and 6
#---------- Prevalence of BRFSS risk factors

#--------------- Read the city-level data
brfss_census = readRDS('data_created/full_output_updated.rds')
brfss_census = brfss_census[, -129]
hh = brfss_census

####----- Age
df = brfss_census %>% select(PlaceFIPS,Age_18_39,Age_40_49,Age_50_59,
                             Age_60_69,Age_70_79,Age_80_150)
df.age = melt(df,id="PlaceFIPS")
levels(df.age$variable) = c("18-40","40-50","50-60","60-70","70-80","80+")
df.age1 = df.age[which(df.age$variable %in% c("18-40","40-50","50-60")),]
df.age2 = df.age[which(df.age$variable %in% c("60-70","70-80","80+")),]
q11 = ggplot(data = df.age1, aes(x=value)) +
  geom_histogram(bins=30, fill = "#6A6599FF",alpha=0.9, colour="black") + 
  scale_fill_manual(values =c("red","gray","blue","green","violet","yellow"))+
  xlab("") + ylab("Number of cities") +
  theme_bw()+   
  theme(axis.text=element_text(size=12,colour="black"),
        axis.title=element_text(size=16))+
  facet_wrap(~variable,scales = "free_x")+
  theme(strip.text.x = element_text(size = 14),
        plot.margin = margin(0.2,0.3,0,0.2,"cm"),
        strip.background = element_rect(fill="azure2"))+
  scale_y_continuous(expand = c(0,0),limits=c(0,70))

q12 = ggplot(data = df.age2, aes(x=value)) +
  geom_histogram(bins=30, fill = "#6A6599FF",alpha=0.9, colour="black") + 
  scale_fill_manual(values =c("red","gray","blue","green","violet","yellow"))+
  xlab("Age") + ylab("Number of cities") +
  theme_bw()+   
  theme(axis.text=element_text(size=12,colour="black"),
        axis.title=element_text(size=16))+
  facet_wrap(~variable,scales = "free_x")+
  theme(strip.text.x = element_text(size = 14),
        plot.margin = margin(-0.3,0.3,0.3,0.2,"cm"),
        strip.background = element_rect(fill="azure2"))+
  scale_y_continuous(expand = c(0,0),limits=c(0,70))

####----- Ethnicity
df.race = brfss_census %>% select(PlaceFIPS,proportion_non_hispanic_white_asian,proportion_black,proportion_hispanic)
df.race = melt(df.race,id="PlaceFIPS")
levels(df.race$variable) = c("Non-Hispanic White/Asian","Black","Hispanic")
q3 = ggplot(data = df.race, aes(x=value)) +
  geom_histogram(bins=30, fill = "#6A6599FF",alpha=0.9, colour="black") + 
  scale_fill_manual(values =c("red","gray","blue","green","violet","yellow"))+
  xlab("Ethnicity") + ylab("Number of cities") +
  theme_bw()+   
  theme(axis.text=element_text(size=12,colour="black"),
        axis.title=element_text(size=16))+
  facet_wrap(~variable, scales = "free_x")+
  theme(strip.text.x = element_text(size = 14),
        plot.margin = margin(0,0.3,0.2,0.2,"cm"),
        strip.background = element_rect(fill="azure2"))+
  scale_y_continuous(expand = c(0,0),limits=c(0,99))+
  scale_x_continuous(breaks = c(0,0.25,0.5,0.75,1),
                     labels = c("0","0.25","0.5","0.75","1"))


####----- Sex
df.sex = brfss_census %>% select(PlaceFIPS,male_proportion,female_proportion)
df.sex = melt(df.sex,id="PlaceFIPS")
levels(df.sex$variable) = c("Male","Female")
q2 = ggplot(data = df.sex, aes(x=value)) +
  geom_histogram(bins=30, fill = "#6A6599FF",alpha=0.9, colour="black") + 
  scale_fill_manual(values =c("red","gray","blue","green","violet","yellow"))+
  xlab("Sex") + ylab("Number of cities") +
  theme_bw()+   
  theme(axis.text=element_text(size=12,colour="black"),
        axis.title=element_text(size=16))+
  facet_wrap(~variable, scales = "free_x")+
  theme(strip.text.x = element_text(size = 14),
        plot.margin = margin(0.2,0.3,0,0.2,"cm"),
        strip.background = element_rect(fill="azure2"))+
  scale_y_continuous(expand = c(0,0),limits=c(0,85))

####----- BMI
q4 = ggplot(data = brfss_census, aes(x=OBESITY_CrudePrev)) +
  geom_histogram(bins=30, fill = "#6A6599FF",alpha=0.9, color = "black") + 
  xlab("Obesity prevalence") + ylab("Number of cities")+
  theme_bw()+ 
  theme(axis.text=element_text(size=12,colour="black"),
        axis.title=element_text(size=16),
        plot.margin = margin(0.2,0.2,0.3,0.2,"cm"))+
  scale_y_continuous(expand = c(0,0),limits=c(0,45))

####----- SDI

df.sdi = as.data.frame(ftable(x = brfss_census$sdi))
q6 = ggplot(data=brfss_census,aes(x=sdi)) + 
  geom_bar(fill="#6A6599FF",width=0.5) +
  labs(title="", Position="center", x = "SDI",y="Number of cities") + 
  theme_bw() + 
  theme(axis.text=element_text(size=12,colour = "black"),
        axis.title=element_text(size=16,colour = "black"),
        plot.margin = margin(-0.4,0.3,0.3,0,"cm"))+
  scale_y_continuous(expand = c(0,0),limits=c(0,180))

####----- haematological cancer & non-haematological cancer
df.c = brfss_census %>% select(PlaceFIPS,hematologic_cancer1,hematologic_cancer2,hematologic_cancer3,
                               non_hematologic_cancer1,non_hematologic_cancer2,non_hematologic_cancer3) %>% 
        group_by(PlaceFIPS) %>%
        mutate(hc=sum(hematologic_cancer1,hematologic_cancer2,hematologic_cancer3),
               nhc=sum(non_hematologic_cancer1,non_hematologic_cancer2,non_hematologic_cancer3)) %>%
        select(PlaceFIPS,hc,nhc)
df.c= melt(df.c,id="PlaceFIPS")
levels(df.c$variable) = c("Haematological malignancy","Non-haematological cancer")

q5 = ggplot(data = df.c, aes(x=value)) +
  geom_histogram(bins=30,fill="#6A6599FF",alpha=0.9, color = "black") + 
  xlab("Cancer prevalence") + ylab("Number of cities")+
  theme_bw()+ 
  theme(legend.position="none",axis.text=element_text(size=12,colour="black"),
        axis.title=element_text(size=16)) +
  facet_wrap(~variable, scales = "free_x") +
  theme(strip.text.x = element_text(size = 14),
        plot.margin = margin(0,0.3,0.2,0.2,"cm"),
        strip.background = element_rect(fill="azure2"))+
  scale_y_continuous(expand = c(0,0),limits=c(0,65))

g1 = ggarrange(q11,q12,q3,nrow=3)
g2 = ggarrange(q2,ggarrange(q4,q6,ncol=2),q5,nrow=3)
supp_fig4 = grid.arrange(g1, g2, ncol = 2, widths = c(4.2, 3))

ggsave("figures_and_tables/output/supp_fig4.png", supp_fig4, width = 17, height=11,dpi = 300)

####----- smoking
p0 = ggplot(data = brfss_census, aes(x=CSMOKING_CrudePrev/100)) +
  geom_histogram(bins = 30, fill = "#6A6599FF",alpha=0.9, color = "black") + 
  xlab("Smoking prevalence") + ylab("Number of cities")+
  theme_bw()+ 
  theme(legend.position = "none",axis.text=element_text(size=12,colour="black"),
        axis.title=element_text(size=16))   


p1 = ggplot(data = brfss_census, aes(x=DIABETES_CrudePrev/100)) +
  geom_histogram(bins = 30, fill = "#6A6599FF",alpha=0.9, color = "black") + 
  xlab("Diabetes prevalence") + ylab("Number of cities") +
  theme_bw()+   
  theme(legend.position = "none",axis.text=element_text(size=12,colour="black"),
        axis.title=element_text(size=16))+
  scale_y_continuous(expand = c(0,0),limits = c(0,45))    


p2 = ggplot(data = brfss_census, aes(x=KIDNEY_CrudePrev)) +
  geom_histogram(bins = 30, fill = "#6A6599FF",alpha=0.9, color = "black") + 
  xlab("Kidney disease prevalence") + ylab("Number of cities")+
  theme_bw()+ 
  theme(legend.position = "none",axis.text=element_text(size=12,colour="black"),
        axis.title=element_text(size=16)) +
  scale_y_continuous(expand = c(0,0),limits = c(0,42))      

p3 = ggplot(data = brfss_census, aes(x=COPD_CrudePrev)) +
  geom_histogram(bins = 30, fill = "#6A6599FF",alpha=0.9, color = "black") + 
  xlab("Respiratory disease prevalence (excluding asthma)") + ylab("Number of cities")+
  theme_bw()+   
  theme(legend.position = "none",axis.text=element_text(size=12,colour="black"),
        axis.title=element_text(size=16))  +
  scale_y_continuous(expand = c(0,0),limits = c(0,42))     

p4 = ggplot(data = brfss_census, aes(x=CHD_CrudePrev)) +
  geom_histogram(bins = 30, fill = "#6A6599FF",alpha=0.9, color = "black") + 
  xlab("Chronic heart disease prevalence") + ylab("Number of cities")+
  theme_bw()+   
  theme(legend.position = "none",axis.text=element_text(size=12,colour="black"),
        axis.title=element_text(size=16))+
  scale_y_continuous(expand = c(0,0),limits = c(0,45))       

p5 = ggplot(data = brfss_census, aes(x=BPHIGH_CrudePrev)) +
  geom_histogram(bins = 30, fill = "#6A6599FF",alpha=0.9, color = "black") + 
  xlab("High blood pressure prevalence") + ylab("Number of cities")+
  theme_bw()+   
  theme(legend.position = "none",axis.text=element_text(size=12,colour="black"),
                      axis.title=element_text(size=16)) +
  scale_y_continuous(expand = c(0,0),limits = c(0,50))    

p6 = ggplot(data = brfss_census, aes(x=ARTHRITIS_CrudePrev)) +
  geom_histogram(bins = 30, fill = "#6A6599FF",alpha=0.9, color = "black") + 
  xlab("Arthritis prevalence") + ylab("Number of cities")+
  theme_bw()+   
  theme(legend.position = "none",axis.text=element_text(size=12,colour="black"),
        axis.title=element_text(size=16)) +
  scale_y_continuous(expand = c(0,0),limits = c(0,42))  

p7 = ggplot(data = brfss_census, aes(x=CASTHMA_CrudePrev)) +
  geom_histogram(bins = 30, fill = "#6A6599FF",alpha=0.9, color = "black") + 
  xlab("Asthma prevalence") + ylab("Number of cities")+
  theme_bw()+
  theme(legend.position = "none",axis.text=element_text(size=12,colour="black"),
        axis.title=element_text(size=16)) +
  scale_y_continuous(expand = c(0,0),limits = c(0,55)) 

p8 = ggplot(data = brfss_census, aes(x=STROKE_CrudePrev)) +
  geom_histogram(bins = 30, fill = "#6A6599FF",alpha=0.9, color = "black") + 
  xlab("Stroke prevalence") + ylab("Number of cities")+
  theme_bw() +
  theme(legend.position="none",axis.text=element_text(size=12,colour="black"),
        axis.title=element_text(size=16)) +
  scale_y_continuous(expand = c(0,0),limits = c(0,60))
  
plot2 = ggarrange(p0,p1,p2,p3,p4,p5,p6,p7,p8, ncol = 3, nrow = 3)
ggsave("figures_and_tables/output/supp_fig5.png", supp_fig5, width = 17, height=11,dpi = 300)

