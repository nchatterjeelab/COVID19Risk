library(ggplot2)

########------- data created path for race data
CDC_race_deathdata = readRDS("CDC_race_deathdata.rds")

########------- plot race graph
p1=ggplot(data=CDC_race_deathdata,aes(x=race,y=logRR,fill=id)) + 
  geom_bar(position=position_dodge(),stat="identity",width=0.8) +
  scale_fill_manual(values=c( "#E41A1C","#377EB8"))+
  labs(title="", Position="center", x = "Race", y = "Relative Risk") + 
  theme_classic() + 
  geom_hline(yintercept = 0) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title=element_text(size=10),
        axis.text.y = element_text(size=10,colour = "black"))+
  theme(legend.key.size=unit(0.5, "cm"),
        legend.title = element_blank(),
        legend.position = c(0.15, 0.95),
        legend.text=element_text(colour="black", size=10,face="bold"))+
  geom_text(aes(label=levels(race),y=c(-0.56,-0.53,-0.20,-0.52,-0.20,-0.53,-0.5,-0.85),
                x=c(1:7,8.4)),
            vjust=0,angle=90,size=3,
            position=position_dodge(1),color="black")+
  scale_y_continuous(breaks=log(c(0.1,0.25,0.5,1,2.0,4.0)),
                     labels = c("0.1","0.25","0.5","1.0","2.0","4.0"),
                     limits = c(-1.8,1.6),expand = c(0,0))

ggsave('fig_race.png', p1, width = 3.5,height=4.5, dpi = 300)

########------- data created path for age data
CDC_age_deathdata = readRDS("CDC_age_deathdata.rds")

########------- plot age graph
p0 = ggplot(data=CDC_age_deathdata,aes(x=age.group,y=logRR,fill=id)) + 
  geom_bar(position=position_dodge(),stat="identity",width=0.8) +
  scale_fill_manual(values=c( "#E41A1C","#377EB8"))+
  labs(title="", Position="center", x = "Age group", y = "Relative Risk") + 
  theme_classic() + 
  geom_hline(yintercept = 0) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title=element_text(size=10),
        axis.text.y = element_text(size=10,colour = "black"))+
  theme(legend.key.size=unit(0.5, "cm"),
        legend.title = element_blank(),
        legend.position = c(0.15, 0.95),
        legend.text=element_text(colour="black", size=10,face="bold"))+
  geom_text(aes(label=age.group,y=c(0.4,0.4,-0.4,-0.4,-0.4,-0.4,0.4,0.4,-0.4,-0.4,-0.4,-0.4)),
            vjust=0,angle=90,size=3,
            position=position_dodge(1),color="black")+
  scale_y_continuous(breaks=log(c(0,0.1,0.25,0.5,1,2.0,4.0,8.0,16.0)),
                     labels = c("0","0.1","0.25","0.5","1.0","2.0","4.0","8.0","16.0"))

p0
ggsave('fig_age.png', p0, width = 3.5,height=4.5, dpi = 300)

