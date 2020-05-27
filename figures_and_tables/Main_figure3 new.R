library(openxlsx)
library(dplyr)
library(ggplot2)
library(reshape2)
library(ggpubr)
library(ggrepel)
library(openxlsx)
library(survey)
library(ggpubr) # for gghistogram
library(gghighlight)
library(ggExtra)
library(ggplot2)
library(data.table)
library(gapminder)
library(tidyverse)
library(grid)
library(gridExtra)
library(ggrepel)




n.bins = 50
subfig.lab = c('A.','B.','C.')
# ------------------------------ histograms:
#  ------------------------------ cases
##### make histograms with labels
scenario = expand.grid(k = c(2,5,10),
                       sample = c('cases'),
                       threshold_type = 'overall-mean'
)
threshold_type = scenario[1,'threshold_type']
sample = scenario[1,'sample']
phist = list()
for (s in 1:nrow(scenario)){
  threshold_type = scenario[s,'threshold_type']
  sample = scenario[s,'sample']
  k = scenario[s,'k']
  
  a=t(sapply(1:nrow(dat.city),function(x){highrisk.size(k, mul1 = dist.rs.city_under40[x,'mu_under40'], 
                                                        sigma2l1 = dist.rs.city_under40[x,'var_under40'], 
                                                        mul2 = dist.rs.city_over40[x,'mu_over40'], 
                                                        sigma2l2 = dist.rs.city_over40[x,'var_over40'], 
                                                        mean.rs = mean.rs, wv.rs = wv.rs,
                                                        population = rs.city.info$population[x],
                                                        threshold_type, sample,
                                                        dat.city = dat.city,city=x)}))
  colnames(a) = c('Proportion','N')
  
  n.tail = 5
  hr = cbind(rs.city.info, a)
  
  
  
  sorted = sort(hr$Proportion,decreasing=T)
  hr$group = NA
  for (i in 1:nrow(hr)){
    hr$rank[i] = which(sorted == hr$Proportion[i])
    if (hr$rank[i]<=n.tail) hr$group[i] = 'Highest Risk'#paste0(n.tail, ' Highest Risk')
    if ((hr$rank[i]>n.tail)&(hr$rank[i]<=nrow(hr)-n.tail)) hr$group[i] = 'Middle'
    if (hr$rank[i]>(nrow(hr)-n.tail)) hr$group[i] = 'Lowest Risk' #paste0(n.tail, ' Lowest Risk')
  }
  majorcities = c('New York','Los Angeles','Chicago','Houston','Philadelphia','Austin',
                  'Atlanta','San Francisco','Washington','Seattle','Boston','Detroit')
  
  hr$lab = hr$PlaceName #paste(hr$PlaceName,'\n ',hr$rank)
  majorcities.P = majorcities # sapply(1:length(majorcities),function(x){paste(majorcities[x],'\n ',hr[which(hr$PlaceName==majorcities[x]),'rank'])})

  phist[[s]] = ggplot(hr,aes(Proportion,fill=brewer.pal(n = 8, name = "YlOrRd")[4],color= brewer.pal(n = 8, name = "RdYlBu")[2] )) + # brewer.pal(n = 8, name = "YlOrRd")[4])) + 
    geom_histogram(bins=n.bins,
                   color="black",alpha=0.9) + #bins=50,binwidth = ((max(hr$Proportion)-min(hr$Proportion))/49)
    #geom_vline(aes(xintercept = median(rs$rs.std)),col='blue',size=0.8) + 
    #scale_fill_manual(values=brewer.pal(n = 8, name = "YlOrRd")[4]) +
    #theme(axis.text.x = element_text(angle = 0, size=12, face="bold"), axis.text.y = element_text(size=12, face="bold")) + theme_bw() +
    theme(axis.text.x = element_text(angle = 0, size=12), axis.text.y = element_text(size=12)) + theme_bw() +
    labs(title=subfig.lab[s], Position="center", x = paste0('Proportion of cases with risk higher than ',k,'-fold of average risk'), y = "Number of cities") +
    theme(plot.title = element_text(hjust = 0, face="bold")) + 
    theme(legend.position = "none",axis.text=element_text(size=12),
          axis.title=element_text(size=14))+ # ,face="bold"
    #scale_x_continuous(labels = ks,breaks = seq(-1.32,0.72,length.out=5)) + 
    stat_bin(bins=n.bins, geom='text', color='white', aes(label=..count..),
             position=position_stack(vjust = 0.5))
  print(s)
}

# ---------- scatterplots:
highrisk_all = read.xlsx("~/Dropbox/NHANES_risk_score/results/highrisk_overall-mean/highrisk_all_mixtureN_overall-mean.xlsx")
highrisk_cases = read.xlsx("~/Dropbox/NHANES_risk_score/results/highrisk_overall-mean/highrisk_cases_mixtureN_overall-mean.xlsx")

colnames(highrisk_all)[-c(1:6)] = paste("all", colnames(highrisk_all)[-c(1:6)], sep = "_")
colnames(highrisk_cases)[-c(1:6)] = paste("cases", colnames(highrisk_cases)[-c(1:6)], sep = "_")

#highrisk = merge(highrisk_all,highrisk_cases,by=c("PlaceFIPS","PlaceName","StateAbbr","population","rs_est","mu.rs","var.rs"),all.x = TRUE,all.y = F)
highrisk = merge(highrisk_all,highrisk_cases,by=c("PlaceFIPS","PlaceName","StateAbbr","population"),all.x = TRUE,all.y = F)
colnames(highrisk) = sub("=", "_", colnames(highrisk))
# inx = grep("_N.",colnames(highrisk))
# highrisk = highrisk[,-c(2:7,inx)]
# hr.plot = melt(highrisk,.id=PlaceFIPS)

city = c('New York','Los Angeles','Chicago','Houston','Philadelphia','Austin',
         'Atlanta','San Francisco','Washington','Seattle','Boston','Detroit')


highrisk$label.ind = ifelse(highrisk$PlaceName %in% city,1,0)

# cornflowerblue, 

p1 = ggplot(highrisk,aes(x=all_Proportion.k_2,y=cases_Proportion.k_2,label=PlaceName)) + 
  #geom_point(fill="darkseagreen3",color="darkseagreen3")  + 
  geom_point(fill=brewer.pal(n = 8, name = "YlOrRd")[4],color=brewer.pal(n = 8, name = "YlOrRd")[4]) + 
  #geom_point(fill=brewer.pal(n = 8, name = "YlOrRd")[5],color=brewer.pal(n = 8, name = "YlOrRd")[5]) + 
  #geom_point(fill="maroon",color="maroon")  + 
  theme(axis.text.x = element_text(angle = 0, size=12, face="bold"), axis.text.y = element_text(size=12, face="bold")) + 
  labs(title="D.", Position="center", x = paste0('Proportion of population with risk higher than 2-fold of average risk'), 
       y = paste0('Proportion of cases within \n the same risk category')) +
       #y = paste0('Proportion of cases with risk \n higher than 5-fold of average risk')) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0, face="bold"))  + 
  theme(legend.position = "none",axis.text=element_text(size=12,colour = "black"),
        axis.title=element_text(size=14)) #+ # ,face="bold"
  #geom_label_repel(data = subset(highrisk, label.ind==1),
  #                 nudge_y = -0.1,  box.padding   = 0.35, 
  #                 point.padding = 0.2,
  #                 segment.color = 'grey50',
  #                 direction="x")

p2 = ggplot(highrisk,aes(x=all_Proportion.k_5,y=cases_Proportion.k_5,label=PlaceName)) + 
  geom_point(fill=brewer.pal(n = 8, name = "YlOrRd")[4],color=brewer.pal(n = 8, name = "YlOrRd")[4]) + 
  #geom_point(fill="darkseagreen3",color="darkseagreen3")  +
  theme(axis.text.x = element_text(angle = 0, size=12, face="bold"), axis.text.y = element_text(size=12, face="bold")) + 
  labs(title="E.", Position="center", x = paste0('Proportion of population with risk higher than 5-fold of average risk'), 
       y = paste0('Proportion of cases within \n the same risk category')) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0, face="bold")) + 
  theme(legend.position = "none",axis.text=element_text(size=12),
        axis.title=element_text(size=14)) #+ #,face="bold"
  #geom_label_repel(data = subset(highrisk, label.ind==1),
  #                 nudge_y = -0.1,  box.padding   = 0.35, 
  #                 point.padding = 0.2,
  #                 segment.color = 'grey50',
  #                 direction="x")

p3 = ggplot(highrisk,aes(x=all_Proportion.k_10,y=cases_Proportion.k_10,label=PlaceName)) + 
  geom_point(fill=brewer.pal(n = 8, name = "YlOrRd")[4],color=brewer.pal(n = 8, name = "YlOrRd")[4]) + 
  #geom_point(fill="darkseagreen3",color="darkseagreen3")  + 
  #scale_color_gradient(low="yellow",high="red")+
  theme(axis.text.x = element_text(angle = 0, size=12, face="bold"), axis.text.y = element_text(size=12, face="bold")) + 
  labs(title="F.", Position="center", x = paste0('Proportion of population with risk higher than 10-fold of average risk'), 
       y = paste0('Proportion of cases within \n the same risk category')) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0, face="bold"))  + 
  theme(legend.position = "none",axis.text=element_text(size=12),
        axis.title=element_text(size=14)) #+ # ,face="bold"
  #geom_text(aes(label=ifelse(label.ind==1 ,as.character(PlaceName),'')),hjust=0,vjust=0)
  #geom_label_repel(data = subset(highrisk, label.ind==1),
  #                 nudge_y = -0.06,  box.padding   = 0.35, 
  #                 point.padding = 0.2,
  #                 segment.color = 'grey50',
  #                 direction="x")

# ------- combine
png(paste0('~/Dropbox/NHANES_risk_score/results/highrisk_',threshold_type,'/fig3_draft.png'),
    units='px', width=1800*5, height=1800*3, res=170*3,type = "cairo")

grid.arrange(phist[[1]], phist[[2]], phist[[3]], p1, p2, p3,
             layout_matrix = rbind(c(1,4),c(2,5),c(3,6)),
             nrow=3, ncol=2,
             widths = c(6,6), heights = c(2,2,2))
dev.off()








# ---------------------------------------- complete figure with no label ----------------------------------------
n.bins = 50
subfig.lab = c('A.','B.','C.')
# ------------------------------ histograms:
#  ------------------------------ cases
##### make histograms with labels
scenario = expand.grid(k = c(2,5,10),
                       sample = c('cases'),
                       threshold_type = 'overall-mean'
)
threshold_type = scenario[1,'threshold_type']
sample = scenario[1,'sample']
phist = list()
for (s in 1:nrow(scenario)){
  threshold_type = scenario[s,'threshold_type']
  sample = scenario[s,'sample']
  k = scenario[s,'k']
  
  a=t(sapply(1:nrow(dat.city),function(x){highrisk.size(k, mul1 = dist.rs.city_under40[x,'mu_under40'], 
                                                        sigma2l1 = dist.rs.city_under40[x,'var_under40'], 
                                                        mul2 = dist.rs.city_over40[x,'mu_over40'], 
                                                        sigma2l2 = dist.rs.city_over40[x,'var_over40'], 
                                                        mean.rs = mean.rs, wv.rs = wv.rs,
                                                        population = rs.city.info$population[x],
                                                        threshold_type, sample,
                                                        dat.city = dat.city,city=x)}))
  colnames(a) = c('Proportion','N')
  
  n.tail = 5
  hr = cbind(rs.city.info, a)
  
  
  
  sorted = sort(hr$Proportion,decreasing=T)
  hr$group = NA
  for (i in 1:nrow(hr)){
    hr$rank[i] = which(sorted == hr$Proportion[i])
    if (hr$rank[i]<=n.tail) hr$group[i] = 'Highest Risk'#paste0(n.tail, ' Highest Risk')
    if ((hr$rank[i]>n.tail)&(hr$rank[i]<=nrow(hr)-n.tail)) hr$group[i] = 'Middle'
    if (hr$rank[i]>(nrow(hr)-n.tail)) hr$group[i] = 'Lowest Risk' #paste0(n.tail, ' Lowest Risk')
  }
  majorcities = c('New York','Los Angeles','Chicago','Houston','Philadelphia','Austin',
                  'Atlanta','San Francisco','Washington','Seattle','Boston','Detroit')
  
  hr$lab = hr$PlaceName #paste(hr$PlaceName,'\n ',hr$rank)
  majorcities.P = majorcities # sapply(1:length(majorcities),function(x){paste(majorcities[x],'\n ',hr[which(hr$PlaceName==majorcities[x]),'rank'])})
  
  phist[[s]] = ggplot(hr,aes(Proportion,fill=brewer.pal(n = 8, name = "YlOrRd")[4],color= brewer.pal(n = 8, name = "RdYlBu")[2] )) + # brewer.pal(n = 8, name = "YlOrRd")[4])) + 
    geom_histogram(bins=n.bins,
                   color="black",alpha=0.9) + #bins=50,binwidth = ((max(hr$Proportion)-min(hr$Proportion))/49)
    #geom_vline(aes(xintercept = median(rs$rs.std)),col='blue',size=0.8) + 
    #scale_fill_manual(values=brewer.pal(n = 8, name = "YlOrRd")[4]) +
    #theme(axis.text.x = element_text(angle = 0, size=12, face="bold"), axis.text.y = element_text(size=12, face="bold")) + theme_bw() +
    theme(axis.text.x = element_text(angle = 0, size=12), axis.text.y = element_text(size=12)) + theme_bw() +
    labs(title=subfig.lab[s], Position="center", x = paste0('Proportion of total cases exceeding the ',k,'-fold risk-threshold'), y = "Number of cities") +
    theme(plot.title = element_text(hjust = 0, face="bold")) + 
    #labs(title="F.", Position="center", x = "Proportion of population greater than 25-fold risk threshold", y = "Proportion of cases") +
    theme(legend.position = "none",axis.text=element_text(size=13),
          axis.title=element_text(size=13.5))
  print(s)
}

# ---------- scatterplots:
highrisk_all = read.xlsx("~/Dropbox/NHANES_risk_score/results/highrisk_overall-mean/highrisk_all_mixtureN_overall-mean.xlsx")
highrisk_cases = read.xlsx("~/Dropbox/NHANES_risk_score/results/highrisk_overall-mean/highrisk_cases_mixtureN_overall-mean.xlsx")

colnames(highrisk_all)[-c(1:6)] = paste("all", colnames(highrisk_all)[-c(1:6)], sep = "_")
colnames(highrisk_cases)[-c(1:6)] = paste("cases", colnames(highrisk_cases)[-c(1:6)], sep = "_")

#highrisk = merge(highrisk_all,highrisk_cases,by=c("PlaceFIPS","PlaceName","StateAbbr","population","rs_est","mu.rs","var.rs"),all.x = TRUE,all.y = F)
highrisk = merge(highrisk_all,highrisk_cases,by=c("PlaceFIPS","PlaceName","StateAbbr","population"),all.x = TRUE,all.y = F)
colnames(highrisk) = sub("=", "_", colnames(highrisk))
# inx = grep("_N.",colnames(highrisk))
# highrisk = highrisk[,-c(2:7,inx)]
# hr.plot = melt(highrisk,.id=PlaceFIPS)

city = c('New York','Los Angeles','Chicago','Houston','Philadelphia','Austin',
         'Atlanta','San Francisco','Washington','Seattle','Boston','Detroit')


highrisk$label.ind = ifelse(highrisk$PlaceName %in% city,1,0)

# cornflowerblue, 

p1 = ggplot(highrisk,aes(x=all_Proportion.k_2,y=cases_Proportion.k_2,label=PlaceName)) + 
  geom_point(fill=brewer.pal(n = 8, name = "YlOrRd")[4],color=brewer.pal(n = 8, name = "YlOrRd")[4]) + # Dark Goldenrod 2
  #geom_point(fill=brewer.pal(n = 8, name = "YlOrRd")[5],color=brewer.pal(n = 8, name = "YlOrRd")[5]) + 
  theme(axis.text.x = element_text(angle = 0, size=12, face="bold"), axis.text.y = element_text(size=12, face="bold")) + 
  labs(title="D.", Position="center", x = paste0('Proportion of population exceeding the 2-fold risk-threshold'), 
       y = paste0('Proportion of total cases exceeding\n the 2-fold risk-threshold')) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0, face="bold"))  + 
  theme(legend.position = "none",axis.text=element_text(size=13,colour = "black"),
        axis.title=element_text(size=13.5)) + # ,face="bold"
  ylim(0,0.6)
#geom_label_repel(data = subset(highrisk, label.ind==1),
#                 nudge_y = -0.1,  box.padding   = 0.35, 
#                 point.padding = 0.2,
#                 segment.color = 'grey50',
#                 direction="x")

p2 = ggplot(highrisk,aes(x=all_Proportion.k_5,y=cases_Proportion.k_5,label=PlaceName)) + 
  geom_point(fill=brewer.pal(n = 8, name = "YlOrRd")[4],color=brewer.pal(n = 8, name = "YlOrRd")[4]) + 
  #geom_point(fill="darkseagreen3",color="darkseagreen3")  +
  theme(axis.text.x = element_text(angle = 0, size=12, face="bold"), axis.text.y = element_text(size=12, face="bold")) + 
  labs(title="E.", Position="center", x = paste0('Proportion of population exceeding the 5-fold risk-threshold'), 
       y = paste0('Proportion of total cases exceeding\n the 5-fold risk-threshold')) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0, face="bold")) + 
  theme(legend.position = "none",axis.text=element_text(size=13),
        axis.title=element_text(size=13.5)) + #,face="bold"
  ylim(0,0.6)
#geom_label_repel(data = subset(highrisk, label.ind==1),
#                 nudge_y = -0.1,  box.padding   = 0.35, 
#                 point.padding = 0.2,
#                 segment.color = 'grey50',
#                 direction="x")

p3 = ggplot(highrisk,aes(x=all_Proportion.k_10,y=cases_Proportion.k_10,label=PlaceName)) + 
  geom_point(fill=brewer.pal(n = 8, name = "YlOrRd")[4],color=brewer.pal(n = 8, name = "YlOrRd")[4]) + 
  #geom_point(fill="darkseagreen3",color="darkseagreen3")  + 
  #scale_color_gradient(low="yellow",high="red")+
  theme(axis.text.x = element_text(angle = 0, size=12, face="bold"), axis.text.y = element_text(size=12, face="bold")) + 
  labs(title="F.", Position="center", x = paste0('Proportion of population exceeding the 10-fold risk-threshold'), 
       y = paste0('Proportion of total cases exceeding\n the 10-fold risk-threshold')) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0, face="bold"))  + 
  theme(legend.position = "none",axis.text=element_text(size=13),
        axis.title=element_text(size=13.5)) + # 
  ylim(0,0.6)
#geom_text(aes(label=ifelse(label.ind==1 ,as.character(PlaceName),'')),hjust=0,vjust=0)
#geom_label_repel(data = subset(highrisk, label.ind==1),
#                 nudge_y = -0.06,  box.padding   = 0.35, 
#                 point.padding = 0.2,
#                 segment.color = 'grey50',
#                 direction="x")

# ------- combine
png(paste0('~/Dropbox/NHANES_risk_score/results/highrisk_',threshold_type,'/fig3_nolabel_new.png'),
    units='px', width=1800*5, height=1800*3, res=170*3,type = "cairo")

grid.arrange(phist[[1]], phist[[2]], phist[[3]], p1, p2, p3,
             layout_matrix = rbind(c(1,4),c(2,5),c(3,6)),
             nrow=3, ncol=2,
             widths = c(6,6), heights = c(2,2,2))
dev.off()


















# ------------------------------------ add city names: ------------------------------------
scenario = expand.grid(k = c(2,5,10),
                       sample = c('cases'),
                       threshold_type = 'overall-mean'
)
threshold_type = scenario[1,'threshold_type']
sample = scenario[1,'sample']

# ------------------------------------ 2-fold ------------------------------------
s=1
threshold_type = scenario[s,'threshold_type']
sample = scenario[s,'sample']
k = scenario[s,'k']

a=t(sapply(1:nrow(dat.city),function(x){highrisk.size(k, mul1 = dist.rs.city_under40[x,'mu_under40'], 
                                                      sigma2l1 = dist.rs.city_under40[x,'var_under40'], 
                                                      mul2 = dist.rs.city_over40[x,'mu_over40'], 
                                                      sigma2l2 = dist.rs.city_over40[x,'var_over40'], 
                                                      mean.rs = mean.rs, wv.rs = wv.rs,
                                                      population = rs.city.info$population[x],
                                                      threshold_type, sample,
                                                      dat.city = dat.city,city=x)}))
colnames(a) = c('Proportion','N')

n.tail = 5
#hr = cbind(rs.city.info, dist.rs.city, a)
hr = cbind(rs.city.info, a)
sorted = sort(hr$Proportion,decreasing=T)
hr$group = NA
hr$rank = NA
if (T == T){
  for (i in 1:nrow(hr)){
    hr$rank[i] = which(sorted == hr$Proportion[i])
    if (hr$rank[i]<=n.tail) hr$group[i] = 'Highest Risk'#paste0(n.tail, ' Highest Risk')
    if ((hr$rank[i]>n.tail)&(hr$rank[i]<=nrow(hr)-n.tail)) hr$group[i] = 'Middle'
    if (hr$rank[i]>(nrow(hr)-n.tail)) hr$group[i] = 'Lowest Risk' #paste0(n.tail, ' Lowest Risk')
  }
}

majorcities = c('New York','Los Angeles','Chicago','Houston','Philadelphia','Austin',
                'Atlanta','San Francisco','Washington','Seattle','Boston','Detroit')

hr$lab = hr$PlaceName # paste(hr$PlaceName,'\n ',hr$rank)
majorcities.P = majorcities #sapply(1:length(majorcities),function(x){paste(majorcities[x],'\n ',hr[which(hr$PlaceName==majorcities[x]),'rank'])})

b = hr[hr$PlaceName %in% majorcities,]
rownames(b) = b$PlaceName
b[majorcities,]

counts = c(1,2,1,2,4,2,7,4,8,10,12,13,18,18,15,17,21,22,17,25,24,20,19,20,16,17,12,10,12,8,9,5,7,9,6,5,5,4,2,1,2,2,4,1,2,1)
sum(counts)
cumulative = sapply(1:length(counts),function(x){sum(counts[1:x])})

b$bar.index = sapply(1:length(majorcities),function(x){min(which(cumulative>=b$rank[x]))})
b$bar.index
rbind(majorcities,b[majorcities,'bar.index'])







# ------------------------------------ 5-fold ------------------------------------
scenario = expand.grid(k = c(5,10,25),
                       sample = c('cases'),
                       threshold_type = 'overall-mean'
)
threshold_type = scenario[1,'threshold_type']
sample = scenario[1,'sample']

s=2
threshold_type = scenario[s,'threshold_type']
sample = scenario[s,'sample']
k = scenario[s,'k']

a=t(sapply(1:nrow(dat.city),function(x){highrisk.size(k, mul1 = dist.rs.city_under40[x,'mu_under40'], 
                                                      sigma2l1 = dist.rs.city_under40[x,'var_under40'], 
                                                      mul2 = dist.rs.city_over40[x,'mu_over40'], 
                                                      sigma2l2 = dist.rs.city_over40[x,'var_over40'], 
                                                      mean.rs = mean.rs, wv.rs = wv.rs,
                                                      population = rs.city.info$population[x],
                                                      threshold_type, sample,
                                                      dat.city = dat.city,city=x)}))
colnames(a) = c('Proportion','N')

n.tail = 5
#hr = cbind(rs.city.info, dist.rs.city, a)
hr = cbind(rs.city.info, a)
sorted = sort(hr$Proportion,decreasing=T)
hr$group = NA
hr$rank = NA
if (T == T){
  for (i in 1:nrow(hr)){
    hr$rank[i] = which(sorted == hr$Proportion[i])
    if (hr$rank[i]<=n.tail) hr$group[i] = 'Highest Risk'#paste0(n.tail, ' Highest Risk')
    if ((hr$rank[i]>n.tail)&(hr$rank[i]<=nrow(hr)-n.tail)) hr$group[i] = 'Middle'
    if (hr$rank[i]>(nrow(hr)-n.tail)) hr$group[i] = 'Lowest Risk' #paste0(n.tail, ' Lowest Risk')
  }
}

majorcities = c('New York','Los Angeles','Chicago','Houston','Philadelphia','Austin',
                'Atlanta','San Francisco','Washington','Seattle','Boston','Detroit')

hr$lab = hr$PlaceName # paste(hr$PlaceName,'\n ',hr$rank)
majorcities.P = majorcities #sapply(1:length(majorcities),function(x){paste(majorcities[x],'\n ',hr[which(hr$PlaceName==majorcities[x]),'rank'])})

b = hr[hr$PlaceName %in% majorcities,]
rownames(b) = b$PlaceName
b[majorcities,]

counts = c(1,1,2,1,1,1,4,3,6,3,5,12,11,10,10,15,12,26,21,23,17,16,21,16,21,25,12,19,19,11,9,12,13,11,7,10,7,7,6,3,2,3,3,1,1,2)
sum(counts)
cumulative = sapply(1:length(counts),function(x){sum(counts[1:x])})

b$bar.index = sapply(1:length(majorcities),function(x){min(which(cumulative>=b$rank[x]))})
b$bar.index
rbind(majorcities,b[majorcities,'bar.index'])





# ------------------------------------ 10-fold ------------------------------------
scenario = expand.grid(k = c(5,10,25),
                       sample = c('cases'),
                       threshold_type = 'overall-mean'
)
threshold_type = scenario[1,'threshold_type']
sample = scenario[1,'sample']

s=3
threshold_type = scenario[s,'threshold_type']
sample = scenario[s,'sample']
k = scenario[s,'k']

a=t(sapply(1:nrow(dat.city),function(x){highrisk.size(k, mul1 = dist.rs.city_under40[x,'mu_under40'], 
                                                      sigma2l1 = dist.rs.city_under40[x,'var_under40'], 
                                                      mul2 = dist.rs.city_over40[x,'mu_over40'], 
                                                      sigma2l2 = dist.rs.city_over40[x,'var_over40'], 
                                                      mean.rs = mean.rs, wv.rs = wv.rs,
                                                      population = rs.city.info$population[x],
                                                      threshold_type, sample,
                                                      dat.city = dat.city,city=x)}))
colnames(a) = c('Proportion','N')

n.tail = 5
#hr = cbind(rs.city.info, dist.rs.city, a)
hr = cbind(rs.city.info, a)
sorted = sort(hr$Proportion,decreasing=T)
hr$group = NA
hr$rank = NA
if (T == T){
  for (i in 1:nrow(hr)){
    hr$rank[i] = which(sorted == hr$Proportion[i])
    if (hr$rank[i]<=n.tail) hr$group[i] = 'Highest Risk'#paste0(n.tail, ' Highest Risk')
    if ((hr$rank[i]>n.tail)&(hr$rank[i]<=nrow(hr)-n.tail)) hr$group[i] = 'Middle'
    if (hr$rank[i]>(nrow(hr)-n.tail)) hr$group[i] = 'Lowest Risk' #paste0(n.tail, ' Lowest Risk')
  }
}

majorcities = c('New York','Los Angeles','Chicago','Houston','Philadelphia','Austin',
                'Atlanta','San Francisco','Washington','Seattle','Boston','Detroit')

hr$lab = hr$PlaceName # paste(hr$PlaceName,'\n ',hr$rank)
majorcities.P = majorcities #sapply(1:length(majorcities),function(x){paste(majorcities[x],'\n ',hr[which(hr$PlaceName==majorcities[x]),'rank'])})

b = hr[hr$PlaceName %in% majorcities,]
rownames(b) = b$PlaceName
b[majorcities,]

counts = c(1,1,2,1,1,2,2,1,4,3,3,4,9,11,13,8,6,8,13,17,21,24,14,22,16,17,20,17,19,15,22,18,13,12,12,15,12,8,10,6,7,4,2,3,3)
sum(counts)
cumulative = sapply(1:length(counts),function(x){sum(counts[1:x])})

b$bar.index = sapply(1:length(majorcities),function(x){min(which(cumulative>=b$rank[x]))})
b$bar.index
rbind(majorcities,b[majorcities,'bar.index'])

