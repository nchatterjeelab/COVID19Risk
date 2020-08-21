library(openxlsx)
library(survey)
library(ggpubr)
library(gghighlight)
library(ggExtra)
library(ggplot2)
library(data.table)
library(gapminder)
library(tidyverse)
library(grid)
library(gridExtra)
library(ggrepel)
library(RColorBrewer)
title.size = 22
panel1.col = 'tomato4'
panel2.col = 'steelblue4'
panel3.col = 'darkgoldenrod2'
n.bins = 50

scenario = c(sample = c('all','cases'))
s=1
hr = read.xlsx(paste0('~/Dropbox/NHANES_risk_score/Nature/Supp Figures Tables/Supplementary Table S3.xlsx'))
hr = as.data.frame(hr)
k=5
hr$lab = hr$state
hr$N = hr$`High-risk.Proportion.among.all.(K=5)`
ks <- function (x) { x = round(x,2) }

#xaxis.display = seq(min(hr$N), max(hr$N),by=(max(hr$N)-min(hr$N))/4)
#xaxis.display = signif(xaxis.display,2)
#xaxis.display = signif(exp(xaxis.display),1)
#xaxis.display = log(xaxis.display)
#xaxis.display = round(seq(0.005, 0.11, by=(0.11-0.005)/4),3)
xaxis.breaks = seq(0,0.4,by=0.4/5)
xaxis.display = format(seq(0,0.4,by=0.4/5), nsmall=2) #round(seq(0.01, 0.585, by=(0.585-0.01)/4),3)
n.bins=51
p3 = ggplot(hr,aes(N,fill='Pale Green 4')) +
  geom_histogram(bins=n.bins,
                 color="white",alpha=0.9) +  
  scale_fill_manual(values=panel2.col) +
  theme(axis.text.x = element_text(angle = 0, size=12), axis.text.y = element_text(size=12)) + theme_bw() +
  labs(title='', Position="center", 
       x = paste0('Proportion of 65+ Medicare population exceeding the ',k,'-fold risk-threshold'), 
       y = "Number of States") +
  theme(plot.title = element_text(hjust = -0.07, face="bold", size = title.size),
        legend.position = "none",axis.text=element_text(size=13),
        axis.title=element_text(size=13.5),
        panel.grid = element_line(colour = "white")) + 
  scale_x_continuous(breaks = xaxis.breaks, 
                     labels = xaxis.display) + 
  scale_y_continuous(expand = c(0,0),limits = c(0,5)) + 
  removeGrid(x = FALSE, y = FALSE) 


p3.counts =  ggplot(hr,aes(N,fill='Pale Green 4')) +
  geom_histogram(bins=n.bins,
                 color="white",alpha=0.9) +  
  scale_fill_manual(values=panel2.col) +
  theme(axis.text.x = element_text(angle = 0, size=12), axis.text.y = element_text(size=12)) + theme_bw() +
  labs(title='', Position="center", 
       x = paste0('Proportion of 65+ Medicare population exceeding the ',k,'-fold risk-threshold'), 
       y = "Number of States") +
  theme(plot.title = element_text(hjust = -0.07, face="bold", size = title.size),
        legend.position = "none",axis.text=element_text(size=13),
        axis.title=element_text(size=13.5),
        panel.grid = element_line(colour = "white")) + 
  scale_x_continuous(breaks = xaxis.breaks, 
                     labels = xaxis.display) + 
  scale_y_continuous(expand = c(0,0),limits = c(0,5)) + 
  removeGrid(x = FALSE, y = FALSE) +
  stat_bin(bins=n.bins, geom='text', color='white', aes(label=..count..),
           position=position_stack(vjust = 0.5))





# -------------- Proportion among the general population v.s. among deths - city -------------
k=5
highrisk.state = read.xlsx(paste0('~/Dropbox/NHANES_risk_score/results_updated/tables/highrisk-medicare-among-all-state-average.xlsx'))
highrisk.state = as.data.frame(highrisk.state)

highrisk.state2 = read.xlsx(paste0('~/Dropbox/NHANES_risk_score/results_updated/tables/highrisk-medicare-among-cases-state-average.xlsx'))
highrisk.state2 = as.data.frame(highrisk.state2)

highrisk.state = highrisk.state[order(highrisk.state$state,decreasing = F),]
highrisk.state2 = highrisk.state2[order(highrisk.state2$state,decreasing = F),]

hrprop = merge(highrisk.state, highrisk.state2, by = 'state')
#colnames(hrprop)[which(colnames(hrprop) == paste0('Proportion.k=',k,'.x'))] = paste0('Proportion.',k,'.x')
#colnames(hrprop)[which(colnames(hrprop) == paste0('Proportion.k=',k,'.y'))] = paste0('Proportion.',k,'.y')

p5 = ggplot(hrprop,aes(x=proportion_k_5.x, y=proportion_k_5.y)) + 
  geom_point(fill=panel3.col,
             color=panel3.col, size = 1.3) + 
  labs(title="e", Position="center", 
       x = paste0('Proportion of 65+ Medicare population exceeding the ',k,'-fold risk-threshold'), 
       y = paste0('Estimated proportion of deaths\nexceeding the ',k,'-fold risk-threshold')) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, size=12), 
        axis.text.y = element_text(size=12),
        plot.title = element_text(hjust = -0.125, face="bold", size = title.size),
        legend.position = "none",axis.text=element_text(size=13,colour = "black"),
        axis.title=element_text(size=13.5),
        panel.grid = element_line(colour = "white")) +
  #scale_x_continuous(expand = c(0,0), limits = c(0,0.6), breaks = seq(0,0.6,0.6/4)[-1]) + 
  #scale_y_continuous(expand = c(0,0), limits = c(0,0.92), breaks = seq(0,0.9,0.9/6)[-1])
  scale_x_continuous(expand = c(0,0), limits = c(0,0.42)) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,0.82))


png(paste0('~/Dropbox/NHANES_risk_score/Nature/Extended Data/Extended Data Figure medicare state.png'),
    units='px', width=1800*6/4, height=1200*1.8, res=150*2,type = "cairo")
grid.arrange(p3, p5,
             layout_matrix = rbind(c(1),c(2)),
             nrow=2, ncol=1,
             widths = c(2.5), heights = c(1.3,1.3))
dev.off()




#p1
#ggsave("~/Dropbox/NHANES_risk_score/Nature/Figures_and_Tables/IER-city.tiff", dpi = 320, width = 50,
#       height = 20)


## add labels:
# -------------------- p1:
majorcities = c('New York','Los Angeles','Chicago','Philadelphia','Miami',
                'San Francisco','Baltimore','Boston','Detroit','Cleveland','Seattle','Houston')

majorcities.P = majorcities #sapply(1:length(majorcities),function(x){paste(majorcities[x],'\n ',hr[which(hr$PlaceName==majorcities[x]),'rank'])})

b = ier.city[ier.city$city %in% majorcities,]
rownames(b) = b$city
b[majorcities,]

counts = c(1,1,1,5,2,3,9,4,5,9,16,15,16,15,20,32,28,27,22,28,16,23,23,24,12,11,10,9,15,11,10,10,9,7,7,3,5,1,2,3,1,2,1,1,1,1)
sum(counts)
cumulative = sapply(1:length(counts),function(x){sum(counts[1:x])})

b$bar.index = sapply(1:length(majorcities),function(x){min(which(cumulative>=b$rank[x]))})
b$bar.index
rbind(majorcities,b[majorcities,'bar.index'])







# ----------------------------------------- 5-fold -----------------------------------------
hr = read.xlsx(paste0('~/Dropbox/NHANES_risk_score/results_updated/tables/highrisk_city_among_all.xlsx'))
hr = as.data.frame(hr)
k=5
hr$lab = hr$PlaceName
hr$N = hr[,paste0('Proportion.k=',k)]

hr$Proportion = hr$`Proportion.k=5`
sorted = sort(hr$Proportion,decreasing=T)
hr$group = NA
hr$rank = NA
for (i in 1:nrow(hr)){
  hr$rank[i] = which(sorted == hr$Proportion[i])
}

majorcities = c('New York','Los Angeles','Chicago','Philadelphia','Miami',
                'San Francisco','Baltimore','Boston','Detroit','Cleveland','Seattle','Houston')

majorcities.P = majorcities #sapply(1:length(majorcities),function(x){paste(majorcities[x],'\n ',hr[which(hr$PlaceName==majorcities[x]),'rank'])})

b = hr[hr$PlaceName %in% majorcities,]
rownames(b) = b$PlaceName
b[majorcities,]

counts = c(1,1,1,1,1,2,1,3,3,2,1,4,5,5,6,5,9,9,10,8,17,15,17,16,25,30,21,21,22,19,29,15,22,19,17,14,17,14,17,9,9,7,2,3,2)
sum(counts)
cumulative = sapply(1:length(counts),function(x){sum(counts[1:x])})

b$bar.index = sapply(1:length(majorcities),function(x){min(which(cumulative>=b$rank[x]))})
b$bar.index
rbind(majorcities,b[majorcities,'bar.index'])






# ----------------------------- Size
hr = cbind(rs.city.info, a)
sorted = sort(hr$N,decreasing=T)
hr$group = NA
for (i in 1:nrow(hr)){
  hr$rank[i] = which(sorted == hr$N[i])
  if (hr$rank[i]<=n.tail) hr$group[i] = 'Highest Risk'#paste0(n.tail, ' Highest Risk')
  if ((hr$rank[i]>n.tail)&(hr$rank[i]<=nrow(hr)-n.tail)) hr$group[i] = 'Middle'
  if (hr$rank[i]>(nrow(hr)-n.tail)) hr$group[i] = 'Lowest Risk' #paste0(n.tail, ' Lowest Risk')
}
majorcities = c('New York','Los Angeles','Chicago','Houston','Philadelphia','Austin',
                'Atlanta','San Francisco','Washington','Seattle','Boston','Detroit')

hr$lab = hr$PlaceName # paste(hr$PlaceName,'\n ',hr$rank)
majorcities.N = majorcities # sapply(1:length(majorcities),function(x){paste(majorcities[x],'\n ',hr[which(hr$PlaceName==majorcities[x]),'rank'])})

b = hr[hr$PlaceName %in% majorcities,]
rownames(b) = b$PlaceName
b[majorcities,]

counts = c(1,1,1,1,1,1,2,1,2,3,2,7,6,8,3,4,6,6,10,16,12,17,16,24,27,35,37,39,36,30,28,21,8,10,7,3,4,2,3,1)
sum(counts)
cumulative = sapply(1:length(counts),function(x){sum(counts[1:x])})

b$bar.index = sapply(1:length(majorcities),function(x){min(which(cumulative>=b$rank[x]))})
rbind(majorcities,b[majorcities,'bar.index'])



