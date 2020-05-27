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
subfig.lab1 = c('A.','B.','C.')
subfig.lab2 = c('D.','E.','F.')
# -------------------------- all population
##### make histograms with labels
scenario = expand.grid(k = c(2,5,10),
                       sample = c('all'),
                       threshold_type = 'overall-mean' #c('overall-median','overall-mean','citywise_median')
)
#if (s%%3 == 1){
threshold_type = scenario[1,'threshold_type']
sample = scenario[1,'sample']
p1 = p2 = list()

png(paste0('~/Dropbox/NHANES_risk_score/results/highrisk_',threshold_type,'/',approxtype,'_',sample,'_',threshold_type,'_draft.png'),
    units='px', width=1800*5, height=1800*2.5, res=170*3,type = "cairo")
#}
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
  
  p1[[s]] = ggplot(hr,aes(Proportion,fill=brewer.pal(n = 8, name = "RdYlBu")[2],color=brewer.pal(n = 8, name = "RdYlBu")[2])) + 
    geom_histogram(bins=n.bins,
                   color="black",alpha=0.9) + #bins=50,binwidth = ((max(hr$Proportion)-min(hr$Proportion))/49)
    #geom_vline(aes(xintercept = median(rs$rs.std)),col='blue',size=0.8) + 
    scale_fill_manual(values=brewer.pal(n = 8, name = "YlOrRd")[4]) +
    #theme(axis.text.x = element_text(angle = 0, size=12, face="bold"), axis.text.y = element_text(size=12, face="bold")) + theme_bw() +
    theme(axis.text.x = element_text(angle = 0, size=12), axis.text.y = element_text(size=12)) + theme_bw() +
    labs(title="", Position="center", x = paste0('Proportion of population with risk higher than ',k,'-fold of average risk'), y = "Number of cities") +
    theme(plot.title = element_text(hjust = 0.5)) + 
    theme(legend.position = "none",axis.text=element_text(size=16),
          axis.title=element_text(size=14))+ # ,face="bold"
    #scale_x_continuous(labels = ks,breaks = seq(-1.32,0.72,length.out=5)) + 
    stat_bin(bins=n.bins, geom='text', color='white', aes(label=..count..),
             position=position_stack(vjust = 0.5))
  
  #### high risk size:
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

  hr$N = log(hr$N)
  ks <- function (x) { x = round(exp(x),2) }
  
  xaxis.display = seq(min(hr$N), max(hr$N),by=(max(hr$N)-min(hr$N))/4)
  xaxis.display = signif(exp(xaxis.display),1) #round(exp(xaxis.display))
  xaxis.display = log(xaxis.display)
  
  p2[[s]] = ggplot(hr,aes(N,fill= 'darkseagreen3')) + # brewer.pal(n = 8, name = "RdYlBu")[2])) + 
    geom_histogram(bins=n.bins,
                   color="black",alpha=0.9) + #bins=50,binwidth = ((max(hr$Proportion)-min(hr$Proportion))/49)
    #geom_vline(aes(xintercept = median(rs$rs.std)),col='blue',size=0.8) + 
    scale_fill_manual(values='darkseagreen3') +
    #theme(axis.text.x = element_text(angle = 0, size=12, face="bold"), axis.text.y = element_text(size=12, face="bold")) + theme_bw() +
    theme(axis.text.x = element_text(angle = 0, size=12), axis.text.y = element_text(size=12)) + theme_bw() +
    labs(title="", Position="center", x = paste0('Size of population with risk higher than ',k,'-fold of average risk'), y = "Number of cities") +
    theme(plot.title = element_text(hjust = 0.5)) + 
    theme(legend.position = "none",axis.text=element_text(size=16),
          axis.title=element_text(size=14))+ # ,face="bold"
    stat_bin(bins=n.bins, geom='text', color='white', aes(label=..count..),
             position=position_stack(vjust = 0.5)) + 
    scale_x_continuous(breaks = xaxis.display, 
                       labels = signif(exp(xaxis.display)))
  
  print(s)
}
grid.arrange(p1[[1]],p2[[1]],
             p1[[2]],p2[[2]],
             p1[[3]],p2[[3]],
             layout_matrix = rbind(c(1,2),c(3,4),c(5,6)),
             nrow=3, ncol=2,
             widths = c(6,6), heights = c(2,2,2))
dev.off()








n.bin = 50
subfig.lab1 = c('A.','B.','C.')
subfig.lab2 = c('D.','E.','F.')
######### the complete histogram without labels
# -------------------------- all population
##### make histograms with labels
scenario = expand.grid(k = c(2,5,10),
                       sample = c('all'),
                       threshold_type = 'overall-mean' #c('overall-median','overall-mean','citywise_median')
)
threshold_type = scenario[1,'threshold_type']
sample = scenario[1,'sample']
p1 = p2 = list()

png(paste0('~/Dropbox/NHANES_risk_score/results/highrisk_',threshold_type,'/fig2_nolabels.png'),
    units='px', width=1800*5, height=1800*2.5, res=170*3,type = "cairo")
#}
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
  
  p1[[s]] = ggplot(hr,aes(Proportion,fill=brewer.pal(n = 8, name = "Set2")[3],color=brewer.pal(n = 8, name = "Set2")[3])) + 
    geom_histogram(bins=n.bins,
                   color="black",alpha=0.9) + #bins=50,binwidth = ((max(hr$Proportion)-min(hr$Proportion))/49)
    #geom_vline(aes(xintercept = median(rs$rs.std)),col='blue',size=0.8) + 
    scale_fill_manual(values=brewer.pal(n = 8, name = "Set2")[3]) +
    #theme(axis.text.x = element_text(angle = 0, size=12, face="bold"), axis.text.y = element_text(size=12, face="bold")) + theme_bw() +
    theme(axis.text.x = element_text(angle = 0, size=12), axis.text.y = element_text(size=12)) + theme_bw() +
    labs(title=subfig.lab1[s], Position="center", x = paste0('Proportion of population exceeding the ',k,'-fold risk-threshold'), y = "Number of cities") +
    theme(plot.title = element_text(hjust = 0, face="bold")) + 
    theme(legend.position = "none",axis.text=element_text(size=13),
          axis.title=element_text(size=13.5))
  
  #  p1[[s]] = p1[[s]] +
  #    geom_label_repel(data = subset(hr, lab %in% majorcities.P),
  #                     nudge_y = -0.1,  box.padding   = 0.35, 
  #                     point.padding = 0.2,
  #                     segment.color = 'grey50',
  #                     direction="x")
  #### high risk size:
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
  
  hr$N = log(hr$N)
  ks <- function (x) { x = round(exp(x),2) }
  
  xaxis.display = seq(min(hr$N), max(hr$N),by=(max(hr$N)-min(hr$N))/4)
  xaxis.display = signif(exp(xaxis.display),1) #round(exp(xaxis.display))
  xaxis.display = log(xaxis.display)
  
  p2[[s]] = ggplot(hr,aes(N,fill='Pale Green 4')) +   # 'darkseagreen3' brewer.pal(n = 8, name = "BrBG")[7] Dark Sea Green
    geom_histogram(bins=n.bins,
                   color="black",alpha=0.9) + #bins=50,binwidth = ((max(hr$Proportion)-min(hr$Proportion))/49)
    #geom_vline(aes(xintercept = median(rs$rs.std)),col='blue',size=0.8) + 
    scale_fill_manual(values='Pale Green 4') +
    #theme(axis.text.x = element_text(angle = 0, size=12, face="bold"), axis.text.y = element_text(size=12, face="bold")) + theme_bw() +
    theme(axis.text.x = element_text(angle = 0, size=12), axis.text.y = element_text(size=12)) + theme_bw() +
    labs(title=subfig.lab2[s], Position="center", x = paste0('Size of population exceeding the ',k,'-fold risk-threshold'), y = "Number of cities") +
    theme(plot.title = element_text(hjust = 0, face="bold")) + 
    theme(legend.position = "none",axis.text=element_text(size=13),
          axis.title=element_text(size=13.5)) + 
    scale_x_continuous(breaks = xaxis.display, 
                       labels = signif(exp(xaxis.display)))
  
  print(s)
}
grid.arrange(p1[[1]],p2[[1]],
             p1[[2]],p2[[2]],
             p1[[3]],p2[[3]],
             layout_matrix = rbind(c(1,2),c(3,4),c(5,6)),
             nrow=3, ncol=2,
             widths = c(6,6), heights = c(2,2,2))
dev.off()







# ------------------------------------ add city names: ------------------------------------
# -------------------------- all population
##### make histograms with labels
scenario = expand.grid(k = c(2,5,10),
                       sample = c('all'),
                       threshold_type = 'overall-mean' #c('overall-median','overall-mean','citywise_median')
)
#if (s%%3 == 1){
threshold_type = scenario[1,'threshold_type']
sample = scenario[1,'sample']

fig1=fig2=list()

# ----------------------------------------- 2-fold -----------------------------------------
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

counts = c(1,1,1,2,3,3,5,7,4,7,7,11,12,11,16,16,23,22,23,17,22,15,24,17,20,14,22,11,10,16,11,7,11,12,15,4,5,4,4,3,2,1)
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




# ----------------------------------------- 5-fold -----------------------------------------
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

counts = c(1,1,1,1,3,1,1,3,4,2,5,5,7,7,10,13,7,8,11,15,20,21,28,21,12,19,19,18,23,20,22,13,17,9,17,15,11,14,6,5,3,3)
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








# ----------------------------------------- 10-fold -----------------------------------------
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

counts = c(1,1,1,2,2,1,1,2,4,2,4,3,5,7,8,14,3,7,11,8,10,21,20,21,25,17,20,18,18,21,23,30,14,17,18,21,14,15,7,5)
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

counts = c(1,1,1,2,2,3,5,1,2,9,6,8,8,10,7,18,19,26,21,25,38,36,33,31,23,20,26,15,9,9,6,5,4,4,2,1,2,1,1,1)
sum(counts)
cumulative = sapply(1:length(counts),function(x){sum(counts[1:x])})

b$bar.index = sapply(1:length(majorcities),function(x){min(which(cumulative>=b$rank[x]))})
rbind(majorcities,b[majorcities,'bar.index'])









