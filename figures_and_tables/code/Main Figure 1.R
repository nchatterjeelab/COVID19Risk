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

# --------------------------- IER by city ---------------------------
ier.city = read.xlsx(paste0('data_created/IER-city.xlsx'))
ier.city$log.IER = log(ier.city$IER)
ks <- function (x) { x = round(exp(x),2) }

p1 = ggplot(ier.city,aes(log.IER)) + 
  geom_histogram(bins=n.bins, fill=panel1.col, 
                 color="white",size=0.5, alpha = 0.9) +
  labs(title="a", #subtitle = 'The general adult (18+) population in 477 US cities',
       Position="center", x = "IER", y = "Number of cities") +
  geom_vline(aes(xintercept=0),col='black',size=1,linetype = "dashed") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, size=12), 
        axis.text.y = element_text(size=12),
        plot.title = element_text(hjust = -0.07, size = title.size, face="bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 15, face="bold"), 
        axis.text=element_text(size=13,colour="black"),
        axis.title=element_text(size=13),
        panel.grid = element_line(colour = "white"))+
  scale_x_continuous(labels = ks,breaks = log(c(0.25,0.5,0.75,1.0,1.5,2.5)))+
  scale_y_continuous(expand = c(0,0), breaks = seq(0,30,by=6), limits=c(0,34)) + 
  removeGrid(x = FALSE, y = FALSE) 



# --------------------------- IER medicare ---------------------------
ier.medicare = read.xlsx(paste0('data_created/IER-medicare.xlsx'))
ier.medicare$log.IER = log(ier.medicare$IER)
ks <- function (x) { x = round(exp(x),2) }
p2 = ggplot(ier.medicare,aes(log.IER)) + 
  geom_histogram(bins=n.bins,fill=panel1.col,
                 color="white",size=0.5, alpha = 0.9) +
  labs(title="b", #subtitle = 'The 65+ year old Medicare population in 3113 US counties',
       Position="center", x = "IER", y = "Number of Counties") +
  geom_vline(aes(xintercept=0),col='black',size=1,linetype = "dashed") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, size=12), 
        axis.text.y = element_text(size=12),
        plot.title = element_text(hjust = -0.08, size = title.size, face="bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 15, face="bold"), 
        axis.text=element_text(size=13,colour="black"),
        axis.title=element_text(size=13),
        panel.grid = element_line(colour = "white"))+
  scale_x_continuous(labels = ks,breaks = log(c(1,1.5,2.5,4,6,10)))+
  scale_y_continuous(expand = c(0,0), breaks = seq(0,200,by=50), limits=c(0,220)) + 
  removeGrid(x = FALSE, y = FALSE) 



# --------------------------- Proportion among the general population by city ---------------------------
scenario = c(sample = c('all','deaths'))
s=1
hr = read.xlsx(paste0('data_created/highrisk_city_among_',scenario[s],'.xlsx'))
hr = as.data.frame(hr)
k=5
hr$lab = hr$PlaceName

hr$N = hr[,paste0('Proportion.k=',k)]
ks <- function (x) { x = round(x,2) }
xaxis.breaks = c(0.01, 0.03, 0.05, 0.07, 0.09, 0.11)
xaxis.display = format(c(0.01, 0.03, 0.05, 0.07, 0.09, 0.11), nsmall=2)

p3 = ggplot(hr,aes(N,fill='Pale Green 4')) +
  geom_histogram(bins=n.bins,
                 color="white",alpha=0.9) +  
  scale_fill_manual(values=panel2.col) +
  theme(axis.text.x = element_text(angle = 0, size=12), axis.text.y = element_text(size=12)) + theme_bw() +
  labs(title='c', Position="center", 
       x = paste0('Proportion of the adult population exceeding the ',k,'-fold risk-threshold'), 
       y = "Number of cities") +
  theme(plot.title = element_text(hjust = -0.07, face="bold", size = title.size),
        legend.position = "none",axis.text=element_text(size=13),
        axis.title=element_text(size=13.5),
        panel.grid = element_line(colour = "white")) + 
  scale_x_continuous(breaks = xaxis.breaks, 
                     labels = xaxis.display) + 
  scale_y_continuous(expand = c(0,0),breaks = seq(0,30,6), limits = c(0,34)) + 
  removeGrid(x = FALSE, y = FALSE) 

  
# --------------------------- Proportion among the general population - medicare ---------------------------
s=1
hr = read.xlsx(paste0('data_created/highrisk_medicare_among_',scenario[s],'.xlsx'))
hr = as.data.frame(hr)
k=5
hr$lab = hr$PlaceName

hr$N = hr[,paste0('Proportion.k=',k)]
ks <- function (x) { x = round(x,2) }

xaxis.breaks = c(0.01, 0.12, 0.24, 0.36, 0.48, 0.6)
xaxis.display = format(c(0.01, 0.12, 0.24, 0.36, 0.48, 0.6), nsmall=2)

p4 = ggplot(hr,aes(N,fill='Pale Green 4')) +
  geom_histogram(bins=n.bins,
                 color="white",alpha=0.9) +  
  scale_fill_manual(values=panel2.col) +
  theme(axis.text.x = element_text(angle = 0, size=12), axis.text.y = element_text(size=12)) + theme_bw() +
  labs(title='d', Position="center", 
       x = paste0('Proportion of the 65+ Medicare population exceeding the ',k,'-fold risk-threshold'), 
       y = "Number of counties") +
  theme(plot.title = element_text(hjust = -0.085, face="bold", size = title.size),
        legend.position = "none",axis.text=element_text(size=13),
        axis.title=element_text(size=13.5),
        panel.grid = element_line(colour = "white")) + 
  scale_x_continuous(breaks = xaxis.breaks, 
                     labels = xaxis.display) + 
  scale_y_continuous(expand = c(0,0), breaks=seq(0,270,50), limits = c(0,270)) + 
  removeGrid(x = FALSE, y = FALSE) 




# -------------- Proportion among the general population v.s. among deths - city -------------
highrisk.state = read.xlsx(paste0('data_created/highrisk_city_among_',scenario[1],'.xlsx'))
highrisk.state = as.data.frame(highrisk.state)

highrisk.state2 = read.xlsx(paste0('data_created/highrisk_city_among_',scenario[2],'.xlsx'))
highrisk.state2 = as.data.frame(highrisk.state2)

highrisk.state = highrisk.state[order(highrisk.state$PlaceFIPS,decreasing = F),]
highrisk.state2 = highrisk.state2[order(highrisk.state2$PlaceFIPS,decreasing = F),]

highrisk.state2 = highrisk.state2[,-c(1,2,4)]
hrprop = merge(highrisk.state, highrisk.state2, by = 'PlaceFIPS')
colnames(hrprop)[which(colnames(hrprop) == paste0('Proportion.k=',k,'.x'))] = paste0('Proportion.',k,'.x')
colnames(hrprop)[which(colnames(hrprop) == paste0('Proportion.k=',k,'.y'))] = paste0('Proportion.',k,'.y')

p5 = ggplot(hrprop,aes(x=Proportion.5.x, y=Proportion.5.y)) + 
    geom_point(fill=panel3.col,
               color=panel3.col, size = 0.5) + 
  labs(title="e", Position="center", 
       x = paste0('Proportion of the adult population exceeding the ',k,'-fold risk-threshold'), 
       y = paste0('Estimated proportion of deaths\nexceeding the ',k,'-fold risk-threshold')) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, size=12), 
        axis.text.y = element_text(size=12),
        plot.title = element_text(hjust = -0.125, face="bold", size = title.size),
        legend.position = "none",axis.text=element_text(size=13,colour = "black"),
        axis.title=element_text(size=13.5),
        panel.grid = element_line(colour = "white")) +
  scale_x_continuous(expand = c(0,0), limits = c(0,0.115), breaks = seq(0,0.1,0.1/5)[-1]) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,0.8), breaks = seq(0,0.8,0.8/5))



# -------------- Proportion among the general population v.s. among deths - medicare -------------
highrisk.medicare = read.xlsx(paste0('data_created/highrisk_medicare_among_',scenario[1],'.xlsx'))
highrisk.medicare = as.data.frame(highrisk.medicare)

highrisk.medicare2 = read.xlsx(paste0('data_created/highrisk_medicare_among_',scenario[2],'.xlsx'))
highrisk.medicare2 = as.data.frame(highrisk.medicare2)

highrisk.medicare = highrisk.medicare[order(highrisk.medicare$fips,decreasing = F),]
highrisk.medicare2 = highrisk.medicare2[order(highrisk.medicare2$fips,decreasing = F),]

highrisk.medicare2 = highrisk.medicare2[,-c(1,2,4)]
hrprop = merge(highrisk.medicare, highrisk.medicare2, by = 'fips')
colnames(hrprop)[which(colnames(hrprop) == paste0('Proportion.k=',k,'.x'))] = paste0('Proportion.',k,'.x')
colnames(hrprop)[which(colnames(hrprop) == paste0('Proportion.k=',k,'.y'))] = paste0('Proportion.',k,'.y')

k=5

p6 = ggplot(hrprop,aes(x=Proportion.5.x, y=Proportion.5.y)) + 
  geom_point(fill=panel3.col,
             color=panel3.col, size = 0.2) + 
  labs(title="f", Position="center", 
       x = paste0('Proportion of the 65+ Medicare population exceeding the ',k,'-fold risk-threshold'), 
       y = paste0('Estimated proportion of deaths\nexceeding the ',k,'-fold risk-threshold')) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, size=12), 
        axis.text.y = element_text(size=12),
        plot.title = element_text(hjust = -0.12, face="bold", size = title.size),
        legend.position = "none",axis.text=element_text(size=13,colour = "black"),
        axis.title=element_text(size=13.5),
        panel.grid = element_line(colour = "white")) +
  scale_x_continuous(expand = c(0,0), limits = c(0,0.61), breaks = seq(0,0.6,0.6/4)[-1]) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,0.92), breaks = seq(0,0.9,0.9/5))


png(paste0('figures_and_tables/output/Figure 1.png'),
    units='px', width=1800*7, height=1200*8, res=140*5.83,type = "cairo")
grid.arrange(p1, p2, p3, p4, p5, p6,
             layout_matrix = rbind(c(1,2),c(3,4),c(5,6)),
             nrow=3, ncol=2,
             widths = c(2.5,2.5), heights = c(1.3,1.3,1.3))
dev.off()
