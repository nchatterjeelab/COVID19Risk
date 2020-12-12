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

s=1
hr = read.xlsx(paste0('data_created/CI/medicare_state_results.xlsx'))
hr = as.data.frame(hr)
k=5
hr$lab = hr$state
hr$N = hr$`HighRisk-Proportion-among-all(k=5)`
ks <- function (x) { x = round(x,2) }

xaxis.breaks = seq(0,0.4,by=0.4/5)
xaxis.display = format(seq(0,0.4,by=0.4/5), nsmall=2)
n.bins=51
p3 = ggplot(hr,aes(N,fill='Pale Green 4')) +
  geom_histogram(bins=n.bins,
                 color="white",alpha=0.9) +  
  scale_fill_manual(values=panel2.col) +
  theme(axis.text.x = element_text(angle = 0, size=12), axis.text.y = element_text(size=12)) + theme_bw() +
  labs(title='a', Position="center", 
       x = paste0('Proportion of 65+ Medicare population exceeding the ',k,'-fold risk-threshold'), 
       y = "Number of States") +
  theme(plot.title = element_text(hjust = -0.044, face="bold", size = title.size),
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
  labs(title='a', Position="center", 
       x = paste0('Proportion of 65+ Medicare population exceeding the ',k,'-fold risk-threshold'), 
       y = "Number of States") +
  theme(plot.title = element_text(hjust = 0, face="bold", size = title.size),
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
highrisk.state = read.xlsx(paste0('data_created/CI/medicare_state_results.xlsx'))
highrisk.state = as.data.frame(highrisk.state)


p5 = ggplot(highrisk.state,aes(x=`HighRisk-Proportion-among-all(k=5)`, y=`HighRisk-Proportion-among-deaths(k=5)`)) + 
  geom_point(fill=panel3.col,
             color=panel3.col, size = 1.3) + 
  labs(title="b", Position="center", 
       x = paste0('Proportion of 65+ Medicare population exceeding the ',k,'-fold risk-threshold'), 
       y = paste0('Estimated proportion of deaths\nexceeding the ',k,'-fold risk-threshold')) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, size=12), 
        axis.text.y = element_text(size=12),
        plot.title = element_text(hjust = -0.09, face="bold", size = title.size),
        legend.position = "none",axis.text=element_text(size=13,colour = "black"),
        axis.title=element_text(size=13.5),
        panel.grid = element_line(colour = "white")) +
  scale_x_continuous(expand = c(0,0), limits = c(0,0.45)) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,0.82))


png(paste0('figures_and_tables/output/Extended Data Figure medicare state.png'),
    units='px', width=1800*6/4, height=1200*1.8, res=150*2,type = "cairo")
grid.arrange(p3, p5,
             layout_matrix = rbind(c(1),c(2)),
             nrow=2, ncol=1,
             widths = c(2.5), heights = c(1.3,1.3))
dev.off()