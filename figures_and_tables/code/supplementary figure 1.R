library(dplyr)
library(openxlsx)
library(data.table)
library(ggplot2)
library(scales)
library(ggpubr)
library(ggsci)
library("scales")
#install.packages('https://cran.r-project.org/src/contrib/Archive/SDMTools/SDMTools_1.1-221.2.tar.gz', repos = NULL, type="source")
library(SDMTools)

display.brewer.pal(n = 8, name = 'BrBG')
display.brewer.pal(n = 8, name = 'Blues')
display.brewer.pal(n = 8, name = 'PuBu')

brewer.pal(n = 8, name = 'BrBG')[7]

#### mean risk without SDI:
rs = readRDS("data_created/individual_rs_covariates.rds")
###### Rl = 7.991241 # mean risk with SDI
Rl = sum(exp(rs$rs_est) * rs$sampling_weights)/sum(rs$sampling_weights)


rs$rs_est = rs$rs_est - log(Rl)
thr2=log(2) 
thr3=log(3) 
thr5=log(5) 
thr10=log(10) 
thr25=log(25)

ks <- function (x) { x = round(x,1) }
rs1 = rs[rs$age<40,]
rs2 = rs[rs$age>=40,]
# --------------------- weighted
mu1 = wt.mean(rs1$rs_est, rs1$sampling_weights)
va1 = wt.var(rs1$rs_est, rs1$sampling_weights)
mu2 = wt.mean(rs2$rs_est, rs2$sampling_weights)
va2 = wt.var(rs2$rs_est, rs2$sampling_weights)
wt.mean((rs1$rs_est)^2, rs1$sampling_weights) - (wt.mean(rs1$rs_est, rs1$sampling_weights))^2
mu = wt.mean(rs$rs_est, rs$sampling_weights); va = wt.var(rs$rs_est, rs$sampling_weights)
w1 = sum(ifelse(rs$age<40,1,0) * rs$sampling_weights)/sum(rs$sampling_weights) # 

n.sample =  nrow(rs)
normal.density = rnorm(n.sample, mu, sqrt(va))
mn.density1 = rnorm(round(n.sample*w1), mu1, sqrt(va1))
mn.density2 = rnorm(n.sample-round(n.sample*w1), mu2, sqrt(va2))
mn.density = c(mn.density1,mn.density2)

### risk with weights whole population
png('figures_and_tables/output/histogram_individuial.png',
    units='px', width=1500, height=1500*0.4, res=150,type = "cairo")
ggplot(rs) +
  geom_histogram(color="black",alpha=0.9,fill='Ivory 4', bins = 50, 
                 aes(rs_est,weight=sampling_weights/mean(sampling_weights),y = ..density..)) +
  #aes(rs_est,weight=sampling_weights)) +
  theme(axis.text.x = element_text(angle = 0, size=12, face="bold"), 
        axis.text.y = element_text(size=10, face="bold")) + 
  theme_bw() +
  labs(title="", Position="center", x = "COVID-19 risk score", y = "") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position = "none",axis.text=element_text(size=12),
        axis.title=element_text(size=12))+ # ,face="bold"
  scale_x_continuous(labels = ks,breaks = seq(min(rs$rs_est)-0.1,max(rs$rs_est)+0.1,length.out=4))+
  scale_y_continuous(expand = c(0,0)) + 
  geom_density(aes(x = mn.density, color = 'orangered4'),alpha = .5, color = 'orangered4',lwd=0.8) + # #4A6990FF   fill = "antiquewhite3" # CD534CFF
  ylim(0, 0.41) +
  geom_vline(xintercept = thr2, 
             color = "grey40", lty = 'dashed', show.legend = F) + 
  annotate(geom = "text", size=3.7,
           label = paste0('2-fold'), 
           x = thr2 +0.4, y = 0.2, color = "grey30") + 
  geom_vline(xintercept = thr3, 
             color = "grey40", lty = 'dashed', show.legend = F) + 
  annotate(geom = "text", size=3.7,
           label = paste0('3-fold'), 
           x = thr3 +0.4, y = 0.22, color = "grey30") + 
  geom_vline(xintercept = thr5, 
             color = "grey40", lty = 'dashed', show.legend = F) + 
  annotate(geom = "text", size=3.7,
           label = paste0('5-fold'), 
           x = thr5 + 0.4, y = 0.25, color = "grey30") + 
  geom_vline(xintercept = thr10, 
             color = "grey40", lty = 'dashed', show.legend = F) + 
  annotate(geom = "text", size=3.7, 
           label = paste0('10-fold'), 
           x = thr10 + 0.5, y = 0.28, color = "grey30") + 
  geom_vline(xintercept = thr25, 
             color = "grey40", lty = 'dashed', show.legend = F) + 
  annotate(geom = "text", size=3.7,
           label = paste0('25-fold'), 
           x = thr25 + 0.6, y = 0.31, color = "grey30") + 
  xlim(-12,5.5)
dev.off()


#### table:
# ----------------------------- mean risk + weight
# table

rs = readRDS('~/Dropbox/NHANES_risk_score/500cities_data/Analysis/data_created/individual_rs_covariates.rds')
Rl = sum(exp(rs$rs_est) * rs$sampling_weights)/sum(rs$sampling_weights)
rs$rs_est = rs$rs_est - log(Rl)
rs1 = rs[rs$age<40,]
rs2 = rs[rs$age>=40,]

RS = lapply(1:nrow(rs),function(x){rep(rs$rs_est[x],rs$sampling_weights[x])})
RS = unlist(RS)
# mixture normal:
mu1 = wt.mean(rs1$rs_est, rs1$sampling_weights)
va1 = wt.var(rs1$rs_est, rs1$sampling_weights)
mu2 = wt.mean(rs2$rs_est, rs2$sampling_weights)
va2 = wt.var(rs2$rs_est, rs2$sampling_weights)
mu = wt.mean(rs$rs_est, rs$sampling_weights); va = wt.var(rs$rs_est, rs$sampling_weights)
w1 = sum(ifelse(rs$age<40,1,0) * rs$sampling_weights)/sum(rs$sampling_weights) # 

res = matrix(NA,5,2)
colnames(res) = c('empirical','mixture normal')
rownames(res) = paste0(c(2,3,5,10,25),'-fold')
k=2
res[1,1] = mean(RS>log(k)) 
res[1,2] = w1 * pnorm(log(k),mu1,sqrt(va1),lower.tail = F) + (1-w1) * pnorm(log(k),mu2,sqrt(va2),lower.tail = F)
k=3
res[2,1] = mean(RS>log(k)) 
res[2,2] = w1 * pnorm(log(k),mu1,sqrt(va1),lower.tail = F) + (1-w1) * pnorm(log(k),mu2,sqrt(va2),lower.tail = F)
k=5
res[3,1] = mean(RS>log(k)) 
res[3,2] = w1 * pnorm(log(k),mu1,sqrt(va1),lower.tail = F) + (1-w1) * pnorm(log(k),mu2,sqrt(va2),lower.tail = F)
k=10
res[4,1] = mean(RS>log(k)) 
res[4,2] = w1 * pnorm(log(k),mu1,sqrt(va1),lower.tail = F) + (1-w1) * pnorm(log(k),mu2,sqrt(va2),lower.tail = F)
k=25
res[5,1] = mean(RS>log(k)) 
res[5,2] = w1 * pnorm(log(k),mu1,sqrt(va1),lower.tail = F) + (1-w1) * pnorm(log(k),mu2,sqrt(va2),lower.tail = F)

res.weighted=res
res.weighted
