# Validate the mixture normal/normal approximation using NHIS data
library(dplyr)
library(openxlsx)
library(data.table)
library(ggplot2)
library(scales)
library(ggpubr)
library(ggsci)
library(ggExtra)
library("scales")
#install.packages('https://cran.r-project.org/src/contrib/Archive/SDMTools/SDMTools_1.1-221.2.tar.gz', repos = NULL, type="source")
library(SDMTools)
ks <- function (x) { x = round(x,1) }

# --------------------- Mixture normal approximation for the general US population (18+) ---------------------
#### mean risk excluding SDI:
rs = readRDS("data_created/individual_rs_covariates.rds")
Rl = sum(exp(rs$rs_est) * rs$sampling_weights)/sum(rs$sampling_weights)
###### Rl = 4.172392
rs$rs_est = rs$rs_est - log(Rl)
thr1.2=log(1.2) 
thr2=log(2) 
thr5=log(5) 
thr10=log(10) 

rs1 = rs[rs$age<45,]
rs2 = rs[(rs$age>=45)&(rs$age<75),]
rs3 = rs[rs$age>=75,]
# --------------------- weighted
mu1 = wt.mean(rs1$rs_est, rs1$sampling_weights)
va1 = wt.var(rs1$rs_est, rs1$sampling_weights)
mu2 = wt.mean(rs2$rs_est, rs2$sampling_weights)
va2 = wt.var(rs2$rs_est, rs2$sampling_weights)
mu3 = wt.mean(rs3$rs_est, rs3$sampling_weights)
va3 = wt.var(rs3$rs_est, rs3$sampling_weights)
mu = wt.mean(rs$rs_est, rs$sampling_weights); va = wt.var(rs$rs_est, rs$sampling_weights)
w1 = sum(ifelse(rs$age<45,1,0) * rs$sampling_weights)/sum(rs$sampling_weights) # 
w3 = sum(ifelse(rs$age>=75,1,0) * rs$sampling_weights)/sum(rs$sampling_weights) # 
w2 = 1 - w1 - w3

n.sample =  nrow(rs)
normal.density = rnorm(n.sample, mu, sqrt(va))
mn.density1 = rnorm(round(n.sample*w1), mu1, sqrt(va1))
mn.density2 = rnorm(round(n.sample*w2), mu2, sqrt(va2))
mn.density3 = rnorm(n.sample-length(mn.density1)-length(mn.density2), mu3, sqrt(va3))
mn.density = c(mn.density1,mn.density2,mn.density3)

png('figures_and_tables/output/NHIS_mixture_normal_approximation.png',
    units='px', width=1500, height=1500*0.4, res=150,type = "cairo")
ggplot(rs) +
  geom_histogram(color="black",alpha=0.9,fill='Ivory 4', bins = 40, 
                 aes(rs_est,weight=sampling_weights/mean(sampling_weights),y = ..density..)) +
  geom_density(aes(x = mn.density, color = 'orangered4'),alpha = .9, 
               color = 'orangered4',lwd=0.8, trim = T) + 
  theme(axis.text.x = element_text(angle = 0, size=12, face="bold"), 
        axis.text.y = element_text(size=10, face="bold")) + 
  theme_bw() +
  labs(title="a", Position="center", x = "COVID-19 risk score of the NHIS population", y = "") +
  theme(legend.position = "none",axis.text=element_text(size=12),
        plot.title = element_text(hjust = -0.05, size = 20, face="bold"),
        axis.title=element_text(size=12),
        panel.grid = element_line(colour = "white")) +
  scale_x_continuous(labels = ks,breaks = c(-4.8,-2,0,2,4.5), 
                     expand = c(0,0))+
  scale_y_continuous(expand = c(0,0), limits = c(0,0.35)) + 
  geom_vline(xintercept = thr1.2, 
             color = "grey40", lty = 'dashed', show.legend = F) + 
  annotate(geom = "text", size=3.7,
           label = paste0('1.2-fold'), 
           x = thr1.2 -0.7, y = 0.22, color = "grey30") + 
  geom_vline(xintercept = thr2, 
             color = "grey40", lty = 'dashed', show.legend = F) + 
  annotate(geom = "text", size=3.7,
           label = paste0('2-fold'), 
           x = thr2 +0.4, y = 0.25, color = "grey30") + 
  geom_vline(xintercept = thr5, 
             color = "grey40", lty = 'dashed', show.legend = F) + 
  annotate(geom = "text", size=3.7,
           label = paste0('5-fold'), 
           x = thr5 + 0.4, y = 0.27, color = "grey30") + 
  geom_vline(xintercept = thr10, 
             color = "grey40", lty = 'dashed', show.legend = F) + 
  annotate(geom = "text", size=3.7, 
           label = paste0('10-fold'), 
           x = thr10 + 0.5, y = 0.29, color = "grey30") + 
  removeGrid(x = FALSE, y = FALSE) + 
  xlim(-12,5)
dev.off()


#### table:
rs = readRDS('data_created/individual_rs_covariates.rds')
thr1 = 45; thr2 = 75
Rl = sum(exp(rs$rs_est) * rs$sampling_weights)/sum(rs$sampling_weights)
rs$rs_est = rs$rs_est - log(Rl)
rs1 = rs[rs$age<thr1,]
rs2 = rs[(rs$age>=thr1)&(rs$age<thr2),]
rs3 = rs[rs$age>=thr2,]

RS = lapply(1:nrow(rs),function(x){rep(rs$rs_est[x],rs$sampling_weights[x])})
RS = unlist(RS)
# mixture normal:
mu1 = wt.mean(rs1$rs_est, rs1$sampling_weights)
va1 = wt.var(rs1$rs_est, rs1$sampling_weights)
mu2 = wt.mean(rs2$rs_est, rs2$sampling_weights)
va2 = wt.var(rs2$rs_est, rs2$sampling_weights)
mu3 = wt.mean(rs3$rs_est, rs3$sampling_weights)
va3 = wt.var(rs3$rs_est, rs3$sampling_weights)
mu = wt.mean(rs$rs_est, rs$sampling_weights); va = wt.var(rs$rs_est, rs$sampling_weights)
w1 = sum(ifelse(rs$age<thr1,1,0) * rs$sampling_weights)/sum(rs$sampling_weights) 
w2 = sum(ifelse((rs$age>=thr1)*(rs$age<thr2),1,0) * rs$sampling_weights)/sum(rs$sampling_weights) 
w3 = sum(ifelse(rs$age>=thr2,1,0) * rs$sampling_weights)/sum(rs$sampling_weights) 

res = matrix(NA,4,2)
colnames(res) = c('empirical','mixture normal')
rownames(res) = paste0(c(1.2,2,5,10),'-fold')
k=1.2
res[1,1] = mean(RS>log(k)) 
res[1,2] = w1 * pnorm(log(k),mu1,sqrt(va1),lower.tail = F) + w2 * pnorm(log(k),mu2,sqrt(va2),lower.tail = F) + w3 * pnorm(log(k),mu3,sqrt(va3),lower.tail = F)
k=2
res[2,1] = mean(RS>log(k)) 
res[2,2] = w1 * pnorm(log(k),mu1,sqrt(va1),lower.tail = F) + w2 * pnorm(log(k),mu2,sqrt(va2),lower.tail = F) + w3 * pnorm(log(k),mu3,sqrt(va3),lower.tail = F)
k=5
res[3,1] = mean(RS>log(k)) 
res[3,2] = w1 * pnorm(log(k),mu1,sqrt(va1),lower.tail = F) + w2 * pnorm(log(k),mu2,sqrt(va2),lower.tail = F) + w3 * pnorm(log(k),mu3,sqrt(va3),lower.tail = F)
k=10
res[4,1] = mean(RS>log(k)) 
res[4,2] = w1 * pnorm(log(k),mu1,sqrt(va1),lower.tail = F) + w2 * pnorm(log(k),mu2,sqrt(va2),lower.tail = F) + w3 * pnorm(log(k),mu3,sqrt(va3),lower.tail = F)
res.weighted=res
signif(res.weighted, 3)



# -------------------------- Normal approximation for 65+ Medicare population --------------------------
#### mean risk without SDI:
rs = readRDS("data_created/individual_rs_covariates.rds")
rs = rs[rs$age>=65,]
Rl = 4.172392 # average of the general 18+ population
rs$rs_est = rs$rs_est - log(Rl)
thr1.2=log(1.2) 
thr2=log(2) 
thr5=log(5) 
thr10=log(10) 
thr25=log(25)
mu = wt.mean(rs$rs_est, rs$sampling_weights); va = wt.var(rs$rs_est, rs$sampling_weights)
n.sample =  nrow(rs)
normal.density = rnorm(n.sample, mu, sqrt(va))

png('figures_and_tables/output/NHIS_65over_approximation.png',
    units='px', width=1500, height=1500*0.4, res=150,type = "cairo")
ggplot(rs) +
  geom_histogram(color="black",alpha=0.9,fill='Ivory 4', bins = 40, 
                 aes(rs_est,weight=sampling_weights/mean(sampling_weights),y = ..density..)) +
  geom_density(aes(x = normal.density, color = 'orangered4'),alpha = .2, 
               color = 'orangered4',lwd=0.8, trim = T) + 
  theme(axis.text.x = element_text(angle = 0, size=12, face="bold"), 
        axis.text.y = element_text(size=10, face="bold")) + 
  theme_bw() +
  labs(title="b", Position="center", x = "COVID-19 risk score of the 65+ year old NHIS population", y = "") +
  theme(legend.position = "none",axis.text=element_text(size=12),
        plot.title = element_text(hjust = -0.05, size = 20, face="bold"),
        axis.title=element_text(size=12),
        panel.grid = element_line(colour = "white")) +
  scale_x_continuous(labels = ks,breaks = c(-4.8,-2,0,2,4.5),
                     expand = c(0,0))+
  scale_y_continuous(expand = c(0,0), limits = c(0,0.5)) + 
  geom_vline(xintercept = thr1.2, 
             color = "grey40", lty = 'dashed', show.legend = F) + 
  annotate(geom = "text", size=3.7,
           label = paste0('1.2-fold'), 
           x = thr1.2 -0.7, y = 0.22, color = "grey30") + 
  geom_vline(xintercept = thr2, 
             color = "grey40", lty = 'dashed', show.legend = F) + 
  annotate(geom = "text", size=3.7,
           label = paste0('2-fold'), 
           x = thr2 +0.4, y = 0.25, color = "grey30") + 
  geom_vline(xintercept = thr5, 
             color = "grey40", lty = 'dashed', show.legend = F) + 
  annotate(geom = "text", size=3.7,
           label = paste0('5-fold'), 
           x = thr5 + 0.4, y = 0.27, color = "grey30") + 
  geom_vline(xintercept = thr10, 
             color = "grey40", lty = 'dashed', show.legend = F) + 
  annotate(geom = "text", size=3.7, 
           label = paste0('10-fold'), 
           x = thr10 + 0.5, y = 0.29, color = "grey30") + 
  removeGrid(x = FALSE, y = FALSE) + 
  xlim(-12,5)
dev.off()



# ----------------------------- table:
rs = readRDS('data_created/individual_rs_covariates.rds')
rs = rs[rs$age>=65,]
rs$rs_est = rs$rs_est - log(Rl)

RS = lapply(1:nrow(rs),function(x){rep(rs$rs_est[x],rs$sampling_weights[x])})
RS = unlist(RS)
mu = wt.mean(rs$rs_est, rs$sampling_weights); va = wt.var(rs$rs_est, rs$sampling_weights)
res = matrix(NA,4,2)
colnames(res) = c('empirical','mixture normal')
rownames(res) = paste0(c(1.2,2,5,10),'-fold')
k=1.2
res[1,1] = mean(RS>log(k)) 
res[1,2] = pnorm(log(k),mu,sqrt(va),lower.tail = F) 
k=2
res[2,1] = mean(RS>log(k)) 
res[2,2] = pnorm(log(k),mu,sqrt(va),lower.tail = F)
k=5
res[3,1] = mean(RS>log(k)) 
res[3,2] = pnorm(log(k),mu,sqrt(va),lower.tail = F)
k=10
res[4,1] = mean(RS>log(k)) 
res[4,2] = pnorm(log(k),mu,sqrt(va),lower.tail = F)




