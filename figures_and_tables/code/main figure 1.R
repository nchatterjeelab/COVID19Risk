library(openxlsx)
library("ggplot2")
theme_set(theme_bw())
library("sf")
library(maps)
library("rnaturalearth")
library("rnaturalearthdata")
library(ggrepel)
library(ggpubr)
library(ggspatial)

#-------------------- Map ---------------------#
### give the right location

flcities = readRDS('data_created/full_output_updated.rds')
flcities_rs = data.frame(flcities$StateAbbr, flcities$PlaceName, flcities$lat, flcities$lon, flcities$rs_est)
mean_var_Nmixture = read.xlsx('data_created/highrisk_overall-mean/highrisk_all_mixtureN_overall-mean.xlsx')
age_distribution = readRDS('data_created/full_output_updated.rds')
Risk = (age_distribution$Age_18_39 * exp(mean_var_Nmixture$mu_under40 + 0.5*mean_var_Nmixture$var_under40)) + ((1-age_distribution$Age_18_39) * exp(mean_var_Nmixture$mu_over40 + 0.5*mean_var_Nmixture$var_over40)) 
wts = (mean_var_Nmixture$population/sum(mean_var_Nmixture$population))
mean.risk.cities = weighted.mean(Risk, w=wts)
IER = Risk/mean.risk.cities
flcities_rs$IER = IER
colnames(flcities_rs)[1:5] = c("State", "Place", "lat", "lng", "Risk")
flcities_rs <- st_as_sf(flcities_rs, coords = c("lng", "lat"), remove = FALSE,
                     crs = 4326, agr = "constant")
world <- ne_countries(scale = "medium", returnclass = "sf")
us = world[which(world$admin == "United States of America"), ]
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
head(states)
states <- cbind(states, st_coordinates(st_centroid(states)))

library("tools")
states$ID <- toTitleCase(states$ID)
counties <- st_as_sf(map("county", plot = FALSE, fill = TRUE))
counties$area <- as.numeric(st_area(counties))

major_states = c('New York','California','Illinois','Texas','Pennsylvania',
                 'Georgia','Maryland','Washington','Massachusetts','Michigan')
major_states_data = states[which(states$ID %in% major_states == T), ]
majorcities = c('New York','Los Angeles','Chicago','Houston','Philadelphia',
                'Atlanta','San Francisco','Baltimore','Seattle','Boston','Detroit', 'Boulder', 'Miami', 'Austin', 'Washington')
majorcities2 = c('Hemet', 'Deerfield Beach', 'Youngstown',
                 'Provo', 'Somerville', 'Cambridge', 'Iowa City')
major_cities_data = flcities_rs[which(flcities_rs$Place %in% majorcities == T), ]
major_cities_data$Place[4] = "Washington, D.C."
major_cities_data2 = flcities_rs[which(flcities_rs$Place %in% majorcities2 == T), ]
major_cities_data = rbind(major_cities_data, major_cities_data2)

map1 = ggplot(data=us) +
  geom_sf(fill = "lightskyblue") +
  geom_sf(data = states, fill = "darkseagreen") +
  geom_sf(data = flcities_rs, aes(fill = IER),size = 6, shape = 21) +
  scale_fill_gradient(low = "yellow", high = "red2", space ="Lab", name = "Index of Excess Risk") +
  geom_point(x =-116.9946 , y = 33.73523, size=6, shape = 21, color = "black", fill = "red2") +
  geom_point(x =-83.10532 , y = 42.38470, size=6, shape = 21, color = "black", fill = "darkorange2") +
  geom_text_repel(data = major_cities_data, aes(x = lng, y = lat, label = Place), 
                  fontface = "bold", size = 5, nudge_x = c(-4, -3, -1.5, 6, 4, 7, 2, 5, 3, 3.5, 4, 4, -3, 3, 3,
                                                           2, 6, 0.25, 4, 4, 5, -0.8), nudge_y = c(-2, -2, -2, -2, -1, -2, 2.5, -2, 0.5, 1.5, -1,-1,0.5, -1.5, 1,
                                                                                             -3, 2, 3, 4, 4, 1.2, -2)) +
  coord_sf(xlim = c(-126, -66), ylim = c(24, 52), expand = FALSE) +
  annotation_scale() +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.9, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  xlab("Longitude") + ylab("Latitude") +
  theme(axis.text = element_text(size = 18, face = "bold", colour = "black"), axis.title=element_text(size = 18, face = "bold"), legend.position = c(0.785,0.9), legend.box = "none", legend.background = element_blank(), legend.direction = "horizontal", legend.text = element_text(colour="black", size=16, 
                                                                                                                                                                                                                                                                                       face="bold"), legend.title = element_text(colour="black", size=16, 
                                                                                                                                                                                                                              face="bold"), legend.key.size = unit(0.75, "cm"))
#-------------------- Histogram ---------------------#
rs = read.xlsx('data_created/highrisk_overall-mean/highrisk_all_mixtureN_overall-mean.xlsx')
prop = readRDS('data_created/full_output_updated.rds')

ref.risk = mean.risk.cities
expected.risk = prop$Age_18_39*exp(rs$mu_under40+0.5*rs$var_under40)+(1-prop$Age_18_39)*exp(rs$mu_over40+0.5*rs$var_over40)
rs = data.frame(rs,IER=expected.risk)
rs = data.frame(rs,rel.IER=expected.risk/ref.risk,log.rel.IER=log(expected.risk/ref.risk))
big_cities = c('New York','Los Angeles','Chicago','Houston','Philadelphia',
               'Atlanta','San Francisco','Seattle','Boston','Detroit', 'Austin', 'Washington')
inx = which(rs$PlaceName %in% big_cities == T)
df = data.frame(big_cities=rs$PlaceName[inx],IER=rs$rel.IER[inx])
ks <- function (x) { x = round(exp(x),2) }

ggplot(rs,aes(log.rel.IER)) + 
  geom_histogram(bins=30,fill="#B24745FF",color="black",size=0.5, alpha = 0.9) +
  #geom_vline(aes(xintercept = log(min(rs$rel.IER)))) +
  geom_vline(aes(xintercept=0),col='black',size=1.2,linetype = "dashed") + 
  theme(axis.text.x = element_text(angle = 0, size=12, face="bold"), 
        axis.text.y = element_text(size=12, face="bold")) + theme_bw() +
  labs(title="", Position="center", x = "Index of Excess Risk (IER)", y = "Number of Cities") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text=element_text(size=18,face="bold",colour="black"),
        axis.title=element_text(size=18,face="bold"))+
  scale_x_continuous(labels = ks,breaks = log(seq(0.25,2.25,0.25)))+
  scale_y_continuous(expand = c(0,0),limits=c(0,48))

ggsave("hist.tiff", dpi = 320, width = 30,
       height = 20)




