lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
library(lme4)
library(nlme)
library(MuMIn)
library(Matrix)
library(gridExtra)
library(dplyr)
library(ggplot2)
library(car)
library(LMERConvenienceFunctions)
library(multcomp)
library(lsmeans)
library(lattice)

str(floating)
rm(list=ls())
a<-subset(floating, site=='Cavemount', select=type)
summary(a$Point)
sum1<-summarise(group_by(floating, Point), mean_bio1=mean(DOC))

floating<-filter(floating, type!="Eriophorum")
floating<-filter(floating, DOC.mg.l!="NA")
floatingC<-filter(floating, site=="Cavemount")
floatingU<-filter(floating, site=="Ummeras")
floatingDO<-filter(floating, DO_.!="NA")

floating$DOY <- as.numeric(floating$DOY)
floating$DOY <- as.factor(floating$DOY)
floating$Month <- as.factor(floating$Month)
floating$T_air<- as.numeric(floating$T_air)
floating$DO_.<- as.numeric(floating$DO_.)
floating$Date <- as.Date(floating$Date, "%d/%m/%Y")
floating<-mutate(floating, Date=dmy(Date))

waterT<-ggplot(floating, aes(x=DOY, y=T_water, colour=site))+
  geom_point(size=2, shape=17)+
  scale_y_continuous(breaks = seq(0, 30, by =5))+
  scale_x_continuous(breaks = seq(0, 365, by =40))+
  coord_cartesian(xlim = c(0, 365), ylim = c(0, 30))+
  scale_colour_manual(name="", values=c('Cavemount' = 'limegreen', 'Ummeras' = 'turquoise4'))+
  labs(title = (expression(paste("Water temperature (°C) by site"))), x="Day of the year", y = "Water temperature (°C)")+
  theme_bw()+
  theme(axis.text = element_text(size = 9, colour='black'),
        axis.text.x = element_text(size = 9, angle = 0, hjust = 0.5, vjust = 0),
        axis.text.y = element_text(size = 10, angle = 90, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        axis.title.x = element_text(size = 15, vjust = -1),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOC_DOY<-ggplot(floating, aes(x=DOY, y=DOC.mg.l, colour=site))+
  geom_point(size=2, shape=17)+
  scale_y_continuous(breaks = seq(0, 100, by =10))+
  coord_cartesian(ylim = c(0, 100), xlim = c(0, 365))+
  scale_x_continuous(breaks = seq(0, 365, by =40))+
  scale_colour_manual(name="", values=c('Cavemount' = 'limegreen', 'Ummeras' = 'turquoise4'))+
  labs(title = (expression(paste("DOC by site"))), x="Day of the year", y = expression(paste ("DOC concentration ( ", mg ," ", l^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 9, colour='black'),
        axis.text.x = element_text(size = 9, angle = 90, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        axis.title.x = element_text(size = 15, vjust = -1),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOC_water0<-ggplot(floating, aes(x=T_water, y=DOC.mg.l, colour=site))+
  geom_point(size=2, shape=17)+
  stat_smooth(method=loess, alpha=0.2, size=0.5)+
  scale_y_continuous(breaks = seq(0, 100, by =10))+
  coord_cartesian(ylim = c(0, 100), xlim = c(0, 30))+
  scale_x_continuous(breaks = seq(0, 30, by =5))+
  scale_colour_manual(name="", values=c('Cavemount' = 'limegreen', 'Ummeras' = 'turquoise4'))+
  labs(title = (expression(paste("DOC by site"))), x="Water temperature (°C)", y = expression(paste ("DOC concentration ( ", mg ," ", l^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 9, colour='black'),
        axis.text.x = element_text(size = 9, angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        axis.title.x = element_text(size = 15, vjust = -1),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOC_water<-ggplot(floating, aes(x=T_water, y=DOC.mg.l, colour=site))+
  geom_point(size=2, shape=17)+
  stat_smooth(method=loess, alpha=0.2, size=0.5)+
  scale_y_continuous(breaks = seq(0, 100, by =10))+
  coord_cartesian(ylim = c(0, 100), xlim = c(0, 30))+
  scale_x_continuous(breaks = seq(0, 30, by =5))+
  scale_colour_manual(name="", values=c('Cavemount' = 'limegreen', 'Ummeras' = 'turquoise4'))+
  labs(title = (expression(paste("DOC by site"))), x="Water temperature (°C)", y = expression(paste ("DOC concentration ( ", mg ," ", l^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 9, colour='black'),
        axis.text.x = element_text(size = 9, angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        axis.title.x = element_text(size = 15, vjust = -1),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOC_DO<-ggplot(floating, aes(x=DO_mg_l, y=DOC.mg.l, colour=site))+
  geom_point(size=2, shape=17)+
  stat_smooth(method=loess, alpha=0.2, size=0.8)+
  scale_y_continuous(breaks = seq(0, 100, by =10))+
  coord_cartesian(ylim = c(0, 100), xlim = c(0, 10))+
  scale_x_continuous(breaks = seq(0, 10, by =1))+
  scale_colour_manual(name="", values=c('Cavemount' = 'limegreen', 'Ummeras' = 'turquoise4'))+
  labs(title = (expression(paste("DOC by site"))), x=expression(paste ("DO concentration ( ", mg ," ", l^-1, ")", sep="")), y = expression(paste ("DOC concentration ( ", mg ," ", l^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 9, colour='black'),
        axis.text.x = element_text(size = 9, angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        axis.title.x = element_text(size = 15, vjust = -1),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOC_point<-ggplot(floating, aes(x=Month, y=DOC.mg.l, colour=site))+
  facet_wrap(~Point)+
  geom_point(size=2, shape=17)+
  stat_smooth(method=loess, alpha=0.2, size=0.8)+
  scale_y_continuous(breaks = seq(0, 120, by =20))+
  coord_cartesian(ylim = c(0, 120))+
  scale_colour_manual(name="", values=c('Cavemount' = 'limegreen', 'Ummeras' = 'turquoise4'))+
  labs(title = (expression(paste("DOC by measurement point"))), x="Month", y = expression(paste ("DOC concentration ( ", mg ," ", l^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 9, colour='black'),
        axis.text.x = element_text(size = 9, angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        axis.title.x = element_text(size = 15, vjust = -1),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOC_point_a <- DOC_point + guides(colour = guide_legend(override.aes = list(fill=NA)))
print(DOC_point_a)

DOC_CO2<-ggplot(floating, aes(x=DOC.mg.l, y=C.CO2, colour=site))+
  geom_point(size=2, shape=17)+
  stat_smooth(method=loess, alpha=0.2, size=0.8)+
  scale_y_continuous(breaks = seq(-0.2, 0.5, by =0.1))+
  coord_cartesian(ylim = c(-0.2, 0.5), xlim = c(0, 120))+
  scale_x_continuous(breaks = seq(0, 120, by =10))+
  scale_colour_manual(name="", values=c('Cavemount' = 'limegreen', 'Ummeras' = 'turquoise4'))+
  labs(title = (expression(paste("", CO[2], " flux"))), x=expression(paste ("DOC concentration ( ", mg ," ", l^-1, ")", sep="")), y = expression(paste ("", CO[2], " flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 9, colour='black'),
        axis.text.x = element_text(size = 9, angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        axis.title.x = element_text(size = 15, vjust = -1),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOC_CH4<-ggplot(floating, aes(x=DOC.mg.l, y=C.CH4, colour=site))+
  scale_y_continuous(breaks = seq(-0.2, 1, by=0.1))+
  coord_cartesian(ylim = c(-0.2, 1), xlim = c(0, 120))+
  geom_point(size=2, shape=17)+
  scale_x_continuous(breaks = seq(0, 120, by =10))+
  scale_colour_manual(name="", values=c('Cavemount' = 'limegreen', 'Ummeras' = 'turquoise4'))+
  labs(title = (expression(paste("", CH[4], " flux"))), x=expression(paste ("DOC concentration ( ", mg ," ", l^-1, ")", sep="")), y = expression(paste ("", CH[4], " flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 9, colour='black'),
        axis.text.x = element_text(size = 9, angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        axis.title.x = element_text(size = 15, vjust = -1),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))



