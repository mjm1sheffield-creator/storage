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

floating$DOY <- as.numeric(floating$DOY)
floating$Month <- as.factor(floating$Month)
floating$T_air<- as.numeric(floating$T_air)
floating$DO_mg_l<- as.numeric(floating$DO_mg_l)
floating$Date <- as.Date(floating$Date, "%d/%m/%Y")
floating<-mutate(floating, Date=dmy(Date))

floating<-filter(floating, C.CO2!="NA" & C.CH4!="NA" & Point!="T")
floatingDO<-filter(floating, DO_mg_l!="NA")
floatingT<-filter(floating, T_water!="NA")
floating$Point <- recode_factor(floating$Point, '2a' = "2", '3a' = "3")

floating_c<-within(floating, Point <- factor(Point,
levels=c('1', '2', '3', '4', '5', '6', 'NE','NW', 'SLE', 'SLW', 'SRE', 'SRW', 'GL', 'GR')))

floatingW<-filter(floating, type!="Eriophorum")
floatingE<-filter(floating, type!="Water")
floating<-filter(floating, C.CO2!="NA" & C.CH4!="NA")
floatingC<-filter(floating, site=="Cavemount")
floatingU<-filter(floating, site=="Ummeras")

floatingC$type <- recode_factor(floatingC$type, 'Eriophorum'="Eriophorum (C)")
floatingU$type <- recode_factor(floatingU$type, 'Water'="Water (U)")
floatingC$type <- recode_factor(floatingC$type, 'Water'="Water (C)")
combined<-rbind(floatingC, floatingU)


DOY_CO2_facet<-ggplot(floating, aes(x=DOY, y=C.CO2, colour=type))+
  facet_wrap(~site)+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.2)+
  scale_y_continuous(breaks = seq(-0.12, 0.1, by =0.02))+
  coord_cartesian(ylim = c(-0.12, 0.1))+
  scale_x_continuous(breaks = seq(0, 365, by =30))+
  scale_colour_manual(name="", values=c('Water'="skyblue", 'Eriophorum'="green2"))+
  labs(title = (expression(paste("Water ", CO[2], " flux"))), x="Day of the year", y = expression(paste ("", CO[2]," flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 11, colour='black'),
        axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOY_CO2_facet_a <- DOY_CO2_facet + 
  guides(colour = guide_legend(override.aes = list(size = 2, shape = NA, fill=NA)))
print(DOY_CO2_facet_a)

DOY_CH4_facet<-ggplot(floating, aes(x=DOY, y=C.CH4, colour=type))+
  facet_wrap(~site)+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.2)+
  scale_y_continuous(breaks = seq(-60, 140, by =20))+
  coord_cartesian(ylim = c(-60, 140))+
  scale_x_continuous(breaks = seq(0, 365, by =30))+
  scale_colour_manual(name="", values=c('Water'="skyblue", 'Eriophorum'="green2"))+
  labs(title = (expression(paste("Water ", CH[4], " flux"))), x="Day of the year", y = expression(paste ("", CH[4]," flux (mg C ",-CH[4]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 11, colour='black'),
        axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOY_CH4_facet_a <- DOY_CH4_facet + 
  guides(colour = guide_legend(override.aes = list(size = 2, shape = NA, fill=NA)))
print(DOY_CH4_facet_a)

boxplotCO2<-ggplot(floating, aes(x=site, y=C.CO2, colour=type))+
  geom_boxplot(width=.4, outlier.size=1, lwd=1)+
  facet_wrap(~Year)+
  scale_colour_manual(name="", values=c('Water'="skyblue", 'Eriophorum'="green2"))+
  scale_x_discrete(name="", breaks=c("Cavemount", "Ummeras"), labels=c("Cavemount", "Ummeras"))+
  scale_y_continuous(breaks = seq(-0.12, 0.1, by =0.02))+
  coord_cartesian(ylim = c(-0.12, 0.1))+
  labs(title = (expression(paste("Water ", CO[2], " flux"))), x="Day of the year", y = expression(paste ("", CO[2]," flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=2, stroke=1.5)+
  theme_bw()+
  theme(axis.text = element_text(size = 11),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_blank(),
        strip.text = element_text(size = 10),
        legend.key.size = unit(1, "cm"))

boxplotCO2_na <- boxplotCO2 + guides(colour = guide_legend(override.aes = list(shape = NA)))
print(boxplotCO2_na)

boxplotCH4<-ggplot(floating, aes(x=site, y=C.CH4, colour=type))+
  geom_boxplot(width=.4, outlier.size=1, lwd=1)+
  facet_wrap(~Year)+
  scale_colour_manual(name="", values=c('Water'="skyblue", 'Eriophorum'="green2"))+
  scale_x_discrete(name="", breaks=c("Cavemount", "Ummeras"), labels=c("Cavemount", "Ummeras"))+
  scale_y_continuous(breaks = seq(-60, 140, by =20))+
  coord_cartesian(ylim = c(-60, 140))+
  labs(title = (expression(paste("Water ", CH[4], " flux"))), x="Day of the year", y = expression(paste ("", CH[4]," flux (mg C ",-CH[4]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=2, stroke=1.5)+
  theme_bw()+
  theme(axis.text = element_text(size = 11),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_blank(),
        strip.text = element_text(size = 10),
        legend.key.size = unit(1, "cm"))

boxplotCH4_na <- boxplotCH4 + guides(colour = guide_legend(override.aes = list(shape = NA)))
print(boxplotCH4_na)

CO2_point<-ggplot(floating_c, aes(x=DOY, y=C.CO2, colour=type))+
  facet_wrap(~site+Point)+
  geom_point(size=1.2)+
  stat_smooth(method=loess, alpha=0.3)+
  scale_y_continuous(breaks = seq(-0.14, 0.1, by =0.04))+
  coord_cartesian(ylim = c(-0.14, 0.1))+
  scale_x_continuous(breaks = seq(0, 365, by =60))+
  scale_colour_manual(name="", values=c('Water'="skyblue", 'Eriophorum'="green2"))+
  labs(title = (expression(paste("Water ", CO[2], " flux"))), x="Day of the year", y = expression(paste ("", CO[2]," flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 11, colour='black'),
        axis.text.x = element_text(size = 10, angle = 90, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        axis.title.x = element_text(size = 15, vjust = -1),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

CO2_point_a <- CO2_point + guides(colour = guide_legend(override.aes = list(shape = NA, fill=NA, size=2)))
print(CO2_point_a)

CH4_point<-ggplot(floating_c, aes(x=DOY, y=C.CH4, colour=type))+
  facet_wrap(~site+Point)+
  geom_point(size=1.2)+
  stat_smooth(method=loess, alpha=0.3)+
  scale_x_continuous(breaks = seq(0, 365, by =60))+
  coord_cartesian(xlim = c(0, 360), ylim = c(-60, 140))+
  scale_y_continuous(breaks = seq(-60, 140, by =30))+
  scale_colour_manual(name="", values=c('Water'="skyblue", 'Eriophorum'="green2"))+
  labs(title = (expression(paste("Water ", CH[4], " flux"))), x="Day of the year", y = expression(paste ("", CH[4]," flux (mg C ",-CH[4]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 10, colour='black'),
        axis.text.x = element_text(size = 10, angle = 50, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        axis.title.x = element_text(size = 15, vjust = -1),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

CH4_point_a <- CH4_point + guides(colour = guide_legend(override.aes = list(shape = NA, fill=NA, size=1)))
print(CH4_point_a)

T_water<-ggplot(floatingT, aes(x=T_water, y=C.CH4, colour=type))+
  facet_wrap(~site)+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.2)+
  scale_x_continuous(breaks = seq(0, 26, by =2))+
  scale_y_continuous(breaks = seq(-60, 140, by =20))+
  coord_cartesian(ylim = c(-60, 140), xlim=c(0, 26))+
  scale_colour_manual(name="", values=c('Water'="skyblue", 'Eriophorum'="green2"))+
  labs(title = (expression(paste("Water ", CH[4], " flux"))), x="Water temperature (°C)", y = expression(paste ("", CH[4]," flux (mg C ",-CH[4]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 11, colour='black'),
        axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

T_water2<-ggplot(floatingT, aes(x=T_water, y=C.CO2, colour=type))+
  facet_wrap(~site)+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.2)+
  scale_x_continuous(breaks = seq(0, 26, by =2))+
  scale_y_continuous(breaks = seq(-0.14, 0.1, by =0.04))+
  coord_cartesian(ylim = c(-0.14, 0.1), xlim=c(0, 26))+
  scale_colour_manual(name="", values=c('Water'="skyblue", 'Eriophorum'="green2"))+
  labs(title = (expression(paste("Water ", CO[2], " flux"))), x="Water temperature (°C)", y = expression(paste ("", CO[2]," flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 11, colour='black'),
        axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DO_water<-ggplot(floatingDO, aes(x=DO_mg_l, y=C.CH4, colour=type))+
  facet_wrap(~site)+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.2)+
  scale_x_continuous(breaks = seq(0, 13, by =1))+
  scale_y_continuous(breaks = seq(-60, 140, by =20))+
  coord_cartesian(ylim = c(-60, 140), xlim=c(0, 13))+
  scale_colour_manual(name="", values=c('Water'="skyblue", 'Eriophorum'="green2"))+
  labs(title = (expression(paste("Water ", CH[4], " flux"))), x="DO mg l", y = expression(paste ("", CH[4]," flux (mg C ",-CH[4]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 11, colour='black'),
        axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

T_Water-co2
T_water-ch4
DO-CO2
DO-CH4
date-co2
date-ch4

##########
combined<-filter(combined, Year!="2023")

combined<-within(combined, type <-factor(type, levels=c('Eriophorum (C)', 'Water (C)', 'Water (U)')))

CH4_point<-ggplot(combined, aes(x=DOY, y=C.CH4, colour=type))+
  facet_wrap(~Year)+
  geom_point(size=1.7)+
  stat_smooth(method=loess, alpha=0.2, size=0.8)+
  scale_x_continuous(breaks = seq(0, 365, by =60))+
  coord_cartesian(xlim = c(0, 360), ylim = c(-60, 140))+
  scale_y_continuous(breaks = seq(-30, 140, by =30))+
  scale_colour_manual(name="", values=c('Water (C)'="steelblue4", 'Water (U)'="deepskyblue", 'Eriophorum (C)'="green2"))+
  labs(title = (expression(paste("Pond ", CH[4], " flux (both sites)"))), x="Day of the year", y = expression(paste ("", CH[4]," flux (mg C ",-CH[4]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 10, colour='black'),
        axis.text.x = element_text(size = 10, angle = 50, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        axis.title.x = element_text(size = 15, vjust = -1),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

CH4_point_a <- CH4_point + guides(colour = guide_legend(override.aes = list(shape = NA, fill=NA, size=3)))
print(CH4_point_a)

CO2_point<-ggplot(combined, aes(x=DOY, y=C.CO2, colour=type))+
  facet_wrap(~Year)+
  geom_point(size=1.7)+
  stat_smooth(method=loess, alpha=0.2, size=0.8)+
  scale_x_continuous(breaks = seq(0, 365, by =60))+
  coord_cartesian(xlim = c(0, 360), ylim = c(-0.14, 0.04))+
  scale_y_continuous(breaks = seq(-0.14, 0.04, by =0.02))+
  scale_colour_manual(name="", values=c('Water (C)'="steelblue4", 'Water (U)'="deepskyblue", 'Eriophorum (C)'="green2"))+
  labs(title = (expression(paste("Pond ", CO[2], " flux (both sites)"))), x="Day of the year", y = expression(paste ("", CO[2]," flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 10, colour='black'),
        axis.text.x = element_text(size = 10, angle = 50, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        axis.title.x = element_text(size = 15, vjust = -1),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

CO2_point_a <- CO2_point + guides(colour = guide_legend(override.aes = list(shape = NA, fill=NA, size=3)))
print(CO2_point_a)
