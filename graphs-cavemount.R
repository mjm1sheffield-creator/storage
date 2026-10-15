lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
library(MuMIn)
library(Matrix)
library(gridExtra)
library(dplyr)
library(ggplot2)
library(car)
library(lsmeans)
library(lubridate)
library(lattice)

str(Cavemount.flux)
summary(Cavemount.flux$Plot)
select(filter(Cavemount.flux_WT, ), WT, Plot)
select(filter(Cavemount.flux, ), CO2)
sum(is.na(Cavemount.flux$WT))

Cavemount.flux$DOY<-as.numeric(Cavemount.flux$DOY)
Cavemount.flux$WT<-as.numeric(Cavemount.flux$WT)
Cavemount.flux$Year<-as.factor(Cavemount.flux$Year)
Cavemount.flux$Date <- as.Date(Cavemount.flux$Date, "%d/%m/%Y")
Cavemount.flux<-mutate(Cavemount.flux, Date=dmy(Date))

Cavemount.flux$Plot<-recode(Cavemount.flux$Plot, "c('BP8B', 'BP8C', 'BP8D')='BP8'; 
                    c('E7B', 'E7C') = 'E7'; c('E5B')='E5'")
Cavemount.flux<-filter(Cavemount.flux, CO2!="NA" & CH4!="NA")
Cavemount.fluxT<-filter(Cavemount.flux, soil_T!="NA")
Cavemount.flux_moi<-filter(Cavemount.flux, moi!="NA")
Cavemount.flux_WT<-filter(Cavemount.flux, WT!="NA")
Cavemount.flux_WT$WT<-(Cavemount.flux_WT$WT*-1)
Cavemount.flux_WT_moi<-filter(Cavemount.flux_WT, moi!="NA")
Cavemount.flux_WTa<-filter(Cavemount.flux_WT, !(Plot %in% c('E4', 'BP2', 'E5', 'E7')))

Cavemount.flux<-filter(Cavemount.flux, Year!="2023"& type!='Sphagnum')
Cavemount.fluxenv<-filter(Cavemount.flux, action!="delete" & soil_T!="NA" & moi!="NA")
Cavemount.flux2020<-filter(Cavemount.flux, Year=="2020")
Cavemount.flux2021<-filter(Cavemount.flux, Year=="2021")
Cavemount.flux2022<-filter(Cavemount.flux, Year=="2022")
Cavemount.fluxT2020<-filter(Cavemount.flux2020, soil_T!="NA")
Cavemount.fluxT2021<-filter(Cavemount.flux2021, soil_T!="NA")
Cavemount.fluxT2022<-filter(Cavemount.flux2022, soil_T!="NA")
Cavemount.flux_moi2020<-filter(Cavemount.flux2020, moi!="NA")
Cavemount.flux_moi2021<-filter(Cavemount.flux2021, moi!="NA")
Cavemount.flux_moi2022<-filter(Cavemount.flux2022, moi!="NA")
Cavemount.flux<-filter(Cavemount.flux, Year!="2023")

sumflux<-summarise(group_by(Cavemount.flux, type), mean_bio1=mean(CO2))
sumflux2<-summarise(group_by(Cavemount.flux, type), mean_bio2=mean(CH4))
sumflux3<-summarise(group_by(Cavemount.flux_WT, Plot), mean_bio3=mean(WT))
#########################

CO2a<-summarise(group_by(Cavemount.flux, type),
                meanCO2a=mean(CO2),
                SEa=sd(CO2)/sqrt(length(CO2)),
                lwra=meanCO2a-SEa,
                upra=meanCO2a+SEa)
limits1<-aes(ymin=lwra, ymax=upra)

CO2u1<-ggplot(Cavemount.flux, aes(x=type, y=CO2, colour=type))+
  geom_boxplot(width=.4, outlier.size=1, lwd=1)+
  scale_colour_manual(name="type", values=c('Bare Peat'="black", 'Birch Scrub'="chocolate", 'Eriophorum'="green2"))+
  scale_x_discrete(name="", breaks=c("Bare Peat", "Birch Scrub", "Eriophorum"), labels=c("Bare Peat", "Birch Scrub", "Eriophorum"))+
  scale_y_continuous(breaks = seq(0, 10, by =1))+
  coord_cartesian(ylim = c(0, 10))+
  labs(title = (expression(paste("ER flux from Cavemount"))), x="plot", y = expression(paste ("", CO[2]," flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3, stroke=1.5)+
  theme_bw()+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_blank(),
        strip.text = element_text(size = 10),
        legend.key.size = unit(1, "cm"))

co2_na <- CO2u1 + guides(colour = guide_legend(override.aes = list(shape = NA)))
print(co2_na)

CO2year<-ggplot(Cavemount.flux, aes(x=type, y=CO2, colour=type))+
  geom_boxplot(width=.4, outlier.size=1, lwd=1)+
  facet_wrap(~Year)+
  scale_colour_manual(name="type", values=c('Bare Peat'="black", 'Birch Scrub'="chocolate", 'Eriophorum'="green2"))+
  scale_x_discrete(name="", breaks=c("Bare Peat", "Birch Scrub", "Eriophorum"), labels=c("Bare Peat", "Birch Scrub", "Eriophorum"))+
  scale_y_continuous(breaks = seq(0, 10, by =1))+
  coord_cartesian(ylim = c(0, 10))+
  labs(title = (expression(paste("Cavemount ", CO[2], " flux"))), x="plot", y = expression(paste ("", CO[2]," flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=2, stroke=1.5)+
  theme_bw()+
  theme(axis.text = element_text(size = 11),
        axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_blank(),
        strip.text = element_text(size = 10),
        legend.key.size = unit(1, "cm"))

CO2year_na <- CO2year + guides(colour = guide_legend(override.aes = list(shape = NA)))
print(CO2year_na)



CO2u2<-ggplot(Cavemount.flux, aes(x=type, y=CO2, fill=type))+
  geom_boxplot(width=.4, outlier.size=1, lwd=1)+
  scale_fill_manual(name="type", values=c('Bare Peat'="black", 'Birch Scrub'="chocolate", 'Eriophorum'="green2"))+
  scale_x_discrete(name="", breaks=c("Bare Peat", "Birch Scrub", "Eriophorum"), labels=c("Bare Peat", "Birch Scrub", "Eriophorum"))+
  scale_y_continuous(breaks = seq(0, 10, by =1))+
  coord_cartesian(ylim = c(0, 10))+
  labs(title = (expression(paste("ER flux from Cavemount"))), x="plot", y = expression(paste ("", CO[2]," flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3, colour='white', stroke=1.5)+
  theme_bw()+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_blank(),
        strip.text = element_text(size = 10),
        legend.key.size = unit(1, "cm"))

co2_na <- CO2u2 + guides(fill = guide_legend(override.aes = list(shape = NA)))
print(co2_na)


CH4a<-summarise(group_by(Cavemount.flux, type),
                meanCH4a=mean(CH4),
                SEa=sd(CH4)/sqrt(length(CH4)),
                lwra=meanCH4a-SEa,
                upra=meanCH4a+SEa)
limits1<-aes(ymin=lwra, ymax=upra)

CH4_1<-ggplot(Cavemount.flux, aes(x=type, y=CH4, colour=type))+
  geom_boxplot(width=.4, outlier.size=1, lwd=1)+
  scale_colour_manual(name="type", values=c('Bare Peat'="black", 'Birch Scrub'="chocolate", 'Eriophorum'="green2"))+
  scale_x_discrete(name="", breaks=c("Bare Peat", "Birch Scrub", "Eriophorum"), labels=c("Bare Peat", "Birch Scrub", "Eriophorum"))+
  scale_y_continuous(breaks = seq(-2, 16, by =2))+
  coord_cartesian(ylim = c(-2, 16))+
  labs(title = (expression(paste("Cavemount ", CH[4], " flux by cover type"))), x="plot", y = expression(paste ("", CH[4]," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3, stroke=1.5)+
  theme_bw()+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_blank(),
        strip.text = element_text(size = 10),
        legend.key.size = unit(1, "cm"))

ccc <- CH4_1 + guides(colour = guide_legend(override.aes = list(shape = NA)))
print(ccc)

CH4year<-ggplot(Cavemount.flux, aes(x=type, y=CH4, colour=type))+
  geom_boxplot(width=.4, outlier.size=1, lwd=1)+
  facet_wrap(~Year)+
  scale_colour_manual(name="type", values=c('Bare Peat'="black", 'Birch Scrub'="chocolate", 'Eriophorum'="green2"))+
  scale_x_discrete(name="", breaks=c("Bare Peat", "Birch Scrub", "Eriophorum"), labels=c("Bare Peat", "Birch Scrub", "Eriophorum"))+
  scale_y_continuous(breaks = seq(-2, 16, by =2))+
  coord_cartesian(ylim = c(-2, 16))+
  labs(title = (expression(paste("Cavemount ", CH[4], " flux"))), x="plot", y = expression(paste ("", CH[4]," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=2, stroke=1.5)+
  theme_bw()+
  theme(axis.text = element_text(size = 11),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_blank(),
        strip.text = element_text(size = 10),
        legend.key.size = unit(1, "cm"))

CH4year_na <- CH4year + guides(colour = guide_legend(override.aes = list(shape = NA)))
print(CH4year_na)

#############

DOY_T<-ggplot(Cavemount.fluxT, aes(x=DOY, y=soil_T, colour=Year))+
  geom_point(size=1)+
  stat_smooth(method=loess)+
  scale_y_continuous(breaks = seq(0, 32, by =5))+
  coord_cartesian(ylim = c(0, 32))+
  scale_x_continuous(breaks = seq(0, 365, by =20))+
  labs(title = (expression(paste("Cavemount soil temperature"))), x="Day of the year", y = "Soil temperature (°C)")+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOY_Ta<-ggplot(Cavemount.fluxT, aes(x=DOY, y=soil_T, colour=type))+
  geom_point(size=1)+
  stat_smooth(method=loess, alpha=0.2)+
  scale_y_continuous(breaks = seq(0, 32, by =5))+
  coord_cartesian(ylim = c(0, 32))+
  scale_x_continuous(breaks = seq(0, 365, by =20))+
  scale_colour_manual(name="", values=c('Bare Peat'="black", 'Birch Scrub'="chocolate", 'Eriophorum'="green2"))+
  labs(title = (expression(paste("Cavemount soil temperature"))), x="Day of the year", y = "Soil temperature (°C)")+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOY_Tb<-ggplot(Cavemount.fluxT, aes(x=DOY, y=soil_T, colour=type))+
  geom_line(size=1)+
  scale_y_continuous(breaks = seq(0, 32, by =5))+
  coord_cartesian(ylim = c(0, 32))+
  scale_x_continuous(breaks = seq(0, 365, by =20))+
  scale_colour_manual(name="", values=c('Bare Peat'="black", 'Birch Scrub'="chocolate", 'Eriophorum'="green2"))+
  labs(title = (expression(paste("Cavemount soil temperature - mean values"))), x="Day of the year", y = "Soil temperature (°C)")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=4)+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOY_moi<-ggplot(Cavemount.flux_moi, aes(x=DOY, y=moi, colour=Year))+
  geom_point(size=1)+
  stat_smooth(method=loess)+
  scale_y_continuous(breaks = seq(0, 0.8, by =0.1))+
  coord_cartesian(ylim = c(0, 0.8))+
  scale_x_continuous(breaks = seq(0, 365, by =20))+
  labs(title = (expression(paste("Cavemount soil water content"))), x="Day of the year", y = expression(paste ("Volumetric water content ( ", m^-3 ," ", m^-3, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

moi_year<-ggplot(Cavemount.flux_moi, aes(x=Year, y=moi, colour=type))+
  geom_boxplot(width=.4, outlier.size=1, lwd=1)+
  scale_colour_manual(name="type", values=c('Bare Peat'="black", 'Birch Scrub'="chocolate", 'Eriophorum'="green2"))+
  scale_y_continuous(breaks = seq(0, 0.9, by =0.1))+
  coord_cartesian(ylim = c(0, 0.9, by=0.1))+
  labs(title = (expression(paste("Cavemount soil water content"))), x="Year", y = expression(paste ("Volumetric water content ( ", m^-3 ," ", m^-3, ")", sep="")))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=2, stroke=1.5, 
               position=position_dodge(0.4))+
  theme_bw()+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_blank(),
        strip.text = element_text(size = 10),
        legend.key.size = unit(1, "cm"))

moi_year_na <- moi_year + guides(colour = guide_legend(override.aes = list(shape = NA)))
print(moi_year_na)

DOY_moi_b<-ggplot(Cavemount.flux_moi, aes(x=DOY, y=moi, colour=type))+
  geom_point(size=1)+
  stat_smooth(method=loess, alpha=0.2)+
  scale_y_continuous(breaks = seq(0, 0.8, by =0.1))+
  coord_cartesian(ylim = c(0, 0.8))+
  scale_x_continuous(breaks = seq(0, 365, by =20))+
  scale_colour_manual(name="", values=c('Bare Peat'="black", 'Birch Scrub'="chocolate", 'Eriophorum'="green2"))+
  labs(title = (expression(paste("Cavemount soil water content - mean values"))), x="Day of the year", y = expression(paste ("Volumetric water content ( ", m^-3 ," ", m^-3, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOY_moi_facet<-ggplot(Cavemount.flux_moi, aes(x=DOY, y=moi, colour=type))+
  facet_wrap(~Plot)+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.2)+
  scale_y_continuous(breaks = seq(0, 0.8, by =0.2))+
  coord_cartesian(ylim = c(0, 0.8))+
  scale_x_continuous(breaks = seq(0, 365, by =60))+
  scale_colour_manual(name="", values=c('Bare Peat'="black", 'Birch Scrub'="chocolate", 'Eriophorum'="green2"))+
  labs(title = (expression(paste("Cavemount soil water content by collar"))), x="Day of the year", y = expression(paste ("Volumetric water content ( ", m^-3 ," ", m^-3, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 11, colour='black'),
        axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        axis.title.x = element_text(size = 15, vjust = -1),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOY_moi_facet_Year<-ggplot(Cavemount.flux_moi, aes(x=DOY, y=moi, colour=type))+
  facet_wrap(~Year, ncol=4)+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.1)+
  scale_y_continuous(breaks = seq(0, 0.85, by =0.1))+
  coord_cartesian(ylim = c(0, 0.85))+
  scale_x_continuous(breaks = seq(0, 365, by =40))+
  scale_colour_manual(name="", values=c('Bare Peat'="black", 'Birch Scrub'="chocolate", 'Eriophorum'="green2"))+
  labs(title = (expression(paste("Cavemount soil water content by vegetation type"))), x="Day of the year", y = expression(paste ("Volumetric water content ( ", m^-3 ," ", m^-3, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 9, colour='black'),
        axis.text.x = element_text(size = 9, angle = 90, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        axis.title.x = element_text(size = 15, vjust = -1),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOY_T2<-ggplot(Cavemount.fluxT, aes(x=DOY, y=soil_T, colour=Year))+
  geom_point(size=1)+
  scale_y_continuous(breaks = seq(0, 32, by =5))+
  coord_cartesian(ylim = c(0, 32))+
  scale_x_continuous(breaks = seq(0, 365, by =20))+
  labs(title = (expression(paste("Soil temperature at Cavemount"))), x="Day of the year", y = "Soil temperature (°C)") +
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOY_T_facet_Year<-ggplot(Cavemount.fluxT, aes(x=DOY, y=soil_T, colour=type))+
  facet_wrap(~Year, ncol=4)+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.1)+
  scale_y_continuous(breaks = seq(0, 32, by =5))+
  coord_cartesian(ylim = c(0, 32))+
  scale_x_continuous(breaks = seq(0, 365, by =40))+
  scale_colour_manual(name="", values=c('Bare Peat'="black", 'Birch Scrub'="chocolate", 'Eriophorum'="green2"))+
  labs(title = (expression(paste("Cavemount soil temperature by site"))), x="Day of the year", y = "Soil temperature (°C)")+
  theme_bw()+
  theme(axis.text = element_text(size = 9, colour='black'),
        axis.text.x = element_text(size = 9, angle = 90, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        axis.title.x = element_text(size = 15, vjust = -1),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

T_moi_impact<-ggplot(Cavemount.flux_moi, aes(x=soil_T, y=moi))+
  geom_point(size=1.5)+
  stat_smooth(method=lm, colour="red")+
  scale_y_continuous(breaks = seq(0, 0.8, by =0.1))+
  coord_cartesian(ylim = c(0, 0.8))+
  scale_x_continuous(breaks = seq(0, 40, by =5))+
  labs(title = (expression(paste("Soil temperature influence on soil water content (Cavemount)"))), x="Soil temperature (°C)", y = expression(paste ("Volumetric water content ( ", m^-3 ," ", m^-3, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

T_moi_impact_type<-ggplot(Cavemount.flux_moi, aes(x=soil_T, y=moi, colour=type))+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.2)+
  scale_y_continuous(breaks = seq(0, 0.8, by =0.1))+
  coord_cartesian(ylim = c(0, 0.8))+
  scale_x_continuous(breaks = seq(0, 40, by =5))+
  scale_colour_manual(name="", values=c('Bare Peat'="black", 'Birch Scrub'="chocolate", 'Eriophorum'="green2"))+
  labs(title = (expression(paste("Soil temperature influence on soil water content by site (Cavemount)"))), x="Soil temperature (°C)", y = expression(paste ("Volumetric water content ( ", m^-3 ," ", m^-3, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

###########################

Month_CO2<-ggplot(Cavemount.flux, aes(x=Month, y=CO2, colour=Year))+
  geom_point(size=1)+
  stat_smooth(method=loess)+
  scale_y_continuous(breaks = seq(0, 12, by =1))+
  coord_cartesian(ylim = c(0, 12))+
  scale_x_continuous(breaks = seq(1, 12, by =1))+
  labs(title = (expression(paste("ER flux from Cavemount"))), x="Month", y = expression(paste ("ER flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10),
        legend.key.size = unit(1, "cm"))

mo1 <- Month_CO2 + guides(colour = guide_legend(override.aes = list(size = 2, shape = NA)))
print(mo1)


DOY_CO2_shape<-ggplot(Cavemount.flux, aes(x=DOY, y=CO2, colour=type, shape=Year))+
  geom_point(size=2)+
  stat_smooth(method=loess, alpha=0.2)+
  scale_y_continuous(breaks = seq(0, 12, by =1))+
  coord_cartesian(ylim = c(0, 12))+
  scale_x_continuous(breaks = seq(0, 365, by =20))+
  scale_colour_manual(name="", values=c('Bare Peat'="black", 'Birch Scrub'="chocolate", 'Eriophorum'="green2"))+
  scale_shape_manual(name="Year", values=c('2020'=0, '2021'=17, '2022'=16, '2023'=10))+
  labs(title = (expression(paste("ER flux from Cavemount"))), x="Day of the year", y = expression(paste ("ER flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(size = 11, angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10),
        legend.key = element_rect(fill = 'transparent'))

ca <- DOY_CO2_shape + guides(shape = guide_legend(override.aes = list(size = 4)))
print(ca)


DOY_CO2_facet<-ggplot(Cavemount.flux, aes(x=DOY, y=CO2, colour=type))+
  facet_wrap(~Year)+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.2)+
  scale_y_continuous(breaks = seq(0, 12, by =2))+
  coord_cartesian(ylim = c(0, 12))+
  scale_x_continuous(breaks = seq(0, 365, by =60))+
  scale_colour_manual(name="", values=c('Bare Peat'="black", 'Birch Scrub'="chocolate", 'Eriophorum'="green2"))+
  labs(title = (expression(paste("ER flux from Cavemount by year"))), x="Day of the year", y = expression(paste ("ER flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 11, colour='black'),
        axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

doy_a <- DOY_CO2_facet + 
guides(colour = guide_legend(override.aes = list(size = 2, shape = NA, fill=NA)))
print(doy_a)


DOY_CO2_collar<-ggplot(Cavemount.flux, aes(x=DOY, y=CO2, colour=type))+
  facet_wrap(~Plot)+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.3)+
  scale_y_continuous(breaks = seq(0, 14, by =2))+
  coord_cartesian(ylim = c(0, 14))+
  scale_x_continuous(breaks = seq(0, 365, by =60))+
  scale_colour_manual(name="", values=c('Bare Peat'="black", 'Birch Scrub'="chocolate", 'Eriophorum'="green2"))+
  labs(title = (expression(paste("ER flux from Cavemount by collar"))), x="Day of the year", y = expression(paste ("ER flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 11, colour='black'),
        axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        axis.title.x = element_text(size = 15, vjust = -1),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))


CO2_collar<-ggplot(Cavemount.flux, aes(x=Plot, y=CO2, colour=type))+
  geom_boxplot(width=.4, outlier.size=1, lwd=1)+
  scale_y_continuous(breaks = seq(0, 14, by =2))+
  coord_cartesian(ylim = c(0, 14))+
  scale_colour_manual(name="", values=c('Bare Peat'="black", 'Birch Scrub'="chocolate", 'Eriophorum'="green2"))+
  labs(title = (expression(paste("ER flux from Cavemount by vegetation type"))), x="Collar", y = expression(paste ("ER flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=2, stroke=1.1, colour='black')+
  theme_bw()+
  theme(axis.text = element_text(size = 13, colour='black'),
        axis.text.x = element_text(size=10, angle = 310, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        axis.title.x = element_text(size = 15, vjust = -1),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))


dateCO2<-ggplot(Cavemount.flux, aes(x=Date, y=CO2, colour=type))+
  geom_point(size=2)+
  scale_y_continuous(breaks = seq(0, 12, by =1))+
  coord_cartesian(ylim = c(0, 12))+
  scale_colour_manual(name="", values=c('Bare Peat'="black", 'Birch Scrub'="chocolate", 'Eriophorum'="green2"))+
  scale_x_date(date_breaks = "30 day", date_labels = "%d/%m/%Y")+
  labs(title = (expression(paste("ER flux from Cavemount by vegetation type"))), x="Date", y = expression(paste ("ER flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOYCO2type<-ggplot(Cavemount.flux, aes(x=DOY, y=CO2, colour=type))+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.3)+
  facet_wrap(~type)+
  scale_y_continuous(breaks = seq(0, 14, by =2))+
  coord_cartesian(ylim = c(0, 14), xlim = c(0, 365))+
  scale_x_continuous(breaks = seq(0, 365, by =40))+
  scale_colour_manual(name="", values=c('Bare Peat'="black", 'Birch Scrub'="chocolate", 'Eriophorum'="green2"))+
  labs(title = (expression(paste("ER flux from Cavemount by cover"))), x="DOY", y = expression(paste ("ER flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

moi_CO2type<-ggplot(Cavemount.flux_moi, aes(x=moi, y=CO2, colour=type))+
  geom_point(size=1.5)+
  stat_smooth(method=loess)+
  scale_y_continuous(breaks = seq(0, 12, by =1))+
  coord_cartesian(ylim = c(0, 12))+
  scale_x_continuous(breaks = seq(0, 0.8, by =0.1))+
  scale_colour_manual(name="", values=c('Bare Peat'="black", 'Birch Scrub'="chocolate", 'Eriophorum'="green2"))+
  labs(title = (expression(paste("Cavemount ", CO[2], " flux by cover type"))), x=expression(paste ("Volumetric water content ( ", m^-3 ," ", m^-3, ")", sep="")), y = expression(paste ("ER flux (??mol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

T_CO2type<-ggplot(Cavemount.fluxT, aes(x=soil_T, y=CO2, colour=type))+
  geom_point(size=1.5)+
  stat_smooth(method=loess)+
  scale_y_continuous(breaks = seq(0, 12, by =1))+
  coord_cartesian(ylim = c(0, 12))+
  scale_x_continuous(breaks = seq(0, 32, by =5))+
  scale_colour_manual(name="", values=c('Bare Peat'="black", 'Birch Scrub'="chocolate", 'Eriophorum'="green2"))+
  labs(title = (expression(paste("Cavemount ", CO[2], " flux by cover type"))), x="Soil temperature (°C)", y = expression(paste ("ER flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOY_CO2_year<-ggplot(Cavemount.flux, aes(x=DOY, y=CO2, colour=Year))+
  geom_point(size=1.5)+
  stat_smooth(method=loess)+
  scale_y_continuous(breaks = seq(0, 14, by =2))+
  coord_cartesian(ylim = c(0, 14))+
  scale_x_continuous(breaks = seq(0, 365, by =40))+
  labs(title = (expression(paste("Average Cavemount ", CO[2], " flux by year"))), x="Day of the year", y = expression(paste ("ER flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title=element_blank(),
        strip.text = element_text(size = 10),
        legend.key.size = unit(1, "cm"))

yearco2 <- DOY_CO2_year + 
guides(colour = guide_legend(override.aes = list(size = 2, shape = NA, fill=NA)))
print(yearco2)

DOYCO2type<-ggplot(Cavemount.flux, aes(x=DOY, y=CO2, colour=type))+
  geom_point(size=1.5)+
  stat_smooth(method=loess)+
  scale_y_continuous(breaks = seq(0, 12, by =1))+
  coord_cartesian(ylim = c(0, 12))+
  scale_x_continuous(breaks = seq(0, 365, by =30))+
  scale_colour_manual(name="", values=c('Bare Peat'="black", 'Birch Scrub'="chocolate", 'Eriophorum'="green2"))+
  labs(title = (expression(paste("Cavemount ", CO[2], " flux by cover type"))), x="Day of the year", y = expression(paste ("ER flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(size = 11, angle = 0, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

co2t <- DOYCO2type + 
guides(colour = guide_legend(override.aes = list(size = 2, shape = NA, fill=NA)))
print(co2t)
#############################

CH4_collar<-ggplot(Cavemount.flux, aes(x=Plot, y=CH4, colour=type))+
  geom_boxplot(width=.4, outlier.size=1, lwd=1)+
  scale_y_continuous(breaks = seq(-2, 20, by =2))+
  coord_cartesian(ylim = c(-2, 20))+
  scale_colour_manual(name="", values=c('Bare Peat'="black", 'Birch Scrub'="chocolate", 'Eriophorum'="green2"))+
  labs(title = (expression(paste("Cavemount ", CH[4], " flux by cover type"))), x="Collar", y = expression(paste ("", CH[4]," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=2, stroke=1.3, colour='black')+
  theme_bw()+
  theme(axis.text = element_text(size = 13, colour='black'),
        axis.text.x = element_text(size = 11, angle = 310, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        axis.title.x = element_text(size = 15, vjust = -1),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOY_CH4_collar<-ggplot(Cavemount.flux, aes(x=DOY, y=CH4, colour=type))+
  facet_wrap(~Plot)+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.3)+
  scale_y_continuous(breaks = seq(-2, 22, by =4))+
  coord_cartesian(ylim = c(-2, 22))+
  scale_x_continuous(breaks = seq(0, 365, by =60))+
  scale_colour_manual(name="", values=c('Bare Peat'="black", 'Birch Scrub'="chocolate", 'Eriophorum'="green2"))+
  labs(title = (expression(paste("Cavemount ", CH[4], " flux by collar"))), x="Day of the year", y = expression(paste ("", CH[4]," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 10, colour='black'),
        axis.text.x = element_text(size = 11, angle = 310, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        axis.title.x = element_text(size = 15, vjust = -1),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOY_CH4_co <- DOY_CH4_collar + 
  guides(colour = guide_legend(override.aes = list(size = 2, shape = NA, fill=NA)))
print(DOY_CH4_co)


DOYCH4type_facet<-ggplot(Cavemount.flux, aes(x=DOY, y=CH4, colour=type))+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.3)+
  facet_wrap(~type)+
  scale_y_continuous(breaks = seq(-4, 22, by =4))+
  coord_cartesian(ylim = c(-4, 22))+
  scale_x_continuous(breaks = seq(0, 365, by =40))+
  scale_colour_manual(name="", values=c('Bare Peat'="black", 'Birch Scrub'="chocolate", 'Eriophorum'="green2"))+
  labs(title = (expression(paste("Cavemount ", CH[4], " flux by cover type"))), x="DOY", y = expression(paste ("", CH[4]," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

dateCH4<-ggplot(Cavemount.flux, aes(x=Date, y=CH4, colour=type))+
  geom_point(size=2)+
  scale_y_continuous(breaks = seq(-3, 8, by =1))+
  coord_cartesian(ylim = c(-3, 8))+
  scale_colour_manual(name="", values=c('Bare Peat'="black", 'Birch Scrub'="chocolate", 'Eriophorum'="green2"))+
  scale_x_date(date_breaks = "30 day", date_labels = "%d/%m/%Y")+
  labs(title = (expression(paste("Cavemount ", CH[4], " flux by cover type"))), x="Date", y = expression(paste ("", CH[4]," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOYCH4type<-ggplot(Cavemount.flux, aes(x=DOY, y=CH4, colour=type))+
  facet_wrap(~Year)+
  geom_point(size=1.5)+
  stat_smooth(method=loess)+
  scale_y_continuous(breaks = seq(-4, 21, by =2))+
  coord_cartesian(ylim = c(-4, 21))+
  scale_x_continuous(breaks = seq(0, 365, by =40))+
  scale_colour_manual(name="", values=c('Bare Peat'="black", 'Birch Scrub'="chocolate", 'Eriophorum'="green2"))+
  labs(title = (expression(paste("Cavemount ", CH[4], " flux by cover type"))), x="Day of the year", y = expression(paste ("", CH[4]," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 11),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

ch4t <- DOYCH4type + 
guides(colour = guide_legend(override.aes = list(size = 2, shape = NA, fill=NA)))
print(ch4t)

DOY_CH4_year<-ggplot(Cavemount.flux, aes(x=DOY, y=CH4, colour=Year))+
  geom_point(size=1.5)+
  stat_smooth(method=loess)+
  scale_y_continuous(breaks = seq(-4, 21, by =2))+
  coord_cartesian(ylim = c(-4, 21))+
  scale_x_continuous(breaks = seq(0, 365, by =40))+
  labs(title = (expression(paste("Average Cavemount ", CH[4], " flux by year"))), x="Day of the year", y = expression(paste ("", CH[4]," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10),
        legend.key.size = unit(1, "cm"))

yearch4 <- DOY_CH4_year + 
guides(colour = guide_legend(override.aes = list(size = 2, shape = NA, fill=NA)))
print(yearch4)

DOY_CH4_facet<-ggplot(Cavemount.flux, aes(x=DOY, y=CH4, colour=type))+
  facet_wrap(~Year)+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.2)+
  scale_y_continuous(breaks = seq(-2, 22, by =2))+
  coord_cartesian(ylim = c(-2, 22))+
  scale_x_continuous(breaks = seq(0, 365, by =40))+
  scale_colour_manual(name="", values=c('Bare Peat'="black", 'Birch Scrub'="chocolate", 'Eriophorum'="green2"))+
  labs(title = (expression(paste("Cavemount ", CH[4], " flux by cover type and year"))), x="Day of the year", y = expression(paste ("", CH[4]," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 11, colour='black'),
        axis.text.x = element_text(size = 10, angle = 310, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

doy_ch4 <- DOY_CH4_facet + 
guides(colour = guide_legend(override.aes = list(size = 2, shape = NA, fill=NA)))
print(doy_ch4)

moi_CH4type<-ggplot(Cavemount.flux_moi, aes(x=moi, y=CH4, colour=type))+
  geom_point(size=1.5)+
  stat_smooth(method=loess)+
  scale_y_continuous(breaks = seq(-4, 22, by =2))+
  coord_cartesian(ylim = c(-4, 22), xlim=c(0, 0.9))+
  scale_x_continuous(breaks = seq(0, 0.9, by =0.1))+
  scale_colour_manual(name="", values=c('Bare Peat'="black", 'Birch Scrub'="chocolate", 'Eriophorum'="green2"))+
  labs(title = (expression(paste("Cavemount ", CH[4], " flux by cover type"))), x=expression(paste ("Volumetric water content ( ", m^-3 ," ", m^-3, ")", sep="")), y = expression(paste ("", CH[4]," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

moi_CH4t <- moi_CH4type + 
guides(colour = guide_legend(override.aes = list(size = 2, shape = NA, fill=NA)))
print(moi_CH4t)

T_CH4type<-ggplot(Cavemount.fluxT, aes(x=soil_T, y=CH4, colour=type))+
  geom_point(size=1.5)+
  stat_smooth(method=loess)+
  scale_y_continuous(breaks = seq(-4, 22, by =2))+
  coord_cartesian(ylim = c(-4, 22), xlim=c(0, 32))+
  scale_x_continuous(breaks = seq(0, 30, by =5))+
  scale_colour_manual(name="", values=c('Bare Peat'="black", 'Birch Scrub'="chocolate", 'Eriophorum'="green2"))+
  labs(title = (expression(paste("Cavemount ", CH[4], " flux by cover type"))), x="Soil temperature (°C)", y = expression(paste ("", CH[4]," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

T_CH4t <- T_CH4type + 
  guides(colour = guide_legend(override.aes = list(size = 2, shape = NA, fill=NA)))
print(T_CH4t)

T_CH4type_year<-ggplot(Cavemount.fluxT, aes(x=soil_T, y=CH4, colour=type))+
  facet_wrap(~Year)+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.1)+
  scale_y_continuous(breaks = seq(-4, 22, by =2))+
  coord_cartesian(ylim = c(-4, 22), xlim=c(0, 32))+
  scale_x_continuous(breaks = seq(0, 30, by =5))+
  scale_colour_manual(name="", values=c('Bare Peat'="black", 'Birch Scrub'="chocolate", 'Eriophorum'="green2"))+
  labs(title = (expression(paste("Relationship between soil temperature and ", CH[4], " flux by year (Cavemount)"))), x="Soil temperature (°C)", y = expression(paste ("", CH[4]," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

T_CH4type_y <- T_CH4type_year + 
guides(colour = guide_legend(override.aes = list(size = 2, shape = NA, fill=NA)))
print(T_CH4type_y)

moi_CH4type_year<-ggplot(Cavemount.flux_moi, aes(x=moi, y=CH4, colour=type))+
  facet_wrap(~Year)+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.1)+
  scale_y_continuous(breaks = seq(-4, 22, by =2))+
  coord_cartesian(ylim = c(-4, 22), xlim=c(0, 0.9))+
  scale_x_continuous(breaks = seq(0, 0.9, by =0.1))+
  scale_colour_manual(name="", values=c('Bare Peat'="black", 'Birch Scrub'="chocolate", 'Eriophorum'="green2"))+
  labs(title = (expression(paste("Relationship between soil water content and ", CH[4], " flux by year (Cavemount)"))), x=expression(paste ("Volumetric water content ( ", m^-3 ," ", m^-3, ")", sep="")), y = expression(paste ("", CH[4]," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(size=11, colour='black', angle = 270, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

moi_CH4type_y <- moi_CH4type_year + 
  guides(colour = guide_legend(override.aes = list(size = 2, shape = NA, fill=NA)))
print(moi_CH4type_y)

T_CO2type_year<-ggplot(Cavemount.fluxT, aes(x=soil_T, y=CO2, colour=type))+
  facet_wrap(~Year)+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.1)+
  scale_y_continuous(breaks = seq(0, 14, by =2))+
  coord_cartesian(ylim = c(0, 14), xlim=c(0, 30))+
  scale_x_continuous(breaks = seq(0, 30, by =5))+
  scale_colour_manual(name="", values=c('Bare Peat'="black", 'Birch Scrub'="chocolate", 'Eriophorum'="green2"))+
  labs(title = (expression(paste("Relationship between soil temperature and ", CO[2], " flux by year (Cavemount)"))), x="Soil temperature (°C)", y = expression(paste ("ER flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

T_CO2type_y <- T_CO2type_year + 
  guides(colour = guide_legend(override.aes = list(size = 2, shape = NA, fill=NA)))
print(T_CO2type_y)

moi_CO2type_year<-ggplot(Cavemount.flux_moi, aes(x=moi, y=CO2, colour=type))+
  facet_wrap(~Year)+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.1)+
  scale_y_continuous(breaks = seq(0, 14, by =2))+
  coord_cartesian(ylim = c(0, 14), xlim=c(0, 0.9))+
  scale_x_continuous(breaks = seq(0, 0.9, by =0.1))+
  scale_colour_manual(name="", values=c('Bare Peat'="black", 'Birch Scrub'="chocolate", 'Eriophorum'="green2"))+
  labs(title = (expression(paste("Relationship between soil water content and ", CO[2], " flux by year (Cavemount)"))), x=expression(paste ("Volumetric water content ( ", m^-3 ," ", m^-3, ")", sep="")), y = expression(paste ("ER flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(size = 10, 
        angle = 270, hjust = 0.5, vjust = 0.5, colour='black'),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

moi_CO2type_y <- moi_CO2type_year + 
  guides(colour = guide_legend(override.aes = list(size = 2, shape = NA, fill=NA)))
print(moi_CO2type_y)

Cavemount.flux_WT<-within(Cavemount.flux_WT, Plot <- factor(Plot, 
levels=c('BP1', 'E3', 'BP6', 'BP8', 'BS9','BS10', 'BS11', 'BS12')))

DOY_WT<-ggplot(Cavemount.flux_WT, aes(x=DOY, y=WT))+
  facet_wrap(~Plot)+
  geom_line(size=2, colour='lightblue2')+
  geom_point(size=2, colour='navyblue')+
  scale_y_continuous(breaks = seq(-80, 0, by =10))+
  coord_cartesian(ylim = c(-80, 0), xlim=c(0, 360))+
  scale_x_continuous(breaks = seq(0, 365, by =40))+
  labs(title = (expression(paste("Cavemount water table changes"))), x="Day of the year", y = expression(paste ("Water table depth (cm)")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(size=11, angle = 310, hjust = 0.5, vjust = 0, colour='black'),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOY_WT2<-ggplot(Cavemount.flux_WT, aes(x=DOY, y=WT, colour=type))+
  facet_wrap(~Year)+
  geom_line(size=2, colour='lightblue2')+
  geom_point(size=2, colour='navyblue')+
  scale_y_continuous(breaks = seq(-80, 0, by =10))+
  coord_cartesian(ylim = c(-80, 0), xlim=c(0, 360))+
  scale_x_continuous(breaks = seq(0, 365, by =40))+
  labs(title = (expression(paste("Cavemount water table changes"))), x="Day of the year", y = expression(paste ("Water table depth (cm)")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(size=11, angle = 310, hjust = 0.5, vjust = 0, colour='black'),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

CH4_WT<-ggplot(Cavemount.flux_WT, aes(x=WT, y=CH4))+
  facet_wrap(~Plot)+
  stat_smooth(method=loess, alpha=0.2, size=1, colour='lightblue2')+
  geom_point(size=2, colour='navyblue')+
  scale_y_continuous(breaks = seq(-4, 22, by =4))+
  coord_cartesian(ylim = c(-4, 22), xlim=c(-70, 0))+
  scale_x_continuous(breaks = seq(-70, 0, by =10))+
  labs(title = (expression(paste("Relationship between water table depth and ", CH[4], " flux (Cavemount)"))), x="Water table (cm)", y = expression(paste ("", CH[4]," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(size=11, angle = 310, hjust = 0.5, vjust = 0, colour='black'),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

CH4_WT_type<-ggplot(Cavemount.flux_WT, aes(x=WT, y=CH4))+
  facet_wrap(~type)+
  stat_smooth(method=loess, alpha=0.2, size=1, colour='lightblue2')+
  geom_point(size=2, colour='navyblue')+
  scale_y_continuous(breaks = seq(-4, 22, by =4))+
  coord_cartesian(ylim = c(-4, 22), xlim=c(-70, 0))+
  scale_x_continuous(breaks = seq(-70, 0, by =10))+
  labs(title = (expression(paste("Relationship between water table depth and ", CH[4], " flux (Cavemount)"))), x="Water table (cm)", y = expression(paste ("", CH[4]," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(size=11, angle = 310, hjust = 0.5, vjust = 0, colour='black'),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

ER_WT<-ggplot(Cavemount.flux_WT, aes(x=WT, y=CO2))+
  facet_wrap(~Plot)+
  stat_smooth(method=loess, alpha=0.2, size=1, colour='lightblue2')+
  geom_point(size=2, colour='navyblue')+
  scale_y_continuous(breaks = seq(0, 14, by =2))+
  coord_cartesian(ylim = c(0, 14), xlim=c(-70, 0))+
  scale_x_continuous(breaks = seq(-70, 0, by =10))+
  labs(title = (expression(paste("Relationship between water table depth and ", CO[2], " flux (Cavemount)"))), x="Water table (cm)", y = expression(paste ("", CO[2]," flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(size=11, angle = 310, hjust = 0.5, vjust = 0, colour='black'),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

moi_WT<-ggplot(Cavemount.flux_WT_moi, aes(x=WT, y=moi))+
  stat_smooth(method=loess, alpha=0.2, size=1, colour='lightblue2')+
  geom_point(size=2, colour='navyblue')+
  scale_y_continuous(breaks = seq(0, 0.9, by =0.1))+
  coord_cartesian(ylim = c(0, 0.9), xlim=c(-70, 0))+
  scale_x_continuous(breaks = seq(-70, 0, by =10))+
  labs(title = (expression(paste("Water table impact on soil water content (Cavemount)"))), x="Water table (cm)", y=expression(paste ("Volumetric water content ( ", m^-3 ," ", m^-3, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(size=12, angle = 0, hjust = 0.5, vjust = 0, colour='black'),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

moi_WTtype<-ggplot(Cavemount.flux_WT_moi, aes(x=WT, y=moi, colour=type))+
  stat_smooth(method=loess, alpha=0.2, size=1)+
  geom_point(size=1.5)+
  scale_y_continuous(breaks = seq(0, 0.9, by =0.1))+
  coord_cartesian(ylim = c(0, 0.9), xlim=c(-80, 0))+
  scale_x_continuous(breaks = seq(-80, 0, by =10))+
  scale_colour_manual(name="", values=c('Bare Peat'="black", 'Birch Scrub'="chocolate", 'Eriophorum'="green2"))+
  labs(title = (expression(paste("Water table impact on soil water content (Cavemount)"))), x="Water table (cm)", y=expression(paste ("Volumetric water content ( ", m^-3 ," ", m^-3, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(size=12, angle = 0, hjust = 0.5, vjust = 0, colour='black'),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))



Cavemount.flux_WT <- data.frame(subset(Cavemount.flux_WT, !(Plot %in% c('E5B', 'E7B', 'BP2'))))

##############################
biomass$Water.table<-as.factor(biomass$Water.table)
biomass<-filter(biomass, Notes!="rotting")
dry<-filter(biomass, Dry.wet=="DRY")
wet<-filter(biomass, Dry.wet=="WET")
dryr<-filter(biomass, Dry.wet=="DRY" & Total.root!="NA")
dryroottotal<-filter(biomass, Dry.wet=="DRY" & Total.root!="NA")
dry_taproot<-filter(biomass, Dry.wet=="DRY")
##############
sumbio<-summarise(group_by(wet, Water.table), mean_bio=mean(Leaves))
sumbio<-summarise(group_by(root_total, Temperature), mean_bio=mean(Total))

########################


wetleaf<-ggplot(wet, aes(x=Water.table, y=Leaves))+
  geom_boxplot(width=.2, outlier.size=0.8)+
  scale_x_discrete(name="Water table", breaks=c("30", "40", "50"),labels=c("-30 cm","-40 cm", "-50 cm"))+
  scale_y_continuous(breaks = seq(0, 200 ,by =20))+
  coord_cartesian(ylim = c(0, 200))+
  labs(title = "Wet leaf biomass (g)", x="Water table (cm)", y ="Weight (g)")+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=2)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

dryleaf<-ggplot(dry, aes(x=Water.table, y=Leaves))+
  geom_boxplot(width=.2, outlier.size=0.8)+
  scale_x_discrete(name="Water table", breaks=c("30", "40", "50"),labels=c("-30 cm","-40 cm", "-50 cm"))+
  scale_y_continuous(breaks = seq(0, 30 ,by =10))+
  coord_cartesian(ylim = c(0, 30))+
  labs(title = "Dry leaf biomass (g)", x="Water table (cm)", y ="Weight (g)")+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=2)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

dryleaf2<-ggplot(dry, aes(x=Temperature, y=Leaves))+
  geom_boxplot(width=.2, outlier.size=0.8)+
  scale_x_discrete(name="Temperature")+
  scale_y_continuous(breaks = seq(0, 30 ,by =10))+
  coord_cartesian(ylim = c(0, 30))+
  labs(title = "Dry leaf biomass (g)", x="Temperature", y ="Weight (g)")+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=2)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

wetleaf2<-ggplot(wet, aes(x=Temperature, y=Leaves))+
  geom_boxplot(width=.2, outlier.size=0.8)+
  scale_x_discrete(name="Temperature")+
  scale_y_continuous(breaks = seq(0, 200 ,by =20))+
  coord_cartesian(ylim = c(0, 200))+
  labs(title = "Wet leaf biomass (g)", x="Temperature", y ="Weight (g)")+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=2)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

taproot1<-ggplot(dry, aes(x=Temperature, y=Dry.tap.root))+
  geom_boxplot(width=.2, outlier.size=0.8)+
  scale_x_discrete(name="Temperature")+
  scale_y_continuous(breaks = seq(0, 6 ,by =1))+
  coord_cartesian(ylim = c(0, 6))+
  labs(title = "Dry tap root biomass (g)", x="Temperature", y ="Weight (g)")+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=2)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

taproot2<-ggplot(dry, aes(x=Water.table, y=Dry.tap.root))+
  geom_boxplot(width=.2, outlier.size=0.8)+
  scale_x_discrete(name="Water table", breaks=c("30", "40", "50"),labels=c("-30 cm","-40 cm", "-50 cm"))+
  scale_y_continuous(breaks = seq(0, 6 ,by =1))+
  coord_cartesian(ylim = c(0, 6))+
  labs(title = "Dry tap root biomass (g)", x="Water table (cm)", y ="Weight (g)")+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=2)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

totalroot1<-ggplot(dryroottotal, aes(x=Water.table, y=Total.root))+
  geom_boxplot(width=.2, outlier.size=0.8)+
  scale_x_discrete(name="Water table", breaks=c("30", "40", "50"),labels=c("-30 cm","-40 cm", "-50 cm"))+
  scale_y_continuous(breaks = seq(0, 7, by =1))+
  coord_cartesian(ylim = c(0, 7))+
  labs(title = "Dry total root biomass (g)", x="Water table (cm)", y ="Weight (g)")+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=2)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

totalroot2<-ggplot(dryroottotal, aes(x=Temperature, y=Total.root))+
  geom_boxplot(width=.2, outlier.size=0.8)+
  scale_x_discrete(name="Temperature")+
  scale_y_continuous(breaks = seq(0, 7, by =1))+
  coord_cartesian(ylim = c(0, 7))+
  labs(title = "Dry total root biomass (g)", x="Temperature", y ="Weight (g)")+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=2)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))
#################################

taproottrt<-ggplot(dry, aes(x=Treatment, y=Dry.tap.root))+
  geom_boxplot(width=.2, outlier.size=0.8)+
  scale_x_discrete(name="", 
                   breaks=c("30Amb", "40Amb", "50Amb", "30Elev", "40Elev", "50Elev"))+
  scale_y_continuous(breaks = seq(0, 6 ,by =1))+
  coord_cartesian(ylim = c(0, 6))+
  labs(title = "Dry tap root biomass (g)", y ="Weight (g)")+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=2)+
  theme(legend.position="none")+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

drytrt<-ggplot(dry, aes(x=Treatment, y=Leaves))+
  geom_boxplot(width=.2, outlier.size=0.8)+
  scale_x_discrete(name="", 
                   breaks=c("30Amb", "40Amb", "50Amb", "30Elev", "40Elev", "50Elev"))+
  scale_y_continuous(breaks = seq(0, 30 ,by =10))+
  coord_cartesian(ylim = c(0, 30))+
  labs(title = "Dry leaf biomass (g)", y ="Weight (g)")+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=2)+
  theme(legend.position="none")+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

wettrt<-ggplot(wet, aes(x=Treatment, y=Leaves))+
  geom_boxplot(width=.2, outlier.size=0.8)+
  scale_x_discrete(name="", 
                   breaks=c("30Amb", "40Amb", "50Amb", "30Elev", "40Elev", "50Elev"))+
  scale_y_continuous(breaks = seq(0, 200 ,by =20))+
  coord_cartesian(ylim = c(0, 200))+
  labs(title = "Wet leaf biomass (g)", y ="Weight (g)")+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=2)+
  theme(legend.position="none")+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

colwettrt<-ggplot(wet, aes(x=Treatment, y=Leaves, fill=Treatment))+
  geom_boxplot(width=.5, outlier.size=0.8)+
  scale_x_discrete(name="", 
                   breaks=c("30Amb", "40Amb", "50Amb", "30Elev", "40Elev", "50Elev"))+
  scale_y_continuous(breaks = seq(0, 200 ,by =20))+
  coord_cartesian(ylim = c(0, 200))+
  labs(title = "Wet leaf biomass (g)", y ="Weight (g)")+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3)+
  theme(plot.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 18))

roottrt<-ggplot(dryroottotal, aes(x=Treatment, y=Total.root, color=Treatment))+
  geom_boxplot(width=.2, outlier.size=0.9, size=0.8)+
  scale_x_discrete(name="", 
                   breaks=c("30Amb", "40Amb", "50Amb", "30Elev", "40Elev", "50Elev"))+
  scale_y_continuous(breaks = seq(0, 7, by =1))+
  coord_cartesian(ylim = c(0, 7))+
  labs(title = "Dry total root biomass (g)", y ="Weight (g)")+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3, colour="black")+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

colroottrt<-ggplot(dryroottotal, aes(x=Treatment, y=Total.root, fill=Treatment))+
  geom_boxplot(width=.5, outlier.size=0.8)+
  scale_x_discrete(name="", 
                   breaks=c("30Amb", "40Amb", "50Amb", "30Elev", "40Elev", "50Elev"))+
  scale_y_continuous(breaks = seq(0, 7, by =1))+
  coord_cartesian(ylim = c(0, 7))+
  labs(title = "Dry total root biomass (g)", y ="Weight (g)")+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3, colour="black")+
  theme(plot.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 18))

###########################

com<-summarise(group_by(dryr, Treatment),
               mean_root_weight1=mean(Total.root), 
               SEdr1=sd(Total.root)/sqrt(length(Total.root)),
               lwr1r=mean_root_weight1-SEdr1,
               upr1r=mean_root_weight1+SEdr1,
               mean_aboveground1=mean(Leaves),
               SEdt1=sd(Leaves)/sqrt(length(Leaves)),
               lwr1t=mean_aboveground1-SEdt1,
               upr1t=mean_aboveground1+SEdt1) 

limits_root<-aes(ymin=lwr1r, ymax=upr1r)
limits_above<-aes(ymin=lwr1t, ymax=upr1t)

abovebelow1<-ggplot(com, aes(x=Treatment, shape=Treatment))+
  geom_errorbar(limits_root, width=0.1)+
  geom_errorbar(limits_above, width=0.1)+
  geom_point (aes (y=mean_root_weight1, colour="Roots"), size=4)+
  geom_point (aes (y=mean_aboveground1, colour="Leaves"), size=4)+
  scale_colour_manual(name="", values=c('Roots'="black", 'Leaves'="red"))+
  labs(title = "Dry leaves and root biomass (g): treatment comparison", x="", y ="Weight (g)")+
  scale_shape_manual(name="", breaks = c("30Amb", "30Elev", "40Amb", "40Elev", "50Amb", "50Elev"), 
                     labels=c("-30cm Ambient", "-30cm Elevated", "-40cm Ambient", "-40cm Elevated", "-50cm Ambient", "-50cm Elevated"), 
                     values = c(19, 1, 15, 0, 17, 2))+
  scale_y_continuous(breaks = seq(0, 25, by =5))+
  coord_cartesian(ylim = c(0, 25))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        strip.text = element_text(size = 12))

#####################

root<-summarise(group_by(dryroottotal, Treatment, Temperature),
               mean_root_weight1=mean(Total.root), 
               SEdr1=sd(Total.root)/sqrt(length(Total.root)),
               lwr1r=mean_root_weight1-SEdr1,
               upr1r=mean_root_weight1+SEdr1)

limits_root<-aes(ymin=lwr1r, ymax=upr1r)

roots1<-ggplot(root, aes(x=Treatment, y=mean_root_weight1, shape=Treatment, colour=Temperature))+
  geom_errorbar(limits_root, width=0.1)+
  geom_point (aes (y=mean_root_weight1, colour="Ambient"), size=4)+
  geom_point (aes (y=mean_root_weight1, colour="Elevated"), size=4)+
  scale_colour_manual(name="", values=c('Ambient'="blue", 'Elevated'="red"))+
  labs(title = "Dry root biomass (g): treatment comparison", x="", y ="Weight (g)")+
  scale_shape_manual(name="", breaks = c("30Amb", "30Elev", "40Amb", "40Elev", "50Amb", "50Elev"), 
                     labels=c("-30cm Ambient", "-30cm Elevated", "-40cm Ambient", "-40cm Elevated", "-50cm Ambient", "-50cm Elevated"), 
                     values = c(19, 19, 15, 15, 17, 17))+
  scale_y_continuous(breaks = seq(0, 7, by =1))+
  coord_cartesian(ylim = c(0, 7))+
  theme_bw()+
  theme(title = element_text(size=14),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        strip.text = element_text(size = 12))

########################################

shoot<-summarise(group_by(wet, Treatment, Temperature),
                mean_shoot_weight1=mean(Leaves), 
                SEdr1=sd(Leaves)/sqrt(length(Leaves)),
                lwr1r=mean_shoot_weight1-SEdr1,
                upr1r=mean_shoot_weight1+SEdr1)

limits_root<-aes(ymin=lwr1r, ymax=upr1r)

wetshoot<-ggplot(shoot, aes(x=Treatment, y=mean_shoot_weight1, shape=Treatment, colour=Temperature))+
  labs(title = "Wet shoot biomass (g): treatment comparison", x="", y ="Weight (g)")+
  scale_shape_manual(name="", breaks = c("30Amb", "30Elev", "40Amb", "40Elev", "50Amb", "50Elev"), 
                     labels=c("-30cm Ambient", "-30cm Elevated", "-40cm Ambient", "-40cm Elevated", "-50cm Ambient", "-50cm Elevated"), 
                     values = c(19, 19, 15, 15, 17, 17))+
  geom_point (aes (y=mean_shoot_weight1, colour="Ambient"), size=4)+
  geom_point (aes (y=mean_shoot_weight1, colour="Elevated"), size=4)+
  scale_colour_manual(name="", values=c('Ambient'="blue", 'Elevated'="red"))+
  geom_errorbar(limits_root, width=0.1)+
  scale_y_continuous(breaks = seq(0, 180, by =20))+
  coord_cartesian(ylim = c(0, 180))+
  theme_bw()+
  theme(title = element_text(size=14),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        strip.text = element_text(size = 12))

#################################################

dryshoot<-summarise(group_by(dry, Treatment, Temperature),
                 mean_shoot_weight1=mean(Leaves), 
                 SEdr1=sd(Leaves)/sqrt(length(Leaves)),
                 lwr1r=mean_shoot_weight1-SEdr1,
                 upr1r=mean_shoot_weight1+SEdr1)

limits_shoot<-aes(ymin=lwr1r, ymax=upr1r)

wetshoot<-ggplot(dryshoot, aes(x=Treatment, y=mean_shoot_weight1, shape=Treatment, colour=Temperature))+
  labs(title = "Dry shoot biomass (g): treatment comparison", x="", y ="Weight (g)")+
  scale_shape_manual(name="", breaks = c("30Amb", "30Elev", "40Amb", "40Elev", "50Amb", "50Elev"), 
                     labels=c("-30cm Ambient", "-30cm Elevated", "-40cm Ambient", "-40cm Elevated", "-50cm Ambient", "-50cm Elevated"), 
                     values = c(19, 19, 15, 15, 17, 17))+
  geom_point (aes (y=mean_shoot_weight1, colour="Ambient"), size=4)+
  geom_point (aes (y=mean_shoot_weight1, colour="Elevated"), size=4)+
  scale_colour_manual(name="", values=c('Ambient'="blue", 'Elevated'="red"))+
  geom_errorbar(limits_shoot, width=0.1)+
  scale_y_continuous(breaks = seq(0, 25, by =5))+
  coord_cartesian(ylim = c(0, 25))+
  theme_bw()+
  theme(title = element_text(size=14),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        strip.text = element_text(size = 12))

########################

DOYCO2type<-ggplot(Cavemount.flux, aes(x=DOY, y=CO2, colour=type))+
  facet_wrap(~Year)+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.3)+
  scale_y_continuous(breaks = seq(0, 12, by =2))+
  coord_cartesian(ylim = c(0, 12))+
  scale_x_continuous(breaks = seq(0, 365, by =30))+
  scale_colour_manual(name="", values=c('Bare Peat'="black", 'Birch Scrub'="chocolate", 'Eriophorum'="green2"))+
  labs(title = (expression(paste("Cavemount ", CO[2], " flux"))), x="DOY", y = expression(paste ("", CO[2]," flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 11, colour='black'),
        axis.text.x = element_text(size = 9, angle = 90, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOYCO2type_a <- DOYCO2type + 
  guides(colour = guide_legend(override.aes = list(size = 2, shape = NA, fill=NA)))
print(DOYCO2type_a)

DOY_CH4_facet<-ggplot(Cavemount.flux, aes(x=DOY, y=CH4, colour=type))+
  facet_wrap(~Year)+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.2)+
  scale_y_continuous(breaks = seq(-4, 20, by =2))+
  coord_cartesian(ylim = c(-4, 20))+
  scale_x_continuous(breaks = seq(0, 360, by =60))+
  scale_colour_manual(name="", values=c('Bare Peat'="black", 'Birch Scrub'="chocolate", 'Eriophorum'="green2"))+
  labs(title = (expression(paste("Cavemount ", CH[4], " flux"))), x="Day of the year", y = expression(paste ("", CH[4]," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 11, colour='black'),
        axis.text.x = element_text(size = 9, angle = 90, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOY_CH4_facet_b1 <- DOY_CH4_facet + guides(colour = guide_legend(override.aes = list(shape = NA, fill=NA, size=2)))
print(DOY_CH4_facet_b1)

DOY_moi_facet<-ggplot(Cavemount.fluxenv, aes(x=DOY, y=moi, colour=type))+
  facet_wrap(~Year)+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.2)+
  scale_y_continuous(breaks = seq(0, 0.9, by =0.1))+
  coord_cartesian(ylim = c(0, 0.9, by=0.1))+
  scale_x_continuous(breaks = seq(0, 360, by =60))+
  labs(title = (expression(paste("Cavemount soil water content"))), x="Year", y = expression(paste ("Volumetric water content ( ", m^3 ," ", m^-3, ")", sep="")))+
  scale_colour_manual(name="", values=c('Bare Peat'="black", 'Birch Scrub'="chocolate", 'Eriophorum'="green2"))+
  theme_bw()+
  theme(axis.text = element_text(size = 11, colour='black'),
        axis.text.x = element_text(size = 9, angle = 90, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOY_moi_facet_b1 <- DOY_moi_facet + guides(colour = guide_legend(override.aes = list(shape = NA, fill=NA, size=2)))
print(DOY_moi_facet_b1)

DOY_T_facet<-ggplot(Cavemount.fluxenv, aes(x=DOY, y=soil_T, colour=type))+
  facet_wrap(~Year)+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.2)+
  scale_y_continuous(breaks = seq(0, 30, by =5))+
  coord_cartesian(ylim = c(0, 30))+
  scale_x_continuous(breaks = seq(0, 360, by =60))+
  scale_colour_manual(name="", values=c('Bare Peat'="black", 'Birch Scrub'="chocolate", 'Eriophorum'="green2"))+
  labs(title = (expression(paste("Cavemount soil temperature by site"))), x="Day of the year", y = "Soil temperature (°C)") +
  theme_bw()+
  theme(axis.text = element_text(size = 11, colour='black'),
        axis.text.x = element_text(size = 9, angle = 90, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOY_T_facet_a1 <- DOY_T_facet + guides(colour = guide_legend(override.aes = list(shape = NA, fill=NA, size=2)))
print(DOY_T_facet_a1)