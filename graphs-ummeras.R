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
library(forcats)

µ

str(Ummeras.flux)
summary(Ummeras.flux)
select(filter(Ummeras.flux, CH4>100), CH4)
select(filter(Ummeras.flux, type=='Bare Peat Dry'), moi)
sumflux<-summarise(group_by(Ummeras.flux, type), mean_bio=mean(CO2))
sumflux2<-summarise(group_by(Ummeras.flux_WT, Plot), mean_bio=mean(WT))

Ummeras.flux$DOY<-as.numeric(Ummeras.flux$DOY)
Ummeras.flux$WT<-as.numeric(Ummeras.flux$WT)
Ummeras.flux$Year<-as.factor(Ummeras.flux$Year)
Ummeras.flux$Date <- as.Date(Ummeras.flux$Date, "%d/%m/%Y")
Ummeras.flux<-mutate(Ummeras.flux, Date=dmy(Date))

Ummeras.flux$Plot <- fct_collapse(Ummeras.flux$Plot, E1 = c('E1B', 'E1'), 
                                  E2 = c('E2B', 'E2'), M10=c('M10', 'M10B'))


Ummeras.flux<-filter(Ummeras.flux, CO2!="NA" & CH4!="NA")
Ummeras.fluxCH4<-filter(Ummeras.flux, action!="delete")
Ummeras.fluxCH4env<-filter(Ummeras.flux, action!="delete" & soil_T!="NA" & moi!="NA")
Ummeras.fluxCO2<-filter(Ummeras.flux, action!="delete")
Ummeras.fluxCO2env<-filter(Ummeras.flux, action!="delete" & soil_T!="NA" & moi!="NA")
Ummeras.fluxenv<-filter(Ummeras.flux, action!="delete" & soil_T!="NA" & moi!="NA")
Ummeras.fluxT<-filter(Ummeras.flux, action!="delete" & soil_T!="NA")
Ummeras.flux_moi<-filter(Ummeras.flux, action!="delete" & moi!="NA")
Ummeras.flux_WT<-filter(Ummeras.flux, WT!="NA")
Ummeras.flux_WT$WT<-(Ummeras.flux_WT$WT*-1)
Ummeras.flux_WT$Plot <- recode_factor(Ummeras.flux_WT$Plot, E1B = "E1", E2B = "E2", M10B='M10')
Ummeras.flux_WTa<-filter(Ummeras.flux_WT, !(Plot %in% c('W4', 'W3', 'M10', 'S01')))

Ummeras.flux2021<-filter(Ummeras.flux, Year=="2021")
Ummeras.flux2022<-filter(Ummeras.flux, Year=="2022")
Ummeras.flux<-filter(Ummeras.flux, Year!="2023"& type!='Sphagnum')
Ummeras.fluxCH4<-filter(Ummeras.fluxCH4, Year!="2023" & type!='Sphagnum')
Ummeras.fluxCO2<-filter(Ummeras.fluxCO2, Year!="2023" & type!='Sphagnum')

#########################

CO2a<-summarise(group_by(Ummeras.fluxCO2, type),
                meanCO2a=mean(CO2),
                SEa=sd(CO2)/sqrt(length(CO2)),
                lwra=meanCO2a-SEa,
                upra=meanCO2a+SEa)
limits1<-aes(ymin=lwra, ymax=upra)

CO2u1<-ggplot(Ummeras.fluxCO2, aes(x=type, y=CO2, colour=type))+
  geom_boxplot(width=.4, outlier.size=1, lwd=1)+
  scale_colour_manual(name="type", values=c('Bare Peat Dry'="brown4", 'Bare Peat Middle'="darkgoldenrod3", 'Bare Peat Wet'="royalblue", 'Eriophorum' = 'green2', 'Sphagnum'='seagreen4'))+
  scale_x_discrete(name="", breaks=c("Bare Peat Dry", "Bare Peat Middle", "Bare Peat Wet", "Eriophorum", "Sphagnum"), labels=c("Bare Peat \n Dry", "Bare Peat \n Middle", "Bare Peat \n Wet", "Eriophorum", 'Sphagnum'))+
  scale_y_continuous(breaks = seq(0, 6, by =1))+
  coord_cartesian(ylim = c(0, 6))+
  labs(title = (expression(paste("ER flux from Ummeras"))), x="plot", y = expression(paste ("", CO[2]," flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
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

CO2u1<-ggplot(Ummeras.fluxCO2, aes(x=type, y=CO2, colour=type))+
  facet_wrap(~Year)+
  geom_boxplot(width=.4, outlier.size=1, lwd=1)+
  scale_colour_manual(name="type", values=c('Bare Peat Dry'="brown4", 'Bare Peat Middle'="darkgoldenrod3", 'Bare Peat Wet'="royalblue", 'Eriophorum' = 'green2', 'Sphagnum'='seagreen4'))+
  scale_x_discrete(name="", breaks=c("Bare Peat Dry", "Bare Peat Middle", "Bare Peat Wet", "Eriophorum", "Sphagnum"), labels=c("Bare Peat \n Dry", "Bare Peat \n Middle", "Bare Peat \n Wet", "Eriophorum", 'Sphagnum'))+
  scale_y_continuous(breaks = seq(0, 5, by =1))+
  coord_cartesian(ylim = c(0, 5))+
  labs(title = (expression(paste("Ummeras ", CO[2], " flux"))), x="plot", y = expression(paste ("", CO[2]," flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=2, stroke=1.5)+
  theme_bw()+
  theme(axis.text = element_text(size = 11),
        axis.text.x=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 12),
        legend.title = element_blank(),
        strip.text = element_text(size = 10),
        legend.key.size = unit(1, "cm"))

co2_na <- CO2u1 + guides(colour = guide_legend(override.aes = list(shape = NA)))
print(co2_na)

CO2u2<-ggplot(Ummeras.fluxCO2, aes(x=type, y=CO2, fill=type))+
  geom_boxplot(width=.4, outlier.size=1, lwd=1)+
  scale_colour_manual(name="type", values=c('Bare Peat Dry'="brown4", 'Bare Peat Middle'="darkgoldenrod3", 'Bare Peat Wet'="royalblue", 'Eriophorum' = 'green2', 'Sphagnum'='seagreen4'))+
  scale_x_discrete(name="", breaks=c("Bare Peat Dry", "Bare Peat Middle", "Bare Peat Wet", "Eriophorum", "Sphagnum"), labels=c("Bare Peat \n Dry", "Bare Peat \n Middle", "Bare Peat \n Wet", "Eriophorum", 'Sphagnum'))+
  scale_y_continuous(breaks = seq(0, 6, by =1))+
  coord_cartesian(ylim = c(0, 6))+
  labs(title = (expression(paste("ER flux from Ummeras"))), x="plot", y = expression(paste ("", CO[2]," flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3, colour='white', stroke=1.5)+
  theme_bw()+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_blank(),
        strip.text = element_text(size = 10),
        legend.key.size = unit(1, "cm"))

co2_na2 <- CO2u2 + guides(fill = guide_legend(override.aes = list(shape = NA)))
print(co2_na2)


CH4a<-summarise(group_by(Ummeras.fluxCH4, type),
                meanCH4a=mean(CH4),
                SEa=sd(CH4)/sqrt(length(CH4)),
                lwra=meanCH4a-SEa,
                upra=meanCH4a+SEa)
limits1<-aes(ymin=lwra, ymax=upra)

CH4_1<-ggplot(Ummeras.fluxCH4, aes(x=type, y=CH4, colour=type))+
  geom_point(size=1)+
  geom_boxplot(width=.4, outlier.size=1, lwd=1)+
  scale_colour_manual(name="type", values=c('Bare Peat Dry'="brown4", 'Bare Peat Middle'="darkgoldenrod3", 'Bare Peat Wet'="royalblue", 'Eriophorum' = 'green2'))+
  scale_x_discrete(name="", breaks=c("Central", "Marginal", "Sub-Central", "Sub-Marginal"), labels=c("Central", "Marginal", "Sub-\n Central", "Sub-\n Marginal"))+
  scale_y_continuous(breaks = seq(0, 120, by =10))+
  coord_cartesian(ylim = c(0, 120))+
  labs(title = (expression(paste("Ummeras ", CH[4], " flux by location"))), x="plot", y = expression(paste ("", CH[4]," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
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

CH4_2<-ggplot(Ummeras.fluxCH4, aes(x=type, y=CH4, colour=type))+
  facet_wrap(~Year)+
  geom_boxplot(width=.4, outlier.size=1, lwd=1)+
  scale_colour_manual(name="type", values=c('Bare Peat Dry'="brown4", 'Bare Peat Middle'="darkgoldenrod3", 'Bare Peat Wet'="royalblue", 'Eriophorum' = 'green2', 'Sphagnum'='seagreen4'))+
  scale_x_discrete(name="", breaks=c("Bare Peat Dry", "Bare Peat Middle", "Bare Peat Wet", "Eriophorum", "Sphagnum"), labels=c("Bare Peat \n Dry", "Bare Peat \n Middle", "Bare Peat \n Wet", "Eriophorum", 'Sphagnum'))+
  scale_y_continuous(breaks = seq(0, 120, by =10))+
  coord_cartesian(ylim = c(0, 120))+
  labs(title = (expression(paste("Ummeras ", CH[4], " flux"))), x="plot", y = expression(paste ("", CH[4]," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=2, stroke=1.5)+
  theme_bw()+
  theme(axis.text = element_text(size = 11),
        axis.text.x=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 12),
        legend.title = element_blank(),
        strip.text = element_text(size = 10),
        legend.key.size = unit(1, "cm"))

CH4_2_na <- CH4_2 + guides(colour = guide_legend(override.aes = list(shape = NA)))
print(CH4_2_na)

#############

DOY_T<-ggplot(Ummeras.fluxT, aes(x=DOY, y=soil_T, colour=Year))+
  geom_point(size=1)+
  stat_smooth(method=loess)+
  scale_y_continuous(breaks = seq(0, 40, by =5))+
  coord_cartesian(ylim = c(0, 40), xlim=c(0, 360))+
  scale_x_continuous(breaks = seq(0, 365, by =20))+
  labs(title = (expression(paste("Ummeras soil temperature"))), x="Day of the year", y = "Soil temperature (°C)")+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(size=12, angle = 310, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOY_T1 <- DOY_T + guides(colour = guide_legend(override.aes = list(shape = NA, fill=NA, size=2)))
print(DOY_T1)

DOY_Ta<-ggplot(Ummeras.fluxT, aes(x=DOY, y=soil_T, colour=type))+
  geom_point(size=1)+
  stat_smooth(method=loess, alpha=0.2)+
  scale_y_continuous(breaks = seq(0, 40, by =5))+
  coord_cartesian(ylim = c(0, 40))+
  scale_x_continuous(breaks = seq(0, 365, by =20))+
  scale_colour_manual(name="", values=c('Bare Peat Dry'="brown4", 'Bare Peat Middle'="darkgoldenrod3", 'Bare Peat Wet'="royalblue", 'Eriophorum' = 'green2'))+
  labs(title = (expression(paste("Ummeras soil temperature"))), x="Day of the year", y = "Soil temperature (°C)")+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOY_Tb<-ggplot(Ummeras.fluxT, aes(x=DOY, y=soil_T, colour=type))+
  geom_line(size=1)+
  scale_y_continuous(breaks = seq(0, 40, by =5))+
  coord_cartesian(ylim = c(0, 40))+
  scale_x_continuous(breaks = seq(0, 365, by =20))+
  scale_colour_manual(name="", values=c('Bare Peat Dry'="brown4", 'Bare Peat Middle'="darkgoldenrod3", 'Bare Peat Wet'="royalblue", 'Eriophorum' = 'green2'))+
  labs(title = (expression(paste("Ummeras soil temperature - mean values"))), x="Day of the year", y = "Soil temperature (°C)")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=4)+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOY_Tdate<-ggplot(Ummeras.fluxT, aes(x=Date, y=soil_T, colour=Year))+
  geom_point(size=1)+
  stat_smooth(method=loess)+
  scale_y_continuous(breaks = seq(0, 40, by =5))+
  coord_cartesian(ylim = c(0, 40))+
  scale_x_date(date_labels = "%d %B %y")+
  labs(title = (expression(paste("Ummeras soil temperature"))), x="Date", y = "Soil temperature (°C)")+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        axis.title.x = element_text(vjust = -1),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOY_moi<-ggplot(Ummeras.flux_moi, aes(x=DOY, y=moi, colour=Year))+
  geom_point(size=1)+
  stat_smooth(method=loess)+
  scale_y_continuous(breaks = seq(0, 0.85, by =0.1))+
  coord_cartesian(ylim = c(0, 0.85))+
  scale_x_continuous(breaks = seq(0, 365, by =20))+
  labs(title = (expression(paste("Ummeras soil water content"))), x="Day of the year", y = expression(paste ("Volumetric water content ( ", m^-3 ," ", m^-3, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOY_moi1 <- DOY_moi + guides(colour = guide_legend(override.aes = list(shape = NA, fill=NA, size=2)))
print(DOY_moi1)

DOY_moi_b<-ggplot(Ummeras.flux_moi, aes(x=DOY, y=moi, colour=type))+
  geom_point(size=1)+
  stat_smooth(method=loess, alpha=0.2)+
  scale_y_continuous(breaks = seq(0, 0.85, by =0.1))+
  coord_cartesian(ylim = c(0, 0.85), xlim=c(0, 360))+
  scale_x_continuous(breaks = seq(0, 365, by =20))+
  scale_colour_manual(name="", values=c('Bare Peat Dry'="brown4", 'Bare Peat Middle'="darkgoldenrod3", 'Bare Peat Wet'="royalblue", 'Eriophorum' = 'green2'))+
  labs(title = (expression(paste("Ummeras soil water content - mean values"))), x="Day of the year", y = expression(paste ("Volumetric water content ( ", m^-3 ," ", m^-3, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 310, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOY_moi_b1 <- DOY_moi_b + guides(colour = guide_legend(override.aes = list(shape = NA, fill=NA, size=2)))
print(DOY_moi_b1)

DOY_moi_facet<-ggplot(Ummeras.flux_moi, aes(x=DOY, y=moi, colour=type))+
  facet_wrap(~Plot)+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.2)+
  scale_y_continuous(breaks = seq(0, 0.85, by =0.2))+
  coord_cartesian(ylim = c(0, 0.85))+
  scale_x_continuous(breaks = seq(0, 365, by =60))+
  scale_colour_manual(name="", values=c('Bare Peat Dry'="brown4", 'Bare Peat Middle'="darkgoldenrod3", 'Bare Peat Wet'="royalblue", 'Eriophorum' = 'green2'))+
  labs(title = (expression(paste("Ummeras soil water content by collar"))), x="Day of the year", y = expression(paste ("Volumetric water content ( ", m^-3 ," ", m^-3, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 9, colour='black'),
        axis.text.x = element_text(size = 9, angle = 310, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        axis.title.x = element_text(size = 15, vjust = -1),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOY_moi_facet_b1 <- DOY_moi_facet + guides(colour = guide_legend(override.aes = list(shape = NA, fill=NA, size=2)))
print(DOY_moi_facet_b1)

DOY_moi_facet_Year<-ggplot(Ummeras.flux_moi, aes(x=DOY, y=moi, colour=type))+
  facet_wrap(~Year)+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.1)+
  scale_y_continuous(breaks = seq(0, 0.85, by =0.1))+
  coord_cartesian(ylim = c(0, 0.85))+
  scale_x_continuous(breaks = seq(0, 365, by =40))+
  scale_colour_manual(name="", values=c('Bare Peat Dry'="brown4", 'Bare Peat Middle'="darkgoldenrod3", 'Bare Peat Wet'="royalblue", 'Eriophorum' = 'green2'))+
  labs(title = (expression(paste("Ummeras soil water content by site"))), x="Day of the year", y = expression(paste ("Volumetric water content ( ", m^-3 ," ", m^-3, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 9, colour='black'),
        axis.text.x = element_text(size = 9, angle = 90, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        axis.title.x = element_text(size = 15, vjust = -1),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

moi_year<-ggplot(Ummeras.flux_moi, aes(x=Year, y=moi, colour=type))+
  geom_boxplot(width=.4, outlier.size=1, lwd=1)+
  scale_colour_manual(name="", values=c('Bare Peat Dry'="brown4", 'Bare Peat Middle'="darkgoldenrod3", 'Bare Peat Wet'="royalblue", 'Eriophorum' = 'green2', 'Sphagnum'='seagreen4'))+
  scale_y_continuous(breaks = seq(0, 0.9, by =0.1))+
  coord_cartesian(ylim = c(0, 0.9, by=0.1))+
  labs(title = (expression(paste("Ummeras soil water content"))), x="Year", y = expression(paste ("Volumetric water content ( ", m^3 ," ", m^-3, ")", sep="")))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=2, stroke=1.5, position=position_dodge(0.4))+
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

DOY_T2<-ggplot(Ummeras.fluxT, aes(x=DOY, y=soil_T, colour=Year))+
  geom_point(size=1)+
  scale_y_continuous(breaks = seq(0, 40, by =5))+
  coord_cartesian(ylim = c(0, 40))+
  scale_x_continuous(breaks = seq(0, 365, by =20))+
  labs(title = (expression(paste("Soil temperature at Ummeras"))), x="Day of the year", y = "Soil temperature (°C)") +
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

ttt <- DOY_T2 + guides(colour = guide_legend(override.aes = list(size = 2)))
print(ttt)

DOY_T_facet_Year<-ggplot(Ummeras.fluxT, aes(x=DOY, y=soil_T, colour=type))+
  facet_wrap(~Year)+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.1)+
  scale_y_continuous(breaks = seq(0, 40, by =5))+
  coord_cartesian(ylim = c(0, 40))+
  scale_x_continuous(breaks = seq(0, 365, by =40))+
  scale_colour_manual(name="", values=c('Bare Peat Dry'="brown4", 'Bare Peat Middle'="darkgoldenrod3", 'Bare Peat Wet'="royalblue", 'Eriophorum' = 'green2'))+
  labs(title = (expression(paste("Ummeras soil temperature by site"))), x="Day of the year", y = "Soil temperature (°C)") +
  theme_bw()+
  theme(axis.text = element_text(size = 10, colour='black'),
        axis.text.x = element_text(size = 10, angle = 310, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        axis.title.x = element_text(size = 15, vjust = -1),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOY_T_facet_Year_b1 <- DOY_T_facet_Year + guides(colour = guide_legend(override.aes = list(shape = NA, fill=NA, size=2)))
print(DOY_T_facet_Year_b1)

T_moi_impact<-ggplot(Ummeras.flux_moi, aes(x=soil_T, y=moi))+
  geom_point(size=1.5)+
  stat_smooth(method=lm, colour="red")+
  scale_y_continuous(breaks = seq(0, 0.8, by =0.1))+
  coord_cartesian(ylim = c(0, 0.8))+
  scale_x_continuous(breaks = seq(0, 40, by =5))+
  labs(title = (expression(paste("Soil temperature influence on soil water content (Ummeras)"))), x="Soil temperature (°C)", y = expression(paste ("Volumetric water content ( ", m^-3 ," ", m^-3, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

T_moi_impact_type<-ggplot(Ummeras.flux_moi, aes(x=soil_T, y=moi, colour=type))+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.2)+
  scale_y_continuous(breaks = seq(0, 0.8, by =0.1))+
  coord_cartesian(ylim = c(0, 0.8), xlim=c(0, 40))+
  scale_x_continuous(breaks = seq(0, 40, by =5))+
  scale_colour_manual(name="", values=c('Bare Peat Dry'="brown4", 'Bare Peat Middle'="darkgoldenrod3", 'Bare Peat Wet'="royalblue", 'Eriophorum' = 'green2'))+
  labs(title = (expression(paste("Soil temperature influence on soil water content by site (Ummeras)"))), x="Soil temperature (°C)", y = expression(paste ("Volumetric water content ( ", m^-3 ," ", m^-3, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

T_moi_impact_type_b1 <- T_moi_impact_type + guides(colour = guide_legend(override.aes = list(shape = NA, fill=NA, size=2)))
print(T_moi_impact_type_b1)
###########################

Month_CO2<-ggplot(Ummeras.fluxCO2, aes(x=Month, y=CO2, colour=Year))+
  geom_point(size=1)+
  stat_smooth(method=loess)+
  scale_y_continuous(breaks = seq(0, 6, by =1))+
  coord_cartesian(ylim = c(0, 6))+
  scale_x_continuous(breaks = seq(1, 12, by =1))+
  labs(title = (expression(paste("ER flux from Ummeras"))), x="Month", y = expression(paste ("ER flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
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

DOY_CO2_shape<-ggplot(Ummeras.fluxCO2, aes(x=DOY, y=CO2, colour=type, shape=Year))+
  geom_point(size=2)+
  stat_smooth(method=loess, alpha=0.2)+
  scale_y_continuous(breaks = seq(0, 6, by =1))+
  coord_cartesian(ylim = c(0, 6))+
  scale_x_continuous(breaks = seq(0, 365, by =20))+
  scale_shape_manual(name="Year", values=c('2020'=0, '2021'=17, '2022'=16))+
  scale_colour_manual(name="", values=c('Bare Peat Dry'="brown4", 'Bare Peat Middle'="darkgoldenrod3", 'Bare Peat Wet'="royalblue", 'Eriophorum' = 'green2'))+
  labs(title = (expression(paste("ER flux from Ummeras"))), x="Day of the year", y = expression(paste ("ER flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
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

DOY_CO2_facet<-ggplot(Ummeras.fluxCO2, aes(x=DOY, y=CO2, colour=type))+
  facet_wrap(~Year)+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.2)+
  scale_y_continuous(breaks = seq(0, 10, by =1))+
  coord_cartesian(ylim = c(0, 10), xlim=c(0, 360))+
  scale_x_continuous(breaks = seq(0, 365, by =30))+
  scale_colour_manual(name="", values=c('Bare Peat Dry'="brown4", 'Bare Peat Middle'="darkgoldenrod3", 'Bare Peat Wet'="royalblue", 'Eriophorum' = 'green2', 'Sphagnum'='seagreen4'))+
  labs(title = (expression(paste("ER flux from Ummeras by year"))), x="Day of the year", y = expression(paste ("ER flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 11, colour='black'),
        axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOY_CO2_facet_b1 <- DOY_CO2_facet + guides(colour = guide_legend(override.aes = list(shape = NA, fill=NA, size=2)))
print(DOY_CO2_facet_b1)

DOY_CO2_collar<-ggplot(Ummeras.fluxCO2, aes(x=DOY, y=CO2, colour=type))+
  facet_wrap(~Plot)+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.3)+
  scale_y_continuous(breaks = seq(0, 10, by =2))+
  coord_cartesian(ylim = c(0, 10))+
  scale_x_continuous(breaks = seq(0, 365, by =60))+
  scale_colour_manual(name="", values=c('Bare Peat Dry'="brown4", 'Bare Peat Middle'="darkgoldenrod3", 'Bare Peat Wet'="royalblue", 'Eriophorum' = 'green2', 'Sphagnum'='seagreen4'))+
  labs(title = (expression(paste("ER flux from Ummeras by collar"))), x="Day of the year", y = expression(paste ("ER flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 11, colour='black'),
        axis.text.x = element_text(size = 10, angle = 90, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        axis.title.x = element_text(size = 15, vjust = -1),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOY_CO2_collar_b1 <- DOY_CO2_collar + guides(colour = guide_legend(override.aes = list(shape = NA, fill=NA, size=2)))
print(DOY_CO2_collar_b1)

CO2_collar<-ggplot(Ummeras.fluxCO2, aes(x=Plot, y=CO2, colour=type))+
  geom_boxplot(width=.4, outlier.size=1, lwd=1)+
  scale_y_continuous(breaks = seq(0, 6, by =1))+
  coord_cartesian(ylim = c(0, 6))+
  scale_colour_manual(name="", values=c('Bare Peat Dry'="brown4", 'Bare Peat Middle'="darkgoldenrod3", 'Bare Peat Wet'="royalblue", 'Eriophorum' = 'green2'))+
  labs(title = (expression(paste("ER flux from Ummeras by site"))), x="Collar", y = expression(paste ("ER flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=2, stroke=1.3, colour='black')+
  theme_bw()+
  theme(axis.text = element_text(size = 13, colour='black'),
        axis.text.x = element_text(size=10, angle = 0, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        axis.title.x = element_text(size = 15, vjust = -1),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))


dateCO2<-ggplot(Ummeras.fluxCO2, aes(x=Date, y=CO2, colour=type))+
  geom_point(size=1.5)+
  scale_y_continuous(breaks = seq(0, 6, by =1))+
  coord_cartesian(ylim = c(0, 6))+
  scale_colour_manual(name="", values=c('Bare Peat Dry'="brown4", 'Bare Peat Middle'="darkgoldenrod3", 'Bare Peat Wet'="royalblue", 'Eriophorum' = 'green2'))+
  scale_x_date(date_breaks = "30 day", date_labels = "%d/%m/%Y")+
  labs(title = (expression(paste("ER flux from Ummeras by site"))), x="Date", y = expression(paste ("ER flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOYCO2type<-ggplot(Ummeras.fluxCO2, aes(x=DOY, y=CO2, colour=type))+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.3)+
  facet_wrap(~type)+
  scale_y_continuous(breaks = seq(0, 10, by =1))+
  coord_cartesian(ylim = c(0, 10))+
  scale_x_continuous(breaks = seq(0, 365, by =30))+
  scale_colour_manual(name="", values=c('Bare Peat Dry'="brown4", 'Bare Peat Middle'="darkgoldenrod3", 'Bare Peat Wet'="royalblue", 'Eriophorum' = 'green2', 'Sphagnum'='seagreen4'))+
  labs(title = (expression(paste("ER flux from Ummeras by location"))), x="DOY", y = expression(paste ("ER flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOYCO2type<-ggplot(Ummeras.fluxCO2, aes(x=DOY, y=CO2, colour=type))+
  facet_wrap(~Year)+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.3)+
  scale_y_continuous(breaks = seq(0, 10, by =1))+
  coord_cartesian(ylim = c(0, 10))+
  scale_x_continuous(breaks = seq(0, 365, by =30))+
  scale_colour_manual(name="", values=c('Bare Peat Dry'="brown4", 'Bare Peat Middle'="darkgoldenrod3", 'Bare Peat Wet'="royalblue", 'Eriophorum' = 'green2', 'Sphagnum'='seagreen4'))+
  labs(title = (expression(paste("ER flux from Ummeras by location"))), x="DOY", y = expression(paste ("ER flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 11, colour='black'),
        axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOYCO2type_a <- DOYCO2type + 
  guides(colour = guide_legend(override.aes = list(size = 2, shape = NA, fill=NA)))
print(DOYCO2type_a)

moi_CO2type<-ggplot(Ummeras.fluxCO2env, aes(x=moi, y=CO2, colour=type))+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.2)+
  scale_y_continuous(breaks = seq(0, 10, by =1))+
  coord_cartesian(ylim = c(0, 10), xlim=c(0, 0.9))+
  scale_x_continuous(breaks = seq(0, 0.9, by =0.1))+
  scale_colour_manual(name="type", values=c('Bare Peat Dry'="brown4", 'Bare Peat Middle'="darkgoldenrod3", 'Bare Peat Wet'="royalblue", 'Eriophorum' = 'green2'))+
  labs(title = (expression(paste("Ummeras ", CO[2], " flux by location"))), x=expression(paste ("Volumetric water content ( ", m^-3 ," ", m^-3, ")", sep="")), y = expression(paste ("ER flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

moi_CO2type_b1 <- moi_CO2type + guides(colour = guide_legend(override.aes = list(shape = NA, fill=NA, size=2)))
print(moi_CO2type_b1)

T_CO2type<-ggplot(Ummeras.fluxCO2env, aes(x=soil_T, y=CO2, colour=type))+
  geom_point(size=1.5)+
  stat_smooth(method=loess)+
  scale_y_continuous(breaks = seq(0, 10, by =1))+
  coord_cartesian(ylim = c(0, 10), xlim=c(0, 40))+
  scale_x_continuous(breaks = seq(0, 40, by =5))+
  scale_colour_manual(name="", values=c('Bare Peat Dry'="brown4", 'Bare Peat Middle'="darkgoldenrod3", 'Bare Peat Wet'="royalblue", 'Eriophorum' = 'green2'))+
  labs(title = (expression(paste("Ummeras ", CO[2], " flux by location"))), x="Soil temperature (°C)", y = expression(paste ("ER flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

T_CO2type_b1 <- T_CO2type + guides(colour = guide_legend(override.aes = list(shape = NA, fill=NA, size=2)))
print(T_CO2type_b1)

DOY_CO2_year<-ggplot(Ummeras.fluxCO2, aes(x=DOY, y=CO2, colour=Year))+
  geom_point(size=1)+
  stat_smooth(method=loess)+
  scale_y_continuous(breaks = seq(0, 10, by =1))+
  coord_cartesian(ylim = c(0, 10), xlim=c(0, 360))+
  scale_x_continuous(breaks = seq(0, 365, by =30))+
  labs(title = (expression(paste("Ummeras ", CO[2], " flux by year"))), x="Day of the year", y = expression(paste ("ER flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10),
        legend.key.size = unit(1, "cm"))

DOY_CO2_year_b1 <- DOY_CO2_year + guides(colour = guide_legend(override.aes = list(shape = NA, fill=NA, size=2)))
print(DOY_CO2_year_b1)

DOYCO2type<-ggplot(Ummeras.fluxCO2, aes(x=DOY, y=CO2, colour=type))+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.1)+
  scale_y_continuous(breaks = seq(0, 10, by =1))+
  coord_cartesian(ylim = c(0, 10), xlim=c(0, 360))+
  scale_x_continuous(breaks = seq(0, 365, by =30))+
  scale_colour_manual(name="", values=c('Bare Peat Dry'="brown4", 'Bare Peat Middle'="darkgoldenrod3", 'Bare Peat Wet'="royalblue", 'Eriophorum' = 'green2'))+
  labs(title = (expression(paste("Ummeras ", CO[2], " flux by location"))), x="Day of the year", y = expression(paste ("ER flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 310, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOYCO2type_b1 <- DOYCO2type + guides(colour = guide_legend(override.aes = list(shape = NA, fill=NA, size=2)))
print(DOYCO2type_b1)
#############################

CH4_collar<-ggplot(Ummeras.fluxCH4, aes(x=Plot, y=CH4, colour=type))+
  geom_boxplot(width=.4, outlier.size=1, lwd=1)+
  scale_y_continuous(breaks = seq(0, 100, by =10))+
  coord_cartesian(ylim = c(0, 100))+
  scale_colour_manual(name="", values=c('Bare Peat Dry'="brown4", 'Bare Peat Middle'="darkgoldenrod3", 'Bare Peat Wet'="royalblue", 'Eriophorum' = 'green2', 'Sphagnum'='seagreen4'))+
  labs(title = (expression(paste("Ummeras ", CH[4], " flux by collar"))), x="Collar", y = expression(paste ("", CH[4]," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=1, stroke=1.3, colour='black')+
  theme_bw()+
  theme(axis.text = element_text(size = 13, colour='black'),
        axis.text.x = element_text(size = 11, angle = 0, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        axis.title.x = element_text(size = 15, vjust = -1),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOY_CH4_collar<-ggplot(Ummeras.fluxCH4, aes(x=DOY, y=CH4, colour=type))+
  facet_wrap(~Plot)+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.3)+
  scale_y_continuous(breaks = seq(0, 120, by =20))+
  coord_cartesian(ylim = c(0, 120), xlim=c(0, 360))+
  scale_x_continuous(breaks = seq(0, 365, by =60))+
  scale_colour_manual(name="", values=c('Bare Peat Dry'="brown4", 'Bare Peat Middle'="darkgoldenrod3", 'Bare Peat Wet'="royalblue", 'Eriophorum' = 'green2', 'Sphagnum'='seagreen4'))+
  labs(title = (expression(paste("Ummeras ", CH[4], " flux by collar"))), x="Day of the year", y = expression(paste ("", CH[4]," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 10, colour='black'),
        axis.text.x = element_text(angle = 310, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        axis.title.x = element_text(size = 15, vjust = -1),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOY_CH4_collar_b1 <- DOY_CH4_collar + guides(colour = guide_legend(override.aes = list(shape = NA, fill=NA, size=2)))
print(DOY_CH4_collar_b1)

DOYCH4type_facet<-ggplot(Ummeras.fluxCH4, aes(x=DOY, y=CH4, colour=type))+
  facet_wrap(~Year)+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.3)+
  scale_y_continuous(breaks = seq(0, 120, by =20))+
  coord_cartesian(ylim = c(0, 120), xlim=c(0, 360))+
  scale_x_continuous(breaks = seq(0, 360, by =30))+
  scale_colour_manual(name="", values=c('Bare Peat Dry'="brown4", 'Bare Peat Middle'="darkgoldenrod3", 'Bare Peat Wet'="royalblue", 'Eriophorum' = 'green2', 'Sphagnum'='seagreen4'))+
  labs(title = (expression(paste("Ummeras ", CH[4], " flux by location"))), x="DOY", y = expression(paste ("", CH[4]," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 11, colour='black'),
        axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOYCH4type_facet_a <- DOYCH4type_facet + 
  guides(colour = guide_legend(override.aes = list(size = 2, shape = NA, fill=NA)))
print(DOYCH4type_facet_a)

DOYCH4type_facet<-ggplot(Ummeras.fluxCH4, aes(x=DOY, y=CH4, colour=type))+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.3)+
  facet_wrap(~type)+
  scale_y_continuous(breaks = seq(0, 120, by =20))+
  coord_cartesian(ylim = c(0, 120), xlim=c(0, 360))+
  scale_x_continuous(breaks = seq(0, 360, by =30))+
  scale_colour_manual(name="", values=c('Bare Peat Dry'="brown4", 'Bare Peat Middle'="darkgoldenrod3", 'Bare Peat Wet'="royalblue", 'Eriophorum' = 'green2', 'Sphagnum'='seagreen4'))+
  labs(title = (expression(paste("Ummeras ", CH[4], " flux by location"))), x="DOY", y = expression(paste ("", CH[4]," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(size=12, angle = 310, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

dateCH4<-ggplot(Ummeras.fluxCH4, aes(x=Date, y=CH4, colour=type))+
  geom_point(size=2)+
  scale_y_continuous(breaks = seq(0, 100, by =10))+
  coord_cartesian(ylim = c(0, 100))+
  scale_colour_manual(name="", values=c('Bare Peat Dry'="brown4", 'Bare Peat Middle'="darkgoldenrod3", 'Bare Peat Wet'="royalblue", 'Eriophorum' = 'green2', 'Sphagnum'='seagreen4'))+
  scale_x_date(date_breaks = "30 day", date_labels = "%d/%m/%Y")+
  labs(title = (expression(paste("Ummeras ", CH[4], " flux by location"))), x="Date", y = expression(paste ("", CH[4]," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOYCH4type<-ggplot(Ummeras.fluxCH4, aes(x=DOY, y=CH4, colour=type))+
  geom_point(size=2)+
  stat_smooth(method=loess)+
  scale_y_continuous(breaks = seq(0, 120, by =20))+
  coord_cartesian(ylim = c(0, 120), xlim=c(0, 360))+
  scale_x_continuous(breaks = seq(0, 360, by =30))+
  scale_colour_manual(name="", values=c('Bare Peat Dry'="brown4", 'Bare Peat Middle'="darkgoldenrod3", 'Bare Peat Wet'="royalblue", 'Eriophorum' = 'green2', 'Sphagnum'='seagreen4'))+
  labs(title = (expression(paste("Ummeras ", CH[4], " flux by location"))), x="Day of the year", y = expression(paste ("", CH[4]," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 310, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOYCH4type_b1 <- DOYCH4type + guides(colour = guide_legend(override.aes = list(shape = NA, fill=NA, size=2)))
print(DOYCH4type_b1)

DOY_CH4_year<-ggplot(Ummeras.fluxCH4, aes(x=DOY, y=CH4, colour=Year))+
  geom_point(size=1)+
  stat_smooth(method=loess)+
  scale_y_continuous(breaks = seq(0, 120, by =10))+
  coord_cartesian(ylim = c(0, 120), xlim=c(0, 360))+
  scale_x_continuous(breaks = seq(0, 365, by =30))+
  labs(title = (expression(paste("Ummeras ", CH[4], " flux by year"))), x="Day of the year", y = expression(paste ("", CH[4]," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10),
        legend.key.size = unit(1, "cm"))

DOY_CH4_year_b1 <- DOY_CH4_year + guides(colour = guide_legend(override.aes = list(shape = NA, fill=NA, size=2)))
print(DOY_CH4_year_b1)

DOY_CH4_facet<-ggplot(Ummeras.fluxCH4, aes(x=DOY, y=CH4, colour=type))+
  facet_wrap(~Year)+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.2)+
  scale_y_continuous(breaks = seq(0, 160, by =20))+
  coord_cartesian(ylim = c(0, 160), xlim=c(0, 360))+
  scale_x_continuous(breaks = seq(0, 360, by =60))+
  scale_colour_manual(name="", values=c('Bare Peat Dry'="brown4", 'Bare Peat Middle'="darkgoldenrod3", 'Bare Peat Wet'="royalblue", 'Eriophorum' = 'green2', 'Sphagnum'='seagreen4'))+
  labs(title = (expression(paste("Ummeras ", CH[4], " flux by location and year"))), x="Day of the year", y = expression(paste ("", CH[4]," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 11, colour='black'),
        axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOY_CH4_facet_b1 <- DOY_CH4_facet + guides(colour = guide_legend(override.aes = list(shape = NA, fill=NA, size=2)))
print(DOY_CH4_facet_b1)

moi_CH4type<-ggplot(Ummeras.fluxCH4env, aes(x=moi, y=CH4, colour=type))+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0)+
  scale_y_continuous(breaks = seq(0, 100, by =10))+
  coord_cartesian(ylim = c(0, 100))+
  scale_x_continuous(breaks = seq(0, 0.8, by =0.1))+
  scale_colour_manual(name="", values=c('Bare Peat Dry'="brown4", 'Bare Peat Middle'="darkgoldenrod3", 'Bare Peat Wet'="royalblue", 'Eriophorum' = 'green2', 'Sphagnum'='seagreen4'))+
  labs(title = (expression(paste("Ummeras ", CH[4], " flux by location"))), x=expression(paste ("Volumetric water content ( ", m^-3 ," ", m^-3, ")", sep="")), y = expression(paste ("", CH[4]," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

T_CH4type<-ggplot(Ummeras.fluxCH4env, aes(x=soil_T, y=CH4, colour = type))+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.1)+
  scale_y_continuous(breaks = seq(0, 100, by =10))+
  coord_cartesian(ylim = c(0, 100))+
  scale_x_continuous(breaks = seq(0, 40, by =5))+
  scale_colour_manual(name="", values=c('Bare Peat Dry'="brown4", 'Bare Peat Middle'="darkgoldenrod3", 'Bare Peat Wet'="royalblue", 'Eriophorum' = 'green2', 'Sphagnum'='seagreen4'))+
  labs(title = (expression(paste("Ummeras ", CH[4], " flux by location"))), x="Soil temperature (°C)", y = expression(paste ("", CH[4]," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

T_CH4type2<-ggplot(Ummeras.fluxCH4env, aes(x=soil_T, y=CH4, colour=type))+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.1)+
  scale_y_continuous(breaks = seq(0, 20, by =5))+
  coord_cartesian(ylim = c(0, 20))+
  scale_x_continuous(breaks = seq(0, 32, by =5))+
  scale_colour_manual(name="", values=c('Bare Peat Dry'="brown4", 'Bare Peat Middle'="darkgoldenrod3", 'Bare Peat Wet'="royalblue", 'Eriophorum' = 'green2', 'Sphagnum'='seagreen4'))+
  labs(title = (expression(paste("Ummeras ", CH[4], " flux by location"))), x="Soil temperature (°C)", y = expression(paste ("", CH[4]," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

T_CH4type_year<-ggplot(Ummeras.fluxCH4env, aes(x=soil_T, y=CH4, colour=type))+
  facet_wrap(~Year)+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.1)+
  scale_y_continuous(breaks = seq(0, 100, by =10))+
  coord_cartesian(ylim = c(0, 100))+
  scale_x_continuous(breaks = seq(0, 40, by =5))+
  scale_colour_manual(name="", values=c('Bare Peat Dry'="brown4", 'Bare Peat Middle'="darkgoldenrod3", 'Bare Peat Wet'="royalblue", 'Eriophorum' = 'green2', 'Sphagnum'='seagreen4'))+
  labs(title = (expression(paste("Relationship between soil temperature and ", CH[4], " flux by year (Ummeras)"))), x="Soil temperature (°C)", y = expression(paste ("", CH[4]," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

moi_CH4type_year<-ggplot(Ummeras.fluxCH4env, aes(x=moi, y=CH4, colour=type))+
  facet_wrap(~Year)+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.1)+
  scale_y_continuous(breaks = seq(0, 100, by =10))+
  coord_cartesian(ylim = c(0, 100))+
  scale_x_continuous(breaks = seq(0, 0.8, by =0.1))+
  scale_colour_manual(name="", values=c('Bare Peat Dry'="brown4", 'Bare Peat Middle'="darkgoldenrod3", 'Bare Peat Wet'="royalblue", 'Eriophorum' = 'green2', 'Sphagnum'='seagreen4'))+
  labs(title = (expression(paste("Relationship between soil water content and ", CH[4], " flux by year (Ummeras)"))), x=expression(paste ("Volumetric water content ( ", m^-3 ," ", m^-3, ")", sep="")), y = expression(paste ("", CH[4]," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

T_CO2type_year<-ggplot(Ummeras.fluxCO2env, aes(x=soil_T, y=CO2, colour=type))+
  facet_wrap(~Year)+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.1)+
  scale_y_continuous(breaks = seq(0, 6, by =1))+
  coord_cartesian(ylim = c(0, 6))+
  scale_x_continuous(breaks = seq(0, 40, by =5))+
  scale_colour_manual(name="", values=c('Bare Peat Dry'="brown4", 'Bare Peat Middle'="darkgoldenrod3", 'Bare Peat Wet'="royalblue", 'Eriophorum' = 'green2', 'Sphagnum'='seagreen4'))+
  labs(title = (expression(paste("Relationship between soil temperature and ", CO[2], " flux by year (Ummeras)"))), x="Soil temperature (°C)", y = expression(paste ("ER flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

moi_CO2type_year<-ggplot(Ummeras.fluxCO2env, aes(x=moi, y=CO2, colour=type))+
  facet_wrap(~Year)+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.1)+
  scale_y_continuous(breaks = seq(0, 6, by =1))+
  coord_cartesian(ylim = c(0, 6))+
  scale_x_continuous(breaks = seq(0, 0.8, by =0.1))+
  scale_colour_manual(name="", values=c('Bare Peat Dry'="brown4", 'Bare Peat Middle'="darkgoldenrod3", 'Bare Peat Wet'="royalblue", 'Eriophorum' = 'green2', 'Sphagnum'='seagreen4'))+
  labs(title = (expression(paste("Relationship between soil water content and ", CO[2], " flux by year (Ummeras)"))), x=expression(paste ("Volumetric water content ( ", m^-3 ," ", m^-3, ")", sep="")), y = expression(paste ("ER flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

Ummeras.flux_WT<-within(Ummeras.flux_WT, Plot <- factor(Plot,
levels=c('E0', 'E1', 'E2', 'S02', 'S03', 'W7', 'W8','M11', 'D12', 'D13', 'D14')))

DOY_WT<-ggplot(Ummeras.flux_WT, aes(x=DOY, y=WT))+
  facet_wrap(~Plot)+
  geom_line(size=2, colour='lightblue2')+
  geom_point(size=2, colour='navyblue')+
  scale_y_continuous(breaks = seq(-100, 0, by =20))+
  coord_cartesian(ylim = c(-100, 0), xlim=c(0, 360))+
  scale_x_continuous(breaks = seq(0, 365, by =40))+
  labs(title = (expression(paste("Ummeras water table changes"))), x="Day of the year", y = expression(paste ("Water table depth (cm)")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(size=10, angle = 310, hjust = 0.5, vjust = 0, colour='black'),
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

moi_WT<-ggplot(Cavemount.flux_WT, aes(x=WT, y=moi))+
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

moi_WTtype<-ggplot(Ummeras.flux_WT, aes(x=WT, y=moi, colour=type))+
  stat_smooth(method=loess, alpha=0.2, size=1)+
  geom_point(size=1.5)+
  scale_y_continuous(breaks = seq(0, 0.9, by =0.1))+
  coord_cartesian(ylim = c(0, 0.9), xlim=c(-80, 0))+
  scale_x_continuous(breaks = seq(-80, 0, by =10))+
  scale_colour_manual(name="", values=c('Bare Peat Dry'="brown4", 'Bare Peat Middle'="darkgoldenrod3", 'Bare Peat Wet'="royalblue", 'Eriophorum' = 'green2', 'Sphagnum'='seagreen4'))+
  labs(title = (expression(paste("Water table impact on soil water content (Ummeras)"))), x="Water table (cm)", y=expression(paste ("Volumetric water content ( ", m^-3 ," ", m^-3, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(size=12, angle = 0, hjust = 0.5, vjust = 0, colour='black'),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOY_moi_facet<-ggplot(Ummeras.flux_moi, aes(x=DOY, y=moi, colour=type))+
  facet_wrap(~Plot)+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.2)+
  scale_y_continuous(breaks = seq(0, 0.8, by =0.2))+
  coord_cartesian(ylim = c(0, 0.8))+
  scale_x_continuous(breaks = seq(0, 365, by =60))+
  scale_colour_manual(name="", values=c('Bare Peat Dry'="brown4", 'Bare Peat Middle'="darkgoldenrod3", 'Bare Peat Wet'="royalblue", 'Eriophorum' = 'green2', 'Sphagnum'='seagreen4'))+
  labs(title = (expression(paste("Ummeras soil water content by collar"))), x="Day of the year", y = expression(paste ("Volumetric water content ( ", m^-3 ," ", m^-3, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 11, colour='black'),
        axis.text.x = element_text(size = 10, angle = 90, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        axis.title.x = element_text(size = 15, vjust = -1),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOY_moi_facet_b1 <- DOY_moi_facet + guides(colour = guide_legend(override.aes = list(shape = NA, fill=NA, size=2)))
print(DOY_moi_facet_b1)

#############

DOYCO2type<-ggplot(Ummeras.fluxCO2, aes(x=DOY, y=CO2, colour=type))+
  facet_wrap(~Year)+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.3)+
  scale_y_continuous(breaks = seq(0, 6, by =1))+
  coord_cartesian(ylim = c(0, 6))+
  scale_x_continuous(breaks = seq(0, 365, by =30))+
  scale_colour_manual(name="", values=c('Bare Peat Dry'="brown4", 'Bare Peat Middle'="darkgoldenrod3", 'Bare Peat Wet'="royalblue", 'Eriophorum' = 'green2', 'Sphagnum'='seagreen4'))+
  labs(title = (expression(paste("Ummeras ", CO[2], " flux"))), x="DOY", y = expression(paste ("", CO[2]," flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
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

DOY_CH4_facet<-ggplot(Ummeras.fluxCH4, aes(x=DOY, y=CH4, colour=type))+
  facet_wrap(~Year)+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.2)+
  scale_y_continuous(breaks = seq(0, 160, by =20))+
  coord_cartesian(ylim = c(0, 160), xlim=c(0, 360))+
  scale_x_continuous(breaks = seq(0, 360, by =60))+
  scale_colour_manual(name="", values=c('Bare Peat Dry'="brown4", 'Bare Peat Middle'="darkgoldenrod3", 'Bare Peat Wet'="royalblue", 'Eriophorum' = 'green2', 'Sphagnum'='seagreen4'))+
  labs(title = (expression(paste("Ummeras ", CH[4], " flux"))), x="Day of the year", y = expression(paste ("", CH[4]," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 11, colour='black'),
        axis.text.x = element_text(size = 9, angle = 90, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOY_CH4_facet_b1 <- DOY_CH4_facet + guides(colour = guide_legend(override.aes = list(shape = NA, fill=NA, size=2)))
print(DOY_CH4_facet_b1)

DOY_moi_facet<-ggplot(Ummeras.fluxenv, aes(x=DOY, y=moi, colour=type))+
  facet_wrap(~Year)+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.2)+
  scale_y_continuous(breaks = seq(0, 0.9, by =0.1))+
  coord_cartesian(ylim = c(0, 0.9, by=0.1))+
  scale_x_continuous(breaks = seq(0, 360, by =60))+
  labs(title = (expression(paste("Ummeras soil water content"))), x="Year", y = expression(paste ("Volumetric water content ( ", m^3 ," ", m^-3, ")", sep="")))+
  scale_colour_manual(name="", values=c('Bare Peat Dry'="brown4", 'Bare Peat Middle'="darkgoldenrod3", 'Bare Peat Wet'="royalblue", 'Eriophorum' = 'green2', 'Sphagnum'='seagreen4'))+
theme_bw()+
  theme(axis.text = element_text(size = 11, colour='black'),
        axis.text.x = element_text(size = 9, angle = 90, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOY_moi_facet_b1 <- DOY_moi_facet + guides(colour = guide_legend(override.aes = list(shape = NA, fill=NA, size=2)))
print(DOY_moi_facet_b1)

DOY_T_facet<-ggplot(Ummeras.fluxenv, aes(x=DOY, y=soil_T, colour=type))+
  facet_wrap(~Year)+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.2)+
  scale_y_continuous(breaks = seq(0, 30, by =5))+
  coord_cartesian(ylim = c(0, 30))+
  scale_x_continuous(breaks = seq(0, 360, by =60))+
  scale_colour_manual(name="", values=c('Bare Peat Dry'="brown4", 'Bare Peat Middle'="darkgoldenrod3", 'Bare Peat Wet'="royalblue", 'Eriophorum' = 'green2', 'Sphagnum'='seagreen4'))+
  labs(title = (expression(paste("Ummeras soil temperature by site"))), x="Day of the year", y = "Soil temperature (°C)") +
  theme_bw()+
  theme(axis.text = element_text(size = 11, colour='black'),
        axis.text.x = element_text(size = 9, angle = 90, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOY_T_facet_a1 <- DOY_T_facet + guides(colour = guide_legend(override.aes = list(shape = NA, fill=NA, size=2)))
print(DOY_T_facet_a1)