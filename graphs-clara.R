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

str(Clara.flux)
summary(Clara.flux)
neg<-nrow(Clara.flux[Clara.flux$CO2<0,])
select(filter(Clara.flux, ), CO2)
select(filter(Clara.flux, CO2==0), CO2)

Clara.flux<-filter(Clara.flux, CO2!="NA" & CH4!="NA")
Clara.flux[Clara.flux$CO2 < 0, "CO2"] <- 0
Clara.flux$DOY<-as.numeric(Clara.flux$DOY)
Clara.flux$Year<-as.factor(Clara.flux$Year)
Clara.flux$Date <- as.Date(Clara.flux$Date, "%d/%m/%Y")
Clara.flux<-mutate(Clara.flux, Date=dmy(Date))

Clara.fluxCH4<-filter(Clara.flux, action!="delete" & action!="ch4h" & action!="delete_flux")
Clara.fluxCH4env<-filter(Clara.flux, action!="delete" & action!="ch4h" & action!="delete_flux" & soil_T!="NA" & moi!="NA")
Clara.fluxCO2<-filter(Clara.flux, action!="delete" & action!="delete_flux")
Clara.fluxCO2env<-filter(Clara.flux, action!="delete" & action!="delete_flux" & soil_T!="NA" & moi!="NA")
Clara.fluxT<-filter(Clara.flux, action!="delete" & soil_T!="NA")
Clara.fluxT2021<-filter(Clara.flux, action!="delete" & soil_T!="NA" & Year=="2021")
Clara.fluxT2022<-filter(Clara.flux, action!="delete" & soil_T!="NA"& Year=="2022")
Clara.flux_moi<-filter(Clara.flux, action!="delete" & moi!="NA")

Clara.flux2021<-filter(Clara.flux, Year=="2021")
Clara.flux2022<-filter(Clara.flux, Year=="2022")


sumflux<-summarise(group_by(Clara.flux, type), mean_bio=mean(CO2))
sumflux2<-summarise(group_by(Clara.flux, type), mean_bio=mean(CH4))
#########################

CO2a<-summarise(group_by(Clara.fluxCO2, type),
                meanCO2a=mean(CO2),
                SEa=sd(CO2)/sqrt(length(CO2)),
                lwra=meanCO2a-SEa,
                upra=meanCO2a+SEa)
limits1<-aes(ymin=lwra, ymax=upra)

CO2u1<-ggplot(Clara.fluxCO2, aes(x=type, y=CO2, colour=type))+
  geom_boxplot(width=.4, outlier.size=1, lwd=1)+
  scale_colour_manual(name="type", values=c('Central'="olivedrab3", 'Marginal'="sienna1", 'Sub-Central'="mediumaquamarine", 'Sub-Marginal' = 'gold'))+
  scale_x_discrete(name="", breaks=c("Central", "Marginal", "Sub-Central", "Sub-Marginal"), labels=c("Central", "Marginal", "Sub-\n Central", "Sub-\n Marginal"))+
  scale_y_continuous(breaks = seq(0, 6, by =1))+
  coord_cartesian(ylim = c(0, 6))+
  labs(title = (expression(paste("ER flux from Clara"))), x="plot", y = expression(paste ("", CO[2]," flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=2, stroke=1.5)+
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

CO2year<-ggplot(Clara.fluxCO2, aes(x=type, y=CO2, colour=type))+
  geom_boxplot(width=.4, outlier.size=1, lwd=1)+
  facet_wrap(~Year)+
  scale_colour_manual(name="type", values=c('Central'="olivedrab3", 'Marginal'="sienna1", 'Sub-Central'="mediumaquamarine", 'Sub-Marginal' = 'gold'))+
  scale_x_discrete(name="", breaks=c("Central", "Marginal", "Sub-Central", "Sub-Marginal"), labels=c("Central", "Marginal", "Sub-\n Central", "Sub-\n Marginal"))+
  scale_y_continuous(breaks = seq(0, 6, by =1))+
  coord_cartesian(ylim = c(0, 6))+
  labs(title = (expression(paste("ER flux from Clara"))), x="plot", y = expression(paste ("", CO[2]," flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=2, stroke=1.5)+
  theme_bw()+
  theme(axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 20, hjust = 0.5, vjust = 1),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_blank(),
        strip.text = element_text(size = 10),
        legend.key.size = unit(1, "cm"))

CO2year_na <- CO2year + guides(colour = guide_legend(override.aes = list(shape = NA)))
print(CO2year_na)

CO2u2<-ggplot(Clara.fluxCO2, aes(x=type, y=CO2, fill=type))+
  geom_boxplot(width=.4, outlier.size=1, lwd=1)+
  scale_fill_manual(name="type", values=c('Central'="olivedrab3", 'Marginal'="sienna1", 'Sub-Central'="mediumaquamarine", 'Sub-Marginal' = 'gold'))+
  scale_x_discrete(name="", breaks=c("Central", "Marginal", "Sub-Central", "Sub-Marginal"), labels=c("Central", "Marginal", "Sub-\n Central", "Sub-\n Marginal"))+
  scale_y_continuous(breaks = seq(0, 6, by =1))+
  coord_cartesian(ylim = c(0, 6))+
  labs(title = (expression(paste("ER flux from Clara"))), x="plot", y = expression(paste ("", CO[2]," flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
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


CH4a<-summarise(group_by(Clara.fluxCH4, type),
                meanCH4a=mean(CH4),
                SEa=sd(CH4)/sqrt(length(CH4)),
                lwra=meanCH4a-SEa,
                upra=meanCH4a+SEa)
limits1<-aes(ymin=lwra, ymax=upra)

CH4_1<-ggplot(Clara.fluxCH4, aes(x=type, y=CH4, colour=type))+
  geom_point(size=1)+
  geom_boxplot(width=.4, outlier.size=1, lwd=1)+
  scale_colour_manual(name="type", values=c('Central'="olivedrab3", 'Marginal'="sienna1", 'Sub-Central'="mediumaquamarine", 'Sub-Marginal' = 'gold'))+
  scale_x_discrete(name="", breaks=c("Central", "Marginal", "Sub-Central", "Sub-Marginal"), labels=c("Central", "Marginal", "Sub-\n Central", "Sub-\n Marginal"))+
  scale_y_continuous(breaks = seq(-10, 70, by =5))+
  coord_cartesian(ylim = c(-10, 70))+
  labs(title = (expression(paste("Clara ", CH[4], " flux by location"))), x="plot", y = expression(paste ("", CH[4]," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
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

CH4_1Year<-ggplot(Clara.fluxCH4, aes(x=type, y=CH4, colour=type))+
  facet_wrap(~Year)+
  geom_point(size=1)+
  geom_boxplot(width=.4, outlier.size=1, lwd=1)+
  scale_colour_manual(name="type", values=c('Central'="olivedrab3", 'Marginal'="sienna1", 'Sub-Central'="mediumaquamarine", 'Sub-Marginal' = 'gold'))+
  scale_x_discrete(name="", breaks=c("Central", "Marginal", "Sub-Central", "Sub-Marginal"), labels=c("Central", "Marginal", "Sub-\n Central", "Sub-\n Marginal"))+
  scale_y_continuous(breaks = seq(-10, 80, by =5))+
  coord_cartesian(ylim = c(-10, 80))+
  labs(title = (expression(paste("Clara ", CH[4], " flux by location"))), x="plot", y = expression(paste ("", CH[4]," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=2, stroke=1.5)+
  theme_bw()+
  theme(axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 20, hjust = 0.5, vjust = 1),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_blank(),
        strip.text = element_text(size = 10),
        legend.key.size = unit(1, "cm"))

CH4_1Year_na <- CH4_1Year + guides(colour = guide_legend(override.aes = list(shape = NA)))
print(CH4_1Year_na)

#############

DOY_T<-ggplot(Clara.fluxT, aes(x=DOY, y=soil_T, colour=Year))+
  geom_point(size=1)+
  stat_smooth(method=loess)+
  scale_y_continuous(breaks = seq(0, 30, by =5))+
  coord_cartesian(ylim = c(0, 30))+
  scale_x_continuous(breaks = seq(0, 365, by =20))+
  labs(title = (expression(paste("Clara soil temperature"))), x="Day of the year", y = "Soil temperature (°C)")+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOY_Ta<-ggplot(Clara.fluxT, aes(x=DOY, y=soil_T, colour=type))+
  geom_point(size=1)+
  stat_smooth(method=loess, alpha=0.2)+
  scale_y_continuous(breaks = seq(0, 30, by =5))+
  coord_cartesian(ylim = c(0, 30))+
  scale_x_continuous(breaks = seq(0, 365, by =20))+
  scale_colour_manual(name="", values=c('Central'="olivedrab3", 'Marginal'="sienna1", 'Sub-Central'="mediumaquamarine", 'Sub-Marginal' = 'gold'))+
  labs(title = (expression(paste("Clara soil temperature"))), x="Day of the year", y = "Soil temperature (°C)")+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOY_Tb<-ggplot(Clara.fluxT, aes(x=DOY, y=soil_T, colour=type))+
  geom_line(size=1)+
  scale_y_continuous(breaks = seq(0, 30, by =5))+
  coord_cartesian(ylim = c(0, 30))+
  scale_x_continuous(breaks = seq(0, 365, by =20))+
  scale_colour_manual(name="", values=c('Central'="olivedrab3", 'Marginal'="sienna1", 'Sub-Central'="mediumaquamarine", 'Sub-Marginal' = 'gold'))+
  labs(title = (expression(paste("Clara soil temperature - mean values"))), x="Day of the year", y = "Soil temperature (°C)")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=4)+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOY_T2021<-ggplot(Clara.fluxT2021, aes(x=DOY, y=soil_T, colour=type))+
  geom_point(size=1)+
  stat_smooth(method=loess, alpha=0.2)+
  scale_y_continuous(breaks = seq(0, 30, by =5))+
  coord_cartesian(ylim = c(0, 30), xlim=c(0, 365))+
  scale_x_continuous(breaks = seq(0, 365, by =40))+
  scale_colour_manual(name="", values=c('Central'="olivedrab3", 'Marginal'="sienna1", 'Sub-Central'="mediumaquamarine", 'Sub-Marginal' = 'gold'))+
  labs(title = (expression(paste("Clara soil temperature in 2021"))), x="Day of the year", y = "Soil temperature (°C)")+
  theme_bw()+
  theme(axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 60, hjust = 0.5, vjust = 0.6),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 10),
        legend.position = "none")

DOY_T2022<-ggplot(Clara.fluxT2022, aes(x=DOY, y=soil_T, colour=type))+
  geom_point(size=1)+
  stat_smooth(method=loess, alpha=0.2)+
  scale_y_continuous(breaks = seq(0, 30, by =5))+
  coord_cartesian(ylim = c(0, 30), xlim=c(0, 365))+
  scale_x_continuous(breaks = seq(0, 365, by =40))+
  scale_colour_manual(name="", values=c('Central'="olivedrab3", 'Marginal'="sienna1", 'Sub-Central'="mediumaquamarine", 'Sub-Marginal' = 'gold'))+
  labs(title = (expression(paste("Clara soil temperature in 2022"))), x="Day of the year", y = "Soil temperature (°C)")+
  theme_bw()+
  theme(axis.text = element_text(size = 12),
        axis.title.y=element_blank(),
        axis.text.x = element_text(angle = 60, hjust = 0.5, vjust = 0.6),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10),
        legend.justification=c(1,0), legend.position=c(1,0))
grid.arrange(DOY_T2021, DOY_T2022, ncol=2)

DOY_moi<-ggplot(Clara.flux_moi, aes(x=DOY, y=moi, colour=Year))+
  geom_point(size=1)+
  stat_smooth(method=loess)+
  scale_y_continuous(breaks = seq(0, 0.85, by =0.1))+
  coord_cartesian(ylim = c(0, 0.85))+
  scale_x_continuous(breaks = seq(0, 365, by =20))+
  labs(title = (expression(paste("Clara soil water content"))), x="Day of the year", y = expression(paste ("Volumetric water content ( ", m^-3 ," ", m^-3, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOY_moi_b<-ggplot(Clara.flux_moi, aes(x=DOY, y=moi, colour=type))+
  geom_point(size=1)+
  stat_smooth(method=loess, alpha=0.2)+
  scale_y_continuous(breaks = seq(0, 0.85, by =0.1))+
  coord_cartesian(ylim = c(0, 0.85))+
  scale_x_continuous(breaks = seq(0, 365, by =20))+
  scale_colour_manual(name="", values=c('Central'="olivedrab3", 'Marginal'="sienna1", 'Sub-Central'="mediumaquamarine", 'Sub-Marginal' = 'gold'))+
  labs(title = (expression(paste("Clara soil water content - mean values"))), x="Day of the year", y = expression(paste ("Volumetric water content ( ", m^-3 ," ", m^-3, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOY_moi_facet<-ggplot(Clara.flux_moi, aes(x=DOY, y=moi, colour=type))+
  facet_wrap(~Plot)+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.2)+
  scale_y_continuous(breaks = seq(0, 0.85, by =0.2))+
  coord_cartesian(ylim = c(0, 0.85))+
  scale_x_continuous(breaks = seq(0, 365, by =60))+
  scale_colour_manual(name="", values=c('Central'="olivedrab3", 'Marginal'="sienna1", 'Sub-Central'="mediumaquamarine", 'Sub-Marginal' = 'gold'))+
  labs(title = (expression(paste("Clara soil water content by collar"))), x="Day of the year", y = expression(paste ("Volumetric water content ( ", m^-3 ," ", m^-3, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 9, colour='black'),
        axis.text.x = element_text(size = 9, angle = 90, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        axis.title.x = element_text(size = 15, vjust = -1),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOY_moi_facet_Year<-ggplot(Clara.flux_moi, aes(x=DOY, y=moi, colour=type))+
  facet_wrap(~Year)+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.1)+
  scale_y_continuous(breaks = seq(0, 0.85, by =0.1))+
  coord_cartesian(ylim = c(0, 0.85))+
  scale_x_continuous(breaks = seq(0, 365, by =40))+
  scale_colour_manual(name="", values=c('Central'="olivedrab3", 'Marginal'="sienna1", 'Sub-Central'="mediumaquamarine", 'Sub-Marginal' = 'gold'))+
  labs(title = (expression(paste("Clara soil water content by site"))), x="Day of the year", y = expression(paste ("Volumetric water content ( ", m^-3 ," ", m^-3, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 10, colour='black'),
        axis.text.x = element_text(size = 10, angle = 90, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        axis.title.x = element_text(size = 15, vjust = -1),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10),
        legend.justification=c(1,0), legend.position=c(1,0))

DOY_T2<-ggplot(Clara.fluxT, aes(x=DOY, y=soil_T, colour=Year))+
  geom_point(size=1)+
  scale_y_continuous(breaks = seq(0, 30, by =5))+
  coord_cartesian(ylim = c(0, 30))+
  scale_x_continuous(breaks = seq(0, 365, by =20))+
  labs(title = (expression(paste("Soil temperature at Clara"))), x="Day of the year", y = "Soil temperature (°C)") +
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOY_T_facet_Year<-ggplot(Clara.fluxT, aes(x=DOY, y=soil_T, colour=type))+
  facet_wrap(~Year)+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.1)+
  scale_y_continuous(breaks = seq(0, 30, by =5))+
  coord_cartesian(ylim = c(0, 30))+
  scale_x_continuous(breaks = seq(0, 365, by =40))+
  scale_colour_manual(name="", values=c('Central'="olivedrab3", 'Marginal'="sienna1", 'Sub-Central'="mediumaquamarine", 'Sub-Marginal' = 'gold'))+
  labs(title = (expression(paste("Clara soil temperature by site"))), x="Day of the year", y = "Soil temperature (°C)")+
  theme_bw()+
  theme(axis.text = element_text(size = 9, colour='black'),
        axis.text.x = element_text(size = 9, angle = 90, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        axis.title.x = element_text(size = 15, vjust = -1),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

T_moi_impact<-ggplot(Clara.flux_moi, aes(x=soil_T, y=moi))+
  geom_point(size=1.5)+
  stat_smooth(method=lm, colour="red")+
  scale_y_continuous(breaks = seq(0, 0.85, by =0.1))+
  coord_cartesian(ylim = c(0, 0.85))+
  scale_x_continuous(breaks = seq(0, 40, by =5))+
  labs(title = (expression(paste("Soil temperature influence on soil water content (Clara)"))), x="Soil temperature (°C)", y = expression(paste ("Volumetric water content ( ", m^-3 ," ", m^-3, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

T_moi_impact_type<-ggplot(Clara.flux_moi, aes(x=soil_T, y=moi, colour=type))+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.2)+
  scale_y_continuous(breaks = seq(0, 0.8, by =0.1))+
  coord_cartesian(ylim = c(0, 0.8))+
  scale_x_continuous(breaks = seq(0, 40, by =5))+
  scale_colour_manual(name="", values=c('Central'="olivedrab3", 'Marginal'="sienna1", 'Sub-Central'="mediumaquamarine", 'Sub-Marginal' = 'gold'))+
  labs(title = (expression(paste("Soil temperature influence on soil water content by site (Clara)"))), x="Soil temperature (°C)", y = expression(paste ("Volumetric water content ( ", m^-3 ," ", m^-3, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))


###########################

Month_CO2<-ggplot(Clara.fluxCO2, aes(x=Month, y=CO2, colour=Year))+
  geom_point(size=1)+
  stat_smooth(method=loess)+
  scale_y_continuous(breaks = seq(0, 6, by =1))+
  coord_cartesian(ylim = c(0, 6))+
  scale_x_continuous(breaks = seq(1, 12, by =1))+
  labs(title = (expression(paste("ER flux from Clara"))), x="Month", y = expression(paste ("ER flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
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


DOY_CO2_shape<-ggplot(Clara.fluxCO2, aes(x=DOY, y=CO2, colour=type, shape=Year))+
  geom_point(size=2)+
  stat_smooth(method=loess, alpha=0.2)+
  scale_y_continuous(breaks = seq(0, 6, by =1))+
  coord_cartesian(ylim = c(0, 6))+
  scale_x_continuous(breaks = seq(0, 365, by =20))+
  scale_shape_manual(name="Year", values=c('2020'=0, '2021'=17, '2022'=16))+
  scale_colour_manual(name="", values=c('Central'="olivedrab3", 'Marginal'="sienna1", 'Sub-Central'="mediumaquamarine", 'Sub-Marginal' = 'gold'))+
  labs(title = (expression(paste("ER flux from Clara"))), x="Day of the year", y = expression(paste ("ER flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
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


DOY_CO2_facet<-ggplot(Clara.fluxCO2, aes(x=DOY, y=CO2, colour=type))+
  facet_wrap(~Year, ncol=2)+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.1)+
  scale_y_continuous(breaks = seq(0, 6, by =1))+
  coord_cartesian(ylim = c(0, 6), xlim = c(0, 360))+
  scale_x_continuous(breaks = seq(0, 365, by =30))+
  scale_colour_manual(name="", values=c('Central'="olivedrab3", 'Marginal'="sienna1", 'Sub-Central'="mediumaquamarine", 'Sub-Marginal' = 'gold'))+
  labs(title = (expression(paste("ER flux from Clara by year"))), x="Day of the year", y = expression(paste ("ER flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 11, colour='black'),
        axis.text.x = element_text(size = 10, angle = 60, hjust = 0.5, vjust = 0.6),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10),
        legend.justification=c(1,0), legend.position=c(1,0))

doy_a <- DOY_CO2_facet + guides(colour = guide_legend(override.aes = list(size = 2, shape = NA, fill=NA)))
print(doy_a)


DOY_CO2_collar<-ggplot(Clara.fluxCO2, aes(x=DOY, y=CO2, colour=type))+
  facet_wrap(~Plot)+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.3)+
  scale_y_continuous(breaks = seq(0, 6, by =2))+
  coord_cartesian(ylim = c(0, 6))+
  scale_x_continuous(breaks = seq(0, 365, by =60))+
  scale_colour_manual(name="", values=c('Central'="olivedrab3", 'Marginal'="sienna1", 'Sub-Central'="mediumaquamarine", 'Sub-Marginal' = 'gold'))+
  labs(title = (expression(paste("ER flux from Clara by collar"))), x="Day of the year", y = expression(paste ("ER flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 11, colour='black'),
        axis.text.x = element_text(size = 10, angle = 90, hjust = 0.5, vjust = 0.8),
        axis.title = element_text(size = 15),
        axis.title.x = element_text(size = 15, vjust = -1),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

CO2_collar<-ggplot(Clara.fluxCO2, aes(x=Plot, y=CO2, colour=type))+
  geom_boxplot(width=.4, outlier.size=1, lwd=1)+
  scale_y_continuous(breaks = seq(0, 6, by =1))+
  coord_cartesian(ylim = c(0, 6))+
  scale_colour_manual(name="", values=c('Central'="olivedrab3", 'Marginal'="sienna1", 'Sub-Central'="mediumaquamarine", 'Sub-Marginal' = 'gold'))+
  labs(title = (expression(paste("ER flux from Clara by vegetation type"))), x="Collar", y = expression(paste ("ER flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=1.5, stroke=1.5, colour='black')+
  theme_bw()+
  theme(axis.text = element_text(size = 13, colour='black'),
        axis.text.x = element_text(size=11, angle = 90, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        axis.title.x = element_text(size = 15, vjust = -1),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))


dateCO2<-ggplot(Clara.fluxCO2, aes(x=Date, y=CO2, colour=type))+
  geom_point(size=1.5)+
  scale_y_continuous(breaks = seq(0, 6, by =1))+
  coord_cartesian(ylim = c(0, 6))+
  scale_colour_manual(name="", values=c('Central'="olivedrab3", 'Marginal'="sienna1", 'Sub-Central'="mediumaquamarine", 'Sub-Marginal'='gold'))+
  scale_x_date(date_breaks = "30 day", date_labels = "%d/%m/%Y")+
  labs(title = (expression(paste("ER flux from Clara by vegetation type"))), x="Date", y = expression(paste ("ER flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOYCO2type<-ggplot(Clara.fluxCO2, aes(x=DOY, y=CO2, colour=type))+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.3)+
  facet_wrap(~type)+
  scale_y_continuous(breaks = seq(0, 6, by =1))+
  coord_cartesian(ylim = c(0, 6))+
  scale_x_continuous(breaks = seq(0, 365, by =30))+
  scale_colour_manual(name="", values=c('Central'="olivedrab3", 'Marginal'="sienna1", 'Sub-Central'="mediumaquamarine", 'Sub-Marginal'='gold'))+
  labs(title = (expression(paste("ER flux from Clara by location"))), x="DOY", y = expression(paste ("ER flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

moi_CO2type<-ggplot(Clara.fluxCO2env, aes(x=moi, y=CO2, colour=type))+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.2)+
  scale_y_continuous(breaks = seq(0, 6, by =1))+
  coord_cartesian(ylim = c(0, 6))+
  scale_x_continuous(breaks = seq(0, 0.8, by =0.1))+
  scale_colour_manual(name="type", values=c('Central'="olivedrab3", 'Marginal'="sienna1", 'Sub-Central'="mediumaquamarine", 'Sub-Marginal'='gold'))+
  labs(title = (expression(paste("Clara ", CO[2], " flux by location"))), x=expression(paste ("Volumetric water content ( ", m^-3 ," ", m^-3, ")", sep="")), y = expression(paste ("ER flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

T_CO2type<-ggplot(Clara.fluxCO2env, aes(x=soil_T, y=CO2, colour=type))+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.2)+
  scale_y_continuous(breaks = seq(0, 6, by =1))+
  coord_cartesian(ylim = c(0, 6))+
  scale_x_continuous(breaks = seq(0, 35, by =5))+
  scale_colour_manual(name="", values=c('Central'="olivedrab3", 'Marginal'="sienna1", 'Sub-Central'="mediumaquamarine", 'Sub-Marginal'='gold'))+
  labs(title = (expression(paste("Clara ", CO[2], " flux by location"))), x="Soil temperature (°C)", y = expression(paste ("ER flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOY_CO2_year<-ggplot(Clara.fluxCO2, aes(x=DOY, y=CO2, colour=Year))+
  geom_point(size=1)+
  stat_smooth(method=loess)+
  scale_y_continuous(breaks = seq(0, 6, by =1))+
  coord_cartesian(ylim = c(0, 6))+
  scale_x_continuous(breaks = seq(0, 365, by =30))+
  labs(title = (expression(paste("Clara ", CO[2], " flux by year"))), x="Day of the year", y = expression(paste ("ER flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10),
        legend.key.size = unit(1, "cm"))

yearco2 <- DOY_CO2_year + 
guides(colour = guide_legend(override.aes = list(size = 2, shape = NA, fill=NA)))
print(yearco2)

DOYCO2type<-ggplot(Clara.fluxCO2, aes(x=DOY, y=CO2, colour=type))+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.1)+
  scale_y_continuous(breaks = seq(0, 6, by =1))+
  coord_cartesian(ylim = c(0, 6))+
  scale_x_continuous(breaks = seq(0, 365, by =30))+
  scale_colour_manual(name="", values=c('Central'="olivedrab3", 'Marginal'="sienna1", 'Sub-Central'="mediumaquamarine", 'Sub-Marginal'='gold'))+
  labs(title = (expression(paste("Clara ", CO[2], " flux by location"))), x="Day of the year", y = expression(paste ("ER flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

co2t <- DOYCO2type + 
guides(colour = guide_legend(override.aes = list(size = 2, shape = NA, fill=NA)))
print(co2t)
#############################

CH4_collar<-ggplot(Clara.fluxCH4, aes(x=Plot, y=CH4, colour=type))+
  geom_boxplot(width=.4, outlier.size=1, lwd=1)+
  scale_y_continuous(breaks = seq(-5, 70, by =5))+
  coord_cartesian(ylim = c(-5, 70))+
  scale_colour_manual(name="", values=c('Central'="olivedrab3", 'Marginal'="sienna1", 'Sub-Central'="mediumaquamarine", 'Sub-Marginal'='gold'))+
  labs(title = (expression(paste("Clara ", CH[4], " flux by collar"))), x="Collar", y = expression(paste ("", CH[4]," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=1, stroke=1.3, colour='black')+
  theme_bw()+
  theme(axis.text = element_text(size = 13, colour='black'),
        axis.text.x = element_text(size = 11, angle = 90, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        axis.title.x = element_text(size = 15, vjust = -1),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOY_CH4_collar<-ggplot(Clara.fluxCH4, aes(x=DOY, y=CH4, colour=type))+
  facet_wrap(~Plot)+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.3)+
  scale_y_continuous(breaks = seq(-5, 70, by =20))+
  coord_cartesian(ylim = c(-5, 70))+
  scale_x_continuous(breaks = seq(0, 365, by =60))+
  scale_colour_manual(name="", values=c('Central'="olivedrab3", 'Marginal'="sienna1", 'Sub-Central'="mediumaquamarine", 'Sub-Marginal'='gold'))+
  labs(title = (expression(paste("Clara ", CH[4], " flux by collar"))), x="Day of the year", y = expression(paste ("", CH[4]," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 10, colour='black'),
        axis.text.x = element_text(size=10, angle = 90, hjust = 0.5, vjust = 0.8),
        axis.title = element_text(size = 15),
        axis.title.x = element_text(size = 15, vjust = -1),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))


DOYCH4type_facet<-ggplot(Clara.fluxCH4, aes(x=DOY, y=CH4, colour=type))+
  facet_wrap(~type)+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.3)+
  scale_y_continuous(breaks = seq(-10, 100, by =10))+
  coord_cartesian(ylim = c(-10, 100))+
  scale_x_continuous(breaks = seq(0, 365, by =30))+
  scale_colour_manual(name="", values=c('Central'="olivedrab3", 'Marginal'="sienna1", 'Sub-Central'="mediumaquamarine", 'Sub-Marginal'='gold'))+
  labs(title = (expression(paste("Clara ", CH[4], " flux by location"))), x="DOY", y = expression(paste ("", CH[4]," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOY_CH4_facet_year<-ggplot(Clara.fluxCH4, aes(x=DOY, y=CH4, colour=type))+
  facet_wrap(~Year, ncol=2)+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.3)+
  scale_y_continuous(breaks = seq(-10, 120, by =10))+
  coord_cartesian(ylim = c(-10, 120))+
  scale_x_continuous(breaks = seq(0, 365, by =30))+
  scale_colour_manual(name="", values=c('Central'="olivedrab3", 'Marginal'="sienna1", 'Sub-Central'="mediumaquamarine", 'Sub-Marginal' = 'gold'))+
  labs(title = (expression(paste("Clara ", CH[4], " flux by location"))), x="DOY", y = expression(paste ("", CH[4]," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 11, colour='black'),
        axis.text.x = element_text(size = 10, angle = 60, hjust = 0.5, vjust = 0.6),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10),
        legend.justification=c(1,0), legend.position=c(1,0))

DOY_CH4_facet_year_a <- DOY_CH4_facet_year + guides(colour = guide_legend(override.aes = list(size = 2, shape = NA, fill=NA)))
print(DOY_CH4_facet_year_a)

dateCH4<-ggplot(Clara.fluxCH4, aes(x=Date, y=CH4, colour=type))+
  geom_point(size=2)+
  scale_y_continuous(breaks = seq(-5, 70, by =5))+
  coord_cartesian(ylim = c(-5, 70))+
  scale_colour_manual(name="", values=c('Central'="olivedrab3", 'Marginal'="sienna1", 'Sub-Central'="mediumaquamarine", 'Sub-Marginal'='gold'))+
  scale_x_date(date_breaks = "30 day", date_labels = "%d/%m/%Y")+
  labs(title = (expression(paste("Clara ", CH[4], " flux by location"))), x="Date", y = expression(paste ("", CH[4]," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DOYCH4type<-ggplot(Clara.fluxCH4, aes(x=DOY, y=CH4, colour=type))+
  geom_point(size=2)+
  stat_smooth(method=loess, alpha=0.2)+
  scale_y_continuous(breaks = seq(-10, 100, by =20))+
  coord_cartesian(ylim = c(-10, 100))+
  scale_x_continuous(breaks = seq(0, 365, by =30))+
  scale_colour_manual(name="", values=c('Central'="olivedrab3", 'Marginal'="sienna1", 'Sub-Central'="mediumaquamarine", 'Sub-Marginal'='gold'))+
  labs(title = (expression(paste("Clara ", CH[4], " flux by location"))), x="Day of the year", y = expression(paste ("", CH[4]," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

ch4t <- DOYCH4type + 
guides(colour = guide_legend(override.aes = list(size = 2, shape = NA, fill=NA)))
print(ch4t)

DOY_CH4_year<-ggplot(Clara.fluxCH4, aes(x=DOY, y=CH4, colour=Year))+
  geom_point(size=1)+
  stat_smooth(method=loess)+
  scale_y_continuous(breaks = seq(-10, 100, by =10))+
  coord_cartesian(ylim = c(-10, 100))+
  scale_x_continuous(breaks = seq(0, 365, by =30))+
  labs(title = (expression(paste("Clara ", CH[4], " flux by year"))), x="Day of the year", y = expression(paste ("", CH[4]," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
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

DOY_CH4_facet<-ggplot(Clara.fluxCH4, aes(x=DOY, y=CH4, colour=type))+
  facet_wrap(~Year)+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.1)+
  scale_y_continuous(breaks = seq(-20, 160, by =20))+
  coord_cartesian(ylim = c(-20, 160), xlim=c(0, 360))+
  scale_x_continuous(breaks = seq(0, 365, by =30))+
  scale_colour_manual(name="", values=c('Central'="olivedrab3", 'Marginal'="sienna1", 'Sub-Central'="mediumaquamarine", 'Sub-Marginal'='gold'))+
  labs(title = (expression(paste("Clara ", CH[4], " flux by location and year"))), x="Day of the year", y = expression(paste ("", CH[4]," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 11, colour='black'),
        axis.text.x = element_text(size = 11, angle = 60, hjust = 0.5, vjust = 0.6),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 10),
        legend.title=element_blank(),
        strip.text = element_text(size = 11),
        legend.justification=c(1,0), legend.position=c(1,0),
        legend.direction="vertical")

doy_ch4 <- DOY_CH4_facet + 
  guides(colour = guide_legend(ncol=2, override.aes = list(size = 2, shape = NA, fill=NA)))
print(doy_ch4)

moi_CH4type<-ggplot(Clara.fluxCH4env, aes(x=moi, y=CH4, colour=type))+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0)+
  scale_y_continuous(breaks = seq(-10, 120, by =20))+
  coord_cartesian(ylim = c(-10, 120))+
  scale_x_continuous(breaks = seq(0, 0.8, by =0.1))+
  scale_colour_manual(name="", values=c('Central'="olivedrab3", 'Marginal'="sienna1", 'Sub-Central'="mediumaquamarine", 'Sub-Marginal'='gold'))+
  labs(title = (expression(paste("Clara ", CH[4], " flux by location"))), x=expression(paste ("Volumetric water content ( ", m^-3 ," ", m^-3, ")", sep="")), y = expression(paste ("", CH[4]," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

T_CH4type<-ggplot(Clara.fluxCH4env, aes(x=soil_T, y=CH4, colour=type))+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0)+
  scale_y_continuous(breaks = seq(-10, 120, by =20))+
  coord_cartesian(ylim = c(-10, 120))+
  scale_x_continuous(breaks = seq(0, 35, by =5))+
  scale_colour_manual(name="", values=c('Central'="olivedrab3", 'Marginal'="sienna1", 'Sub-Central'="mediumaquamarine", 'Sub-Marginal'='gold'))+
  labs(title = (expression(paste("Clara ", CH[4], " flux by location"))), x="Soil temperature (°C)", y = expression(paste ("", CH[4]," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

T_CH4type_year<-ggplot(Clara.fluxCH4env, aes(x=soil_T, y=CH4, colour=type))+
  facet_wrap(~Year)+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.1)+
  scale_y_continuous(breaks = seq(-20, 120, by =20))+
  coord_cartesian(ylim = c(-20, 120), xlim=c(0, 30))+
  scale_x_continuous(breaks = seq(0, 30, by =5))+
  scale_colour_manual(name="", values=c('Central'="olivedrab3", 'Marginal'="sienna1", 'Sub-Central'="mediumaquamarine", 'Sub-Marginal'='gold'))+
  labs(title = (expression(paste("Relationship between soil temperature and ", CH[4], " flux by year (Clara)"))), x="Soil temperature (°C)", y = expression(paste ("", CH[4]," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 10),
        legend.title=element_blank(),
        strip.text = element_text(size = 10),
        legend.justification=c(1,0), legend.position=c(1,0))

T_doy_ch4 <- T_CH4type_year + 
  guides(colour = guide_legend(ncol=4, override.aes = list(size = 2, shape = NA, fill=NA)))
print(T_doy_ch4)

moi_CH4type_year<-ggplot(Clara.fluxCH4env, aes(x=moi, y=CH4, colour=type))+
  facet_wrap(~Year)+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.1)+
  scale_y_continuous(breaks = seq(-10, 140, by =20))+
  coord_cartesian(ylim = c(-10, 140), xlim=c(0, 0.9))+
  scale_x_continuous(breaks = seq(0, 0.9, by =0.1))+
  scale_colour_manual(name="", values=c('Central'="olivedrab3", 'Marginal'="sienna1", 'Sub-Central'="mediumaquamarine", 'Sub-Marginal'='gold'))+
  labs(title = (expression(paste("Relationship between soil water content and ", CH[4], " flux by year (Clara)"))), x=expression(paste ("Volumetric water content ( ", m^-3 ," ", m^-3, ")", sep="")), y = expression(paste ("", CH[4]," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13, colour='black'),
        axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 10),
        legend.title=element_blank(),
        strip.text = element_text(size = 10))

moi_year <- moi_CH4type_year + 
  guides(colour = guide_legend(override.aes = list(size = 2, shape = NA, fill=NA)))
print(moi_year)

T_CO2type_year<-ggplot(Clara.fluxCO2env, aes(x=soil_T, y=CO2, colour=type))+
  facet_wrap(~Year)+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.1)+
  scale_y_continuous(breaks = seq(0, 6, by =1))+
  coord_cartesian(ylim = c(0, 6))+
  scale_x_continuous(breaks = seq(0, 30, by =5))+
  scale_colour_manual(name="", values=c('Central'="olivedrab3", 'Marginal'="sienna1", 'Sub-Central'="mediumaquamarine", 'Sub-Marginal'='gold'))+
  labs(title = (expression(paste("Relationship between soil temperature and ", CO[2], " flux by year (Clara)"))), x="Soil temperature (°C)", y = expression(paste ("ER flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13, colour='black'),
        axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 10),
        legend.title=element_blank(),
        strip.text = element_text(size = 10))

T_CO2type <- T_CO2type_year + 
  guides(colour = guide_legend(override.aes = list(size = 2, shape = NA, fill=NA)))
print(T_CO2type)

moi_CO2type_year<-ggplot(Clara.fluxCO2env, aes(x=moi, y=CO2, colour=type))+
  facet_wrap(~Year)+
  geom_point(size=1.5)+
  stat_smooth(method=loess, alpha=0.1)+
  scale_y_continuous(breaks = seq(0, 6, by =1))+
  coord_cartesian(ylim = c(0, 6), xlim=c(0, 0.9))+
  scale_x_continuous(breaks = seq(0, 0.9, by =0.1))+
  scale_colour_manual(name="", values=c('Central'="olivedrab3", 'Marginal'="sienna1", 'Sub-Central'="mediumaquamarine", 'Sub-Marginal'='gold'))+
  labs(title = (expression(paste("Relationship between soil water content and ", CO[2], " flux by year (Clara)"))), x=expression(paste ("Volumetric water content ( ", m^-3 ," ", m^-3, ")", sep="")), y = expression(paste ("ER flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13, colour='black'),
        axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 10),
        legend.title=element_blank(),
        strip.text = element_text(size = 10))

moi_CO2type <- moi_CO2type_year + 
  guides(colour = guide_legend(override.aes = list(size = 2, shape = NA, fill=NA)))
print(moi_CO2type)
