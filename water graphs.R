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

str(WT)
summary(WT)
rm(list=ls())

WT<-filter(WT, year!="2023")
summarise(group_by(WT, site), mean_WT=mean(WT..cm.))

WT<-filter(WT, WT..cm.!="NA")
WT$WT..cm.<-(WT$WT..cm.*-1)
WT$plot <- recode_factor(WT$plot, 'W8/W7' = "W7")
WT_U<-filter(WT, site=="Ummeras")
WT_C<-filter(WT, site=="Cavemount")

WT_C<-within(WT_C, plot <- factor(plot,
levels=c('BP2/BP1', 'E4/E3', 'TF', 'TB', 'BS9','BS10', 'BS11', 'BS12', 'T1', 'T2', 'T3', 'T4')))
WT_U<-within(WT_U, plot <- factor(plot,
levels=c('Ex', 'E0', 'W4/E1', 'W3/E2', 'W7','W8', 'M11/M10', 'M111', 'D12', 'D13', 'D14')))
WT$type<-factor(WT$type, levels=c("Bare Peat", "Birch Scrub" ,"Wet", "Middle", "Dry"))

DOY_WT_U<-ggplot(WT_U, aes(x=DOY, y=WT..cm.))+
  facet_wrap(~plot)+
  geom_line(size=2, colour='lightblue2')+
  geom_point(size=2, colour='navyblue')+
  scale_y_continuous(breaks = seq(-100, 0, by =20))+
  coord_cartesian(ylim = c(-100, 0), xlim=c(0, 360))+
  scale_x_continuous(breaks = seq(0, 360, by =60))+
  labs(title = (expression(paste("Ummeras water table changes"))), x="Day of the year", y = expression(paste ("Water table depth (cm)")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(size=11, angle = 310, hjust = 0.5, vjust = 0, colour='black'),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

print(DOY_WT_U)

DOY_WT_C<-ggplot(WT_C, aes(x=DOY, y=WT..cm.))+
  facet_wrap(~plot)+
  geom_line(size=2, colour='lightblue2')+
  geom_point(size=2, colour='navyblue')+
  scale_y_continuous(breaks = seq(-100, 0, by =20))+
  coord_cartesian(ylim = c(-100, 0), xlim=c(0, 360))+
  scale_x_continuous(breaks = seq(0, 360, by =60))+
  labs(title = (expression(paste("Cavemount water table changes"))), x="Day of the year", y = expression(paste ("Water table depth (cm)")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(size=11, angle = 310, hjust = 0.5, vjust = 0, colour='black'),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

print(DOY_WT_C)

WT_type<-ggplot(WT, aes(x=DOY, y=WT..cm., colour=type))+
  facet_wrap(~site)+
  stat_smooth(method=loess, alpha=0.2)+
  geom_point(size=1.5)+
  scale_y_continuous(breaks = seq(-100, 0, by =20))+
  coord_cartesian(ylim = c(-100, 0), xlim=c(0, 360))+
  scale_x_continuous(breaks = seq(0, 360, by =60))+
  scale_colour_manual(name="", values=c('Birch Scrub'="limegreen", 'Dry'="brown4", 'Middle'="darkgoldenrod3", 'Bare Peat' = 'black', 'Wet'='royalblue'))+
  labs(title = (expression(paste("Water table fluctuations at Cavemount and Ummeras"))), x="Day of the year", y = expression(paste ("Water table depth (cm)")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(size=11, angle = 310, hjust = 0.5, vjust = 0, colour='black'),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

WT_type_1 <- WT_type + guides(colour = guide_legend(override.aes = list(shape = NA, fill=NA, size=2)))
print(WT_type_1)

WT_type2<-ggplot(WT, aes(x=DOY, y=WT..cm., colour=type))+
  facet_wrap(~year+site)+
  stat_smooth(method=loess, alpha=0.2)+
  geom_point(size=1.5)+
  scale_y_continuous(breaks = seq(-100, 0, by =20))+
  coord_cartesian(ylim = c(-100, 0), xlim=c(0, 360))+
  scale_x_continuous(breaks = seq(0, 360, by =60))+
  scale_colour_manual(name="", values=c('Birch Scrub'="limegreen", 'Dry'="brown4", 'Middle'="darkgoldenrod3", 'Bare Peat' = 'black', 'Wet'='royalblue'))+
  labs(title = (expression(paste("Water table fluctuations at Cavemount and Ummeras"))), x="Day of the year", y = expression(paste ("Water table depth (cm)")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(size=11, angle = 310, hjust = 0.5, vjust = 0, colour='black'),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

WT_type_2 <- WT_type2 + guides(colour = guide_legend(override.aes = list(shape = NA, fill=NA, size=2)))
print(WT_type_2)
########################

str(floating)
summary(floating)
min(floatingCH4_0$C.CH4)
max(floating$C.CH4)
summarise(group_by(floating, Point), mean_bio1=mean(C.CO2))

floating$Point <- recode_factor(floating$Point, '2a' = "2", '3a'='3')
floatingDOC<-filter(floating, DOC.mg.l!="NA" & type!='Eriophorum')
floating_env<-filter(floating, T_water!="NA" & type!='Eriophorum')
floating_env2<-filter(floating, DO_mg_l!="NA" & type!='Eriophorum')
floatingCO2_0<-filter(floating, C.CO2!="NA")
floatingCO2_1<-filter(floating, C.CO2!="NA" & T_water!="NA")
floatingCO2_2<-filter(floating, C.CO2!="NA" & DO_mg_l!="NA")
floatingCH4_0<-filter(floating, C.CH4!="NA")
floatingCH4_1<-filter(floating, C.CH4!="NA" & T_water!="NA")
floatingCH4_2<-filter(floating, C.CH4!="NA" & DO_mg_l!="NA")
floating_Points_water<-filter(floating, C.CO2!="NA" & C.CH4!="NA")

DOC_U<-filter(floatingDOC, site=="Ummeras")
DOC_C<-filter(floatingDOC, site=="Cavemount")
floatingGHG_0_U<-filter(floatingCO2_0, site=="Ummeras")
floatingGHG_1_U<-filter(floatingCO2_1, site=="Ummeras")
floatingGHG_2_U<-filter(floatingCO2_2, site=="Ummeras")
floatingGHG_0_C<-filter(floatingCH4_0, site=="Cavemount")
floatingGHG_1_C<-filter(floatingCH4_1, site=="Cavemount")
floatingGHG_2_C<-filter(floatingCH4_2, site=="Cavemount")

###################
floating_Points_water<-within(floating_Points_water, Point <- factor(Point,
levels=c('1', '2', '3', '4', '5','6', 'GL', 'GR', 'NW',
'SLW', 'SRW', 'T', 'SLE', 'SRE', 'NE')))

floating_point<-ggplot(floating_Points_water, aes(x=DOY, y=C.CO2, colour=site))+
  facet_wrap(~Point)+
  stat_smooth(method=loess, alpha=0.2)+
  geom_point(size=1)+
  scale_y_continuous(breaks = seq(-0.4, 0.2, by =0.1))+
  coord_cartesian(ylim = c(-0.4, 0.2), xlim=c(0, 360))+
  scale_x_continuous(breaks = seq(0, 360, by =60))+
  scale_colour_manual(name="", values=c('Cavemount'="tomato1", 'Ummeras'="mediumspringgreen"))+
  labs(title = (expression(paste("Water ", CO[2], " emissions - measurement points"))), x="Day of the year", y = expression(paste ("", CO[2]," flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(size=11, angle = 310, hjust = 0.5, vjust = 0, colour='black'),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))
floating_point_a <- floating_point + guides(colour = guide_legend(override.aes = list(shape = NA, fill=NA, size=2)))
print(floating_point_a)

floating_point2<-ggplot(floating_Points_water, aes(x=DOY, y=C.CH4, colour=site))+
  facet_wrap(~Point)+
  stat_smooth(method=loess, alpha=0.2)+
  geom_point(size=1)+
  scale_y_continuous(breaks = seq(-70, 400, by =100))+
  coord_cartesian(ylim = c(-70, 400), xlim=c(0, 360))+
  scale_x_continuous(breaks = seq(0, 360, by =60))+
  scale_colour_manual(name="", values=c('Cavemount'="tomato1", 'Ummeras'="mediumspringgreen"))+
  labs(title = (expression(paste("Water ", CH[4], " emissions - measurement points"))), x="Day of the year", y = expression(paste ("", CH[4]," Flux (mg C ",-CH[4]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(size=11, angle = 310, hjust = 0.5, vjust = 0, colour='black'),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))
floating_point_b <- floating_point2 + guides(colour = guide_legend(override.aes = list(shape = NA, fill=NA, size=2)))
print(floating_point_b)

floating_type_CO2<-ggplot(floatingCO2_0, aes(x=DOY, y=C.CO2, colour=type))+
  facet_wrap(~site)+
  stat_smooth(method=loess, alpha=0.2)+
  geom_point(size=1)+
  scale_y_continuous(breaks = seq(-0.4, 0.2, by =0.1))+
  coord_cartesian(ylim = c(-0.4, 0.2), xlim=c(0, 360))+
  scale_x_continuous(breaks = seq(0, 360, by =60))+
  scale_colour_manual(name="", values=c('Eriophorum'="lawngreen", 'Water'="steelblue"))+
  labs(title = (expression(paste("Water ", CO[2], " emissions"))), x="Day of the year", y = expression(paste ("", CO[2]," flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(size=11, angle = 310, hjust = 0.5, vjust = 0, colour='black'),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

floating_type_CO2a <- floating_type_CO2 + guides(colour = guide_legend(override.aes = list(shape = NA, fill=NA, size=2)))
print(floating_type_CO2a)

floating_type_CH4<-ggplot(floatingCH4_0, aes(x=DOY, y=C.CH4, colour=type))+
  facet_wrap(~site)+
  stat_smooth(method=loess, alpha=0.2)+
  geom_point(size=1)+
  scale_y_continuous(breaks = seq(-60, 300, by =30))+
  coord_cartesian(ylim = c(-60, 300), xlim=c(0, 360))+
  scale_x_continuous(breaks = seq(0, 360, by =60))+
  scale_colour_manual(name="", values=c('Eriophorum'="lawngreen", 'Water'="steelblue"))+
  labs(title = (expression(paste("Water ", CH[4], " emissions"))), x="Day of the year", y = expression(paste ("", CH[4]," Flux (mg C ",-CH[4]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(size=11, angle = 310, hjust = 0.5, vjust = 0, colour='black'),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

floating_type_CH4a <- floating_type_CH4 + guides(colour = guide_legend(override.aes = list(shape = NA, fill=NA, size=2)))
print(floating_type_CH4a)

barCO2C<-ggplot(floatingGHG_0_C, aes(x=type, y=C.CO2, colour=type))+
  geom_boxplot(width=.4, outlier.size=1, lwd=1)+
  scale_colour_manual(name="type", values=c('Eriophorum'="lawngreen", 'Water'="steelblue"))+
  scale_x_discrete(name="", breaks=c("Eriophorum", "Water"), labels=c("Eriophorum", "Water"))+
  scale_y_continuous(breaks = seq(-0.4, 0.2, by =0.1))+
  coord_cartesian(ylim = c(-0.4, 0.2))+
  labs(title = (expression(paste("Water ", CO[2], " emissions at Cavemount"))), x="plot", y = expression(paste ("", CO[2]," flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3, stroke=1.5)+
  theme_bw()+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_blank(),
        strip.text = element_text(size = 10),
        legend.key.size = unit(1, "cm"))

barCH4C<-ggplot(floatingGHG_0_C, aes(x=type, y=C.CH4, colour=type))+
  geom_boxplot(width=.4, outlier.size=1, lwd=1)+
  scale_colour_manual(name="type", values=c('Eriophorum'="lawngreen", 'Water'="steelblue"))+
  scale_x_discrete(name="", breaks=c("Eriophorum", "Water"), labels=c("Eriophorum", "Water"))+
  scale_y_continuous(breaks = seq(-60, 300, by =30))+
  coord_cartesian(ylim = c(-60, 300))+
  labs(title = (expression(paste("Water ", CH[4], " emissions at Cavemount"))), x="plot", y = expression(paste ("", CO[2]," flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3, stroke=1.5)+
  theme_bw()+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_blank(),
        strip.text = element_text(size = 10),
        legend.key.size = unit(1, "cm"))

floating_DOC_site<-ggplot(floatingDOC, aes(x=DOY, y=DOC.mg.l, colour=site))+
  stat_smooth(method=loess, alpha=0.2)+
  geom_point(size=1)+
  scale_y_continuous(breaks = seq(0, 110, by =10))+
  coord_cartesian(ylim = c(0, 110), xlim=c(0, 360))+
  scale_x_continuous(breaks = seq(0, 360, by =40))+
  scale_colour_manual(name="", values=c('Cavemount'="tomato1", 'Ummeras'="mediumspringgreen"))+
  labs(title = (expression(paste("DOC concentration in pond water"))), x="Day of the year", y = expression(paste ("DOC concentration ( ", mg ," ", l^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(size=11, angle = 310, hjust = 0.5, vjust = 0, colour='black'),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

floating_DOC_site_a <- floating_DOC_site + guides(colour = guide_legend(override.aes = list(shape = NA, fill=NA, size=2)))
print(floating_DOC_site_a)

floating_DOC_waterT<-ggplot(floatingDOC, aes(x=T_water, y=DOC.mg.l, colour=site))+
  stat_smooth(method=loess, alpha=0.2)+
  geom_point(size=1)+
  scale_y_continuous(breaks = seq(0, 110, by =10))+
  coord_cartesian(ylim = c(0, 110), xlim=c(0, 28))+
  scale_x_continuous(breaks = seq(0, 28, by =2))+
  scale_colour_manual(name="", values=c('Cavemount'="tomato1", 'Ummeras'="mediumspringgreen"))+
  labs(title = (expression(paste("Water temperature effects on DOC concentration in ponds"))), x="Water temperature (°C)", y = expression(paste ("DOC concentration ( ", mg ," ", l^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(size=11, angle = 0, hjust = 0.5, vjust = 0, colour='black'),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

floating_DOC_waterT_a <- floating_DOC_waterT + guides(colour = guide_legend(override.aes = list(shape = NA, fill=NA, size=2)))
print(floating_DOC_waterT_a)

floating_T<-ggplot(floating_env, aes(x=DOY, y=T_water, colour=site))+
  stat_smooth(method=loess, alpha=0.2)+
  geom_point(size=1)+
  scale_y_continuous(breaks = seq(0, 27, by =2))+
  coord_cartesian(ylim = c(0, 27), xlim=c(0, 360))+
  scale_x_continuous(breaks = seq(0, 360, by =40))+
  scale_colour_manual(name="", values=c('Cavemount'="tomato1", 'Ummeras'="mediumspringgreen"))+
  labs(title = (expression(paste("Pond water temperature"))), x="Day of the year", y = "Water temperature (°C)")+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(size=11, angle = 310, hjust = 0.5, vjust = 0, colour='black'),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

floating_T_a <- floating_T + guides(colour = guide_legend(override.aes = list(shape = NA, fill=NA, size=2)))
print(floating_T_a)

floatingCH4_T<-ggplot(floatingCH4_1, aes(x=T_water, y=C.CH4, colour=type))+
  facet_wrap(~site)+
  stat_smooth(method=loess, alpha=0.2)+
  geom_point(size=1)+
  scale_y_continuous(breaks = seq(-60, 300, by =30))+
  coord_cartesian(ylim = c(-60, 300), xlim=c(0, 28))+
  scale_x_continuous(breaks = seq(0, 28, by =3))+
  scale_colour_manual(name="", values=c('Eriophorum'="lawngreen", 'Water'="steelblue"))+
  labs(title = (expression(paste("Water ", CH[4], " emissions"))), x="Water temperature (°C)", y = expression(paste ("", CH[4]," Flux (mg C ",-CH[4]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(size=11, angle = 0, hjust = 0.5, vjust = 0, colour='black'),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

floatingCH4_Ta <- floatingCH4_T + guides(colour = guide_legend(override.aes = list(shape = NA, fill=NA, size=2)))
print(floatingCH4_Ta)

floatingCO2_T<-ggplot(floatingCO2_1, aes(x=T_water, y=C.CO2, colour=type))+
  facet_wrap(~site)+
  stat_smooth(method=loess, alpha=0.2)+
  geom_point(size=1)+
  scale_y_continuous(breaks = seq(-0.2, 0.2, by =0.1))+
  coord_cartesian(ylim = c(-0.2, 0.2), xlim=c(0, 28))+
  scale_x_continuous(breaks = seq(0, 28, by =3))+
  scale_colour_manual(name="", values=c('Eriophorum'="lawngreen", 'Water'="steelblue"))+
  labs(title = (expression(paste("Water ", CO[2], " emissions"))), x="Water temperature (°C)", y = expression(paste ("", CO[2]," flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(size=11, angle = 0, hjust = 0.5, vjust = 0, colour='black'),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

floatingCO2_Ta <- floatingCO2_T + guides(colour = guide_legend(override.aes = list(shape = NA, fill=NA, size=2)))
print(floatingCO2_Ta)

floatingCH4_DO<-ggplot(floatingCH4_2, aes(x=DO_mg_l, y=C.CH4, colour=type))+
  facet_wrap(~site)+
  stat_smooth(method=loess, alpha=0.2)+
  geom_point(size=1)+
  scale_y_continuous(breaks = seq(-60, 300, by =30))+
  coord_cartesian(ylim = c(-60, 300), xlim=c(0, 13))+
  scale_x_continuous(breaks = seq(0, 13, by =1))+
  scale_colour_manual(name="", values=c('Eriophorum'="lawngreen", 'Water'="steelblue"))+
  labs(title = (expression(paste("Water ", CH[4], " emissions and Dissolved Oxygen"))), x=expression(paste ("Dissolved Oxygen ( ", mg ," ", l^-1, ")", sep="")), y = expression(paste ("", CH[4]," Flux (mg C ",-CH[4]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(size=11, angle = 0, hjust = 0.5, vjust = 0, colour='black'),
        axis.title = element_text(size = 15),
        legend.position = "none",
        strip.text = element_text(size = 10))


floating_DO<-ggplot(floating_env2, aes(x=DOY, y=DO_mg_l, colour=site))+
  stat_smooth(method=loess, alpha=0.2)+
  geom_point(size=1)+
  scale_y_continuous(breaks = seq(0, 14, by =1))+
  coord_cartesian(ylim = c(0, 14), xlim=c(0, 360))+
  scale_x_continuous(breaks = seq(0, 360, by =40))+
  scale_colour_manual(name="", values=c('Cavemount'="tomato1", 'Ummeras'="mediumspringgreen"))+
  labs(title = (expression(paste("Pond water dissolved oxygen"))), x="Day of the year", y=expression(paste ("Dissolved Oxygen ( ", mg ," ", l^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(size=11, angle = 310, hjust = 0.5, vjust = 0, colour='black'),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

floating_DO_a <- floating_DO + guides(colour = guide_legend(override.aes = list(shape = NA, fill=NA, size=2)))
print(floating_DO_a)

floating_DO_T<-ggplot(floating_env2, aes(x=T_water, y=DO_mg_l, colour=site))+
  stat_smooth(method=lm, alpha=0.2)+
  geom_point(size=1)+
  scale_y_continuous(breaks = seq(0, 14, by =1))+
  coord_cartesian(ylim = c(0, 14), xlim=c(0, 28))+
  scale_x_continuous(breaks = seq(0, 28, by =2))+
  scale_colour_manual(name="", values=c('Cavemount'="tomato1", 'Ummeras'="mediumspringgreen"))+
  labs(title = (expression(paste("Pond water dissolved oxygen"))), x="Water temperature (°C)", y=expression(paste ("Dissolved Oxygen ( ", mg ," ", l^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(size=11, angle = 0, hjust = 0.5, vjust = 0, colour='black'),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

floating_DO_T_a <- floating_DO_T + guides(colour = guide_legend(override.aes = list(shape = NA, fill=NA, size=2)))
print(floating_DO_T_a)

