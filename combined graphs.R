lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
library(Matrix)
library(lme4)
library(nlme)
library(MuMIn)
library(gridExtra)
library(dplyr)
library(ggplot2)
library(car)
library(LMERConvenienceFunctions)
library(multcomp)
library(lsmeans)
library(lattice)
library(forcats)

rm(list=ls())

Cavemount.flux$type <- recode_factor(Cavemount.flux$type, 'Eriophorum'="Eriophorum (C)")
Ummeras.flux$type <- recode_factor(Ummeras.flux$type, 'Eriophorum'="Eriophorum (U)")
Ummeras.flux<-mutate(Ummeras.flux, site="Ummeras")
Cavemount.flux<-mutate(Cavemount.flux, site="Cavemount")
combined<-rbind(Ummeras.flux, Cavemount.flux)

combined<-filter(combined, CO2!="NA" & CH4!="NA" & action!="delete" & type!='Sphagnum')
combined$type <- recode_factor(combined$type, 'Bare Peat Dry'="Bare Peat Dry (U)" , 'Bare Peat Middle'="Bare Peat Middle (U)", "Bare Peat Wet" = "Bare Peat Wet (U)",
                               'Bare Peat' = 'Bare Peat (C)', 'Birch Scrub'='Birch Scrub (C)')
combined$DOY<-as.numeric(combined$DOY)

combinedER<-filter(combined, type=='Birch Scrub (C)' | type=='Eriophorum (U)' | type=='Eriophorum (C)')
combinedRh<-filter(combined, type=="Bare Peat (C)" | type== "Bare Peat Dry (U)" | type=="Bare Peat Middle (U)" | type=="Bare Peat Wet (U)")
combinedU<-filter(combined, site=='Ummeras')
combinedC<-filter(combined, site=='Cavemount')

combinedER<-within(combinedER, type <-factor(type, levels=c('Birch Scrub (C)', 'Eriophorum (C)', 'Eriophorum (U)')))

ER_point<-ggplot(combinedER, aes(x=DOY, y=CO2, colour=type))+
  facet_wrap(~Year)+
  geom_point(size=1.7)+
  stat_smooth(method=loess, alpha=0.2, size=0.8)+
  scale_x_continuous(breaks = seq(0, 365, by =60))+
  coord_cartesian(xlim = c(0, 360), ylim = c(0, 13))+
  scale_y_continuous(breaks = seq(0, 13, by =2))+
  scale_colour_manual(name="", values=c('Birch Scrub (C)'="chocolate", 'Eriophorum (C)'="green2", 'Eriophorum (U)'="olivedrab"))+
  labs(title = (expression(paste("ER flux (both sites)"))), x="Day of the year", y = expression(paste ("", CO[2]," flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 10, colour='black'),
        axis.text.x = element_text(size = 10, angle = 50, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        axis.title.x = element_text(size = 15, vjust = -1),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

ER_point_a <- ER_point + guides(colour = guide_legend(override.aes = list(shape = NA, fill=NA, size=3)))
print(ER_point_a)

Rh_point<-ggplot(combinedRh, aes(x=DOY, y=CO2, colour=type))+
  facet_wrap(~Year)+
  geom_point(size=1.7)+
  stat_smooth(method=loess, alpha=0.2, size=0.8)+
  scale_x_continuous(breaks = seq(0, 365, by =60))+
  coord_cartesian(xlim = c(0, 360), ylim = c(0, 10))+
  scale_y_continuous(breaks = seq(0, 10, by =2))+
  scale_colour_manual(name="", values=c('Bare Peat Dry (U)'="brown4", 'Bare Peat Middle (U)'="darkgoldenrod3", 'Bare Peat Wet (U)'="thistle4", 'Bare Peat (C)' = 'black'))+
  labs(title = (expression(paste("Rh flux (both sites)"))), x="Day of the year", y = expression(paste ("", CO[2]," flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 10, colour='black'),
        axis.text.x = element_text(size = 10, angle = 50, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        axis.title.x = element_text(size = 15, vjust = -1),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

Rh_point_a <- Rh_point + guides(colour = guide_legend(override.aes = list(shape = NA, fill=NA, size=3)))
print(Rh_point_a)

CH4U<-ggplot(combinedU, aes(x=DOY, y=CH4, colour=type))+
  facet_wrap(~Year)+
  geom_point(size=1.7)+
  stat_smooth(method=loess, alpha=0.2, size=0.8)+
  scale_x_continuous(breaks = seq(0, 365, by =60))+
  coord_cartesian(xlim = c(0, 360), ylim = c(0, 180))+
  scale_y_continuous(breaks = seq(0, 180, by =40))+
  scale_colour_manual(name="", values=c('Bare Peat Dry (U)'="brown4", 'Bare Peat Middle (U)'="darkgoldenrod3", 'Bare Peat Wet (U)'="thistle4", 'Eriophorum (U)'="olivedrab"))+
  labs(title = (expression(paste("", CH[4], " flux from Ummeras"))), x="Day of the year", y = expression(paste ("", CH[4]," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 10, colour='black'),
        axis.text.x = element_text(size = 10, angle = 50, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        axis.title.x = element_text(size = 15, vjust = -1),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

CH4U_a <- CH4U + guides(colour = guide_legend(override.aes = list(shape = NA, fill=NA, size=3)))
print(CH4U_a)

CH4C<-ggplot(combinedC, aes(x=DOY, y=CH4, colour=type))+
  facet_wrap(~Year)+
  geom_point(size=1.7)+
  stat_smooth(method=loess, alpha=0.2, size=0.8)+
  scale_x_continuous(breaks = seq(0, 365, by =60))+
  coord_cartesian(xlim = c(0, 360), ylim = c(-5, 20))+
  scale_y_continuous(breaks = seq(-5, 20, by =5))+
  scale_colour_manual(name="", values=c('Bare Peat (C)'="black", 'Birch Scrub (C)'="chocolate", 'Eriophorum (C)'="green2"))+
  labs(title = (expression(paste("", CH[4], " flux from Cavemount"))), x="Day of the year", y = expression(paste ("", CH[4]," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 10, colour='black'),
        axis.text.x = element_text(size = 10, angle = 50, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        axis.title.x = element_text(size = 15, vjust = -1),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

CH4C_a <- CH4C + guides(colour = guide_legend(override.aes = list(shape = NA, fill=NA, size=3)))
print(CH4C_a)



scale_colour_manual(name="", values=c('Bare Peat'="black", 'Birch Scrub'="chocolate", 'Eriophorum'="chartreuse4"))+
scale_colour_manual(name="", values=c('Bare Peat Dry'="brown4", 'Bare Peat Middle'="darkgoldenrod3", 'Bare Peat Wet'="royalblue", 'Eriophorum' = 'green2'))+

  