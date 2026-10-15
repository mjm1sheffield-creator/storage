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

arctic.daily.means$date <- as.Date(arctic.daily.means$date, "%d-%m-%Y")
arctic.daily.means$port<-as.factor(arctic.daily.means$port)
arctic.water.table$chamber<-as.factor(arctic.water.table$chamber)
arctic.daily.means$DOY<-as.numeric(arctic.daily.means$DOY)
arctic.daily.means<-filter(arctic.daily.means, CH4.mgC.CH4.m2.h!="NA")
arctic.CH4<-filter(arctic.water.table, CH4..week.!="NA")
arctic.tower<-filter(arctic.daily.means, type!="chamber")
arctic.chamber<-filter(arctic.daily.means, type!="tower")
arctic.chamber00<-filter(arctic.chamber, Temperature!='NA' & water.content!='NA')
arctic.chamber1<-filter(arctic.chamber, Temperature!='NA' & port!="6" & port!="15")
arctic.chamber2<-filter(arctic.chamber, water.content!='NA')

raw2csv$Port<-as.factor(raw2csv$Port)
raw0a<-filter(raw2csv, CH4.flux.mgC.CH4.m2.h!='NA' & CH4.flux.mgC.CH4.m2.h<=20 & CH4.flux.mgC.CH4.m2.h>=-1 
              & Cut.CH4!="cutoff")
raw0b<-filter(raw2csv, CO2.mg.m2.h !='NA')

arctic.d<-filter(arctic.daily.means, port!="6" & port!="15")
arctic.d2<-filter(arctic.daily.means, port!="15")

mod1<-lm(plant.growth.rate ~ soil.moisture.content, data=plant.growth.rate)
model1<-lmer(CO2..week.~ water.table +
               (1|chamber), data=arctic.water.table)
model2<-lmer(CO2..mg.m2.h.~ Temperature +
               (1|port), data=arctic.daily.means)


summary(model1)$r.squared
#r2 value (general)

r.squaredLR(model2)
#pseudo-r2 value for nonlinear regression models (Likelihood Ratio-based) (MuMIn package)

r.squaredGLMM(model1)
#pseudo-r2 value for mixed effect models models? (MuMIn package)
#####
arctic.daily.means$date <- as.Date(arctic.daily.means$date, "%d-%m-%Y")

plot1<-ggplot(arctic.daily.means, aes(x=date, y=CH4.mgC.CH4.m2.h))+
  geom_point(size=1)+
  scale_x_date(date_breaks = "10 day", date_labels = "%d-%b-%Y")+
  geom_smooth(method="lm", colour="red")+
  labs(title = (expression(paste("Average daily ", CH[4], " flux"))), x="Date", y = expression(paste (CH[4]," flux (mg C ",-CH[4]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

ggplot(arctic.daily.means, aes(x=date, y=CO2..mg.m2.h))+
  geom_point(size=1)+
  scale_x_date(date_breaks = "10 day", date_labels = "%d-%b-%Y")+
  geom_smooth(method="lm", colour="red")+
  labs(title = (expression(paste("Average daily ", CO[2], " flux"))), x="Date", y = expression(paste (CO[2]," flux (mg C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

ggplot(arctic.water.table, aes(x=week, y=CH4..week.))+
  geom_point(size=1)+
  geom_smooth(method="lm", colour="red")+
  scale_x_continuous(breaks = seq(33, 35,by =1))+
  coord_cartesian(xlim = c(33, 35))+
  labs(title = (expression(paste("Average weekly ", CH[4], " flux"))), x="Week", y = expression(paste (CH[4]," flux (mg C ",-CH[4]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

ggplot(arctic.water.table, aes(x=week, y=CO2..week.))+
  geom_point(size=1)+
  geom_smooth(method="lm", colour="red")+
  labs(title = (expression(paste("Average weekly ", CO[2], " flux"))), x="Week", y = expression(paste (CO[2]," flux (mg C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))
#date

arctic.daily.means<-mutate(arctic.daily.means, port=factor(port))

ggplot(arctic.daily.means, aes(x=Temperature, y=CO2..mg.m2.h., colour=port))+
 facet_wrap(~port)+
  geom_smooth(method="lm", colour="red")+
  geom_point(size=1)+
  scale_x_continuous(breaks = seq(-2,7.5,by =1))+
  coord_cartesian(xlim = c(-2, 7.5))+
  labs(title = (expression(paste("Average daily ", CO[2], " flux"))), x="Soil temperature (°C)", y = expression(paste (CO[2]," flux (mg C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

ggplot(arctic.daily.means, aes(x=water.content, y=CO2..mg.m2.h., colour=port))+
  facet_wrap(~port)+
  geom_point(size=1)+
  geom_smooth(method="lm", colour="red")+
  labs(title = (expression(paste("Average daily ", CO[2], " flux"))), x="Soil water content", y = expression(paste (CO[2]," flux (mg C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

ggplot(arctic.daily.means, aes(x=Temperature, y=CH4..mgC.CH4.m2.h., colour=port))+
  geom_point(size=1)+
  facet_wrap(~port)+
  geom_smooth(method="lm", colour="red")+
  scale_x_continuous(breaks = seq(0,7.5,by =1))+
  coord_cartesian(xlim = c(0, 7.5))+
  labs(title = (expression(paste("Average daily ", CH[4], " flux"))), x="Soil temperature (°C)", y = expression(paste (CH[4]," flux (mg C ",-CH[4]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

ggplot(arctic.daily.means, aes(x=water.content, y=CH4..mgC.CH4.m2.h., colour=port))+
  geom_point(size=1)+
  facet_wrap(~port)+
  geom_smooth(method="lm", colour="red")+
  scale_x_continuous(breaks = seq(0.3,0.75,by =0.1))+
  coord_cartesian(xlim = c(0.3, 0.75))+
  labs(title = (expression(paste("Average daily ", CH[4], " flux"))), x="Soil water content", y = expression(paste (CH[4]," flux (mg C ",-CH[4]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

#### day

ggplot(arctic.water.table, aes(x=water.table, y=CO2..week.))+
  geom_point(size=1)+
  geom_smooth(method="lm", colour="red")+
  scale_x_continuous(breaks = seq(-5,3,by =1))+
  coord_cartesian(xlim = c(-5, 3))+
  labs(title = (expression(paste("Average weekly ", CO[2], " flux"))), x="Water table depth (cm)", y = expression(paste (CO[2]," flux (mg C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

ggplot(arctic.water.table, aes(x=thaw.depth, y=CO2..week.))+
  geom_point(size=1)+
  geom_smooth(method="lm", colour="red")+
  scale_x_continuous(breaks = seq(-44,-32,by =2))+
  coord_cartesian(xlim = c(-44,-32))+
  labs(title = (expression(paste("Average weekly ", CO[2], " flux"))), x="Thaw depth (cm)", y = expression(paste (CO[2]," flux (mg C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

ggplot(arctic.water.table, aes(x=water.table, y=CH4..week.))+
  geom_point(size=1)+
  geom_smooth(method="lm", colour="red")+
  scale_x_continuous(breaks = seq(-5,0,by =1))+
  coord_cartesian(xlim = c(-5, 0))+
  labs(title = (expression(paste("Average weekly ", CH[4], " flux"))), x="Water table depth (cm)", y = expression(paste (CH[4]," flux (mg C ",-CH[4]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

ggplot(arctic.water.table, aes(x=thaw.depth, y=CH4..week.))+
  geom_point(size=1)+
  geom_smooth(method="lm", colour="red")+
  scale_x_continuous(breaks = seq(-44,-32,by =2))+
  coord_cartesian(xlim = c(-44,-32))+
  labs(title = (expression(paste("Average weekly ", CH[4], " flux"))), x="Thaw depth (cm)", y = expression(paste (CH[4]," flux (mg C ",-CH[4]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

## 
library(scales) 
library(lubridate)
a<- ymd("20150705")
z<- ymd("20150901")

geom_line()+

ggplot(arctic.daily.means, aes(x=date, y=CH4.mgC.CH4.m2.h, colour=type))+
  geom_point(size=2)+
  scale_y_continuous(breaks = seq(-3, 7, by =1))+
  coord_cartesian(ylim = c(-3, 7))+
  scale_x_date(date_breaks = "4 day", date_labels = "%d-%b-%Y", limits=c(a, z))+
  labs(title = (expression(paste("Average daily ", CH[4], " flux"))), x="Date", y = expression(paste (CH[4]," flux (mg C ",-CH[4]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

########################
arctic.water.table$date <- as.Date(arctic.water.table$date, "%d-%m-%Y")
a<- ymd("20150810")
z<- ymd("20150907")

r1<-summarise(group_by(arctic.water.table, date),
                mean_1=mean(water.table), 
                SEdr1=sd(water.table)/sqrt(length(water.table)),
                lwr1r=mean_1-SEdr1,
                upr1r=mean_1+SEdr1)
limits_1<-aes(ymin=lwr1r, ymax=upr1r)

ggplot(r1, aes(x=date, y=mean_1))+
  geom_point(size=3)+
  geom_line()+
  geom_errorbar(limits_1, width=0.2)+
  scale_y_continuous(breaks = seq(-4,2, by =1))+
  coord_cartesian(ylim = c(-4,2))+
  scale_x_date(date_labels = "%d-%b-%Y")+
  labs(title = (expression(paste(""))), x="", y = expression(paste ("Mean water table position (cm)")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

r2<-summarise(group_by(arctic.water.table, date),
              mean_2=mean(thaw.depth), 
              SEdr2=sd(thaw.depth)/sqrt(length(thaw.depth)),
              lwr2r=mean_2-SEdr2,
              upr2r=mean_2+SEdr2)
limits_2<-aes(ymin=lwr2r, ymax=upr2r)

ggplot(r2, aes(x=date, y=mean_2))+
  geom_point(size=3)+
  geom_line()+
  geom_errorbar(limits_2, width=0.2)+
  scale_y_continuous(breaks = seq(-40, -30, by =2))+
  coord_cartesian(ylim = c(-40, -30))+
  scale_x_date(date_labels = "%d-%b-%Y")+
  labs(title = (expression(paste(""))), x="", y = expression(paste ("Mean thaw depth (cm)")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

ggplot(arctic.water.table, aes(x=date, y=thaw.depth, colour=chamber))+
  geom_point(size=2)+
  geom_line()+
  scale_y_continuous(breaks = seq(-45, -30, by =5))+
  coord_cartesian(ylim = c(-45, -30))+
  scale_x_date(date_labels = "%d-%b-%Y")+
  labs(title = (expression(paste(""))), x="", y = expression(paste ("Thaw depth (cm)" )))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

ggplot(arctic.water.table, aes(x=date, y=difference, colour=chamber))+
  geom_point(size=2)+
  geom_line()+
  scale_y_continuous(breaks = seq(0, 45, by =5))+
  coord_cartesian(ylim = c(0, 45))+
  scale_x_date(date_labels = "%d-%b-%Y")+
  labs(title = (expression(paste(""))), x="", y = expression(paste ("Interval between thaw depth and water table (cm)" )))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

ggplot(arctic.water.table, aes(x=water.table, y=soil.water.content.mean))+
  geom_point(size=2)+
  scale_y_continuous(breaks = seq(0, 1, by =0.1))+
  coord_cartesian(ylim = c(0, 1))+
  scale_x_continuous(breaks = seq(-5, 4, by =1))+
  labs(title = (expression(paste(""))), x="", y = expression(paste ("Interval between thaw depth and water table (cm)" )))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

###############
a<- ymd("20150712")
b<- ymd("20150911")

ggplot(arctic.chamber1, aes(x=date, y=Temperature))+
  geom_point(size=1)+
  scale_y_continuous(breaks = seq(-2, 9, by =1))+
  coord_cartesian(ylim = c(-2, 9))+
  scale_x_date(date_breaks = "3 day", date_labels = "%d-%b-%Y", limits=c(a, b))+
  geom_smooth(method="lm", colour="red")+
  labs(title = (expression(paste())), x="", y = expression(paste ("Peat temperature (°C)")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

ggplot(arctic.chamber2, aes(x=date, y=water.content))+
  geom_point(size=1)+
  scale_y_continuous(breaks = seq(0, 1, by =0.1))+
  coord_cartesian(ylim = c(0, 1))+
  scale_x_date(date_breaks = "3 day", date_labels = "%d-%b-%Y")+
  geom_smooth(method="lm", colour="red")+
  labs(title = (expression(paste())), x="", y = expression(paste ("Volumetric water content ( ", m^-3 ," ", m^-3, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))
#######################################
raw2csv$Port<-as.factor(raw2csv$Port)
raw2csv$hour<-as.factor(raw2csv$hour)
raw1<-filter(raw2csv, Temperature!="NA" & Temperature!="0.000" & Temperature<=20 & Port!="15" & Port!="6")
raw2<-filter(raw2csv, soil.water.content!="NA"&soil.water.content<1&soil.water.content>0 & Port!="15" & Port!="6")
raw1$hour<-as.integer(raw1$hour)

r1<-summarise(group_by(raw1, hour),
              mean_1=mean(Temperature), 
              SEdr1=sd(Temperature)/sqrt(length(Temperature)),
              lwr1r=mean_1-SEdr1,
              upr1r=mean_1+SEdr1)
limits_1<-aes(ymin=lwr1r, ymax=upr1r)

ggplot(r1, aes(x=hour, y=mean_1))+
  geom_point(size=2)+
  geom_line()+
  geom_errorbar(limits_1, width=0.2)+
  scale_y_continuous(breaks = seq(0, 5, by =1))+
  coord_cartesian(ylim = c(0, 5))+
  labs(title = (expression(paste(""))), x="Hour", y = expression(paste ("Mean peat temperature (°C)")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

r2<-summarise(group_by(raw2, Hour),
              mean_2=mean(soil.water.content), 
              SEdr2=sd(soil.water.content)/sqrt(length(soil.water.content)),
              lwr2r=mean_2-SEdr2,
              upr2r=mean_2+SEdr2)
limits_2<-aes(ymin=lwr2r, ymax=upr2r)

ggplot(r2, aes(x=Hour, y=mean_2))+
  geom_point(size=2)+
  geom_line()+
  geom_errorbar(limits_2, width=0.2)+
  scale_y_continuous(breaks = seq(0, 1, by =0.1))+
  coord_cartesian(ylim = c(0, 1))+
  labs(title = (expression(paste(""))), x="Hour", y = expression(paste ("Volumetric water content ( ", m^-3 ," ", m^-3, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

r3<-summarise(group_by(raw0a, hour),
              mean_3=mean(CH4.flux.mgC.CH4.m2.h), 
              SEdr3=sd(CH4.flux.mgC.CH4.m2.h)/sqrt(length(CH4.flux.mgC.CH4.m2.h)),
              lwr2r=mean_3-SEdr3,
              upr2r=mean_3+SEdr3)
limits_3<-aes(ymin=lwr2r, ymax=upr2r)

ggplot(r3, aes(x=hour, y=mean_3))+
  geom_point(size=2)+
  geom_errorbar(limits_3, width=0.2)+
  scale_y_continuous(breaks = seq(0, 2.5, by =0.5))+
  coord_cartesian(ylim = c(0, 2.5))+
  labs(title = (expression(paste(""))), x="Hour", y = expression(paste (CH[4]," flux (mg C ",-CH[4]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

r4<-summarise(group_by(raw0b, hour),
              mean_4=mean(CO2.mg.m2.h), 
              SEdr4=sd(CO2.mg.m2.h)/sqrt(length(CO2.mg.m2.h)),
              lwr2r=mean_4-SEdr4,
              upr2r=mean_4+SEdr4)
limits_4<-aes(ymin=lwr2r, ymax=upr2r)

ggplot(r4, aes(x=hour, y=mean_4))+
  geom_point(size=2)+
  geom_errorbar(limits_4, width=0.2)+
  scale_y_continuous(breaks = seq(0, 150, by =50))+
  coord_cartesian(ylim = c(0, 150))+
  labs(title = (expression(paste(""))), x="Hour", y = expression(paste (CO[2]," flux (mg C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

###############################
raw$Date_IV <- as.Date(raw$Date_IV, "%Y-%m-%d")

plot1<-ggplot(arctic.chamber, aes(x=DOY, y=CH4.mgC.CH4.m2.h))+
  geom_point(size=1)+
  scale_y_continuous(breaks = seq(-3, 8, by =1))+
  coord_cartesian(ylim = c(-3, 8))+
  scale_x_continuous(breaks = seq(184, 236, by =2))+
  geom_smooth(method="lm", colour="red")+
  labs(title = (expression(paste("Average daily ", CH[4], " flux"))), x="Day of the year", y = expression(paste (CH[4]," flux (mg C ",-CH[4]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

plot2<-ggplot(arctic.chamber, aes(x=DOY, y=CO2.mg.m2.h))+
  geom_point(size=1)+
  scale_y_continuous(breaks = seq(0, 400, by =100))+
  coord_cartesian(ylim = c(0, 400))+
  scale_x_continuous(breaks = seq(184, 236, by =2))+
  geom_smooth(method="lm", colour="red")+
  labs(title = (expression(paste("Average daily ER flux"))), x="Day of the year", y = expression(paste (CO[2]," flux (mg C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

plot3<-ggplot(arctic.tower, aes(x=DOY, y=CH4.mgC.CH4.m2.h))+
  geom_point(size=1)+
  scale_y_continuous(breaks = seq(-3, 5, by =1))+
  coord_cartesian(ylim = c(-3, 5))+
  scale_x_continuous(breaks = seq(175, 325, by =4))+
  geom_smooth(method="lm", colour="red")+
  labs(title = (expression(paste("Average daily ", CH[4], " flux from tower data"))), x="Day of the year", y = expression(paste (CH[4]," flux (mg C ",-CH[4]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

############################
CH4comp<-ggplot(arctic.daily.means, aes(x=type, y=CH4.mgC.CH4.m2.h))+
  geom_boxplot(width=.2, outlier.size=0.8)+
  scale_x_discrete(name="Source", breaks=c("chamber", "tower"), labels=c("Chambers", "Tower"))+
  scale_y_continuous(breaks = seq(-3, 6, by =1))+
  coord_cartesian(ylim = c(-3, 6))+
  labs(title = (expression(paste("Average daily ", CH[4], " flux from two sources"))), x="Source", y = expression(paste (CH[4]," flux (mg C ",-CH[4]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=2)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))
##########################
sum1$Port<-as.factor(sum1$Port)
sum2$Port<-as.factor(sum2$Port)

ggplot(sum1, aes(x=DOY, y=meanCH4, colour=Port))+
  geom_point(size=2)+
  scale_y_continuous(breaks = seq(-1, 8, by =1))+
  coord_cartesian(ylim = c(-1, 8))+
  scale_x_continuous(breaks = seq(175, 325, by =4))+
  labs(title = (expression(paste(""))), x="DOY", y = expression(paste (CH[4]," flux (mg C ",-CH[4]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

ggplot(sum2, aes(x=DOY, y=meanCO2, colour=Port))+
  geom_point(size=2)+
  scale_y_continuous(breaks = seq(0, 400, by =100))+
  coord_cartesian(ylim = c(0, 400))+
  scale_x_continuous(breaks = seq(175, 325, by =4))+
  labs(title = (expression(paste(""))), x="DOY", y = expression(paste (CO[2]," flux (mg C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))



ggplot(sum1, aes(x=DOY, y=meanCH4, colour=Port))+
  facet_wrap(~Port)+
  geom_smooth(method="lm", colour="black", size=0.8)+
  geom_point(size=2)+
  scale_y_continuous(breaks = seq(-1, 8, by =1))+
  coord_cartesian(ylim = c(-1, 8))+
  scale_x_continuous(breaks = seq(175, 325, by =10))+
  labs(title = (expression(paste(""))), x="DOY", y = expression(paste (CH[4]," flux (mg C ",-CH[4]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

ggplot(sum2, aes(x=DOY, y=meanCO2, colour=Port))+
  facet_wrap(~Port)+
  geom_smooth(method="lm", colour="black", size=0.8)+
  geom_point(size=2)+
  scale_y_continuous(breaks = seq(0, 400, by =100))+
  coord_cartesian(ylim = c(0, 400))+
  scale_x_continuous(breaks = seq(175, 325, by =10))+
  labs(title = (expression(paste(""))), x="DOY", y = expression(paste (CO[2]," flux (mg C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

#####################
raw1$Date <- as.Date(raw1$Date, "%Y-%m-%d")

ggplot(raw0a, aes(x=hour, y=CH4.flux.mgC.CH4.m2.h, colour=Date))+
  facet_wrap(~Date)+
  geom_smooth(method="lm", colour="black", size=0.2)+
  geom_point(size=1)+
  scale_y_continuous(breaks = seq(-1, 8, by =2))+
  coord_cartesian(ylim = c(-1, 8))+
  scale_x_continuous(breaks = seq(0, 24, by =4))+
  labs(title = (expression(paste(""))), x="Hour", y = expression(paste (CH[4]," flux (mg C ",-CH[4]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme(legend.position="none")+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

ggplot(raw0b, aes(x=hour, y=CO2.mg.m2.h , colour=Date))+
  facet_wrap(~Date)+
  geom_smooth(method="lm", colour="black", size=0.2)+
  geom_point(size=1)+
  scale_y_continuous(breaks = seq(0, 500, by =200))+
  coord_cartesian(ylim = c(0, 500))+
  scale_x_continuous(breaks = seq(0, 24, by =4))+
  labs(title = (expression(paste(""))), x="Hour", y = expression(paste (CO[2]," flux (mg C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme(legend.position="none")+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

ggplot(raw1, aes(x=hour, y=Temperature , colour=Date))+
  facet_wrap(~Date)+
  geom_smooth(method="lm", colour="black", size=0.2)+
  geom_point(size=1)+
  scale_y_continuous(breaks = seq(-5, 10, by =2))+
  coord_cartesian(ylim = c(-5, 10))+
  scale_x_continuous(breaks = seq(0, 24, by =4))+
  labs(title = (expression(paste(""))), x="Hour", y = expression(paste ("Soil temperature (°C)")))+
  theme(legend.position="none")+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

ggplot(raw1, aes(x=Date, y=Temperature, colour=Port))+
  facet_wrap(~Port)+
  geom_smooth(method="lm", colour="black", size=0.2)+
  geom_point(size=1)+
  scale_y_continuous(breaks = seq(-5, 10, by =1))+
  coord_cartesian(ylim = c(-5, 10))+
  scale_x_date(date_breaks = "5 day", date_labels = "%Y-%m-%d")+
  labs(title = (expression(paste(""))), x="Date", y = expression(paste ("Soil temperature (°C)")))+
  theme(legend.position="none")+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

r1<-summarise(group_by(Book1, HOUR),
              mean_1=mean(NEE), 
              SEdr1=sd(NEE)/sqrt(length(NEE)),
              lwr1r=mean_1-SEdr1,
              upr1r=mean_1+SEdr1)
limits_1<-aes(ymin=lwr1r, ymax=upr1r)

ggplot(r1, aes(x=HOUR, y=mean_1))+
  geom_point(size=2)+
  geom_line()+
  geom_errorbar(limits_1, width=0.2)+
  scale_y_continuous(breaks = seq(-1, 2, by =1))+
  coord_cartesian(ylim = c(-1, 2))+
  labs(title = (expression(paste(""))), x="Hour", y = expression(paste ("NEE")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))
