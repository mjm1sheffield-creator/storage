lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
library(MuMIn)
library(Matrix)
library(gridExtra)
library(dplyr)
library(ggplot2)
library(car)

sum$Water.table<-as.factor(sum$Water.table)
sum$Chamber<-as.factor(sum$Chamber)
sum$week<-as.integer(sum$week)
sum0$Water.table<-as.factor(sum0$Water.table)
sum0$Chamber<-as.factor(sum0$Chamber)
sum0$week<-as.factor(sum0$week)
treatment$week<-as.integer(treatment$week)
sum$week<-as.factor(sum$week)
unplanted$week<-as.factor(unplanted$week)
planted$Chamber<-as.factor(planted$Chamber)
planted$Water.table..cm.<-as.factor(planted$Water.table..cm.)
sum0a$week<-as.factor(sum0a$week)
GPPonly$week<-as.factor(GPPonly$week)
GPPonly$Chamber<-as.factor(GPPonly$Chamber)

planted<-filter(sum0, Planting=="Planted")
Tsoil<-filter(sum0, Soil.temperature...10cm....C.!="NA")
M_soil<-filter(sum0, Moisture...12cm.....!="NA")
min(unplanted$CO2)
max(treatmentch4$CH4)
###########
sum0a <- sum0[-c(504, 440),]
#########remove CH4 outliers in sum0
sum <- sum[-c(206, 207),]
#################remove CO2 outliers in sum (unplanted only)

GPPonly<-filter(sum0, GPP!="NA" & Planting=="Planted")
########### 
ch4clean <- sum0[-c(504, 440),]  
sumNP <- sum[-c(206, 207),]
treatmentch4 <- treatment[-c(120, 125),]
planted<-filter(treatment, Planting=="Planted")
GPPonly<-filter(treatment, GPP!="NA" & Planting=="Planted")
treatmentCO2NP <- treatmentCO2NP[-c(58, 207),]

#######################
CO2a<-summarise(group_by(unplanted, Chamber),
                      meanCO2a=mean(CO2),
                      SEa=sd(CO2)/sqrt(length(CO2)),
                      lwra=meanCO2a-SEa,
                      upra=meanCO2a+SEa)
limits1<-aes(ymin=lwra, ymax=upra)

CO2u1<-ggplot(unplanted, aes(x=Chamber, y=CO2))+
  geom_boxplot(width=.2, outlier.size=0.8)+
  scale_x_discrete(name="Temperature", breaks=c("301", "302"), labels=c("Ambient", "Elevated"))+
  scale_y_continuous(breaks = seq(-0.1, 0.3, by =0.1))+
  coord_cartesian(ylim = c(-0.1, 0.3))+
  labs(title = (expression(paste("Soil respiration"))), x="Temperature", y = expression(paste ("", CO[2]," flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=2)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

ColCO2u1<-ggplot(unplanted, aes(x=Chamber, y=CO2, fill=Chamber))+
  geom_boxplot(width=.2, outlier.size=0.8)+
  scale_x_discrete(name="Temperature", breaks=c("301", "302"), labels=c("Ambient", "Elevated"))+
  scale_y_continuous(breaks = seq(-0.1, 0.3, by =0.1))+
  coord_cartesian(ylim = c(-0.1, 0.3))+
  labs(title = (expression(paste("Soil respiration"))), x="Temperature", y = expression(paste ("", CO[2]," flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3)+
  theme(plot.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 18))

CO2b<-summarise(group_by(unplanted, Water.table),
                meanCO2b=mean(CO2),
                SEb=sd(CO2)/sqrt(length(CO2)),
                lwrb=meanCO2b-SEb,
                uprb=meanCO2b+SEb)
limits2<-aes(ymin=lwrb, ymax=uprb)

CO2ub<-ggplot(unplanted, aes(x=Water.table, y=CO2))+
  geom_boxplot(width=.2, outlier.size=0.8)+
  scale_x_discrete(name="", breaks=c("50", "40", "30"), labels=c("-50cm", "-40cm", "-30cm"))+
  scale_y_continuous(breaks = seq(-0.1, 0.3, by =0.1))+
  coord_cartesian(ylim = c(-0.1, 0.3))+
  labs(title = (expression(paste("Soil respiration"))), x="Water table", y = expression(paste ("", CO[2]," flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=2)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

ColCO2ub<-ggplot(unplanted, aes(x=Water.table, y=CO2, fill=Water.table))+
  geom_boxplot(width=.2, outlier.size=0.8)+
  scale_x_discrete(name="Water table", breaks=c("50", "40", "30"), labels=c("-50cm", "-40cm", "-30cm"))+
  scale_y_continuous(breaks = seq(-0.1, 0.3, by =0.1))+
  coord_cartesian(ylim = c(-0.1, 0.3))+
  labs(title = (expression(paste("Soil respiration"))), x="Water table", y = expression(paste ("", CO[2]," flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3)+
  theme(plot.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 18))

GPPwt<-summarise(group_by(GPPonly, Water.table),
                meanGPP=mean(GPP),
                SEb=sd(GPP)/sqrt(length(GPP)),
                lwrb=meanGPP-SEb,
                uprb=meanGPP+SEb)
limits<-aes(ymin=lwrb, ymax=uprb)

GPP1<-ggplot(GPPonly, aes(x=Water.table, y=GPP))+
  geom_boxplot(width=.2, outlier.size=0.8)+
  scale_x_discrete(name="", breaks=c("50", "40", "30"), labels=c("-50cm", "-40cm", "-30cm"))+
  scale_y_continuous(breaks = seq(-0.5, 2, by =0.5))+
  coord_cartesian(ylim = c(-0.5, 2))+
  labs(title = (expression(paste("Average GPP"))), x="Water table", y = expression(paste ("", CO[2]," flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=2)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

GPPt<-summarise(group_by(GPPonly, Chamber),
                 meanGPP=mean(GPP),
                 SEb=sd(GPP)/sqrt(length(GPP)),
                 lwrb=meanGPP-SEb,
                 uprb=meanGPP+SEb)
limits<-aes(ymin=lwrb, ymax=uprb)

GPP2<-ggplot(GPPonly, aes(x=Chamber, y=GPP))+
  geom_boxplot(width=.2, outlier.size=0.8)+
  scale_x_discrete(name="Temperature", breaks=c("301", "302"), labels=c("Ambient", "Elevated"))+
  scale_y_continuous(breaks = seq(-0.5, 2, by =0.5))+
  coord_cartesian(ylim = c(-0.5, 2))+
  labs(title = (expression(paste("Average GPP"))), x="Temperature", y = expression(paste ("", CO[2]," flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=2)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

NEEt<-summarise(group_by(planted, Chamber),
                meanCO2=mean(CO2),
                SEb=sd(GPP)/sqrt(length(CO2)),
                lwrb=meanGPP-SEb,
                uprb=meanGPP+SEb)
limits<-aes(ymin=lwrb, ymax=uprb)

NEE2<-ggplot(planted, aes(x=Chamber, y=CO2))+
  geom_boxplot(width=.2, outlier.size=0.8)+
  scale_x_discrete(name="Temperature", breaks=c("301", "302"), labels=c("Ambient", "Elevated"))+
  scale_y_continuous(breaks = seq(-1.5, 1, by =0.5))+
  coord_cartesian(ylim = c(-1.5, 1))+
  labs(title = (expression(paste("Average NEE"))), x="Temperature", y = expression(paste ("", CO[2]," flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=2)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

NEE1<-ggplot(planted, aes(x=Water.table..cm., y=CO2))+
  geom_boxplot(width=.2, outlier.size=0.8)+
  scale_x_discrete(name="", breaks=c("50", "40", "30"), labels=c("-50cm", "-40cm", "-30cm"))+
  scale_y_continuous(breaks = seq(-1.5, 1, by =0.5))+
  coord_cartesian(ylim = c(-1.5, 1))+
  labs(title = (expression(paste("Average NEE"))), x="Water table", y = expression(paste ("", CO[2]," flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=2)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

dark1<-ggplot(planted, aes(x=Water.table..cm., y=CO2.Dark))+
  geom_boxplot(width=.2, outlier.size=0.8)+
  scale_x_discrete(name="", breaks=c("50", "40", "30"), labels=c("-50cm", "-40cm", "-30cm"))+
  scale_y_continuous(breaks = seq(-0.5, 2, by =0.5))+
  coord_cartesian(ylim = c(-0.5, 2))+
  labs(title = (expression(paste("Average ecosystem respiration"))), x="Water table", y = expression(paste ("", CO[2]," flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=2)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

dark2<-ggplot(planted, aes(x=Chamber, y=CO2.Dark))+
  geom_boxplot(width=.2, outlier.size=0.8)+
  scale_x_discrete(name="Temperature", breaks=c("301", "302"), labels=c("Ambient", "Elevated"))+
  scale_y_continuous(breaks = seq(-0.5, 2, by =0.5))+
  coord_cartesian(ylim = c(-0.5, 2))+
  labs(title = (expression(paste("Average ecosystem respiration"))), x="Temperature", y = expression(paste ("", CO[2]," flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=2)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

####################
lightCH4<-summarise(group_by(sum0, Water.table),
                    medianCH4u=median(CH4),
                    SEuCH4=sd(CH4)/sqrt(length(CH4)),
                    lwruCH4=medianCH4u-SEuCH4,
                    upruCH4=medianCH4u+SEuCH4)
limitsCH4<-aes(ymin=lwruCH4, ymax=upruCH4)

CH4combined1<-ggplot(sum0, aes(x=Water.table, y=CH4))+
  geom_boxplot(width=.2, outlier.size=0.8)+
  scale_x_discrete(name="Water table", breaks=c("30", "40", "50"),labels=c("-30 cm", "-40 cm", "-50 cm"))+
  scale_y_continuous(breaks = seq(-0.2, 0.1,by =0.1))+
  coord_cartesian(ylim = c(-0.2, 0.1))+
  labs(title = (expression(paste("", CH[4], " flux"))), x="Water table", y = expression(paste ("", CH[4]," Flux (mg C ",-CH[4]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=2)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

colCH4combined1<-ggplot(sum0, aes(x=Water.table, y=CH4, fill=Water.table))+
  geom_boxplot(width=.2, outlier.size=0.8)+
  scale_x_discrete(name="Water table", breaks=c("30", "40", "50"),labels=c("-30 cm", "-40 cm", "-50 cm"))+
  scale_y_continuous(breaks = seq(-0.2, 0.1,by =0.1))+
  coord_cartesian(ylim = c(-0.2, 0.1))+
  labs(title = (expression(paste("", CH[4], " flux"))), x="Water table", y = expression(paste ("", CH[4]," Flux (mg C ",-CH[4]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3)+
  theme(plot.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 18))


lightCH42<-summarise(group_by(sum0, Water.table),
                    medianCH4u2=median(CH4),
                    SEuCH42=sd(CH4)/sqrt(length(CH4)),
                    lwruCH42=medianCH4u2-SEuCH42,
                    upruCH42=medianCH4u2+SEuCH42)
limitsCH42<-aes(ymin=lwruCH42, ymax=upruCH42)

CH4combined2<-ggplot(sum0, aes(x=Planting, y=CH4))+
  geom_boxplot(width=.2, outlier.size=0.8)+
  scale_color_discrete(name="Planting status")+
  scale_y_continuous(breaks = seq(-0.2, 0.1, by =0.1))+
  coord_cartesian(ylim = c(-0.2, 0.1))+
  labs(title = (expression(paste("", CH[4], " flux"))), x="Planting", y = expression(paste ("", CH[4]," Flux (mg C ",-CH[4]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=2)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

colCH4combined2<-ggplot(sum0, aes(x=Planting, y=CH4, fill=Planting))+
  geom_boxplot(width=.2, outlier.size=0.8)+
  scale_color_discrete(name="Planting status")+
  scale_y_continuous(breaks = seq(-0.2, 0.1, by =0.1))+
  coord_cartesian(ylim = c(-0.2, 0.1))+
  labs(title = (expression(paste("", CH[4], " flux"))), x="Planting", y = expression(paste ("", CH[4]," Flux (mg C ",-CH[4]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3)+
  theme(plot.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 18))

lightCH43<-summarise(group_by(sum0, Chamber),
                     medianCH4u3=median(CH4),
                     SEuCH43=sd(CH4)/sqrt(length(CH4)),
                     lwruCH43=medianCH4u3-SEuCH43,
                     upruCH43=medianCH4u3+SEuCH43)
limitsCH43<-aes(ymin=lwruCH43, ymax=upruCH43)

CH4combined3<-ggplot(sum0, aes(x=Chamber, y=CH4))+
  geom_boxplot(width=.2, outlier.size=0.8)+
  scale_x_discrete(name="Temperature", breaks=c("301", "302"), labels=c("Ambient", "Elevated"))+
  scale_y_continuous(breaks = seq(-0.2, 0.1, by =0.1))+
  coord_cartesian(ylim = c(-0.2, 0.1))+
  labs(title = (expression(paste("", CH[4], " flux"))), x="Temperature", y = expression(paste ("", CH[4]," Flux (mg C ",-CH[4]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=2)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

colCH4combined3<-ggplot(sum0, aes(x=Chamber, y=CH4, fill=Chamber))+
  geom_boxplot(width=.2, outlier.size=0.8)+
  scale_x_discrete(name="Temperature", breaks=c("301", "302"), labels=c("Ambient", "Elevated"))+
  scale_y_continuous(breaks = seq(-0.2, 0.1, by =0.1))+
  coord_cartesian(ylim = c(-0.2, 0.1))+
  labs(title = (expression(paste("", CH[4], " flux"))), x="Temperature", y = expression(paste ("", CH[4]," Flux (mg C ",-CH[4]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3)+
  theme(plot.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 18))

grid.arrange(CO2u, CO2u3, CH4combined1, CH4_3, ncol=2)
#########################################

CH4trt<-summarise(group_by(treatmentch4, Treatment),
                   mean_flux=mean(CH4),
                   SEdt=sd(CH4)/sqrt(length(CH4)),
                   lwr1=mean_flux-SEdt,
                   upr1=mean_flux+SEdt)
limits_above<-aes(ymin=lwr1, ymax=upr1)
CH4group<-ggplot(CH4trt, aes(x=Treatment, y=mean_flux, shape=Treatment))+
  geom_errorbar(limits_above, width=0.1)+
  geom_point (aes (y=mean_flux), size=4)+
  scale_shape_manual(name="", breaks = c("30AmbNP", "30AmbP", "40AmbNP", "40AmbP", "50AmbNP", "50AmbP", "30ElevNP", "30ElevP", "40ElevNP", "40ElevP", "50ElevNP", "50ElevP"), 
                     labels=c("-30cm Ambient Not planted", "-30cm Ambient Planted", "-40cm Ambient Not planted", "-40cm Ambient Planted", "-50cm Ambient Not planted", "-50cm Ambient Planted", "-30cm Elevated Not planted", "-30cm Elevated Planted", "-40cm Elevated Not planted", "-40cm Elevated Planted", "-50cm Elevated Not planted", "-50cm Elevated Planted"),
                     values = c(15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15))+
  scale_y_continuous(breaks = seq(-0.08, 0.03, by =0.01))+
  coord_cartesian(ylim = c(-0.08, 0.03))+
  labs(title = (expression(paste("Average ", CH[4], " flux: treatment comparison"))), y =expression(paste ("Average ", CH[4]," Flux (mg C ",-CH[4]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        strip.text = element_text(size = 12))

CH4group<-ggplot(treatmentch4, aes(x=Treatment, y=CH4))+
  geom_boxplot(width=.2, outlier.size=0.8)+
  scale_y_continuous(breaks = seq(-0.1, 0.05, by =0.05))+
  coord_cartesian(ylim = c(-0.1, 0.05))+
  scale_x_discrete(name="", breaks = c("30AmbNP", "30AmbP", "40AmbNP", "40AmbP", "50AmbNP", "50AmbP", "30ElevNP", "30ElevP", "40ElevNP", "40ElevP", "50ElevNP", "50ElevP"), 
                     labels=c())+
  labs(title = (expression(paste("Average ", CH[4], " flux: treatment comparison"))), y =expression(paste ("Average ", CH[4]," Flux (mg C ",-CH[4]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=2)+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        strip.text = element_text(size = 12))

CO2trt<-summarise(group_by(planted, Treatment),
                  mean_flux=mean(CO2),
                  SEdt=sd(CO2)/sqrt(length(CO2)),
                  lwr=mean_flux-SEdt,
                  upr=mean_flux+SEdt)
limits_NEE<-aes(ymin=lwr, ymax=upr)
CO2group<-ggplot(CO2trt, aes(x=Treatment, y=mean_flux, shape=Treatment))+
  geom_errorbar(limits_NEE, width=0.1)+
  geom_point (aes (y=mean_flux), size=4)+
  scale_shape_manual(name="", breaks = c("30AmbP", "40AmbP", "50AmbP", "30ElevP", "40ElevP", "50ElevP"), 
                     labels=c("-30cm Ambient Planted", "-40cm Ambient Planted", "-50cm Ambient Planted", "-30cm Elevated Planted", "-40cm Elevated Planted", "-50cm Elevated Planted"),
                     values = c( 15, 15, 15, 15, 15, 15))+
  scale_y_continuous(breaks = seq(-2.5, 0, by =0.5))+
  coord_cartesian(ylim = c(-2.5, 0))+
  labs(title = (expression(paste("Average NEE: treatment comparison"))), y =expression(paste ("Average ", CO[2]," Flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        strip.text = element_text(size = 12))

GPPtrt<-summarise(group_by(GPPonly, Treatment),
                  mean_flux=mean(GPP),
                  SEdt=sd(GPP)/sqrt(length(GPP)),
                  lwr=mean_flux-SEdt,
                  upr=mean_flux+SEdt)
limits_GPP<-aes(ymin=lwr, ymax=upr)
GPPgroup<-ggplot(GPPtrt, aes(x=Treatment, y=mean_flux, shape=Treatment))+
  geom_errorbar(limits_GPP, width=0.1)+
  geom_point (aes (y=mean_flux), size=4)+
  scale_shape_manual(name="", breaks = c("30AmbP", "40AmbP", "50AmbP", "30ElevP", "40ElevP", "50ElevP"), 
                     labels=c("-30cm Ambient Planted", "-40cm Ambient Planted", "-50cm Ambient Planted", "-30cm Elevated Planted", "-40cm Elevated Planted", "-50cm Elevated Planted"),
                     values = c( 15, 15, 15, 15, 15, 15))+
  scale_y_continuous(breaks = seq(-1, 0.5, by =0.5))+
  coord_cartesian(ylim = c(-1, 0.5))+
  labs(title = (expression(paste("Average GPP: treatment comparison"))), y =expression(paste ("Average ", CO[2]," Flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        strip.text = element_text(size = 12))

DarkCO2trt<-summarise(group_by(planted, Treatment),
                  mean_flux=mean(CO2.Dark),
                  SEdt=sd(CO2.Dark)/sqrt(length(CO2.Dark)),
                  lwr=mean_flux-SEdt,
                  upr=mean_flux+SEdt)
limits_Dark<-aes(ymin=lwr, ymax=upr)
CO2.Darkgroup<-ggplot(DarkCO2trt, aes(x=Treatment, y=mean_flux, shape=Treatment))+
  geom_errorbar(limits_Dark, width=0.1)+
  geom_point (aes (y=mean_flux), size=4)+
  scale_shape_manual(name="", breaks = c("30AmbP", "40AmbP", "50AmbP", "30ElevP", "40ElevP", "50ElevP"), 
                     labels=c("-30cm Ambient Planted", "-40cm Ambient Planted", "-50cm Ambient Planted", "-30cm Elevated Planted", "-40cm Elevated Planted", "-50cm Elevated Planted"),
                     values = c( 15, 15, 15, 15, 15, 15))+
  scale_y_continuous(breaks = seq(0, 2.5, by =0.5))+
  coord_cartesian(ylim = c(0, 2.5))+
  labs(title = (expression(paste("Average ecosystem respiration: treatment comparison"))), y =expression(paste ("Average ", CO[2]," Flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        strip.text = element_text(size = 12))

soiltrt<-summarise(group_by(treatmentCO2NP, Treatment),
                  mean_flux=mean(CO2),
                  SEdt=sd(CO2)/sqrt(length(CO2)),
                  lwr=mean_flux-SEdt,
                  upr=mean_flux+SEdt)
limits_U<-aes(ymin=lwr, ymax=upr)
soilgroup<-ggplot(soiltrt, aes(x=Treatment, y=mean_flux, shape=Treatment))+
  geom_errorbar(limits_U, width=0.1)+
  geom_point (aes (y=mean_flux), size=4)+
  scale_shape_manual(name="", breaks = c("30Amb", "40Amb", "50Amb", "30Elev", "40Elev", "50Elev"), 
                     labels=c("-30cm Ambient", "-40cm Ambient", "-50cm Ambient", "-30cm Elevated", "-40cm Elevated", "-50cm Elevated"),
                     values = c( 15, 15, 15, 15, 15, 15))+
  scale_y_continuous(breaks = seq(0, 0.5, by =0.1))+
  coord_cartesian(ylim = c(0, 0.5))+
  labs(title = (expression(paste("Average soil respiration: treatment comparison"))), y =expression(paste ("Average ", CO[2]," Flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        strip.text = element_text(size = 12))

###############################################
soiltime<-summarise(group_by(sum, week),
                   mean_flux=mean(CO2),
                   SEdt=sd(CO2)/sqrt(length(CO2)),
                   lwr=mean_flux-SEdt,
                   upr=mean_flux+SEdt)
limits_U<-aes(ymin=lwr, ymax=upr)
soilweek<-ggplot(soiltime, aes(x=week, y=mean_flux))+
geom_point (aes (y=mean_flux), size=4, shape=15)+
  scale_x_discrete(name="Week", breaks=c("1", "2", "3", "4", "5", "6", "7", "8"), 
                   labels=c("2", "3", "4", "6", "7", "8", "9", "10"))+
  scale_y_continuous(breaks = seq(-0.1, 0.3, by =0.1))+
  coord_cartesian(ylim = c(-0.1, 0.3))+
  geom_errorbar(limits_U, width=0.2)+
  labs(title = "Average soil respiration by week", x="Week", y =expression(paste ("Average soil respiration (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(14),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        strip.text = element_text(size = 12))

ch4time<-summarise(group_by(ch4clean, week),
                    mean_flux=mean(CH4),
                    SEdt=sd(CH4)/sqrt(length(CH4)),
                    lwr=mean_flux-SEdt,
                    upr=mean_flux+SEdt)
limits_CH4<-aes(ymin=lwr, ymax=upr)
ch4week<-ggplot(ch4time, aes(x=week, y=mean_flux))+
  geom_point (aes (y=mean_flux), size=4, shape=15)+
  scale_x_continuous(breaks = seq(1,8, by =1))+
  coord_cartesian(xlim = c(1, 8))+
  scale_y_continuous(breaks = seq(-0.02, 0, by =0.01))+
  coord_cartesian(ylim = c(-0.02, 0))+
  geom_errorbar(limits_CH4, width=0.2)+
  labs(title = (expression(paste("Average ", CH[4], " flux by week"))), x="Week", y =expression(paste("Average ", CH[4]," Flux (mg C ",-CH[4]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        strip.text = element_text(size = 12))

NEEtime<-summarise(group_by(planted, week),
                    mean_flux=mean(CO2),
                    SEdt=sd(CO2)/sqrt(length(CO2)),
                    lwr=mean_flux-SEdt,
                    upr=mean_flux+SEdt)
limits_NEE<-aes(ymin=lwr, ymax=upr)
NEEweek<-ggplot(NEEtime, aes(x=week, y=mean_flux))+
  geom_point (aes (y=mean_flux), size=4, shape=15)+
  scale_x_continuous(breaks = seq(1,8, by =1))+
  coord_cartesian(xlim = c(1, 8))+
  scale_y_continuous(breaks = seq(-3.5, 0, by =0.5))+
  coord_cartesian(ylim = c(-3.5, 0))+
  geom_errorbar(limits_NEE, width=0.2)+
  labs(title = "Average NEE by week", x="Week", y =expression(paste ("Average NEE (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        strip.text = element_text(size = 12))

###########################
GPPweek<-ggplot(GPPonly, aes(x=week, y=GPP, fill=Chamber))+
  geom_boxplot(width=.5, outlier.size=0.9, alpha=0.8)+
  scale_x_discrete(name="Week", breaks=c("1", "2", "3", "4", "5", "6", "7", "8"), 
                   labels=c("2", "3", "4", "6", "7", "8", "9", "10"))+
  scale_y_continuous(breaks = seq(0, 2, by =0.5))+
  coord_cartesian(ylim = c(0, 2))+
  scale_fill_manual(name="Temperature", breaks = c("301", "302"), 
                     labels=c("Ambient", "Elevated"),
                    values = c(4, 2))+
  labs(title = (expression(paste("GPP by week"))), x="week", y = expression(paste ("", CO[2]," flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))



NEEweek<-ggplot(planted, aes(x=week, y=CO2, fill=Chamber))+
geom_boxplot(width=.5, outlier.size=0.9, alpha=0.8)+
  scale_x_discrete(name="Week", breaks=c("1", "2", "3", "4", "5", "6", "7", "8"), 
                   labels=c("2", "3", "4", "6", "7", "8", "9", "10"))+
  scale_y_continuous(breaks = seq(-1, 0.5, by =0.2))+
  coord_cartesian(ylim = c(-1, 0.5))+
  scale_fill_manual(name="Temperature", breaks = c("301", "302"), 
                    labels=c("Ambient", "Elevated"),
                    values = c(4, 2))+
  labs(title = (expression(paste("NEE by week"))), x="week", y = expression(paste ("", CO[2]," flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

soilrespweek<-ggplot(sum, aes(x=week, y=CO2, fill=Chamber))+
  geom_boxplot(width=.5, outlier.size=0.9, alpha=0.8)+
  scale_x_discrete(name="Week", breaks=c("1", "2", "3", "4", "5", "6", "7", "8"), 
                   labels=c("2", "3", "4", "6", "7", "8", "9", "10"))+
  scale_y_continuous(breaks = seq(0, 0.15, by =0.05))+
  coord_cartesian(ylim = c(0, 0.15))+
  scale_fill_manual(name="Temperature", breaks = c("301", "302"), 
                    labels=c("Ambient", "Elevated"),
                    values = c(4, 2))+
  labs(title = (expression(paste("Soil respiration by week"))), x="week", y = expression(paste ("", CO[2]," flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DarkCO2week<-ggplot(planted, aes(x=week, y=CO2.Dark, fill=Chamber))+
  geom_boxplot(width=.5, outlier.size=0.9, alpha=0.8)+
  scale_x_discrete(name="Week", breaks=c("1", "2", "3", "4", "5", "6", "7", "8"), 
                   labels=c("2", "3", "4", "6", "7", "8", "9", "10"))+
  scale_y_continuous(breaks = seq(-0.1, 1.5, by =0.2))+
  coord_cartesian(ylim = c(-0.1, 1.5))+
  scale_fill_manual(name="Temperature", breaks = c("301", "302"), 
                    labels=c("Ambient", "Elevated"),
                    values = c(4, 2))+
  labs(title = (expression(paste("Ecosystem respiration by week"))), x="week", y = expression(paste ("", CO[2]," flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

CH4pweek<-ggplot(sum0a, aes(x=week, y=CH4, fill=Chamber))+
  geom_boxplot(width=.5, outlier.size=0.9, alpha=0.8)+
  scale_x_discrete(name="Week", breaks=c("1", "2", "3", "4", "5", "6", "7", "8"), 
                   labels=c("2", "3", "4", "6", "7", "8", "9", "10"))+
  scale_y_continuous(breaks = seq(-0.1, 0.1, by =0.05))+
  coord_cartesian(ylim = c(-0.1, 0.1))+
  scale_fill_manual(name="Temperature", breaks = c("301", "302"), 
                    labels=c("Ambient", "Elevated"),
                    values = c(4, 2))+
  labs(title = (expression(paste("", CH[4], " flux"))), x="week", y = expression(paste ("", CH[4]," Flux (mg C ",-CH[4]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

Tweek<-ggplot(Tsoil, aes(x=week, y=Soil.temperature...10cm....C., fill=Chamber))+
  geom_boxplot(width=.5, outlier.size=0.9, alpha=0.8)+
  scale_x_discrete(name="Week", breaks=c("1", "2", "3", "4", "5", "6", "7", "8"), 
                   labels=c("2", "3", "4", "6", "7", "8", "9", "10"))+
  scale_y_continuous(breaks = seq(0, 22, by =2))+
  coord_cartesian(ylim = c(0, 22))+
  scale_fill_manual(name="Temperature", breaks = c("301", "302"), 
                    labels=c("Ambient", "Elevated"),
                    values = c(4, 2))+
  labs(title = (expression(paste("Soil temperature at -10 cm"))), x="week", y = expression(paste ("Temperature [",degree,"C]")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

Mweek<-ggplot(M_soil, aes(x=week, y=Moisture...12cm....., fill=Chamber))+
  geom_boxplot(width=.5, outlier.size=0.9, alpha=0.8)+
  scale_x_discrete(name="Week", breaks=c("1", "2", "3", "4", "5", "6", "7", "8"), 
                   labels=c("2", "3", "4", "6", "7", "8", "9", "10"))+
  scale_y_continuous(breaks = seq(0, 0.6, by =0.1))+
  coord_cartesian(ylim = c(0, 0.6))+
  scale_fill_manual(name="Temperature", breaks = c("301", "302"), 
                    labels=c("Ambient", "Elevated"),
                    values = c(4, 2))+
  labs(title = (expression(paste("Soil water content"))), x="week", y = expression(paste ("Volumetric water content ( ", m^-3 ," ", m^-3, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

#########################################
sum0_30<-filter(sum0, Water.table=="30" )
sum0_40<-filter(sum0, Water.table=="40" )
sum0_50<-filter(sum0, Water.table=="50" )
sum_30<-filter(sum, Water.table=="30" )
sum_40<-filter(sum, Water.table=="40" )
sum_50<-filter(sum, Water.table=="50" )
planted_30<-filter(planted, Water.table=="30" )
planted_40<-filter(planted, Water.table=="40" )
planted_50<-filter(planted, Water.table=="50" )
sum0a_30<-filter(sum0a, Water.table=="30" )
sum0a_40<-filter(sum0a, Water.table=="40" )
sum0a_50<-filter(sum0a, Water.table=="50" )

GPPweek30<-ggplot(planted_30, aes(x=week, y=GPP, fill=Chamber))+
  geom_boxplot(width=.5, outlier.size=0.9, alpha=0.8)+
  scale_x_discrete(name="Week", breaks=c("1", "2", "3", "4", "5", "6", "7", "8"), 
                   labels=c("2", "3", "4", "6", "7", "8", "9", "10"))+
  scale_y_continuous(breaks = seq(0, 1.6, by =0.5))+
  coord_cartesian(ylim = c(0, 1.6))+
  scale_fill_manual(name="Temperature", breaks = c("301", "302"), 
                    labels=c("Ambient", "Elevated"),
                    values = c(4, 2))+
  labs(title = (expression(paste("GPP at -30 cm water table"))), x="week", y = expression(paste ("", CO[2]," flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

GPPweek40<-ggplot(planted_40, aes(x=week, y=GPP, fill=Chamber))+
  geom_boxplot(width=.5, outlier.size=0.9, alpha=0.8)+
  scale_x_discrete(name="Week", breaks=c("1", "2", "3", "4", "5", "6", "7", "8"), 
                   labels=c("2", "3", "4", "6", "7", "8", "9", "10"))+
  scale_y_continuous(breaks = seq(0, 1.6, by =0.5))+
  coord_cartesian(ylim = c(0, 1.6))+
  scale_fill_manual(name="Temperature", breaks = c("301", "302"), 
                    labels=c("Ambient", "Elevated"),
                    values = c(4, 2))+
  labs(title = (expression(paste("GPP at -40 cm water table"))), x="week", y = expression(paste ("", CO[2]," flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

GPPweek50<-ggplot(planted_50, aes(x=week, y=GPP, fill=Chamber))+
  geom_boxplot(width=.5, outlier.size=0.9, alpha=0.8)+
  scale_x_discrete(name="Week", breaks=c("1", "2", "3", "4", "5", "6", "7", "8"), 
                   labels=c("2", "3", "4", "6", "7", "8", "9", "10"))+
  scale_y_continuous(breaks = seq(0, 1.6, by =0.5))+
  coord_cartesian(ylim = c(0, 1.6))+
  scale_fill_manual(name="Temperature", breaks = c("301", "302"), 
                    labels=c("Ambient", "Elevated"),
                    values = c(4, 2))+
  labs(title = (expression(paste("GPP at -50 cm water table"))), x="week", y = expression(paste ("", CO[2]," flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

grid.arrange(GPPweek30, GPPweek40, GPPweek50, ncol=1)

GPPweek30<-ggplot(planted_30, aes(x=week, y=GPP, fill=Chamber))+
  geom_boxplot(width=.5, outlier.size=0.9, alpha=0.8)+
  scale_x_discrete(name="Week", breaks=c("1", "2", "3", "4", "5", "6", "7", "8"), 
                   labels=c("2", "3", "4", "6", "7", "8", "9", "10"))+
  scale_y_continuous(breaks = seq(-1.6, 0, by =0.5))+
  coord_cartesian(ylim = c(-1.6, 0))+
  scale_fill_manual(name="Temperature", breaks = c("301", "302"), 
                    labels=c("Ambient", "Elevated"),
                    values = c(4, 2))+
  labs(title = (expression(paste("GPP at -30 cm water table"))), x="week", y = expression(paste ("", CO[2]," flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

GPPweek40<-ggplot(planted_40, aes(x=week, y=GPP, fill=Chamber))+
  geom_boxplot(width=.5, outlier.size=0.9, alpha=0.8)+
  scale_x_discrete(name="Week", breaks=c("1", "2", "3", "4", "5", "6", "7", "8"), 
                   labels=c("2", "3", "4", "6", "7", "8", "9", "10"))+
  scale_y_continuous(breaks = seq(-1.6, 0, by =0.5))+
  coord_cartesian(ylim = c(-1.6, 0))+
  scale_fill_manual(name="Temperature", breaks = c("301", "302"), 
                    labels=c("Ambient", "Elevated"),
                    values = c(4, 2))+
  labs(title = (expression(paste("GPP at -40 cm water table"))), x="week", y = expression(paste ("", CO[2]," flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

GPPweek50<-ggplot(planted_50, aes(x=week, y=GPP, fill=Chamber))+
  geom_boxplot(width=.5, outlier.size=0.9, alpha=0.8)+
  scale_x_discrete(name="Week", breaks=c("1", "2", "3", "4", "5", "6", "7", "8"), 
                   labels=c("2", "3", "4", "6", "7", "8", "9", "10"))+
  scale_y_continuous(breaks = seq(-1.6, 0, by =0.5))+
  coord_cartesian(ylim = c(-1.6, 0))+
  scale_fill_manual(name="Temperature", breaks = c("301", "302"), 
                    labels=c("Ambient", "Elevated"),
                    values = c(4, 2))+
  labs(title = (expression(paste("GPP at -50 cm water table"))), x="week", y = expression(paste ("", CO[2]," flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

grid.arrange(GPPweek30, GPPweek40, GPPweek50, ncol=1)

NEEweek30<-ggplot(planted_30, aes(x=week, y=CO2, fill=Chamber))+
  geom_boxplot(width=.5, outlier.size=0.9, alpha=0.8)+
  scale_x_discrete(name="Week", breaks=c("1", "2", "3", "4", "5", "6", "7", "8"), 
                   labels=c("2", "3", "4", "6", "7", "8", "9", "10"))+
  scale_y_continuous(breaks = seq(-1, 0.1, by =0.2))+
  coord_cartesian(ylim = c(-1, 0.1))+
  scale_fill_manual(name="Temperature", breaks = c("301", "302"), 
                    labels=c("Ambient", "Elevated"),
                    values = c(4, 2))+
  labs(title = (expression(paste("NEE at -30 cm water table"))), x="week", y = expression(paste ("", CO[2]," flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

NEEweek40<-ggplot(planted_40, aes(x=week, y=CO2, fill=Chamber))+
  geom_boxplot(width=.5, outlier.size=0.9, alpha=0.8)+
  scale_x_discrete(name="Week", breaks=c("1", "2", "3", "4", "5", "6", "7", "8"), 
                   labels=c("2", "3", "4", "6", "7", "8", "9", "10"))+
  scale_y_continuous(breaks = seq(-1, 0.1, by =0.2))+
  coord_cartesian(ylim = c(-1, 0.1))+
  scale_fill_manual(name="Temperature", breaks = c("301", "302"), 
                    labels=c("Ambient", "Elevated"),
                    values = c(4, 2))+
  labs(title = (expression(paste("NEE at -40 cm water table"))), x="week", y = expression(paste ("", CO[2]," flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

NEEweek50<-ggplot(planted_50, aes(x=week, y=CO2, fill=Chamber))+
  geom_boxplot(width=.5, outlier.size=0.9, alpha=0.8)+
  scale_x_discrete(name="Week", breaks=c("1", "2", "3", "4", "5", "6", "7", "8"), 
                   labels=c("2", "3", "4", "6", "7", "8", "9", "10"))+
  scale_y_continuous(breaks = seq(-1, 0.1, by =0.2))+
  coord_cartesian(ylim = c(-1, 0.1))+
  scale_fill_manual(name="Temperature", breaks = c("301", "302"), 
                    labels=c("Ambient", "Elevated"),
                    values = c(4, 2))+
  labs(title = (expression(paste("NEE at -50 cm water table"))), x="week", y = expression(paste ("", CO[2]," flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

grid.arrange(NEEweek30, NEEweek40, NEEweek50, ncol=1)

soilrespweek30<-ggplot(sum_30, aes(x=week, y=CO2, fill=Chamber))+
  geom_boxplot(width=.5, outlier.size=0.9, alpha=0.8)+
  scale_x_discrete(name="Week", breaks=c("1", "2", "3", "4", "5", "6", "7", "8"), 
                   labels=c("2", "3", "4", "6", "7", "8", "9", "10"))+
  scale_y_continuous(breaks = seq(0, 0.15, by =0.05))+
  coord_cartesian(ylim = c(0, 0.15))+
  scale_fill_manual(name="Temperature", breaks = c("301", "302"), 
                    labels=c("Ambient", "Elevated"),
                    values = c(4, 2))+
  labs(title = (expression(paste("Soil respiration at -30 cm water table"))), x="week", y = expression(paste ("", CO[2]," flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

soilrespweek40<-ggplot(sum_40, aes(x=week, y=CO2, fill=Chamber))+
  geom_boxplot(width=.5, outlier.size=0.9, alpha=0.8)+
  scale_x_discrete(name="Week", breaks=c("1", "2", "3", "4", "5", "6", "7", "8"), 
                   labels=c("2", "3", "4", "6", "7", "8", "9", "10"))+
  scale_y_continuous(breaks = seq(0, 0.15, by =0.05))+
  coord_cartesian(ylim = c(0, 0.15))+
  scale_fill_manual(name="Temperature", breaks = c("301", "302"), 
                    labels=c("Ambient", "Elevated"),
                    values = c(4, 2))+
  labs(title = (expression(paste("Soil respiration at -40 cm water table"))), x="week", y = expression(paste ("", CO[2]," flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

soilrespweek50<-ggplot(sum_50, aes(x=week, y=CO2, fill=Chamber))+
  geom_boxplot(width=.5, outlier.size=0.9, alpha=0.8)+
  scale_x_discrete(name="Week", breaks=c("1", "2", "3", "4", "5", "6", "7", "8"), 
                   labels=c("2", "3", "4", "6", "7", "8", "9", "10"))+
  scale_y_continuous(breaks = seq(0, 0.15, by =0.05))+
  coord_cartesian(ylim = c(0, 0.15))+
  scale_fill_manual(name="Temperature", breaks = c("301", "302"), 
                    labels=c("Ambient", "Elevated"),
                    values = c(4, 2))+
  labs(title = (expression(paste("Soil respiration at -50 cm water table"))), x="week", y = expression(paste ("", CO[2]," flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

grid.arrange(soilrespweek30, soilrespweek40, soilrespweek50, ncol=1)

DarkCO2week30<-ggplot(planted_30, aes(x=week, y=CO2.Dark, fill=Chamber))+
  geom_boxplot(width=.5, outlier.size=0.9, alpha=0.8)+
  scale_x_discrete(name="Week", breaks=c("1", "2", "3", "4", "5", "6", "7", "8"), 
                   labels=c("2", "3", "4", "6", "7", "8", "9", "10"))+
  scale_y_continuous(breaks = seq(0, 1.2, by =0.2))+
  coord_cartesian(ylim = c(0, 1.2))+
  scale_fill_manual(name="Temperature", breaks = c("301", "302"), 
                    labels=c("Ambient", "Elevated"),
                    values = c(4, 2))+
  labs(title = (expression(paste("Ecosystem respiration at -30 cm water table"))), x="week", y = expression(paste ("", CO[2]," flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DarkCO2week40<-ggplot(planted_40, aes(x=week, y=CO2.Dark, fill=Chamber))+
  geom_boxplot(width=.5, outlier.size=0.9, alpha=0.8)+
  scale_x_discrete(name="Week", breaks=c("1", "2", "3", "4", "5", "6", "7", "8"), 
                   labels=c("2", "3", "4", "6", "7", "8", "9", "10"))+
  scale_y_continuous(breaks = seq(0, 1.2, by =0.2))+
  coord_cartesian(ylim = c(0, 1.2))+
  scale_fill_manual(name="Temperature", breaks = c("301", "302"), 
                    labels=c("Ambient", "Elevated"),
                    values = c(4, 2))+
  labs(title = (expression(paste("Ecosystem respiration at -40 cm water table"))), x="week", y = expression(paste ("", CO[2]," flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DarkCO2week50<-ggplot(planted_50, aes(x=week, y=CO2.Dark, fill=Chamber))+
  geom_boxplot(width=.5, outlier.size=0.9, alpha=0.8)+
  scale_x_discrete(name="Week", breaks=c("1", "2", "3", "4", "5", "6", "7", "8"), 
                   labels=c("2", "3", "4", "6", "7", "8", "9", "10"))+
  scale_y_continuous(breaks = seq(0, 1.2, by =0.2))+
  coord_cartesian(ylim = c(0, 1.2))+
  scale_fill_manual(name="Temperature", breaks = c("301", "302"), 
                    labels=c("Ambient", "Elevated"),
                    values = c(4, 2))+
  labs(title = (expression(paste("Ecosystem respiration at -50 cm water table"))), x="week", y = expression(paste ("", CO[2]," flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

grid.arrange(DarkCO2week30, DarkCO2week40, DarkCO2week50, ncol=1)

CH4pweek30<-ggplot(sum0a_30, aes(x=week, y=CH4, fill=Chamber))+
  geom_boxplot(width=.5, outlier.size=0.9, alpha=0.8)+
  scale_x_discrete(name="Week", breaks=c("1", "2", "3", "4", "5", "6", "7", "8"), 
                   labels=c("2", "3", "4", "6", "7", "8", "9", "10"))+
  scale_y_continuous(breaks = seq(-0.03, 0.04, by =0.01))+
  coord_cartesian(ylim = c(-0.03, 0.04))+
  scale_fill_manual(name="Temperature", breaks = c("301", "302"), 
                    labels=c("Ambient", "Elevated"),
                    values = c(4, 2))+
  labs(title = (expression(paste("", CH[4], " flux at -30 cm water table"))), x="week", y = expression(paste ("", CH[4]," Flux (mg C ",-CH[4]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

CH4pweek40<-ggplot(sum0a_40, aes(x=week, y=CH4, fill=Chamber))+
  geom_boxplot(width=.5, outlier.size=0.9, alpha=0.8)+
  scale_x_discrete(name="Week", breaks=c("1", "2", "3", "4", "5", "6", "7", "8"), 
                   labels=c("2", "3", "4", "6", "7", "8", "9", "10"))+
  scale_y_continuous(breaks = seq(-0.03, 0.04, by =0.01))+
  coord_cartesian(ylim = c(-0.03, 0.04))+
  scale_fill_manual(name="Temperature", breaks = c("301", "302"), 
                    labels=c("Ambient", "Elevated"),
                    values = c(4, 2))+
  labs(title = (expression(paste("", CH[4], " flux at -40 cm water table"))), x="week", y = expression(paste ("", CH[4]," Flux (mg C ",-CH[4]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

CH4pweek50<-ggplot(sum0a_50, aes(x=week, y=CH4, fill=Chamber))+
  geom_boxplot(width=.5, outlier.size=0.9, alpha=0.8)+
  scale_x_discrete(name="Week", breaks=c("1", "2", "3", "4", "5", "6", "7", "8"), 
                   labels=c("2", "3", "4", "6", "7", "8", "9", "10"))+
  scale_y_continuous(breaks = seq(-0.03, 0.04, by =0.01))+
  coord_cartesian(ylim = c(-0.03, 0.04))+
  scale_fill_manual(name="Temperature", breaks = c("301", "302"), 
                    labels=c("Ambient", "Elevated"),
                    values = c(4, 2))+
  labs(title = (expression(paste("", CH[4], " flux at -50 cm water table"))), x="week", y = expression(paste ("", CH[4]," Flux (mg C ",-CH[4]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

grid.arrange(CH4pweek30, CH4pweek40, CH4pweek50, ncol=1)

Mweek30<-ggplot(sum0_30, aes(x=week, y=Moisture...12cm....., fill=Chamber))+
  geom_boxplot(width=.5, outlier.size=0.9, alpha=0.8)+
  scale_x_discrete(name="Week", breaks=c("1", "2", "3", "4", "5", "6", "7", "8"), 
                   labels=c("2", "3", "4", "6", "7", "8", "9", "10"))+
  scale_y_continuous(breaks = seq(0, 0.6, by =0.1))+
  coord_cartesian(ylim = c(0, 0.6))+
  scale_fill_manual(name="Temperature", breaks = c("301", "302"), 
                    labels=c("Ambient", "Elevated"),
                    values = c(4, 2))+
  labs(title = (expression(paste("Soil water content at -30 cm water table"))), x="week", y = expression(paste ("Volumetric water content ( ", m^-3 ," ", m^-3, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

Mweek40<-ggplot(sum0_40, aes(x=week, y=Moisture...12cm....., fill=Chamber))+
  geom_boxplot(width=.5, outlier.size=0.9, alpha=0.8)+
  scale_x_discrete(name="Week", breaks=c("1", "2", "3", "4", "5", "6", "7", "8"), 
                   labels=c("2", "3", "4", "6", "7", "8", "9", "10"))+
  scale_y_continuous(breaks = seq(0, 0.6, by =0.1))+
  coord_cartesian(ylim = c(0, 0.6))+
  scale_fill_manual(name="Temperature", breaks = c("301", "302"), 
                    labels=c("Ambient", "Elevated"),
                    values = c(4, 2))+
  labs(title = (expression(paste("Soil water content at -40 cm water table"))), x="week", y = expression(paste ("Volumetric water content ( ", m^-3 ," ", m^-3, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

Mweek50<-ggplot(sum0_50, aes(x=week, y=Moisture...12cm....., fill=Chamber))+
  geom_boxplot(width=.5, outlier.size=0.9, alpha=0.8)+
  scale_x_discrete(name="Week", breaks=c("1", "2", "3", "4", "5", "6", "7", "8"), 
                   labels=c("2", "3", "4", "6", "7", "8", "9", "10"))+
  scale_y_continuous(breaks = seq(0, 0.6, by =0.1))+
  coord_cartesian(ylim = c(0, 0.6))+
  scale_fill_manual(name="Temperature", breaks = c("301", "302"), 
                    labels=c("Ambient", "Elevated"),
                    values = c(4, 2))+
  labs(title = (expression(paste("Soil water content at -50 cm water table"))), x="week", y = expression(paste ("Volumetric water content ( ", m^-3 ," ", m^-3, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

grid.arrange(Mweek30, Mweek40, Mweek50, ncol=1)
grid.arrange(NEEweek30, NEEweek40, NEEweek50, ncol=1)

###############

GPPweekA<-ggplot(planted, aes(x=week, y=GPP, fill=Chamber))+
  geom_boxplot(width=.5, outlier.size=0.9, alpha=0.8)+
  scale_x_discrete(name="Week", breaks=c("1", "2", "3", "4", "5", "6", "7", "8"), 
                   labels=c("2", "3", "4", "6", "7", "8", "9", "10"))+
  scale_y_continuous(breaks = seq(0, 2, by =0.5))+
  coord_cartesian(ylim = c(0, 2))+
  scale_fill_manual(name="Temperature", breaks = c("301", "302"), 
                    labels=c("Ambient", "Elevated"),
                    values = c(4, 2))+
  labs(title = (expression(paste("GPP at ambient temperature"))), x="week", y = expression(paste ("", CO[2]," flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

GPPweekE<-ggplot(planted, aes(x=week, y=GPP, fill=Chamber))+
  geom_boxplot(width=.5, outlier.size=0.9, alpha=0.8)+
  scale_x_discrete(name="Week", breaks=c("1", "2", "3", "4", "5", "6", "7", "8"), 
                   labels=c("2", "3", "4", "6", "7", "8", "9", "10"))+
  scale_y_continuous(breaks = seq(0, 2, by =0.5))+
  coord_cartesian(ylim = c(0, 2))+
  scale_fill_manual(name="Temperature", breaks = c("301", "302"), 
                    labels=c("Ambient", "Elevated"),
                    values = c(4, 2))+
  labs(title = (expression(paste("GPP at elevated temperature"))), x="week", y = expression(paste ("", CO[2]," flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))




NEEweekA<-ggplot(planted, aes(x=week, y=CO2, fill=Chamber))+
  geom_boxplot(width=.5, outlier.size=0.9, alpha=0.8)+
  scale_x_discrete(name="Week", breaks=c("1", "2", "3", "4", "5", "6", "7", "8"), 
                   labels=c("2", "3", "4", "6", "7", "8", "9", "10"))+
  scale_y_continuous(breaks = seq(-1, 0.5, by =0.2))+
  coord_cartesian(ylim = c(-1, 0.5))+
  scale_fill_manual(name="Temperature", breaks = c("301", "302"), 
                    labels=c("Ambient", "Elevated"),
                    values = c(4, 2))+
  labs(title = (expression(paste("NEE at ambient temperature"))), x="week", y = expression(paste ("", CO[2]," flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

NEEweekE<-ggplot(planted, aes(x=week, y=CO2, fill=Chamber))+
  geom_boxplot(width=.5, outlier.size=0.9, alpha=0.8)+
  scale_x_discrete(name="Week", breaks=c("1", "2", "3", "4", "5", "6", "7", "8"), 
                   labels=c("2", "3", "4", "6", "7", "8", "9", "10"))+
  scale_y_continuous(breaks = seq(-1, 0.5, by =0.2))+
  coord_cartesian(ylim = c(-1, 0.5))+
  scale_fill_manual(name="Temperature", breaks = c("301", "302"), 
                    labels=c("Ambient", "Elevated"),
                    values = c(4, 2))+
  labs(title = (expression(paste("NEE at elevate temperature"))), x="week", y = expression(paste ("", CO[2]," flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))


soilrespweekA<-ggplot(sum, aes(x=week, y=CO2, fill=Chamber))+
  geom_boxplot(width=.5, outlier.size=0.9, alpha=0.8)+
  scale_x_discrete(name="Week", breaks=c("1", "2", "3", "4", "5", "6", "7", "8"), 
                   labels=c("2", "3", "4", "6", "7", "8", "9", "10"))+
  scale_y_continuous(breaks = seq(0, 0.15, by =0.05))+
  coord_cartesian(ylim = c(0, 0.15))+
  scale_fill_manual(name="Temperature", breaks = c("301", "302"), 
                    labels=c("Ambient", "Elevated"),
                    values = c(4, 2))+
  labs(title = (expression(paste("Soil respiration at ambient temperature"))), x="week", y = expression(paste ("", CO[2]," flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

soilrespweekE<-ggplot(sum, aes(x=week, y=CO2, fill=Chamber))+
  geom_boxplot(width=.5, outlier.size=0.9, alpha=0.8)+
  scale_x_discrete(name="Week", breaks=c("1", "2", "3", "4", "5", "6", "7", "8"), 
                   labels=c("2", "3", "4", "6", "7", "8", "9", "10"))+
  scale_y_continuous(breaks = seq(0, 0.15, by =0.05))+
  coord_cartesian(ylim = c(0, 0.15))+
  scale_fill_manual(name="Temperature", breaks = c("301", "302"), 
                    labels=c("Ambient", "Elevated"),
                    values = c(4, 2))+
  labs(title = (expression(paste("Soil respiration at elevated temperature"))), x="week", y = expression(paste ("", CO[2]," flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DarkCO2weekA<-ggplot(planted, aes(x=week, y=CO2.Dark, fill=Chamber))+
  geom_boxplot(width=.5, outlier.size=0.9, alpha=0.8)+
  scale_x_discrete(name="Week", breaks=c("1", "2", "3", "4", "5", "6", "7", "8"), 
                   labels=c("2", "3", "4", "6", "7", "8", "9", "10"))+
  scale_y_continuous(breaks = seq(0, 1.5, by =0.2))+
  coord_cartesian(ylim = c(0, 1.5))+
  scale_fill_manual(name="Temperature", breaks = c("301", "302"), 
                    labels=c("Ambient", "Elevated"),
                    values = c(4, 2))+
  labs(title = (expression(paste("Ecosystem respiration at ambient temperature"))), x="week", y = expression(paste ("", CO[2]," flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

DarkCO2weekE<-ggplot(planted, aes(x=week, y=CO2.Dark, fill=Chamber))+
  geom_boxplot(width=.5, outlier.size=0.9, alpha=0.8)+
  scale_x_discrete(name="Week", breaks=c("1", "2", "3", "4", "5", "6", "7", "8"), 
                   labels=c("2", "3", "4", "6", "7", "8", "9", "10"))+
  scale_y_continuous(breaks = seq(0, 1.5, by =0.2))+
  coord_cartesian(ylim = c(0, 1.5))+
  scale_fill_manual(name="Temperature", breaks = c("301", "302"), 
                    labels=c("Ambient", "Elevated"),
                    values = c(4, 2))+
  labs(title = (expression(paste("Ecosystem respiration at elevated temperature"))), x="week", y = expression(paste ("", CO[2]," flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))


CH4pweekA<-ggplot(sum0a, aes(x=week, y=CH4, fill=Chamber))+
  geom_boxplot(width=.5, outlier.size=0.9, alpha=0.8)+
  scale_x_discrete(name="Week", breaks=c("1", "2", "3", "4", "5", "6", "7", "8"), 
                   labels=c("2", "3", "4", "6", "7", "8", "9", "10"))+
  scale_y_continuous(breaks = seq(-0.1, 0.1, by =0.05))+
  coord_cartesian(ylim = c(-0.1, 0.1))+
  scale_fill_manual(name="Temperature", breaks = c("301", "302"), 
                    labels=c("Ambient", "Elevated"),
                    values = c(4, 2))+
  labs(title = (expression(paste("", CH[4], " flux at ambient temperature"))), x="week", y = expression(paste ("", CH[4]," Flux (mg C ",-CH[4]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

CH4pweekE<-ggplot(sum0a, aes(x=week, y=CH4, fill=Chamber))+
  geom_boxplot(width=.5, outlier.size=0.9, alpha=0.8)+
  scale_x_discrete(name="Week", breaks=c("1", "2", "3", "4", "5", "6", "7", "8"), 
                   labels=c("2", "3", "4", "6", "7", "8", "9", "10"))+
  scale_y_continuous(breaks = seq(-0.1, 0.1, by =0.05))+
  coord_cartesian(ylim = c(-0.1, 0.1))+
  scale_fill_manual(name="Temperature", breaks = c("301", "302"), 
                    labels=c("Ambient", "Elevated"),
                    values = c(4, 2))+
  labs(title = (expression(paste("", CH[4], " flux at elevated temperature"))), x="week", y = expression(paste ("", CH[4]," Flux (mg C ",-CH[4]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

MweekA<-ggplot(sum0, aes(x=week, y=Moisture...12cm....., fill=Chamber))+
  geom_boxplot(width=.5, outlier.size=0.9, alpha=0.8)+
  scale_x_discrete(name="Week", breaks=c("1", "2", "3", "4", "5", "6", "7", "8"), 
                   labels=c("2", "3", "4", "6", "7", "8", "9", "10"))+
  scale_y_continuous(breaks = seq(0, 0.6, by =0.1))+
  coord_cartesian(ylim = c(0, 0.6))+
  scale_fill_manual(name="Temperature", breaks = c("301", "302"), 
                    labels=c("Ambient", "Elevated"),
                    values = c(4, 2))+
  labs(title = (expression(paste("Soil water content at ambient temperature"))), x="week", y = expression(paste ("Volumetric water content ( ", m^-3 ," ", m^-3, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

MweekE<-ggplot(sum0, aes(x=week, y=Moisture...12cm....., fill=Chamber))+
  geom_boxplot(width=.5, outlier.size=0.9, alpha=0.8)+
  scale_x_discrete(name="Week", breaks=c("1", "2", "3", "4", "5", "6", "7", "8"), 
                   labels=c("2", "3", "4", "6", "7", "8", "9", "10"))+
  scale_y_continuous(breaks = seq(0, 0.6, by =0.1))+
  coord_cartesian(ylim = c(0, 0.6))+
  scale_fill_manual(name="Temperature", breaks = c("301", "302"), 
                    labels=c("Ambient", "Elevated"),
                    values = c(4, 2))+
  labs(title = (expression(paste("Soil water content at elevated temperature"))), x="week", y = expression(paste ("Volumetric water content ( ", m^-3 ," ", m^-3, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

grid.arrange(MweekA, MweekE, ncol=1)

sum0_P<-filter(sum0, Planting=="Planted")
sum0_U<-filter(sum0, Planting=="Not planted")

MweekP<-ggplot(sum0_P, aes(x=week, y=Moisture...12cm....., fill=Chamber))+
  geom_boxplot(width=.5, outlier.size=0.9, alpha=0.8)+
  scale_x_discrete(name="Week", breaks=c("1", "2", "3", "4", "5", "6", "7", "8"), 
                   labels=c("2", "3", "4", "6", "7", "8", "9", "10"))+
  scale_y_continuous(breaks = seq(0, 0.6, by =0.1))+
  coord_cartesian(ylim = c(0, 0.6))+
  scale_fill_manual(name="Temperature", breaks = c("301", "302"), 
                    labels=c("Ambient", "Elevated"),
                    values = c(4, 2))+
  labs(title = (expression(paste("Soil water content in planted cores"))), x="week", y = expression(paste ("Volumetric water content ( ", m^-3 ," ", m^-3, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

MweekU<-ggplot(sum0_U, aes(x=week, y=Moisture...12cm....., fill=Chamber))+
  geom_boxplot(width=.5, outlier.size=0.9, alpha=0.8)+
  scale_x_discrete(name="Week", breaks=c("1", "2", "3", "4", "5", "6", "7", "8"), 
                   labels=c("2", "3", "4", "6", "7", "8", "9", "10"))+
  scale_y_continuous(breaks = seq(0, 0.6, by =0.1))+
  coord_cartesian(ylim = c(0, 0.6))+
  scale_fill_manual(name="Temperature", breaks = c("301", "302"), 
                    labels=c("Ambient", "Elevated"),
                    values = c(4, 2))+
  labs(title = (expression(paste("Soil water content in unplanted cores"))), x="week", y = expression(paste ("Volumetric water content ( ", m^-3 ," ", m^-3, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

CH4pweekP<-ggplot(sum0_P, aes(x=week, y=CH4, fill=Chamber))+
  geom_boxplot(width=.5, outlier.size=0.9, alpha=0.8)+
  scale_x_discrete(name="Week", breaks=c("1", "2", "3", "4", "5", "6", "7", "8"), 
                   labels=c("2", "3", "4", "6", "7", "8", "9", "10"))+
  scale_y_continuous(breaks = seq(-0.03, 0.04, by =0.01))+
  coord_cartesian(ylim = c(-0.03, 0.04))+
  scale_fill_manual(name="Temperature", breaks = c("301", "302"), 
                    labels=c("Ambient", "Elevated"),
                    values = c(4, 2))+
  labs(title = (expression(paste("Planted cores"))), x="week", y = expression(paste ("", CH[4]," Flux (mg C ",-CH[4]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

CH4pweekU<-ggplot(sum0_U, aes(x=week, y=CH4, fill=Chamber))+
  geom_boxplot(width=.5, outlier.size=0.9, alpha=0.8)+
  scale_x_discrete(name="Week", breaks=c("1", "2", "3", "4", "5", "6", "7", "8"), 
                   labels=c("2", "3", "4", "6", "7", "8", "9", "10"))+
  scale_y_continuous(breaks = seq(-0.03, 0.04, by =0.01))+
  coord_cartesian(ylim = c(-0.03, 0.04))+
  scale_fill_manual(name="Temperature", breaks = c("301", "302"), 
                    labels=c("Ambient", "Elevated"),
                    values = c(4, 2))+
  labs(title = (expression(paste("Unplanted cores"))), x="week", y = expression(paste ("", CH[4]," Flux (mg C ",-CH[4]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

grid.arrange(CH4pweekU, CH4pweekP, CH4pweekU, ncol=1)
##############################

DarkCO2tr<-ggplot(planted, aes(x=Treatment, y=CO2.Dark, color=Treatment))+
  geom_boxplot(width=.2, outlier.size=0.9, size=0.8)+
  scale_x_discrete(name="", 
                   breaks = c("30AmbP", "40AmbP", "50AmbP", "30ElevP", "40ElevP", "50ElevP"))+
  scale_y_continuous(breaks = seq(-0.5, 1.5, by =0.5))+
  coord_cartesian(ylim = c(-0.5, 1.5))+
  labs(title = (expression(paste("Ecosystem respiration"))), x="", y = expression(paste ("", CO[2]," flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3, colour="black")+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

CH4tr<-ggplot(treatmentch4, aes(x=Treatment, y=CH4, color=Treatment))+
  geom_boxplot(width=.2, outlier.size=0.9, size=0.8)+
  scale_x_discrete(name="", 
                   breaks = c("30AmbNP", "30AmbP", "40AmbNP", "40AmbP", "50AmbNP", "50AmbP", "30ElevNP", "30ElevP", "40ElevNP", "40ElevP", "50ElevNP", "50ElevP"))+
                   scale_y_continuous(breaks = seq(-0.05, 0.1, by =0.05))+
  coord_cartesian(ylim = c(-0.05, 0.1))+
  labs(title = (expression(paste("", CH[4], " flux"))), x="week", y = expression(paste ("", CH[4]," Flux (mg C ",-CH[4]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3, colour="black")+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

NEEtr<-ggplot(planted, aes(x=Treatment, y=CO2, color=Treatment))+
  geom_boxplot(width=.2, outlier.size=0.9, size=0.8)+
  scale_x_discrete(name="", 
                   breaks = c("30AmbP", "40AmbP", "50AmbP", "30ElevP", "40ElevP", "50ElevP"))+
  scale_y_continuous(breaks = seq(-1.5, 0.5, by =0.5))+
  coord_cartesian(ylim = c(-1.5, 0.5))+
  labs(title = (expression(paste("Average NEE: treatment comparison"))), x="", y = expression(paste ("", CO[2]," flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3, colour="black")+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

resptr<-ggplot(treatmentCO2NP, aes(x=Treatment, y=CO2, color=Treatment))+
  geom_boxplot(width=.2, outlier.size=0.9, size=0.8)+
  scale_x_discrete(name="", 
                   breaks = c("30Amb", "40Amb", "50Amb", "30Elev", "40Elev", "50Elev"))+
  scale_y_continuous(breaks = seq(0, 0.4, by =0.1))+
  coord_cartesian(ylim = c(0, 0.4))+
  labs(title = (expression(paste("Average soil respiration: treatment comparison"))), x="", y = expression(paste ("", CO[2]," flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3, colour="black")+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

GPPtr<-ggplot(GPPonly, aes(x=Treatment, y=GPP, color=Treatment))+
  geom_boxplot(width=.2, outlier.size=0.9, size=0.8)+
  scale_x_discrete(name="", 
                   breaks = c("30AmbP", "40AmbP", "50AmbP", "30ElevP", "40ElevP", "50ElevP"))+
  scale_y_continuous(breaks = seq(0, 2, by =0.5))+
  coord_cartesian(ylim = c(0, 2))+
  labs(title = (expression(paste("Average GPP: treatment comparison"))), x="", y = expression(paste ("", CO[2]," flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3, colour="black")+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

###########
weekT$Week<-as.factor(weekT$Week)

a<-ggplot(weekT, aes(x= Week, y= Day, colour=Chamber))+
  geom_point (aes (y=Day), size=4)+
  geom_path(aes(group=Chamber), size=2)+
  scale_x_discrete(name="Week")+
  scale_y_continuous(breaks = seq(0, 16, by =2))+
  coord_cartesian(ylim = c(0, 16))+
  theme_bw()+
  labs(title = (expression(paste("Daytime temperture settings"))), x="", y = expression(paste ("Temperature [",degree,"C]")))+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

b<-ggplot(weekT, aes(x= Week, y=Night, colour=Chamber))+
  geom_point (aes (y=Night), size=4)+
  geom_path(aes(group=Chamber), size=2)+
  scale_x_discrete(name="Week")+
  scale_y_continuous(breaks = seq(0, 16, by =2))+
  coord_cartesian(ylim = c(0, 16))+
  theme_bw()+
  labs(title = (expression(paste("Nighttime temperture settings"))), x="", y = expression(paste ("Temperature [",degree,"C]")))+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

grid.arrange(a, b, ncol=2)
################
to be removed 


GPPweek30<-ggplot(planted_30, aes(x=week, y=GPP, fill=Chamber))+
  geom_boxplot(width=.5, outlier.size=0.9, alpha=0.8)+
  scale_x_discrete(name="Week", breaks=c("1", "2", "3", "4", "5", "6", "7", "8"), 
                   labels=c("2", "3", "4", "6", "7", "8", "9", "10"))+
  scale_y_continuous(breaks = seq(0, 1.6, by =0.5))+
  coord_cartesian(ylim = c(0, 1.6))+
  scale_fill_manual(name="Temperature", breaks = c("301", "302"), 
                    labels=c("Ambient", "Elevated"),
                    values = c(4, 2))+
  labs(title = (expression(paste("GPP at -30 cm water table"))), x="week", y = expression(paste ("GPP flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

GPPweek40<-ggplot(planted_40, aes(x=week, y=GPP, fill=Chamber))+
  geom_boxplot(width=.5, outlier.size=0.9, alpha=0.8)+
  scale_x_discrete(name="Week", breaks=c("1", "2", "3", "4", "5", "6", "7", "8"), 
                   labels=c("2", "3", "4", "6", "7", "8", "9", "10"))+
  scale_y_continuous(breaks = seq(0, 1.6, by =0.5))+
  coord_cartesian(ylim = c(0, 1.6))+
  scale_fill_manual(name="Temperature", breaks = c("301", "302"), 
                    labels=c("Ambient", "Elevated"),
                    values = c(4, 2))+
  labs(title = (expression(paste("GPP at -40 cm water table"))), x="week", y = expression(paste ("ER flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

GPPweek50<-ggplot(planted_50, aes(x=week, y=GPP, fill=Chamber))+
  geom_boxplot(width=.5, outlier.size=0.9, alpha=0.8)+
  scale_x_discrete(name="Week", breaks=c("1", "2", "3", "4", "5", "6", "7", "8"), 
                   labels=c("2", "3", "4", "6", "7", "8", "9", "10"))+
  scale_y_continuous(breaks = seq(0, 1.6, by =0.5))+
  coord_cartesian(ylim = c(0, 1.6))+
  scale_fill_manual(name="Temperature", breaks = c("301", "302"), 
                    labels=c("Ambient", "Elevated"),
                    values = c(4, 2))+
  labs(title = (expression(paste("GPP at -50 cm water table"))), x="week", y = expression(paste ("NEE flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

grid.arrange(GPPweek30, GPPweek40, GPPweek50, ncol=1)

