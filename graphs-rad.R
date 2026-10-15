lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
library(lme4)
library(nlme)
library(MuMIn)
library(Matrix)
library(gridExtra)
library(ggplot2)
library(car)
library(LMERConvenienceFunctions)
library(multcomp)
library(dplyr)

biomass.weight$CO2<-as.factor(biomass.weight$CO2)
biomass.weight<-filter(biomass.weight, status!="old")
wet<-filter(biomass.weight, wet.dry=="wet")
dry<-filter(biomass.weight, wet.dry=="dry")

cn.peat$Water.table<-as.factor(cn.peat$Water.table)
cn.peat$CO2<-as.factor(cn.peat$CO2)
cn.radish$water.table<-as.factor(cn.radish$water.table)
cn.radish$chamber<-as.factor(cn.radish$chamber)
cn.radish<-filter(cn.radish, Source!="bulb")
cn.peat<-filter(cn.peat, wet.dry!="dry")
cn.peatT<-filter(cn.peat, top.bottom!="bottom" & NO3.mg.l!="NA")
cn.peatB<-filter(cn.peat, top.bottom!="top" & NO3.mg.l!="NA")
cn.peatP<-filter(cn.peat, Planting=="P")
cn.peatU<-filter(cn.peat, Planting=="U")
cn.peatPtop<-filter(cn.peat, Planting=="P" &top.bottom!="bottom")
cn.peatUtop<-filter(cn.peat, Planting=="U" &top.bottom!="bottom")

Summary_Table_February_cor$Water.table<-as.factor(Summary_Table_February_cor$Water.table)
Summary_Table_February_cor$Chamber.temperature<-as.factor(Summary_Table_February_cor$Chamber.temperature)
Summary_Table_February_cor$CO2.level<-as.factor(Summary_Table_February_cor$CO2.level)
Summary_Table_February_cor$CO2.Dark<-as.numeric(Summary_Table_February_cor$CO2.Dark)
Summary_Table_February_cor$week<-as.factor(Summary_Table_February_cor$week)
Summary_Table_February_cor<-filter(Summary_Table_February_cor, Status!="Old")
Summary_Table_February_cor<-filter(Summary_Table_February_cor, Core!="1a"&Core!="5a"&Core!="16al"&Core!="2e"&Core!="5e")

Summary_Table_February_corP<-filter(Summary_Table_February_cor, Planting=="Planted")
Summary_Table_February_corU<-filter(Summary_Table_February_cor, Planting=="Not Planted")


com<-summarise(group_by(wet, Treatment),
                mean_bulb_weight1=mean(bulbs..g..mean), 
                SEdr1=sd(bulbs..g..mean)/sqrt(length(bulbs..g..mean)),
                lwr1r=mean_bulb_weight1-SEdr1,
                upr1r=mean_bulb_weight1+SEdr1,
                mean_aboveground1=mean(leaves..g..mean),
                SEdt1=sd(leaves..g..mean)/sqrt(length(leaves..g..mean)),
                lwr1t=mean_aboveground1-SEdt1,
                upr1t=mean_aboveground1+SEdt1, 
                mean_abovegroundR=mean(roots..g..mean),
                SEdtR=sd(roots..g..mean)/sqrt(length(roots..g..mean)),
                lwr1R=mean_abovegroundR-SEdtR,
                upr1R=mean_abovegroundR+SEdtR) 

limits_bulb<-aes(ymin=lwr1r, ymax=upr1r)
limits_above<-aes(ymin=lwr1t, ymax=upr1t)
limits_root<-aes(ymin=lwr1R, ymax=upr1R)

abovebelow1<-ggplot(com, aes(x=Treatment, shape=Treatment))+
  geom_point (aes (y=mean_bulb_weight1, colour="Bulbs"), size=5)+
  geom_point (aes (y=mean_aboveground1, colour="Leaves"), size=5)+
  geom_point (aes (y=mean_abovegroundR, colour="Roots"), size=5)+
  scale_colour_manual(name="", values=c('Bulbs'="red", 'Leaves'="green", 'Roots'="brown"))+
  labs(title = "Wet leaves, bulb and root biomass (g): treatment comparison", x="", y ="Weight (g)")+
  scale_shape_manual(name="", breaks = c("400ppm30", "400ppm50", "850ppm30", "850ppm50"), 
                     labels=c("-30cm 400 ppm", "-50cm 400 ppm", "-30cm 850 ppm", "-50cm 850 ppm"), 
                     values = c(19, 15, 17, 18))+
  scale_y_continuous(breaks = seq(0, 35, by =5))+
  coord_cartesian(ylim = c(0, 35))+
  geom_errorbar(limits_bulb, width=0.1)+
  geom_errorbar(limits_above, width=0.1)+
  geom_errorbar(limits_root, width=0.1)+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        strip.text = element_text(size = 12))


com2<-summarise(group_by(dry, Treatment),
               mean_bulb_weight1=mean(bulbs..g..mean), 
               SEdr1=sd(bulbs..g..mean)/sqrt(length(bulbs..g..mean)),
               lwr1r=mean_bulb_weight1-SEdr1,
               upr1r=mean_bulb_weight1+SEdr1,
               mean_aboveground1=mean(leaves..g..mean),
               SEdt1=sd(leaves..g..mean)/sqrt(length(leaves..g..mean)),
               lwr1t=mean_aboveground1-SEdt1,
               upr1t=mean_aboveground1+SEdt1, 
               mean_abovegroundR=mean(roots..g..mean),
                 SEdtR=sd(roots..g..mean)/sqrt(length(roots..g..mean)),
                 lwr1R=mean_abovegroundR-SEdtR,
                 upr1R=mean_abovegroundR+SEdtR) 

limits_bulb2<-aes(ymin=lwr1r, ymax=upr1r)
limits_above2<-aes(ymin=lwr1t, ymax=upr1t)
limits_root<-aes(ymin=lwr1R, ymax=upr1R)

abovebelow2<-ggplot(com2, aes(x=Treatment, shape=Treatment))+
  geom_point (aes (y=mean_bulb_weight1, colour="Bulbs"), size=5)+
  geom_point (aes (y=mean_aboveground1, colour="Leaves"), size=5)+
  geom_point (aes (y=mean_abovegroundR, colour="Roots"), size=5)+
  scale_colour_manual(name="", values=c('Bulbs'="red", 'Leaves'="green", 'Roots'="brown"))+
  labs(title = "Dry leaves, bulb and root biomass (g): treatment comparison", x="", y ="Weight (g)")+
  scale_shape_manual(name="", breaks = c("400ppm30", "400ppm50", "850ppm30", "850ppm50"), 
                     labels=c("-30cm 400 ppm", "-50cm 400 ppm", "-30cm 850 ppm", "-50cm 850 ppm"), 
                     values = c(19, 15, 17, 18))+
  scale_y_continuous(breaks = seq(0, 1.5, by =0.5))+
  coord_cartesian(ylim = c(0, 1.5))+
  geom_errorbar(limits_bulb2, width=0.1)+
  geom_errorbar(limits_above2, width=0.1)+
  geom_errorbar(limits_root, width=0.1)+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        strip.text = element_text(size = 12))

########################

GPPtr<-ggplot(Summary_Table_February_corP, aes(x=week, y=GPP, fill=Treatment))+
  geom_boxplot(width=.6, outlier.size=0.9, size=0.5)+
  scale_x_discrete(name="Week", breaks=c("1", "2", "3", "4", "5", "6"), 
                   labels=c("1", "2", "3", "4", "5","6"))+
  scale_fill_manual(name="Treatment", breaks = c("400ppm30", "400ppm50", "850ppm30", "850ppm50"), 
                     labels=c("-30cm 400 ppm", "-50cm 400 ppm", "-30cm 850 ppm", "-50cm 850 ppm"), 
                     values = c("turquoise3", "deeppink", "springgreen", "yellow"))+
  scale_y_continuous(breaks = seq(0, 3, by =0.5))+
  coord_cartesian(ylim = c(0, 3))+
  labs(title = (expression(paste("Average GPP: treatment comparison"))), x="", y = expression(paste ("", CO[2]," flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=18, size=4, colour="black", position=position_dodge(0.5))+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

Darktr<-ggplot(Summary_Table_February_corP, aes(x=week, y=CO2.Dark, fill=Treatment))+
  geom_boxplot(width=.6, outlier.size=0.9, size=0.5)+
  scale_x_discrete(name="Week", breaks=c("1", "2", "3", "4", "5", "6"), 
                   labels=c("1", "2", "3", "4", "5","6"))+
  scale_fill_manual(name="Treatment", breaks = c("400ppm30", "400ppm50", "850ppm30", "850ppm50"), 
                    labels=c("-30cm 400 ppm", "-50cm 400 ppm", "-30cm 850 ppm", "-50cm 850 ppm"), 
                    values = c("turquoise3", "deeppink", "springgreen", "yellow"))+
  scale_y_continuous(breaks = seq(0, 2, by =0.5))+
  coord_cartesian(ylim = c(0, 2))+
  labs(title = (expression(paste("Average ER: treatment comparison"))), x="", y = expression(paste ("", CO[2]," flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=18, size=4, colour="black", position=position_dodge(0.5))+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

Rh<-ggplot(Summary_Table_February_corU, aes(x=week, y=CO2, fill=Treatment))+
  geom_boxplot(width=.6, outlier.size=0.9, size=0.5)+
  scale_x_discrete(name="Week", breaks=c("1", "2", "3", "4", "5", "6"), 
                   labels=c("1", "2", "3", "4", "5","6"))+
  scale_fill_manual(name="Treatment", breaks = c("400ppm30", "400ppm50", "850ppm30", "850ppm50"), 
                    labels=c("-30cm 400 ppm", "-50cm 400 ppm", "-30cm 850 ppm", "-50cm 850 ppm"), 
                    values = c("turquoise3", "deeppink", "springgreen", "yellow"))+
  scale_y_continuous(breaks = seq(-0.1, 0.3, by =0.1))+
  coord_cartesian(ylim = c(-0.1, 0.3))+
  labs(title = (expression(paste("Average Rh: treatment comparison"))), x="", y = expression(paste ("", CO[2]," flux (g C ",-CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=18, size=4, colour="black", position=position_dodge(0.5))+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

CH4P<-ggplot(Summary_Table_February_corP, aes(x=week, y=CH4, fill=Treatment))+
  geom_boxplot(width=.6, outlier.size=0.9, size=0.5)+
  scale_x_discrete(name="Week", breaks=c("1", "2", "3", "4", "5", "6"), 
                   labels=c("1", "2", "3", "4", "5","6"))+
  scale_fill_manual(name="Treatment", breaks = c("400ppm30", "400ppm50", "850ppm30", "850ppm50"), 
                    labels=c("-30cm 400 ppm", "-50cm 400 ppm", "-30cm 850 ppm", "-50cm 850 ppm"), 
                    values = c("turquoise3", "deeppink", "springgreen", "yellow"))+
  scale_y_continuous(breaks = seq(-0.01, 0.01, by =0.005))+
  coord_cartesian(ylim = c(-0.01, 0.01))+
  labs(title = (expression(paste("Average ", CH[4], " flux in planted cores"))), x="", y = expression(paste ("", CH[4]," Flux (mg C ",-CH[4]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=18, size=4, colour="black", position=position_dodge(0.5))+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

CH4U<-ggplot(Summary_Table_February_corU, aes(x=week, y=CH4, fill=Treatment))+
  geom_boxplot(width=.6, outlier.size=0.9, size=0.5)+
  scale_x_discrete(name="Week", breaks=c("1", "2", "3", "4", "5", "6"), 
                   labels=c("1", "2", "3", "4", "5","6"))+
  scale_fill_manual(name="Treatment", breaks = c("400ppm30", "400ppm50", "850ppm30", "850ppm50"), 
                    labels=c("-30cm 400 ppm", "-50cm 400 ppm", "-30cm 850 ppm", "-50cm 850 ppm"), 
                    values = c("turquoise3", "deeppink", "springgreen", "yellow"))+
  scale_y_continuous(breaks = seq(-0.015, 0.01, by =0.005))+
  coord_cartesian(ylim = c(-0.015, 0.01))+
  labs(title = (expression(paste("Average ", CH[4], " flux in unplanted cores"))), x="", y = expression(paste ("", CH[4]," Flux (mg C ",-CH[4]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=18, size=4, colour="black", position=position_dodge(0.5))+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))



swc1<-ggplot(Summary_Table_February_corU, aes(x=Treatment, y=Moisture, fill=Treatment))+
  geom_boxplot(width=.5, outlier.size=0.9, size=0.5)+
  scale_fill_manual(name="Treatment", breaks = c("400ppm30", "400ppm50", "850ppm30", "850ppm50"), 
                    labels=c("-30cm 400 ppm", "-50cm 400 ppm", "-30cm 850 ppm", "-50cm 850 ppm"), 
                    values = c("turquoise3", "deeppink", "springgreen", "yellow"))+
  labs(title = "Soil water content in unplanted cores", x="", y = expression(paste ("Volumetric water content ( ", m^-3 ," ", m^-3, ")", sep="")))+
  scale_y_continuous(breaks = seq(0, 0.6, by =0.1))+
  coord_cartesian(ylim = c(0, 0.6))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=18, size=4, colour="black", position=position_dodge(0.5))+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

swc2<-ggplot(Summary_Table_February_corP, aes(x=Treatment, y=Moisture, fill=Treatment))+
  geom_boxplot(width=.5, outlier.size=0.9, size=0.5)+
  scale_fill_manual(name="Treatment", breaks = c("400ppm30", "400ppm50", "850ppm30", "850ppm50"), 
                    labels=c("-30cm 400 ppm", "-50cm 400 ppm", "-30cm 850 ppm", "-50cm 850 ppm"), 
                    values = c("turquoise3", "deeppink", "springgreen", "yellow"))+
  labs(title = "Soil water content in planted cores", x="", y = expression(paste ("Volumetric water content ( ", m^-3 ," ", m^-3, ")", sep="")))+
  scale_y_continuous(breaks = seq(0, 0.6, by =0.1))+
  coord_cartesian(ylim = c(0, 0.6))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=18, size=4, colour="black", position=position_dodge(0.5))+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

moiU<-ggplot(Summary_Table_February_corU, aes(x=week, y=Moisture, fill=Treatment))+
  geom_boxplot(width=.6, outlier.size=0.9, size=0.5)+
  scale_x_discrete(name="Week", breaks=c("1", "2", "3", "4", "5", "6"), 
                   labels=c("1", "2", "3", "4", "5","6"))+
  scale_fill_manual(name="Treatment", breaks = c("400ppm30", "400ppm50", "850ppm30", "850ppm50"), 
                    labels=c("-30cm 400 ppm", "-50cm 400 ppm", "-30cm 850 ppm", "-50cm 850 ppm"), 
                    values = c("turquoise3", "deeppink", "springgreen", "yellow"))+
  labs(title = "Soil water content in unplanted cores", x="", y = expression(paste ("Volumetric water content ( ", m^-3 ," ", m^-3, ")", sep="")))+
  scale_y_continuous(breaks = seq(0, 0.5, by =0.1))+
  coord_cartesian(ylim = c(0, 0.5))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=18, size=4, colour="black", position=position_dodge(0.5))+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

moiP<-ggplot(Summary_Table_February_corP, aes(x=week, y=Moisture, fill=Treatment))+
  geom_boxplot(width=.6, outlier.size=0.9, size=0.5)+
  scale_x_discrete(name="Week", breaks=c("1", "2", "3", "4", "5", "6"), 
                   labels=c("1", "2", "3", "4", "5","6"))+
  scale_fill_manual(name="Treatment", breaks = c("400ppm30", "400ppm50", "850ppm30", "850ppm50"), 
                    labels=c("-30cm 400 ppm", "-50cm 400 ppm", "-30cm 850 ppm", "-50cm 850 ppm"), 
                    values = c("turquoise3", "deeppink", "springgreen", "yellow"))+
  labs(title = "Soil water content in planted cores", x="", y = expression(paste ("Volumetric water content ( ", m^-3 ," ", m^-3, ")", sep="")))+
  scale_y_continuous(breaks = seq(0, 0.5, by =0.1))+
  coord_cartesian(ylim = c(0, 0.5))+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=18, size=4, colour="black", position=position_dodge(0.5))+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

#############################################

radCN<-summarise(group_by(cn.radish, Treatment),
               mean_cn_weight1=mean(CNRatio), 
               SEdr1=sd(CNRatio)/sqrt(length(CNRatio)),
               lwr1r=mean_cn_weight1-SEdr1,
               upr1r=mean_cn_weight1+SEdr1)

limits_cn<-aes(ymin=lwr1r, ymax=upr1r)

CNrat<-ggplot(radCN, aes(x=Treatment, shape=Treatment))+
  geom_point (aes (y=mean_cn_weight1), size=5)+
  labs(title = "Radish leaf C:N ratio", x="", y ="C:N ratio")+
  scale_shape_manual(name="", breaks = c("400ppm30", "400ppm50", "850ppm30", "850ppm50"), 
                     labels=c("-30cm 400 ppm", "-50cm 400 ppm", "-30cm 850 ppm", "-50cm 850 ppm"), 
                     values = c(19, 15, 17, 18))+
  scale_y_continuous(breaks = seq(0, 30, by =10))+
  coord_cartesian(ylim = c(0, 30))+
  geom_errorbar(limits_cn, width=0.1)+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        strip.text = element_text(size = 12))

soilCN<-summarise(group_by(cn.peatUtop, Treatment),
                 mean_cn_peat=mean(C.N), 
                 SEdr1=sd(C.N)/sqrt(length(C.N)),
                 lwr1r=mean_cn_peat-SEdr1,
                 upr1r=mean_cn_peat+SEdr1)

limits_cn_peat<-aes(ymin=lwr1r, ymax=upr1r)

CNpeat1<-ggplot(soilCN, aes(x=Treatment, shape=Treatment))+
  geom_point (aes (y=mean_cn_peat), size=4)+
  labs(title = "Topsoil C:N ratio in unplanted cores", x="", y ="C:N ratio")+
  scale_shape_manual(name="", breaks = c("400ppm30", "400ppm50", "850ppm30", "850ppm50"), 
                     labels=c("-30cm 400 ppm", "-50cm 400 ppm", "-30cm 850 ppm", "-50cm 850 ppm"), 
                     values = c(19, 15, 17, 18))+
  scale_y_continuous(breaks = seq(0, 20, by =5))+
  coord_cartesian(ylim = c(0, 20))+
  geom_errorbar(limits_cn_peat, width=0.1)+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        strip.text = element_text(size = 12))

soilCN2<-summarise(group_by(cn.peatPtop, Treatment),
                  mean_cn_peat2=mean(C.N), 
                  SEdr1=sd(C.N)/sqrt(length(C.N)),
                  lwr1r=mean_cn_peat2-SEdr1,
                  upr1r=mean_cn_peat2+SEdr1)

limits_cn_peat2<-aes(ymin=lwr1r, ymax=upr1r)

CNpeat2<-ggplot(soilCN2, aes(x=Treatment, shape=Treatment))+
  geom_point (aes (y=mean_cn_peat2), size=4)+
  labs(title = "Topsoil C:N ratio in planted cores", x="", y ="C:N ratio")+
  scale_shape_manual(name="", breaks = c("400ppm30", "400ppm50", "850ppm30", "850ppm50"), 
                     labels=c("-30cm 400 ppm", "-50cm 400 ppm", "-30cm 850 ppm", "-50cm 850 ppm"), 
                     values = c(19, 15, 17, 18))+
  scale_y_continuous(breaks = seq(0, 20, by =5))+
  coord_cartesian(ylim = c(0, 20))+
  geom_errorbar(limits_cn_peat2, width=0.1)+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        strip.text = element_text(size = 12))
######################################3

Ngen<-summarise(group_by(cn.peatT, Treatment),
                mean_NO3=mean(NO3.mg.l), 
                SEdr1=sd(NO3.mg.l)/sqrt(length(NO3.mg.l)),
                lwr1r=mean_NO3-SEdr1,
                upr1r=mean_NO3+SEdr1,
                mean_NH3=mean(NH3.mg.l),
                SEdt1=sd(NH3.mg.l)/sqrt(length(NH3.mg.l)),
                lwr1t=mean_NH3-SEdt1,
                upr1t=mean_NH3+SEdt1)

limits_N1<-aes(ymin=lwr1r, ymax=upr1r)
limits_N2<-aes(ymin=lwr1t, ymax=upr1t)

NONH<-ggplot(Ngen, aes(x=Treatment, shape=Treatment))+
  geom_point (aes (y=mean_NO3, colour="Nitrate"), size=5)+
  geom_point (aes (y=mean_NH3, colour="Ammonium"), size=5)+
  scale_colour_manual(name="", values=c("Nitrate"="blue", 'Ammonium'="green"))+
  labs(title = (expression(paste("", NH[4]^+""," and ", NO[3]^-""," concentration in topsoil of planted cores"))), x="", y = expression(paste ("", NH[4]^+""," and  ", NO[3]^-"","  (", mg^-1 ," ", l^-1 , ")", sep="")))+
  scale_shape_manual(name="", breaks = c("400ppm30", "400ppm50", "850ppm30", "850ppm50"), 
                     labels=c("-30cm 400 ppm", "-50cm 400 ppm", "-30cm 850 ppm", "-50cm 850 ppm"), 
                     values = c(19, 15, 17, 18))+
  scale_y_continuous(breaks = seq(0, 4, by =1))+
  coord_cartesian(ylim = c(0, 4))+
  geom_errorbar(limits_N1, width=0.1)+
  geom_errorbar(limits_N2, width=0.1)+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        strip.text = element_text(size = 12))

Ngen2<-summarise(group_by(cn.peatB, Treatment),
                mean_NO3=mean(NO3.mg.l), 
                SEdr1=sd(NO3.mg.l)/sqrt(length(NO3.mg.l)),
                lwr1r=mean_NO3-SEdr1,
                upr1r=mean_NO3+SEdr1,
                mean_NH3=mean(NH3.mg.l),
                SEdt1=sd(NH3.mg.l)/sqrt(length(NH3.mg.l)),
                lwr1t=mean_NH3-SEdt1,
                upr1t=mean_NH3+SEdt1)

limits_N1<-aes(ymin=lwr1r, ymax=upr1r)
limits_N2<-aes(ymin=lwr1t, ymax=upr1t)

NONH<-ggplot(Ngen2, aes(x=Treatment, shape=Treatment))+
  geom_point (aes (y=mean_NO3, colour="Nitrate"), size=5)+
  geom_point (aes (y=mean_NH3, colour="Ammonium"), size=5)+
  scale_colour_manual(name="", values=c("Nitrate"="blue", 'Ammonium'="green"))+
  labs(title = (expression(paste("", NH[4]^+""," and ", NO[3]^-""," concentration in deep soil of planted cores"))), x="", y = expression(paste ("", NH[4]^+""," and  ", NO[3]^-"","  (", mg^-1 ," ", l^-1 , ")", sep="")))+
  scale_shape_manual(name="", breaks = c("400ppm30", "400ppm50", "850ppm30", "850ppm50"), 
                     labels=c("-30cm 400 ppm", "-50cm 400 ppm", "-30cm 850 ppm", "-50cm 850 ppm"), 
                     values = c(19, 15, 17, 18))+
  scale_y_continuous(breaks = seq(0, 4, by =1))+
  coord_cartesian(ylim = c(0, 4))+
  geom_errorbar(limits_N1, width=0.1)+
  geom_errorbar(limits_N2, width=0.1)+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        strip.text = element_text(size = 12))
########################
