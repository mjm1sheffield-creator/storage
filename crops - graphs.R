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

rm(list=ls())
str(co2)

co2$day <- as.numeric(co2$day)
co2$day <- as.factor(co2$day)

co2_maize<-filter(co2, Crop=="Maize")
co2_mungbean<-filter(co2, Crop=="Mungbean")

CO2_T<-ggplot(co2, aes(x=ST, y=CO2, colour=Treatment))+
  facet_wrap(~Crop)+
  stat_smooth(method=lm, alpha=0.3)+
  scale_y_continuous(breaks = seq(0, 120, by =20))+
  coord_cartesian(ylim = c(0, 120), xlim = c(14, 26))+
  scale_x_continuous(breaks = seq(14, 26, by =2))+
scale_colour_manual(name="Treatment", values=c('BR'="chartreuse4", 'BR+SM'="gold1", 'SM'="tomato"))+
  labs(x="Soil temperature (°C)", y = expression(paste ("", CO[2]," flux (mg ", CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
    stat_summary(fun.y=mean, geom="point", shape=20, size=4)+
    theme_bw()+
    theme(axis.text = element_text(size = 13),
          axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
          axis.title = element_text(size = 15),
          legend.text = element_text(size = 11),
          legend.title = element_text(size = 13),
          strip.text = element_text(size = 12),
          legend.justification = c(1,0),
          strip.background = element_rect(colour="black", fill="white"))

CO2_1<-CO2_T+ guides(colour = guide_legend(override.aes = list(fill=NA)))
print(CO2_1)


CO2_WFPS<-ggplot(co2, aes(x=WFPS, y=CO2, colour=Treatment))+
  facet_wrap(~Crop)+
  stat_smooth(method=lm, alpha=0.3)+
  scale_y_continuous(breaks = seq(0, 120, by =20))+
  coord_cartesian(ylim = c(0, 120), xlim = c(0, 70))+
  scale_x_continuous(breaks = seq(0, 70, by =10))+
  scale_colour_manual(name="Treatment", values=c('BR'="chartreuse4", 'BR+SM'="gold1", 'SM'="tomato"))+
  labs(x="Water-filled pore space (%)", y = expression(paste ("", CO[2]," flux (mg ", CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  stat_summary(fun.y=mean, geom="point", shape=20, size=4)+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 12),
        legend.justification = c(1,0),
        strip.background = element_rect(colour="black", fill="white"))

CO2_2<-CO2_WFPS+ guides(colour = guide_legend(override.aes = list(fill=NA)))
print(CO2_2)

CO2_MBC<-ggplot(co2, aes(x=MBC, y=CO2, colour=Treatment))+
  facet_wrap(~Crop)+
  stat_smooth(method=lm, alpha=0.3)+
  scale_y_continuous(breaks = seq(0, 120, by =20))+
  coord_cartesian(ylim = c(0, 120), xlim = c(0, 1000))+
  scale_x_continuous(breaks = seq(0, 1000, by =200))+
  scale_colour_manual(name="Treatment", values=c('BR'="chartreuse4", 'BR+SM'="gold1", 'SM'="tomato"))+
  labs(x=(expression(paste("Microbial Biomass Carbon (", mg," ", Kg^-1, ")", sep=""))), y = expression(paste ("", CO[2]," flux (mg ", CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  stat_summary(fun.y=mean, geom="point", shape=20, size=4)+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 12),
        legend.justification = c(1,0),
        strip.background = element_rect(colour="black", fill="white"))

CO2_3<-CO2_MBC+ guides(colour = guide_legend(override.aes = list(fill=NA)))
print(CO2_3)

N2O_T<-ggplot(co2, aes(x=ST, y=N2O, colour=Treatment))+
  facet_wrap(~Crop)+
  stat_smooth(method=lm, alpha=0.3)+
  scale_y_continuous(breaks = seq(0, 90, by =10))+
  coord_cartesian(ylim = c(0, 90), xlim = c(14, 26))+
  scale_x_continuous(breaks = seq(14, 26, by =2))+
  scale_colour_manual(name="Treatment", values=c('BR'="chartreuse4", 'BR+SM'="gold1", 'SM'="tomato"))+
  labs(x="Soil temperature (°C)", y = expression(paste ("", N[2],O," flux (µg ", N[2],O," ", m^-2 ," ", hr^-1, ")", sep="")))+
  stat_summary(fun.y=mean, geom="point", shape=20, size=4)+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 12),
        legend.justification = c(1,0),
        strip.background = element_rect(colour="black", fill="white"))

N2O_1<-N2O_T+ guides(colour = guide_legend(override.aes = list(fill=NA)))
print(N2O_1)


N2O_WFPS<-ggplot(co2, aes(x=WFPS, y=N2O, colour=Treatment))+
  facet_wrap(~Crop)+
  stat_smooth(method=lm, alpha=0.3)+
  scale_y_continuous(breaks = seq(0, 90, by =10))+
  coord_cartesian(ylim = c(0, 90), xlim = c(0, 70))+
  scale_x_continuous(breaks = seq(0, 70, by =10))+
  scale_colour_manual(name="Treatment", values=c('BR'="chartreuse4", 'BR+SM'="gold1", 'SM'="tomato"))+
  labs(x="Water-filled pore space (%)", y = expression(paste ("", N[2],O," flux (µg ", N[2],O," ", m^-2 ," ", hr^-1, ")", sep="")))+
  stat_summary(fun.y=mean, geom="point", shape=20, size=4)+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 12),
        legend.justification = c(1,0),
        strip.background = element_rect(colour="black", fill="white"))

N2O_2<-N2O_WFPS+ guides(colour = guide_legend(override.aes = list(fill=NA)))
print(N2O_2)

N2O_MBC<-ggplot(co2, aes(x=MBC, y=N2O, colour=Treatment))+
  facet_wrap(~Crop)+
  stat_smooth(method=lm, alpha=0.3)+
  scale_y_continuous(breaks = seq(0, 90, by =10))+
  coord_cartesian(ylim = c(0, 90), xlim = c(0, 1000))+
  scale_x_continuous(breaks = seq(0, 1000, by =200))+
  scale_colour_manual(name="Treatment", values=c('BR'="chartreuse4", 'BR+SM'="gold1", 'SM'="tomato"))+
  labs(x=(expression(paste("Microbial Biomass Carbon (", mg," ", Kg^-1, ")", sep=""))), y = expression(paste ("", N[2],O," flux (µg ", N[2],O," ", m^-2 ," ", hr^-1, ")", sep="")))+
  stat_summary(fun.y=mean, geom="point", shape=20, size=4)+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 12),
        legend.justification = c(1,0),
        strip.background = element_rect(colour="black", fill="white"))

N2O_3<-N2O_MBC+ guides(colour = guide_legend(override.aes = list(fill=NA)))
print(N2O_3)

grid.arrange(CO2_1, CO2_2, CO2_3, N2O_1, N2O_2, N2O_3, ncol=2)
grid.arrange(CO2_1, CO2_2, CO2_3, ncol=1)

CO2_box<-ggplot(co2, aes(x=Treatment, y=CO2, colour=Treatment))+
  geom_boxplot(width=.4, outlier.size=1, lwd=1)+
  facet_wrap(~Crop)+
  scale_colour_manual(name="Treatment", values=c('BR'="chartreuse4", 'BR+SM'="gold1", 'SM'="tomato"))+
  scale_x_discrete(name="", breaks=c("BR", "BR+SM", "SM"), labels=c("BR", "BR+SM", "SM"))+
  scale_y_continuous(breaks = seq(0, 120, by =10))+
  coord_cartesian(ylim = c(0, 120))+
  labs(y = expression(paste ("", CO[2]," flux (mg ", CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=2, stroke=1.5)+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_blank(),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 12),
        legend.justification = c(1,0),
        strip.background = element_rect(colour="black", fill="white"))

CO2_box1 <- CO2_box + guides(colour = guide_legend(override.aes = list(shape = NA)))
print(CO2_box1)

N2O_box<-ggplot(co2, aes(x=Treatment, y=N2O, colour=Treatment))+
  geom_boxplot(width=.4, outlier.size=1, lwd=1)+
  facet_wrap(~Crop)+
  scale_colour_manual(name="Treatment", values=c('BR'="chartreuse4", 'BR+SM'="gold1", 'SM'="tomato"))+
  scale_x_discrete(name="", breaks=c("BR", "BR+SM", "SM"), labels=c("BR", "BR+SM", "SM"))+
  scale_y_continuous(breaks = seq(0, 90, by =10))+
  coord_cartesian(ylim = c(0, 90))+
  labs(y = expression(paste ("", N[2],O," flux (µg ", N[2],O," ", m^-2 ," ", hr^-1, ")", sep="")))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=2, stroke=1.5)+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_blank(),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 12),
        legend.justification = c(1,0),
        strip.background = element_rect(colour="black", fill="white"))

N2O_box1 <- N2O_box + guides(colour = guide_legend(override.aes = list(shape = NA)))
print(N2O_box1)

CO2_box_day<-ggplot(co2, aes(x=day, y=CO2, colour=Treatment))+
  geom_boxplot(width=0.8, outlier.size=1, lwd=1, fatten=0.8,
               position=position_dodge(width=1))+
  facet_wrap(~Crop)+
  scale_colour_manual(name="Treatment", values=c('BR'="chartreuse4", 'BR+SM'="gold1", 'SM'="tomato"))+
  scale_x_discrete(name="Day of experiment", breaks=c("26", "36", "46", "53", "63", "76"), 
                   labels=c("26", "36", "46", "53", "63", "76"))+
  scale_y_continuous(breaks = seq(0, 120, by =10))+
  coord_cartesian(ylim = c(0, 120))+
  labs(y = expression(paste ("", CO[2]," flux (mg ", CO[2]," ", m^-2 ," ", hr^-1, ")", sep="")))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=1.5, stroke=1.5, 
            position=position_dodge(width=1))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 12),
        legend.justification = c(1,0),
        strip.background = element_rect(colour="black", fill="white"))

CO2_box_day1 <- CO2_box_day + guides(colour = guide_legend(override.aes = list(shape = NA)))
print(CO2_box_day1)

N2O_box_day<-ggplot(co2, aes(x=day, y=N2O, colour=Treatment))+
  geom_boxplot(width=0.8, outlier.size=1, lwd=1, fatten=0.8,
               position=position_dodge(width=1))+
  facet_wrap(~Crop)+
  scale_colour_manual(name="Treatment", values=c('BR'="chartreuse4", 'BR+SM'="gold1", 'SM'="tomato"))+
  scale_x_discrete(name="Day of experiment", breaks=c("26", "36", "46", "53", "63", "76"), 
                   labels=c("26", "36", "46", "53", "63", "76"))+
  scale_y_continuous(breaks = seq(0, 90, by =10))+
  coord_cartesian(ylim = c(0, 90))+
  labs(y = expression(paste ("", N[2],O," flux (µg ", N[2],O," ", m^-2 ," ", hr^-1, ")", sep="")))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=1.5, stroke=1.5, 
               position=position_dodge(width=1))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 12),
        legend.justification = c(1,0),
        strip.background = element_rect(colour="black", fill="white"))

N2O_box_day1 <- N2O_box_day + guides(colour = guide_legend(override.aes = list(shape = NA)))
print(N2O_box_day1)

############

N2O_MBC<-ggplot(co2, aes(x=MBC, y=N2O, colour=Treatment))+
  stat_smooth(method=lm, alpha=0.3)+
  scale_y_continuous(breaks = seq(0, 90, by =10))+
  coord_cartesian(ylim = c(0, 90), xlim = c(0, 1000))+
  scale_x_continuous(breaks = seq(0, 1000, by =200))+
  scale_colour_manual(name="Treatment", values=c('BR'="chartreuse4", 'BR+SM'="gold1", 'SM'="tomato"))+
  labs(x=(expression(paste("Microbial Biomass Carbon (", mg," ", Kg^-1, ")", sep=""))), y = expression(paste ("", N[2],O," flux (µg ", N[2],O," ", m^-2 ," ", hr^-1, ")", sep="")))+
  stat_summary(fun.y=mean, geom="point", shape=20, size=4)+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 12),
        legend.justification = c(1,0),
        strip.background = element_rect(colour="black", fill="white"))

N2O_3<-N2O_MBC+ guides(colour = guide_legend(override.aes = list(fill=NA)))
print(N2O_3)

N2O_T<-ggplot(co2, aes(x=ST, y=N2O, colour=Treatment))+
  stat_smooth(method=lm, alpha=0.3)+
  scale_y_continuous(breaks = seq(0, 90, by =10))+
  coord_cartesian(ylim = c(0, 90), xlim = c(14, 26))+
  scale_x_continuous(breaks = seq(14, 26, by =2))+
  scale_colour_manual(name="Treatment", values=c('BR'="chartreuse4", 'BR+SM'="gold1", 'SM'="tomato"))+
  labs(x="Soil temperature (°C)", y = expression(paste ("", N[2],O," flux (µg ", N[2],O," ", m^-2 ," ", hr^-1, ")", sep="")))+
  stat_summary(fun.y=mean, geom="point", shape=20, size=4)+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 12),
        legend.justification = c(1,0),
        strip.background = element_rect(colour="black", fill="white"))

N2O_1<-N2O_T+ guides(colour = guide_legend(override.aes = list(fill=NA)))
print(N2O_1)

################


total_n2o<-filter(total, flux=="N2O")
total_co2<-filter(total, flux=="CO2")

N2O_bar<-ggplot(total_n2o, aes(x=Treatment, y=ha, fill=Treatment))+
  geom_col(width=.4)+
  facet_wrap(~Crop)+
  scale_fill_manual(name="Treatment", values=c('BR'="chartreuse4", 'BR+SM'="gold1", 'SM'="tomato"))+
  scale_x_discrete(name="", breaks=c("BR", "BR+SM", "SM"), labels=c("BR", "BR+SM", "SM"))+
  scale_y_continuous(breaks = seq(0, 40, by =5))+
  coord_cartesian(ylim = c(0, 40))+
  labs(y = expression(paste ("Total ", N[2],O," flux (µg ", N[2],O," ", ha^-1 ," )", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_blank(),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 12),
        legend.justification = c(1,0),
        strip.background = element_rect(colour="black", fill="white"))

print(N2O_bar)


CO2_bar<-ggplot(total_co2, aes(x=Treatment, y=ha, fill=Treatment))+
  geom_col(width=.4)+
  facet_wrap(~Crop)+
  scale_fill_manual(name="Treatment", values=c('BR'="chartreuse4", 'BR+SM'="gold1", 'SM'="tomato"))+
  scale_x_discrete(name="", breaks=c("BR", "BR+SM", "SM"), labels=c("BR", "BR+SM", "SM"))+
  scale_y_continuous(breaks = seq(0, 40, by =5))+
  coord_cartesian(ylim = c(0, 40))+
  labs(y = expression(paste ("Total ", CO[2]," flux (mg ", CO[2]," ", ha^-1 ," )", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_blank(),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 12),
        legend.justification = c(1,0),
        strip.background = element_rect(colour="black", fill="white"))

print(CO2_bar)
