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

lsmeans$Yr<-as.factor(lsmeans$Yr)
C_CH4_E<-filter(lsmeans, Yr!='na' & site=='C' & var=='CH4' & type=='E')
C_CH4_BP<-filter(lsmeans, Yr!='na' & site=='C' & var=='CH4' & type=='BP')
C_CH4_BS<-filter(lsmeans, Yr!='na' & site=='C' & var=='CH4' & type=='BS')
C_CO2_E<-filter(lsmeans, Yr!='na' & site=='C' & var=='CO2' & type=='E')
C_CO2_BP<-filter(lsmeans, Yr!='na' & site=='C' & var=='CO2' & type=='BP')
C_CO2_BS<-filter(lsmeans, Yr!='na' & site=='C' & var=='CO2' & type=='BS')
C_CH4all<-filter(lsmeans, site=='C' & var=='CH4' & type.com=='n' & type=='all types')
C_CO2all<-filter(lsmeans, site=='C' & var=='CO2' & type.com=='n'& type=='all types')

C_CH4<-filter(lsmeans, site=='C' & var=='CH4' & type.com!='n')
C_CO2<-filter(lsmeans, site=='C' & var=='CO2' & type.com!='n')

U_CH4_E<-filter(lsmeans, Yr!='na' & site=='U' & var=='CH4' & type=='E')
U_CH4_BP<-filter(lsmeans, Yr!='na' & site=='U' & var=='CH4' & type=='BP')
U_CO2_E<-filter(lsmeans, Yr!='na' & site=='U' & var=='CO2' & type=='E')
U_CO2_BP<-filter(lsmeans, Yr!='na' & site=='U' & var=='CO2' & type=='BP')
U_CO2_Yr_all<-filter(lsmeans, Yr!='na' & site=='U' & var=='CO2' & type=='all types')
U_CH4_Yr_all<-filter(lsmeans, Yr!='na' & site=='U' & var=='CH4' & type=='all types')

U_CH4<-filter(lsmeans, site=='U' & var=='CH4' & type.com!='n' & type!='all types')
U_CO2<-filter(lsmeans, site=='U' & var=='CO2' & type.com!='n'& type!='all types')
U_CH4all<-filter(lsmeans, site=='U' & var=='CH4' & type.com!='n' & type=='all types')
U_CO2all<-filter(lsmeans, site=='U' & var=='CO2' & type.com!='n'& type=='all types')

C_SW<-filter(lsmeans, site=='C' & var=='SW' & type.com!='n')
C_T<-filter(lsmeans, site=='C' & var=='T' & type.com!='n')
U_SW<-filter(lsmeans, site=='U' & var=='SW' & type.com!='n')
U_T<-filter(lsmeans, site=='U' & var=='T' & type.com!='n')


sd=se1*sqrt(le)

ls1<-ggplot(C_CH4_E, aes(x=Yr, y=lsmean))+
  geom_point(stat='identity', size=4)+
  theme_bw()+
  coord_cartesian(ylim = c(0, 2))+
  scale_y_continuous() +
  geom_errorbar(aes(ymin=lsmean-se1, ymax = lsmean+se1), width=0.2, size=1)+
  labs(title = (expression(paste(""))), y = expression(paste ("LSM ", CH[4]^"1/3"," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme(axis.title.x=element_blank(),
        axis.title.y = element_text(size = 20),
        axis.text = element_text(size = 17))

ls2<-ggplot(C_CH4_BP, aes(x=Yr, y=lsmean))+
  geom_point(stat='identity', size=4)+
  theme_bw()+
  coord_cartesian(ylim = c(-0.5, 2))+
  scale_y_continuous() +
  geom_errorbar(aes(ymin=lsmean-se1, ymax = lsmean+se1), width=0.2, size=1)+
  labs(title = (expression(paste(""))), y = expression(paste ("LSM ", CH[4]^"1/3"," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme(axis.title.x=element_blank(),
        axis.title.y = element_text(size = 20),
        axis.text = element_text(size = 17))

ls3<-ggplot(C_CH4_BS, aes(x=Yr, y=lsmean))+
  geom_point(stat='identity', size=4)+
  theme_bw()+
  coord_cartesian(ylim = c(-0.5, 2))+
  scale_y_continuous() +
  geom_errorbar(aes(ymin=lsmean-se1, ymax = lsmean+se1), width=0.2, size=1)+
  labs(title = (expression(paste(""))), y = expression(paste ("LSM ", CH[4]^"1/3"," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme(axis.title.x=element_blank(),
        axis.title.y = element_text(size = 20),
        axis.text = element_text(size = 17))

ls4<-ggplot(C_CO2_E, aes(x=Yr, y=lsmean))+
  geom_point(stat='identity', size=4)+
  theme_bw()+
  coord_cartesian(ylim = c(1, 1.5))+
  scale_y_continuous() +
  geom_errorbar(aes(ymin=lsmean-se1, ymax = lsmean+se1), width=0.2, size=1)+
  labs(title = (expression(paste(""))), y = expression(paste ("LSM ", CO[2]^"1/3"," flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+  
  theme(axis.title.x=element_blank(),
        axis.title.y = element_text(size = 20),
        axis.text = element_text(size = 17))

ls5<-ggplot(C_CO2_BP, aes(x=Yr, y=lsmean))+
  geom_point(stat='identity', size=4)+
  theme_bw()+
  coord_cartesian(ylim = c(1, 1.5))+
  scale_y_continuous() +
  geom_errorbar(aes(ymin=lsmean-se1, ymax = lsmean+se1), width=0.2, size=1)+
  labs(title = (expression(paste(""))), y = expression(paste ("LSM ", CO[2]^"1/3"," flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme(axis.title.x=element_blank(),
        axis.title.y = element_text(size = 20),
        axis.text = element_text(size = 17))

ls6<-ggplot(C_CO2_BS, aes(x=Yr, y=lsmean))+
  geom_point(stat='identity', size=4)+
  theme_bw()+
  coord_cartesian(ylim = c(1, 1.5))+
  scale_y_continuous() +
  geom_errorbar(aes(ymin=lsmean-se1, ymax = lsmean+se1), width=0.2, size=1)+
  labs(title = (expression(paste(""))), y = expression(paste ("LSM ", CO[2]^"1/3"," flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme(axis.title.x=element_blank(),
        axis.title.y = element_text(size = 20),
        axis.text = element_text(size = 17))

ls7<-ggplot(C_CH4, aes(x=type.com, y=lsmean))+
  geom_point(stat='identity', size=4)+
  theme_bw()+
  coord_cartesian(ylim = c(0, 1.5))+
  scale_y_continuous() +
  geom_errorbar(aes(ymin=lsmean-se1, ymax = lsmean+se1), width=0.2, size=1)+
  labs(title = (expression(paste(""))), y = expression(paste ("LSM ", CH[4]^"1/3"," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme(axis.title.x=element_blank(),
        axis.title.y = element_text(size = 20),
        axis.text = element_text(size = 17))

ls8<-ggplot(C_CO2, aes(x=type.com, y=lsmean))+
  geom_point(stat='identity', size=4)+
  theme_bw()+
  coord_cartesian(ylim = c(1, 1.5))+
  scale_y_continuous() +
  geom_errorbar(aes(ymin=lsmean-se1, ymax = lsmean+se1), width=0.2, size=1)+
  labs(title = (expression(paste(""))), y = expression(paste ("LSM ", CO[2]^"1/3"," flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme(axis.title.x=element_blank(),
        axis.title.y = element_text(size = 20),
        axis.text = element_text(size = 17))

ls9<-ggplot(U_CH4_E, aes(x=Yr, y=lsmean))+
  geom_point(stat='identity', size=4)+
  theme_bw()+
  coord_cartesian(ylim = c(2, 4.5))+
  scale_y_continuous() +
  geom_errorbar(aes(ymin=lsmean-se1, ymax = lsmean+se1), width=0.2, size=1)+
  labs(title = (expression(paste(""))), y = expression(paste ("LSM ", CH[4]^"1/3"," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme(axis.title.x=element_blank(),
        axis.title.y = element_text(size = 20),
        axis.text = element_text(size = 17))

ls10<-ggplot(U_CH4_BP, aes(x=Yr, y=lsmean))+
  geom_point(stat='identity', size=4)+
  theme_bw()+
  coord_cartesian(ylim = c(0, 1))+
  scale_y_continuous() +
  geom_errorbar(aes(ymin=lsmean-se1, ymax = lsmean+se1), width=0.2, size=1)+
  labs(title = (expression(paste(""))), y = expression(paste ("LSM ", CH[4]^"1/3"," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme(axis.title.x=element_blank(),
        axis.title.y = element_text(size = 20),
        axis.text = element_text(size = 17))

ls11<-ggplot(U_CO2_E, aes(x=Yr, y=lsmean))+
  geom_point(stat='identity', size=4)+
  theme_bw()+
  coord_cartesian(ylim = c(0.5, 1.5))+
  scale_y_continuous() +
  geom_errorbar(aes(ymin=lsmean-se1, ymax = lsmean+se1), width=0.2, size=1)+
  labs(title = (expression(paste(""))), y = expression(paste ("LSM ", CO[2]^"1/3"," flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme(axis.title.x=element_blank(),
        axis.title.y = element_text(size = 20),
        axis.text = element_text(size = 17))

ls12<-ggplot(U_CO2_BP, aes(x=Yr, y=lsmean))+
  geom_point(stat='identity', size=4)+
  theme_bw()+
  coord_cartesian(ylim = c(0.5, 1))+
  scale_y_continuous() +
  geom_errorbar(aes(ymin=lsmean-se1, ymax = lsmean+se1), width=0.2, size=1)+
  labs(title = (expression(paste(""))), y = expression(paste ("LSM ", CO[2]^"1/3"," flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme(axis.title.x=element_blank(),
        axis.title.y = element_text(size = 20),
        axis.text = element_text(size = 17))

ls13<-ggplot(U_CH4, aes(x=type.com, y=lsmean))+
  geom_point(stat='identity', size=4)+
  theme_bw()+
  coord_cartesian(ylim = c(-0.2, 1.5))+
  scale_y_continuous() +
  geom_errorbar(aes(ymin=lsmean-se1, ymax = lsmean+se1), width=0.2, size=1)+
  labs(title = (expression(paste(""))), y = expression(paste ("LSM ", CH[4]^"1/3"," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme(axis.title.x=element_blank(),
        axis.title.y = element_text(size = 20),
        axis.text = element_text(size = 17))

ls14<-ggplot(U_CO2, aes(x=type.com, y=lsmean))+
  geom_point(stat='identity', size=4)+
  theme_bw()+
  coord_cartesian(ylim = c(0.5, 1.1))+
  scale_y_continuous() +
  geom_errorbar(aes(ymin=lsmean-se1, ymax = lsmean+se1), width=0.2, size=1)+
  labs(title = (expression(paste(""))), y = expression(paste ("LSM ", CO[2]^"1/3"," flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme(axis.title.x=element_blank(),
        axis.title.y = element_text(size = 20),
        axis.text = element_text(size = 17))

ls15<-ggplot(U_CH4all, aes(x=type.com, y=lsmean))+
  geom_point(stat='identity', size=4)+
  theme_bw()+
  coord_cartesian(ylim = c(0, 4.1))+
  scale_y_continuous() +
  geom_errorbar(aes(ymin=lsmean-se1, ymax = lsmean+se1), width=0.2, size=1)+
  labs(title = (expression(paste(""))), y = expression(paste ("LSM ", CH[4]^"1/3"," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme(axis.title.x=element_blank(),
        axis.title.y = element_text(size = 20),
        axis.text = element_text(size = 17))

ls16<-ggplot(U_CO2all, aes(x=type.com, y=lsmean))+
  geom_point(stat='identity', size=4)+
  theme_bw()+
  coord_cartesian(ylim = c(0.5, 1.2))+
  scale_y_continuous() +
  geom_errorbar(aes(ymin=lsmean-se1, ymax = lsmean+se1), width=0.2, size=1)+
  labs(title = (expression(paste(""))), y = expression(paste ("LSM ", CO[2]^"1/3"," flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme(axis.title.x=element_blank(),
        axis.title.y = element_text(size = 20),
        axis.text = element_text(size = 17))

ls17<-ggplot(C_CO2all, aes(x=Yr, y=lsmean))+
  geom_point(stat='identity', size=4)+
  theme_bw()+
  coord_cartesian(ylim = c(0.5, 1.5))+
  scale_y_continuous() +
  geom_errorbar(aes(ymin=lsmean-se1, ymax = lsmean+se1), width=0.2, size=1)+
  labs(title = (expression(paste(""))), y = expression(paste ("LSM ", CO[2]^"1/3"," flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme(axis.title.x=element_blank(),
        axis.title.y = element_text(size = 20),
        axis.text = element_text(size = 17))

ls18<-ggplot(C_CH4all, aes(x=Yr, y=lsmean))+
  geom_point(stat='identity', size=4)+
  theme_bw()+
  coord_cartesian(ylim = c(0, 1))+
  scale_y_continuous() +
  geom_errorbar(aes(ymin=lsmean-se1, ymax = lsmean+se1), width=0.2, size=1)+
  labs(title = (expression(paste(""))), y = expression(paste ("LSM ", CH[4]^"1/3"," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme(axis.title.x=element_blank(),
        axis.title.y = element_text(size = 20),
        axis.text = element_text(size = 17))

ls19<-ggplot(U_CO2_Yr_all, aes(x=Yr, y=lsmean))+
geom_point(stat='identity', size=4)+
theme_bw()+
coord_cartesian(ylim = c(0.5, 1))+
scale_y_continuous() +
geom_errorbar(aes(ymin=lsmean-se1, ymax = lsmean+se1), width=0.2, size=1)+
  labs(title = (expression(paste(""))), y = expression(paste ("LSM ", CO[2]^"1/3"," flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
theme(axis.title.x=element_blank(),
axis.title.y = element_text(size = 20),
axis.text = element_text(size = 17))

ls20<-ggplot(U_CH4_Yr_all, aes(x=Yr, y=lsmean))+
geom_point(stat='identity', size=4)+
theme_bw()+
coord_cartesian(ylim = c(1, 2))+
 scale_y_continuous() +
geom_errorbar(aes(ymin=lsmean-se1, ymax = lsmean+se1), width=0.2, size=1)+
  labs(title = (expression(paste(""))), y = expression(paste ("LSM ", CH[4]^"1/3"," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
 theme(axis.title.x=element_blank(),
      axis.title.y = element_text(size = 20),
        axis.text = element_text(size = 17))




lsc1<-ggplot(C_SW, aes(x=type.com, y=lsmean))+
  geom_point(stat='identity', size=4)+
  theme_bw()+
  coord_cartesian(ylim = c(0, 0.7))+
  scale_y_continuous("LSM (SW)") +
  geom_errorbar(aes(ymin=lsmean-se1, ymax = lsmean+se1), width=0.2, size=0.7)+
  theme(axis.title.x=element_blank(),
        axis.title.y = element_text(size = 20),
        axis.text = element_text(size = 18))

lsc2<-ggplot(C_T, aes(x=type.com, y=lsmean))+
  geom_point(stat='identity', size=4)+
  theme_bw()+
  coord_cartesian(ylim = c(0, 17))+
  scale_y_continuous("LSM (T)") +
  geom_errorbar(aes(ymin=lsmean-se1, ymax = lsmean+se1), width=0.2, size=0.7)+
  theme(axis.title.x=element_blank(),
        axis.title.y = element_text(size = 20),
        axis.text = element_text(size = 18))

lsu1<-ggplot(U_SW, aes(x=type.com, y=lsmean))+
  geom_point(stat='identity', size=4)+
  theme_bw()+
  coord_cartesian(ylim = c(0, 0.7))+
  scale_y_continuous("LSM (SW)") +
  geom_errorbar(aes(ymin=lsmean-se1, ymax = lsmean+se1), width=0.2, size=0.7)+
  theme(axis.title.x=element_blank(),
        axis.title.y = element_text(size = 20),
        axis.text = element_text(size = 18))

lsu2<-ggplot(U_T, aes(x=type.com, y=lsmean))+
  geom_point(stat='identity', size=4)+
  theme_bw()+
  coord_cartesian(ylim = c(0, 18))+
  scale_y_continuous("LSM (T)") +
  geom_errorbar(aes(ymin=lsmean-se1, ymax = lsmean+se1), width=0.2, size=0.7)+
  theme(axis.title.x=element_blank(),
        axis.title.y = element_text(size = 20),
        axis.text = element_text(size = 18))

grid.arrange(ls7, ls8, ls17, ls18, ls15, ls16, ls19, ls20, ncol=4)
grid.arrange(ls4, ls6, ls5, ls1, ls3, ls2, ncol=3)
grid.arrange(ls11, ls12, ls14, ls10, ls9, ls13, ncol=3)
grid.arrange(lsc1, lsc2, lsu1, lsu2, ncol=2)


grid.arrange(ls7, ls8, ls17, ls18, ncol=2)
grid.arrange(ls15, ls16, ls19, ls20, ncol=2)
