rm(list=ls())

lm(formula = CUBtrCO2~ Type + d.$Av_T + Yr)

lm(formula = CUBtrCO2~ Type + d.$Av_SW + Yr)


lm(formula = CUBtrCH4~ Type + d.$Av_T + Yr)

lm(formula = CUBtrCH4~ Type + d.$Av_SW + Yr)



Code to run the above models:
  
  # Used datasets
  
  # ’meansCavemount_.csv’ and
  
  # ‘meansUmmeras_.csv’
  
  
  
  # d. … is the dataframe;  it contains the data for Av_CO2, Av_CH4, Type, Av_SW, Av_T and Yr
  


# omit NAs

meansCavemount <- meansCavemount[ , -20:-22]
meansUmmeras <- meansUmmeras[ , -20:-22]
d.<- na.omit(d.)
d.<- na.omit(meansCavemount)
d.<- na.omit(meansUmmeras)

d.<-filter(d., Year!="2023")
d.<-filter(d., soil_T_mean!="NA" & moi_mean!="NA")
d.<-filter(d., soil_T_mean!="NA" & moi_mean!="NA" & CO2_mean!="NA" & CH4_mean!="NA")

d.<-filter(meansUmmeras, Year!="2023")
d.<-filter(d., soil_T_mean!="NA" & moi_mean!="NA")
d.<-filter(d., soil_T_mean!="NA" & moi_mean!="NA" & CO2_mean!="NA" & CH4_mean!="NA")

d.<-filter(d., type=="Eriophorum")
d.<-filter(d., type!="Eriophorum")
d.<-filter(d., type=="Bare Peat Dry")
d.<-filter(d., type=="Bare Peat Middle")
d.<-filter(d., type=="Bare Peat Wet")

d.<-filter(d., type=="Bare Peat")
d.<-filter(d., type=="Birch Scrub")


# transforming data to the power of 1/3

CUBtrCO2<- (d.$CO2_mean)^(1/3)

CUBtrCH4<- (d.$CH4_mean)^(1/3)

CUBtrCH4<-sign(d.$CH4_mean) * abs(d.$CH4_mean)^(1/3)


# ‘Type’ and ‘Yr’are ‘factor’ varibles

Type <- as.factor(d.$type)
Yr <- as.factor(d.$Year)

Ch4model<-lm(CUBtrCH4  ~  Type +d.$soil_T_mean + d.$moi_mean + Yr)
Ch4model2<-lm(CUBtrCH4  ~  d.$soil_T_mean + d.$moi_mean + Yr)
Ch4model0<-lm(CUBtrCH4  ~  d.$soil_T_mean + d.$moi_mean )

Co2model<-lm(CUBtrCO2  ~  Type +d.$soil_T_mean + d.$moi_mean + Yr)
Co2model2<-lm(CUBtrCO2  ~  d.$soil_T_mean + d.$moi_mean + Yr)
Co2model0<-lm(CUBtrCO2  ~  d.$soil_T_mean + d.$moi_mean )

par(mfrow=c(2,2))
plot(Ch4model2)

summary(Co2model2)
Anova(Co2model2, type=2)
Anova(Co2model2, type=3)

summary(Ch4model2)
Anova(Ch4model2, type=2)
Anova(Ch4model2, type=3)

print(lsmeans(Ch4model, list(pairwise~ Type)), adjust = c("tukey"))
print(lsmeans(Ch4model, list(pairwise~ Yr)), adjust = c("tukey"))
print(lsmeans(Co2model, list(pairwise~ Type)), adjust = c("tukey"))
print(lsmeans(Co2model, list(pairwise~ Yr)), adjust = c("tukey"))

For CH4
  
  # model including only Moisture
  
Ch4model.1<-lm(CUBtrCH4  ~  Type +d.$moi_mean +Yr)

summary(Ch4model.1)

# model including only Temperature
Ch4model.2<-lm(CUBtrCH4  ~  Type +d.$soil_T_mean +Yr)

summary(Ch4model.2)



For CO2
  
  # model including only Moisture
  
  Co2model.1<-lm(CUBtrCO2  ~  Type +d.$moi_mean +Yr)

summary(Co2model.1)

# model including only Temperature

Co2model.2<-lm(CUBtrCO2  ~  Type +d.$soil_T_mean +Yr)

summary(Co2model.2)

#################
d.<-filter(meansCavemount, soil_T_mean!="NA" & moi_mean!="NA" & Year!="2023")
d.<-filter(meansUmmeras, soil_T_mean!="NA" & moi_mean!="NA" & Year!="2023")

Type <- as.factor(d.$type)

Temp.mod1<-lm(d.$soil_T_mean ~  Type +d.$moi_mean)
Temp.mod2<-lm(d.$moi_mean ~  Type +d.$soil_T_mean)
Temp.mod3<-lm(d.$soil_T_mean ~  Type)
Temp.mod4<-lm(d.$moi_mean ~  Type)

summary(Temp.mod1)
Anova(Temp.mod4, type=2)

par(mfrow=c(2,2))
plot(Temp.mod1)

print(lsmeans(Temp.mod4, list(pairwise~ Type)), adjust = c("tukey"))
print(moi<-summarise(group_by(d., type), meanmoi=mean(moi_mean)))


d.<-filter(meansCavemount, WT_mean!="NA" & Year!="2023")
d.<-filter(meansUmmeras, WT_mean!="NA" & Year!="2023")
Type <- as.factor(d.$type)
print(moi<-summarise(group_by(d., type), meanWT=mean(WT_mean)))

summarise(group_by(WT, site), mean_WT=mean(WT..cm.))

################## CO2
 
d.$type <- fct_collapse(d.$type, 'Bare Peat (all)' = c('Bare Peat Dry', 'Bare Peat Middle', 'Bare Peat Wet'), 
                        Eriophorum = c('Eriophorum'))

moi_C_lm<-ggplot(d., aes(x=moi_mean, y=CO2_mean, colour=Type))+
  geom_point(size=1)+
  stat_smooth(method=lm)+
  scale_y_continuous(breaks = seq(0, 10, by =1))+
  coord_cartesian(ylim = c(0, 10))+
  scale_x_continuous(breaks = seq(0, 0.9, by =0.1))+
  labs(title = (expression(paste("Cavemount moi"))), x="SWC", y = "CO2")+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

moi_C_loess<-ggplot(d., aes(x=moi_mean, y=CO2_mean, colour=Type))+
  geom_point(size=1)+
  stat_smooth(method=loess)+
  scale_y_continuous(breaks = seq(0, 10, by =1))+
  coord_cartesian(ylim = c(0, 10))+
  scale_x_continuous(breaks = seq(0, 0.9, by =0.1))+
  labs(title = (expression(paste("Cavemount moi"))), x="SWC", y = "CO2")+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

T_C_lm<-ggplot(d., aes(x=soil_T_mean, y=CO2_mean, colour=Type))+
  geom_point(size=1)+
  stat_smooth(method=lm)+
  scale_y_continuous(breaks = seq(0, 10, by =1))+
  coord_cartesian(ylim = c(0, 10))+
  scale_x_continuous(breaks = seq(0, 30, by =5))+
  labs(title = (expression(paste("Cavemount T"))), x="T", y = "CO2")+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

CO2_C<-ggplot(d., aes(x=Type, y=CO2_mean, colour=Type))+
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

co2_na <- CO2_C + guides(colour = guide_legend(override.aes = list(shape = NA)))
print(co2_na)

moi_U_lm2<-ggplot(d., aes(x=moi_mean, y=CO2_mean, colour=type))+
  geom_point(size=1)+
  stat_smooth(method=lm)+
  scale_y_continuous(breaks = seq(1, 4, by =1))+
  coord_cartesian(ylim = c(0, 4))+
  scale_x_continuous(breaks = seq(0, 0.9, by =0.1))+
  labs(title = (expression(paste("Ummeras moi"))), x="SWC", y = "CO2")+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

moi_U_loess2<-ggplot(d., aes(x=moi_mean, y=CO2_mean, colour=type))+
  geom_point(size=1)+
  stat_smooth(method=loess)+
  scale_y_continuous(breaks = seq(0, 4, by =1))+
  coord_cartesian(ylim = c(0, 4))+
  scale_x_continuous(breaks = seq(0, 0.9, by =0.1))+
  labs(title = (expression(paste("Ummeras moi"))), x="SWC", y = "CO2")+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

T_U_lm2<-ggplot(d., aes(x=soil_T_mean, y=CO2_mean, colour=type))+
  geom_point(size=1)+
  stat_smooth(method=lm)+
  scale_y_continuous(breaks = seq(0, 4, by =1))+
  coord_cartesian(ylim = c(0, 4))+
  scale_x_continuous(breaks = seq(0, 40, by =5))+
  labs(title = (expression(paste("Ummeras T"))), x="T", y = "CO2")+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

CO2_U<-ggplot(d., aes(x=type, y=CO2_mean, colour=type))+
  geom_boxplot(width=.4, outlier.size=1, lwd=1)+
  scale_colour_manual(name="Type", values=c('Bare Peat Dry'="brown4", 'Bare Peat Middle'="darkgoldenrod3", 'Bare Peat Wet'="royalblue", 'Eriophorum' = 'green2'))+
  scale_x_discrete(name="", breaks=c("Bare Peat Dry", "Bare Peat Middle", "Bare Peat Wet", "Eriophorum"), labels=c("Bare Peat \n Dry", "Bare Peat \n Middle", "Bare Peat \n Wet", "Eriophorum"))+
  scale_y_continuous(breaks = seq(0, 4, by =1))+
  coord_cartesian(ylim = c(0, 4))+
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

co2_na2 <- CO2_U + guides(colour = guide_legend(override.aes = list(shape = NA)))
print(co2_na2)

##################### CH4

moi_C_lm<-ggplot(d., aes(x=moi_mean, y=CH4_mean, colour=type))+
  geom_point(size=1)+
  stat_smooth(method=lm)+
  scale_y_continuous(breaks = seq(-1, 13, by =1))+
  coord_cartesian(ylim = c(-1, 13))+
  scale_x_continuous(breaks = seq(0, 0.9, by =0.1))+
  labs(title = (expression(paste("Cavemount moi"))), x="SWC", y = "CH4")+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

moi_C_lm2<-ggplot(d., aes(x=moi_mean, y=CH4_mean, colour=type))+
  geom_point(size=1)+
  stat_smooth(method=lm)+
  scale_y_continuous(breaks = seq(-1, 1, by =0.1))+
  coord_cartesian(ylim = c(-1, 1))+
  scale_x_continuous(breaks = seq(0, 0.9, by =0.1))+
  labs(title = (expression(paste("Cavemount moi"))), x="SWC", y = "CH4")+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

moi_C_loess<-ggplot(d., aes(x=moi_mean, y=CH4_mean, colour=type))+
  geom_point(size=1)+
  stat_smooth(method=loess)+
  scale_y_continuous(breaks = seq(-1, 13, by =1))+
  coord_cartesian(ylim = c(-1, 13))+
  scale_x_continuous(breaks = seq(0, 0.9, by =0.1))+
  labs(title = (expression(paste("Cavemount moi"))), x="SWC", y = "CH4")+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

moi_C_loess2<-ggplot(d., aes(x=moi_mean, y=CH4_mean, colour=type))+
  geom_point(size=1)+
  stat_smooth(method=loess)+
  scale_y_continuous(breaks = seq(-1, 1, by =0.1))+
  coord_cartesian(ylim = c(-1, 1))+
  scale_x_continuous(breaks = seq(0, 0.9, by =0.1))+
  labs(title = (expression(paste("Cavemount moi"))), x="SWC", y = "CH4")+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

T_C_lm<-ggplot(d., aes(x=soil_T_mean, y=CH4_mean))+
  geom_point(size=2)+
  stat_smooth(method=lm)+
  scale_y_continuous(breaks = seq(-1, 13, by =1))+
  coord_cartesian(ylim = c(-1, 13))+
  scale_x_continuous(breaks = seq(0, 30, by =5))+
  labs(title = (expression(paste(""))), x="Soil temperature (°C)", y = expression(paste ("", CH[4]^"1/3"," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

T_C_lm <- T_C_lm + 
  guides(colour = guide_legend(override.aes = list(size = 2, shape = NA, fill=NA)))
print(T_C_lm)

T_C_lm2<-ggplot(d., aes(x=soil_T_mean, y=CH4_mean, colour=type))+
  geom_point(size=1)+
  stat_smooth(method=lm)+
  scale_y_continuous(breaks = seq(-1, 1, by =0.1))+
  coord_cartesian(ylim = c(-1, 1))+
  scale_x_continuous(breaks = seq(0, 30, by =5))+
  labs(title = (expression(paste("Cavemount T"))), x="T", y = "CH4")+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

T_C_loess<-ggplot(d., aes(x=soil_T_mean, y=CH4_mean, colour=type))+
  geom_point(size=1)+
  stat_smooth(method=loess)+
  scale_y_continuous(breaks = seq(-1, 13, by =1))+
  coord_cartesian(ylim = c(-1, 13))+
  scale_x_continuous(breaks = seq(0, 30, by =5))+
  labs(title = (expression(paste("Cavemount T"))), x="T", y = "CH4")+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

T_C_loess2<-ggplot(d., aes(x=soil_T_mean, y=CH4_mean, colour=type))+
  geom_point(size=1)+
  stat_smooth(method=loess)+
  scale_y_continuous(breaks = seq(-1, 1, by =0.1))+
  coord_cartesian(ylim = c(-1, 1))+
  scale_x_continuous(breaks = seq(0, 30, by =5))+
  labs(title = (expression(paste("Cavemount T"))), x="T", y = "CH4")+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

CH4_C<-ggplot(d., aes(x=type, y=CH4_mean, colour=type))+
  geom_boxplot(width=.4, outlier.size=1, lwd=1)+
  scale_colour_manual(name="type", values=c('Bare Peat'="black", 'Birch Scrub'="chocolate", 'Eriophorum'="green2"))+
  scale_x_discrete(name="", breaks=c("Bare Peat", "Birch Scrub", "Eriophorum"), labels=c("Bare Peat", "Birch Scrub", "Eriophorum"))+
  scale_y_continuous(breaks = seq(-1, 13, by =1))+
  coord_cartesian(ylim = c(-1, 13))+
  labs(title = (expression(paste("CH4 flux from Cavemount"))), x="plot", y = expression(paste ("", CO[2]," flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3, stroke=1.5)+
  theme_bw()+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_blank(),
        strip.text = element_text(size = 10),
        legend.key.size = unit(1, "cm"))

CH4_na <- CH4_C + guides(colour = guide_legend(override.aes = list(shape = NA)))
print(CH4_na)

moi_U_lm2<-ggplot(d., aes(x=moi_mean, y=CH4_mean, colour=type))+
  geom_point(size=1)+
  stat_smooth(method=lm)+
  scale_y_continuous(breaks = seq(-1, 13, by =1))+
  coord_cartesian(ylim = c(-1, 13))+
  scale_x_continuous(breaks = seq(0, 0.9, by =0.1))+
  labs(title = (expression(paste("Ummeras moi"))), x="SWC", y = "CH4")+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

moi_U_lm3<-ggplot(d., aes(x=moi_mean, y=CH4_mean, colour=type))+
  geom_point(size=1)+
  stat_smooth(method=lm)+
  scale_y_continuous(breaks = seq(0, 150, by =10))+
  coord_cartesian(ylim = c(0, 150))+
  scale_x_continuous(breaks = seq(0, 0.9, by =0.1))+
  labs(title = (expression(paste("Ummeras moi"))), x="SWC", y = "CH4")+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

moi_U_loess2<-ggplot(d., aes(x=moi_mean, y=CH4_mean, colour=type))+
  geom_point(size=1)+
  stat_smooth(method=loess)+
  scale_y_continuous(breaks = seq(0, 150, by =10))+
  coord_cartesian(ylim = c(0, 150))+
  scale_x_continuous(breaks = seq(0, 0.9, by =0.1))+
  labs(title = (expression(paste("Ummeras moi"))), x="SWC", y = "CH4")+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

T_U_lm2<-ggplot(d., aes(x=soil_T_mean, y=CH4_mean, colour=type))+
  geom_point(size=1)+
  stat_smooth(method=lm)+
  scale_y_continuous(breaks = seq(-1, 20, by =1))+
  coord_cartesian(ylim = c(-1, 20))+
  scale_x_continuous(breaks = seq(0, 40, by =5))+
  labs(title = (expression(paste("Ummeras T"))), x="T", y = "CH4")+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

T_U_loess<-ggplot(d., aes(x=soil_T_mean, y=CH4_mean, colour=type))+
  geom_point(size=1)+
  stat_smooth(method=loess)+
  scale_y_continuous(breaks = seq(0, 150, by =10))+
  coord_cartesian(ylim = c(0, 150))+
  scale_x_continuous(breaks = seq(0, 40, by =5))+
  labs(title = (expression(paste("Ummeras T"))), x="T", y = "CH4")+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

CH4_U<-ggplot(d., aes(x=type, y=CH4_mean, colour=type))+
  geom_boxplot(width=.4, outlier.size=1, lwd=1)+
  scale_colour_manual(name="Type", values=c('Bare Peat Dry'="brown4", 'Bare Peat Middle'="darkgoldenrod3", 'Bare Peat Wet'="royalblue", 'Eriophorum' = 'green2'))+
  scale_x_discrete(name="", breaks=c("Bare Peat Dry", "Bare Peat Middle", "Bare Peat Wet", "Eriophorum"), labels=c("Bare Peat \n Dry", "Bare Peat \n Middle", "Bare Peat \n Wet", "Eriophorum"))+
  scale_y_continuous(breaks = seq(-1, 15, by =1))+
  coord_cartesian(ylim = c(-1, 15))+
  labs(title = (expression(paste("CH4 flux from Ummeras"))), x="plot", y = expression(paste ("", CO[2]," flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3, stroke=1.5)+
  theme_bw()+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_blank(),
        strip.text = element_text(size = 10),
        legend.key.size = unit(1, "cm"))

CH4_na2 <- CH4_U + guides(colour = guide_legend(override.aes = list(shape = NA)))
print(CH4_na2)

CH4_U2<-ggplot(d., aes(x=type, y=CH4_mean, colour=type))+
  geom_boxplot(width=.4, outlier.size=1, lwd=1)+
  scale_colour_manual(name="Type", values=c('Bare Peat Dry'="brown4", 'Bare Peat Middle'="darkgoldenrod3", 'Bare Peat Wet'="royalblue", 'Eriophorum' = 'green2'))+
  scale_x_discrete(name="", breaks=c("Bare Peat Dry", "Bare Peat Middle", "Bare Peat Wet", "Eriophorum"), labels=c("Bare Peat \n Dry", "Bare Peat \n Middle", "Bare Peat \n Wet", "Eriophorum"))+
  scale_y_continuous(breaks = seq(0, 180, by =20))+
  coord_cartesian(ylim = c(0, 180))+
  labs(title = (expression(paste("CH4 flux from Ummeras"))), x="plot", y = expression(paste ("", CO[2]," flux (µmol ",-CO[2]," ", m^-2 ," ", s^-1, ")", sep="")))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3, stroke=1.5)+
  theme_bw()+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_blank(),
        strip.text = element_text(size = 10),
        legend.key.size = unit(1, "cm"))

CH4_na3 <- CH4_U2 + guides(colour = guide_legend(override.aes = list(shape = NA)))
print(CH4_na3)

####################### year C
d.$Year <- as.factor(d.$Year)

moi_C_lm<-ggplot(d., aes(x=moi_mean, y=CH4_mean, colour=type))+
  facet_wrap(~Year)+
  geom_point(size=1)+
  stat_smooth(method=lm)+
  scale_y_continuous(breaks = seq(-1, 13, by =1))+
  coord_cartesian(ylim = c(-1, 13))+
  scale_x_continuous(breaks = seq(0, 0.9, by =0.1))+
  labs(title = (expression(paste("Cavemount moi"))), x="SWC", y = "CH4")+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

moi_C_lm2<-ggplot(d., aes(x=moi_mean, y=CH4_mean, colour=type))+
  facet_wrap(~Year)+
  geom_point(size=1)+
  stat_smooth(method=lm)+
  scale_y_continuous(breaks = seq(-1, 1, by =0.1))+
  coord_cartesian(ylim = c(-1, 1))+
  scale_x_continuous(breaks = seq(0, 0.9, by =0.1))+
  labs(title = (expression(paste("Cavemount moi"))), x="SWC", y = "CH4")+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

moi_C_loess<-ggplot(d., aes(x=moi_mean, y=CH4_mean, colour=type))+
  facet_wrap(~Year)+
  geom_point(size=1)+
  stat_smooth(method=loess)+
  scale_y_continuous(breaks = seq(-1, 13, by =1))+
  coord_cartesian(ylim = c(-1, 13))+
  scale_x_continuous(breaks = seq(0, 0.9, by =0.1))+
  labs(title = (expression(paste("Cavemount moi"))), x="SWC", y = "CH4")+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

moi_C_loess2<-ggplot(d., aes(x=moi_mean, y=CH4_mean, colour=type))+
  facet_wrap(~Year)+
  geom_point(size=1)+
  stat_smooth(method=loess)+
  scale_y_continuous(breaks = seq(-1, 1, by =0.1))+
  coord_cartesian(ylim = c(-1, 1))+
  scale_x_continuous(breaks = seq(0, 0.9, by =0.1))+
  labs(title = (expression(paste("Cavemount moi"))), x="SWC", y = "CH4")+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

T_C_lm<-ggplot(d., aes(x=soil_T_mean, y=CH4_mean, colour=type))+
  facet_wrap(~Year)+
  geom_point(size=1)+
  stat_smooth(method=lm)+
  scale_y_continuous(breaks = seq(-1, 13, by =1))+
  coord_cartesian(ylim = c(-1, 13))+
  scale_x_continuous(breaks = seq(0, 30, by =5))+
  labs(title = (expression(paste("Cavemount T"))), x="T", y = "CH4")+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

T_C_lm2<-ggplot(d., aes(x=soil_T_mean, y=CH4_mean, colour=type))+
  facet_wrap(~Year)+
  geom_point(size=1)+
  stat_smooth(method=lm)+
  scale_y_continuous(breaks = seq(-1, 1, by =0.1))+
  coord_cartesian(ylim = c(-1, 1))+
  scale_x_continuous(breaks = seq(0, 30, by =5))+
  labs(title = (expression(paste("Cavemount T"))), x="T", y = "CH4")+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

T_C_loess<-ggplot(d., aes(x=soil_T_mean, y=CH4_mean, colour=type))+
  facet_wrap(~Year)+
  geom_point(size=1)+
  stat_smooth(method=loess)+
  scale_y_continuous(breaks = seq(-1, 13, by =1))+
  coord_cartesian(ylim = c(-1, 13))+
  scale_x_continuous(breaks = seq(0, 30, by =5))+
  labs(title = (expression(paste("Cavemount T"))), x="T", y = "CH4")+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

T_C_loess2<-ggplot(d., aes(x=soil_T_mean, y=CH4_mean, colour=type))+
  facet_wrap(~Year)+
  geom_point(size=1)+
  stat_smooth(method=loess)+
  scale_y_continuous(breaks = seq(-1, 1, by =0.1))+
  coord_cartesian(ylim = c(-1, 1))+
  scale_x_continuous(breaks = seq(0, 30, by =5))+
  labs(title = (expression(paste("Cavemount T"))), x="T", y = "CH4")+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))
