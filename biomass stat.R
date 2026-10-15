biomass$Water.table<-as.factor(biomass$Water.table)
biomass$Temperature<-as.factor(biomass$Temperature)

biomass<-filter(biomass, Notes!="rotting")
dry<-filter(biomass, Dry.wet=="DRY")
wet<-filter(biomass, Dry.wet=="WET")
dryr<-filter(biomass, Dry.wet=="DRY" & Total.root!="NA")
dryroottotal<-filter(biomass, Dry.wet=="DRY" & Total.root!="NA")
dry_taproot<-filter(biomass, Dry.wet=="DRY")



##############
sumbio<-summarise(group_by(wet, Water.table), mean_bio=mean(Leaves))
sumbio<-summarise(group_by(root_total, Temperature), mean_bio=mean(Total))
###########
model1<-lm(Leaves~ Water.table + Temperature, data=dry)
model1a<-lm(Leaves~ Water.table*Temperature, data=dry)
model2<-lm(Leaves~ Water.table + Temperature, data=wet)
model2a<-lm(Leaves~ Water.table*Temperature, data=wet)
model3<-lm(Dry.tap.root~ Water.table + Temperature, data=dry)
model3a<-lm(Dry.tap.root~ Water.table*Temperature, data=dry)
model4<-lm(Root.shoot~ Water.table*Temperature, data=dryr)

hist(dry$Root.shoot, prob = TRUE)
lines(density(dry$Root.shoot))

par(mfrow=c(2,2))
plot(model4)

ncvTest(model3a)
bartlett.test(Leaves~Water.table, data=dry)   ###for simple ANOVA
qqPlot(model2a, labels=row.names(wet), envelope=.95, main="Q-Q Plot")

summary(model1a)
anova(model3a)
########################
summary(glht(model1a, linfct=mcp(Water.table="Tukey", interaction_average = TRUE)))
TukeyHSD(model1a)

model1<-aov(Leaves~ Water.table + Temperature, data=dry)
model1a<-aov(Leaves~ Water.table*Temperature, data=dry)
model2<-aov(Leaves~ Water.table + Temperature, data=wet)
model2a<-aov(Leaves~ Water.table*Temperature, data=wet)
model3<-aov(Dry.tap.root~ Water.table + Temperature, data=dry)
model3a<-aov(Dry.tap.root~ Water.table*Temperature, data=dry)
########################

wetleaf<-ggplot(wet, aes(x=Water.table, y=Leaves))+
  geom_boxplot(width=.2, outlier.size=0.8)+
  scale_x_discrete(name="Water table", breaks=c("30", "40", "50"),labels=c("-30 cm","-40 cm", "-50 cm"))+
  scale_y_continuous(breaks = seq(0, 200 ,by =20))+
  coord_cartesian(ylim = c(0, 200))+
  labs(title = "Wet leaf biomass (g)", x="Water table (cm)", y ="Weight (g)")+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=2)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

dryleaf<-ggplot(dry, aes(x=Water.table, y=Leaves))+
  geom_boxplot(width=.2, outlier.size=0.8)+
  scale_x_discrete(name="Water table", breaks=c("30", "40", "50"),labels=c("-30 cm","-40 cm", "-50 cm"))+
  scale_y_continuous(breaks = seq(0, 30 ,by =10))+
  coord_cartesian(ylim = c(0, 30))+
  labs(title = "Dry leaf biomass (g)", x="Water table (cm)", y ="Weight (g)")+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=2)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

dryleaf2<-ggplot(dry, aes(x=Temperature, y=Leaves))+
  geom_boxplot(width=.2, outlier.size=0.8)+
  scale_x_discrete(name="Temperature")+
  scale_y_continuous(breaks = seq(0, 30 ,by =10))+
  coord_cartesian(ylim = c(0, 30))+
  labs(title = "Dry leaf biomass (g)", x="Temperature", y ="Weight (g)")+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=2)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

wetleaf2<-ggplot(wet, aes(x=Temperature, y=Leaves))+
  geom_boxplot(width=.2, outlier.size=0.8)+
  scale_x_discrete(name="Temperature")+
  scale_y_continuous(breaks = seq(0, 200 ,by =20))+
  coord_cartesian(ylim = c(0, 200))+
  labs(title = "Wet leaf biomass (g)", x="Temperature", y ="Weight (g)")+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=2)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

taproot1<-ggplot(dry, aes(x=Temperature, y=Dry.tap.root))+
  geom_boxplot(width=.2, outlier.size=0.8)+
  scale_x_discrete(name="Temperature")+
  scale_y_continuous(breaks = seq(0, 6 ,by =1))+
  coord_cartesian(ylim = c(0, 6))+
  labs(title = "Dry tap root biomass (g)", x="Temperature", y ="Weight (g)")+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=2)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

taproot2<-ggplot(dry, aes(x=Water.table, y=Dry.tap.root))+
  geom_boxplot(width=.2, outlier.size=0.8)+
  scale_x_discrete(name="Water table", breaks=c("30", "40", "50"),labels=c("-30 cm","-40 cm", "-50 cm"))+
  scale_y_continuous(breaks = seq(0, 6 ,by =1))+
  coord_cartesian(ylim = c(0, 6))+
  labs(title = "Dry tap root biomass (g)", x="Water table (cm)", y ="Weight (g)")+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=2)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

totalroot1<-ggplot(dryroottotal, aes(x=Water.table, y=Total.root))+
  geom_boxplot(width=.2, outlier.size=0.8)+
  scale_x_discrete(name="Water table", breaks=c("30", "40", "50"),labels=c("-30 cm","-40 cm", "-50 cm"))+
  scale_y_continuous(breaks = seq(0, 7, by =1))+
  coord_cartesian(ylim = c(0, 7))+
  labs(title = "Dry total root biomass (g)", x="Water table (cm)", y ="Weight (g)")+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=2)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

totalroot2<-ggplot(dryroottotal, aes(x=Temperature, y=Total.root))+
  geom_boxplot(width=.2, outlier.size=0.8)+
  scale_x_discrete(name="Temperature")+
  scale_y_continuous(breaks = seq(0, 7, by =1))+
  coord_cartesian(ylim = c(0, 7))+
  labs(title = "Dry total root biomass (g)", x="Temperature", y ="Weight (g)")+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=2)+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))
#################################

taproottrt<-ggplot(dry, aes(x=Treatment, y=Dry.tap.root))+
  geom_boxplot(width=.2, outlier.size=0.8)+
  scale_x_discrete(name="", 
                   breaks=c("30Amb", "40Amb", "50Amb", "30Elev", "40Elev", "50Elev"))+
  scale_y_continuous(breaks = seq(0, 6 ,by =1))+
  coord_cartesian(ylim = c(0, 6))+
  labs(title = "Dry tap root biomass (g)", y ="Weight (g)")+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=2)+
  theme(legend.position="none")+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

drytrt<-ggplot(dry, aes(x=Treatment, y=Leaves))+
  geom_boxplot(width=.2, outlier.size=0.8)+
  scale_x_discrete(name="", 
                   breaks=c("30Amb", "40Amb", "50Amb", "30Elev", "40Elev", "50Elev"))+
  scale_y_continuous(breaks = seq(0, 30 ,by =10))+
  coord_cartesian(ylim = c(0, 30))+
  labs(title = "Dry leaf biomass (g)", y ="Weight (g)")+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=2)+
  theme(legend.position="none")+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

wettrt<-ggplot(wet, aes(x=Treatment, y=Leaves))+
  geom_boxplot(width=.2, outlier.size=0.8)+
  scale_x_discrete(name="", 
                   breaks=c("30Amb", "40Amb", "50Amb", "30Elev", "40Elev", "50Elev"))+
  scale_y_continuous(breaks = seq(0, 200 ,by =20))+
  coord_cartesian(ylim = c(0, 200))+
  labs(title = "Wet leaf biomass (g)", y ="Weight (g)")+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=2)+
  theme(legend.position="none")+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

colwettrt<-ggplot(wet, aes(x=Treatment, y=Leaves, fill=Treatment))+
  geom_boxplot(width=.5, outlier.size=0.8)+
  scale_x_discrete(name="", 
                   breaks=c("30Amb", "40Amb", "50Amb", "30Elev", "40Elev", "50Elev"))+
  scale_y_continuous(breaks = seq(0, 200 ,by =20))+
  coord_cartesian(ylim = c(0, 200))+
  labs(title = "Wet leaf biomass (g)", y ="Weight (g)")+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3)+
  theme(plot.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 18))

roottrt<-ggplot(dryroottotal, aes(x=Treatment, y=Total.root, color=Treatment))+
  geom_boxplot(width=.2, outlier.size=0.9, size=0.8)+
  scale_x_discrete(name="", 
                   breaks=c("30Amb", "40Amb", "50Amb", "30Elev", "40Elev", "50Elev"))+
  scale_y_continuous(breaks = seq(0, 7, by =1))+
  coord_cartesian(ylim = c(0, 7))+
  labs(title = "Dry total root biomass (g)", y ="Weight (g)")+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3, colour="black")+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

colroottrt<-ggplot(dryroottotal, aes(x=Treatment, y=Total.root, fill=Treatment))+
  geom_boxplot(width=.5, outlier.size=0.8)+
  scale_x_discrete(name="", 
                   breaks=c("30Amb", "40Amb", "50Amb", "30Elev", "40Elev", "50Elev"))+
  scale_y_continuous(breaks = seq(0, 7, by =1))+
  coord_cartesian(ylim = c(0, 7))+
  labs(title = "Dry total root biomass (g)", y ="Weight (g)")+
  theme_bw()+
  stat_summary(fun.y=mean, geom="point", shape=5, size=3, colour="black")+
  theme(plot.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 18))

###########################

com<-summarise(group_by(dryr, Treatment),
               mean_root_weight1=mean(Total.root), 
               SEdr1=sd(Total.root)/sqrt(length(Total.root)),
               lwr1r=mean_root_weight1-SEdr1,
               upr1r=mean_root_weight1+SEdr1,
               mean_aboveground1=mean(Leaves),
               SEdt1=sd(Leaves)/sqrt(length(Leaves)),
               lwr1t=mean_aboveground1-SEdt1,
               upr1t=mean_aboveground1+SEdt1) 

limits_root<-aes(ymin=lwr1r, ymax=upr1r)
limits_above<-aes(ymin=lwr1t, ymax=upr1t)

abovebelow1<-ggplot(com, aes(x=Treatment, shape=Treatment))+
  geom_errorbar(limits_root, width=0.1)+
  geom_errorbar(limits_above, width=0.1)+
  geom_point (aes (y=mean_root_weight1, colour="Roots"), size=4)+
  geom_point (aes (y=mean_aboveground1, colour="Leaves"), size=4)+
  scale_colour_manual(name="", values=c('Roots'="black", 'Leaves'="red"))+
  labs(title = "Dry leaves and root biomass (g): treatment comparison", x="", y ="Weight (g)")+
  scale_shape_manual(name="", breaks = c("30Amb", "30Elev", "40Amb", "40Elev", "50Amb", "50Elev"), 
                     labels=c("-30cm Ambient", "-30cm Elevated", "-40cm Ambient", "-40cm Elevated", "-50cm Ambient", "-50cm Elevated"), 
                     values = c(19, 1, 15, 0, 17, 2))+
  scale_y_continuous(breaks = seq(0, 25, by =5))+
  coord_cartesian(ylim = c(0, 25))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        strip.text = element_text(size = 12))

#####################

root<-summarise(group_by(dryroottotal, Treatment, Temperature),
               mean_root_weight1=mean(Total.root), 
               SEdr1=sd(Total.root)/sqrt(length(Total.root)),
               lwr1r=mean_root_weight1-SEdr1,
               upr1r=mean_root_weight1+SEdr1)

limits_root<-aes(ymin=lwr1r, ymax=upr1r)

roots1<-ggplot(root, aes(x=Treatment, y=mean_root_weight1, shape=Treatment, colour=Temperature))+
  geom_errorbar(limits_root, width=0.1)+
  geom_point (aes (y=mean_root_weight1, colour="Ambient"), size=4)+
  geom_point (aes (y=mean_root_weight1, colour="Elevated"), size=4)+
  scale_colour_manual(name="", values=c('Ambient'="blue", 'Elevated'="red"))+
  labs(title = "Dry root biomass (g): treatment comparison", x="", y ="Weight (g)")+
  scale_shape_manual(name="", breaks = c("30Amb", "30Elev", "40Amb", "40Elev", "50Amb", "50Elev"), 
                     labels=c("-30cm Ambient", "-30cm Elevated", "-40cm Ambient", "-40cm Elevated", "-50cm Ambient", "-50cm Elevated"), 
                     values = c(19, 19, 15, 15, 17, 17))+
  scale_y_continuous(breaks = seq(0, 7, by =1))+
  coord_cartesian(ylim = c(0, 7))+
  theme_bw()+
  theme(title = element_text(size=14),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        strip.text = element_text(size = 12))

########################################

shoot<-summarise(group_by(wet, Treatment, Temperature),
                mean_shoot_weight1=mean(Leaves), 
                SEdr1=sd(Leaves)/sqrt(length(Leaves)),
                lwr1r=mean_shoot_weight1-SEdr1,
                upr1r=mean_shoot_weight1+SEdr1)

limits_root<-aes(ymin=lwr1r, ymax=upr1r)

wetshoot<-ggplot(shoot, aes(x=Treatment, y=mean_shoot_weight1, shape=Treatment, colour=Temperature))+
  labs(title = "Wet shoot biomass (g): treatment comparison", x="", y ="Weight (g)")+
  scale_shape_manual(name="", breaks = c("30Amb", "30Elev", "40Amb", "40Elev", "50Amb", "50Elev"), 
                     labels=c("-30cm Ambient", "-30cm Elevated", "-40cm Ambient", "-40cm Elevated", "-50cm Ambient", "-50cm Elevated"), 
                     values = c(19, 19, 15, 15, 17, 17))+
  geom_point (aes (y=mean_shoot_weight1, colour="Ambient"), size=4)+
  geom_point (aes (y=mean_shoot_weight1, colour="Elevated"), size=4)+
  scale_colour_manual(name="", values=c('Ambient'="blue", 'Elevated'="red"))+
  geom_errorbar(limits_root, width=0.1)+
  scale_y_continuous(breaks = seq(0, 180, by =20))+
  coord_cartesian(ylim = c(0, 180))+
  theme_bw()+
  theme(title = element_text(size=14),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        strip.text = element_text(size = 12))

#################################################

dryshoot<-summarise(group_by(dry, Treatment, Temperature),
                 mean_shoot_weight1=mean(Leaves), 
                 SEdr1=sd(Leaves)/sqrt(length(Leaves)),
                 lwr1r=mean_shoot_weight1-SEdr1,
                 upr1r=mean_shoot_weight1+SEdr1)

limits_shoot<-aes(ymin=lwr1r, ymax=upr1r)

wetshoot<-ggplot(dryshoot, aes(x=Treatment, y=mean_shoot_weight1, shape=Treatment, colour=Temperature))+
  labs(title = "Dry shoot biomass (g): treatment comparison", x="", y ="Weight (g)")+
  scale_shape_manual(name="", breaks = c("30Amb", "30Elev", "40Amb", "40Elev", "50Amb", "50Elev"), 
                     labels=c("-30cm Ambient", "-30cm Elevated", "-40cm Ambient", "-40cm Elevated", "-50cm Ambient", "-50cm Elevated"), 
                     values = c(19, 19, 15, 15, 17, 17))+
  geom_point (aes (y=mean_shoot_weight1, colour="Ambient"), size=4)+
  geom_point (aes (y=mean_shoot_weight1, colour="Elevated"), size=4)+
  scale_colour_manual(name="", values=c('Ambient'="blue", 'Elevated'="red"))+
  geom_errorbar(limits_shoot, width=0.1)+
  scale_y_continuous(breaks = seq(0, 25, by =5))+
  coord_cartesian(ylim = c(0, 25))+
  theme_bw()+
  theme(title = element_text(size=14),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        strip.text = element_text(size = 12))