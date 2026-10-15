
dry_l<-summarise(group_by(dry, Water.table),
                 mean_aboveground1=mean(Leaves),
                 SEdt1=sd(Leaves)/sqrt(length(Leaves)),
                 lwr1=mean_aboveground1-SEdt1,
                 upr1=mean_aboveground1+SEdt1)
limits_above<-aes(ymin=lwr1, ymax=upr1)
total1<-ggplot(dry_l, aes(x=Water.table))+
  geom_errorbar(limits_above, width=0.1)+
  geom_point (aes (y=mean_aboveground1), size=5)+
  scale_x_discrete(name="Water table", breaks=c("30", "40", "50"), labels=c("-30 cm", "-40 cm", "-50cm"))+
  labs(title = "Dry leaf biomass (g)", x="Water table (cm)", y ="Weight (g)")+
  scale_y_continuous(breaks = seq(0, 21,by =2))+
  coord_cartesian(ylim = c(0, 21))+
  theme_bw()+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

dry_l2<-summarise(group_by(dry, Temperature),
                  mean_aboveground1=mean(Leaves),
                  SEdt1=sd(Leaves)/sqrt(length(Leaves)),
                  lwr1=mean_aboveground1-SEdt1,
                  upr1=mean_aboveground1+SEdt1)
limits_above<-aes(ymin=lwr1, ymax=upr1)
total1<-ggplot(dry_l2, aes(x=Temperature))+
  geom_errorbar(limits_above, width=0.1)+
  geom_point (aes (y=mean_aboveground1), size=5)+
  labs(title = "Dry leaf biomass (g)", x="Temperature", y ="Weight (g)")+
  scale_y_continuous(breaks = seq(0, 21,by =2))+
  coord_cartesian(ylim = c(0, 21))+
  theme_bw()+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

wet_l<-summarise(group_by(wet, Temperature),
                 mean_aboveground1=mean(Leaves),
                 SEdt1=sd(Leaves)/sqrt(length(Leaves)),
                 lwr1=mean_aboveground1-SEdt1,
                 upr1=mean_aboveground1+SEdt1)
limits_above<-aes(ymin=lwr1, ymax=upr1)
total1<-ggplot(wet_l, aes(x=Temperature))+
  geom_errorbar(limits_above, width=0.1)+
  geom_point (aes (y=mean_aboveground1), size=5)+
  labs(title = "Wet leaf biomass (g)", x="Temperature", y ="Weight (g)")+
  scale_y_continuous(breaks = seq(0, 150,by =20))+
  coord_cartesian(ylim = c(0, 150))+
  theme_bw()+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

wet2<-summarise(group_by(wet, Water.table),
                mean_aboveground1=mean(Leaves),
                SEdt1=sd(Leaves)/sqrt(length(Leaves)),
                lwr1=mean_aboveground1-SEdt1,
                upr1=mean_aboveground1+SEdt1)
limits_above<-aes(ymin=lwr1, ymax=upr1)
total1<-ggplot(wet2, aes(x=Water.table))+
  geom_errorbar(limits_above, width=0.1)+
  geom_point (aes (y=mean_aboveground1), size=5)+
  labs(title = "Wet leaf biomass (g)", x="Water table", y ="Weight (g)")+
  scale_x_discrete(name="Water table", breaks=c("30", "40", "50"), labels=c("-30 cm", "-40 cm", "-50cm"))+
  scale_y_continuous(breaks = seq(0, 150,by =20))+
  coord_cartesian(ylim = c(0, 150))+
  theme_bw()+
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))






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

