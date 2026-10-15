rm(list=ls())

meansCavemount<-summarise(group_by(Cavemount.flux, Year, DOY2, Month, Hour, type), count = n(), 
                          CO2_mean=mean(CO2), CO2_sd=sd(CO2), CO2_se=sd(CO2)/sqrt(length(CO2)),
                 CH4_mean=mean(CH4), CH4_sd=sd(CH4), CH4_se=sd(CH4)/sqrt(length(CH4)),
                 moi_mean=mean(moi), moi_sd=sd(moi), moi_se=sd(moi)/sqrt(length(moi)),
                 soil_T_mean=mean(soil_T), soil_T_sd=sd(soil_T), soil_T_se=sd(soil_T)/sqrt(length(soil_T)),
                 WT_mean=mean(WT), WT_sd=sd(WT), WT_se=sd(WT)/sqrt(length(WT))) 
 
meansClara<-summarise(group_by(Clara.flux, Year, DOY2, Month, Hour, type), count = n(), 
                          CO2_mean=mean(CO2), CO2_sd=sd(CO2), CO2_se=sd(CO2)/sqrt(length(CO2)),
                          CH4_mean=mean(CH4), CH4_sd=sd(CH4), CH4_se=sd(CH4)/sqrt(length(CH4)),
                          moi_mean=mean(moi), moi_sd=sd(moi), moi_se=sd(moi)/sqrt(length(moi)),
                          soil_T_mean=mean(soil_T), soil_T_sd=sd(soil_T), soil_T_se=sd(soil_T)/sqrt(length(soil_T))) 

meansUmmeras<-summarise(group_by(Ummeras.flux, Year, DOY2, Month, Hour, type), count = n(), 
                          CO2_mean=mean(CO2), CO2_sd=sd(CO2), CO2_se=sd(CO2)/sqrt(length(CO2)),
                          CH4_mean=mean(CH4), CH4_sd=sd(CH4), CH4_se=sd(CH4)/sqrt(length(CH4)),
                          moi_mean=mean(moi), moi_sd=sd(moi), moi_se=sd(moi)/sqrt(length(moi)),
                          soil_T_mean=mean(soil_T), soil_T_sd=sd(soil_T), soil_T_se=sd(soil_T)/sqrt(length(soil_T)),
                          WT_mean=mean(WT), WT_sd=sd(WT), WT_se=sd(WT)/sqrt(length(WT))) 

write.csv(meansCavemount,'meansCavemount.csv')      
write.csv(meansClara,'meansClara.csv')    
write.csv(meansUmmeras,'meansUmmeras.csv')    

mm<-count(Cavemount.flux, DOY2, Month, Hour, type)


summer_mean1<-summarise(group_by(summer1, Plot, type, site), meanCH4=mean(CH4), meanCO2=mean(CO2),
                        meanT=mean(soil_T), meanmoi=mean(moi))
summer_mean4<-summarise(group_by(summer4, Plot, type, site), meanCH4=mean(CH4), meanCO2=mean(CO2))
summer_meanWT<-summarise(group_by(summer_WT, Plot, type, site), meanCH4=mean(CH4), meanCO2=mean(CO2),
                         meanWT=mean(WT))

SEa=sd(CO2)/sqrt(length(CO2))
compensation<-mutate(compensation, FRratio=Fruit/Root)

means<-tally(means)
means<-add_count(means)

########################


meansCavemount<-summarise(group_by(Cavemount.flux, Year, Month, type), count = n(), 
                          CO2_mean=mean(CO2), CO2_sd=sd(CO2), CO2_se=sd(CO2)/sqrt(length(CO2)),
                          CH4_mean=mean(CH4), CH4_sd=sd(CH4), CH4_se=sd(CH4)/sqrt(length(CH4)),
                          moi_mean=mean(moi), moi_sd=sd(moi), moi_se=sd(moi)/sqrt(length(moi)),
                          soil_T_mean=mean(soil_T), soil_T_sd=sd(soil_T), soil_T_se=sd(soil_T)/sqrt(length(soil_T)),
                          WT_mean=mean(WT), WT_sd=sd(WT), WT_se=sd(WT)/sqrt(length(WT))) 

meansClara<-summarise(group_by(Clara.flux, Year, Month, type), count = n(), 
                      CO2_mean=mean(CO2), CO2_sd=sd(CO2), CO2_se=sd(CO2)/sqrt(length(CO2)),
                      CH4_mean=mean(CH4), CH4_sd=sd(CH4), CH4_se=sd(CH4)/sqrt(length(CH4)),
                      moi_mean=mean(moi), moi_sd=sd(moi), moi_se=sd(moi)/sqrt(length(moi)),
                      soil_T_mean=mean(soil_T), soil_T_sd=sd(soil_T), soil_T_se=sd(soil_T)/sqrt(length(soil_T))) 

meansUmmeras<-summarise(group_by(Ummeras.flux, Year, Month, type), count = n(), 
                        CO2_mean=mean(CO2), CO2_sd=sd(CO2), CO2_se=sd(CO2)/sqrt(length(CO2)),
                        CH4_mean=mean(CH4), CH4_sd=sd(CH4), CH4_se=sd(CH4)/sqrt(length(CH4)),
                        moi_mean=mean(moi), moi_sd=sd(moi), moi_se=sd(moi)/sqrt(length(moi)),
                        soil_T_mean=mean(soil_T), soil_T_sd=sd(soil_T), soil_T_se=sd(soil_T)/sqrt(length(soil_T)),
                        WT_mean=mean(WT), WT_sd=sd(WT), WT_se=sd(WT)/sqrt(length(WT))) 

write.csv(meansCavemount,'meansCavemount_month.csv')      
write.csv(meansClara,'meansClara_month.csv')    
write.csv(meansUmmeras,'meansUmmeras_month.csv')  
##############################

Cavemount.flux0<-mutate(Cavemount.flux, CO2month=CO2*3600*24*30/1000000)
Cavemount.flux0<-mutate(Cavemount.flux0, CH4month=CH4*3600*24*30/1000000000)
Clara.flux0<-mutate(Clara.flux, CO2month=CO2*3600*24*30/1000000)
Clara.flux0<-mutate(Clara.flux0, CH4month=CH4*3600*24*30/1000000000)
Ummeras.flux0<-mutate(Ummeras.flux, CO2month=CO2*3600*24*30/1000000)
Ummeras.flux0<-mutate(Ummeras.flux0, CH4month=CH4*3600*24*30/1000000000)

meansCavemount2<-summarise(group_by(Cavemount.flux0, Year, Month, type), count = n(), 
                          CO2_mean=mean(CO2month), CO2_sd=sd(CO2month), CO2_se=sd(CO2month)/sqrt(length(CO2month)),
                          CH4_mean=mean(CH4month), CH4_sd=sd(CH4month), CH4_se=sd(CH4month)/sqrt(length(CH4month)),
                          moi_mean=mean(moi), moi_sd=sd(moi), moi_se=sd(moi)/sqrt(length(moi)),
                          soil_T_mean=mean(soil_T), soil_T_sd=sd(soil_T), soil_T_se=sd(soil_T)/sqrt(length(soil_T)),
                          WT_mean=mean(WT), WT_sd=sd(WT), WT_se=sd(WT)/sqrt(length(WT))) 

meansClara2<-summarise(group_by(Clara.flux0, Year,  Month, type), count = n(), 
                      CO2_mean=mean(CO2month), CO2_sd=sd(CO2month), CO2_se=sd(CO2month)/sqrt(length(CO2month)),
                      CH4_mean=mean(CH4month), CH4_sd=sd(CH4month), CH4_se=sd(CH4month)/sqrt(length(CH4month)),
                      moi_mean=mean(moi), moi_sd=sd(moi), moi_se=sd(moi)/sqrt(length(moi)),
                      soil_T_mean=mean(soil_T), soil_T_sd=sd(soil_T), soil_T_se=sd(soil_T)/sqrt(length(soil_T))) 

meansUmmeras2<-summarise(group_by(Ummeras.flux0, Year,  Month, type), count = n(), 
                        CO2_mean=mean(CO2month), CO2_sd=sd(CO2month), CO2_se=sd(CO2month)/sqrt(length(CO2month)),
                        CH4_mean=mean(CH4month), CH4_sd=sd(CH4month), CH4_se=sd(CH4month)/sqrt(length(CH4month)),
                        moi_mean=mean(moi), moi_sd=sd(moi), moi_se=sd(moi)/sqrt(length(moi)),
                        soil_T_mean=mean(soil_T), soil_T_sd=sd(soil_T), soil_T_se=sd(soil_T)/sqrt(length(soil_T)),
                        WT_mean=mean(WT), WT_sd=sd(WT), WT_se=sd(WT)/sqrt(length(WT)))


write.csv(meansCavemount2,'meansCavemountMONTH.csv')      
write.csv(meansClara2,'meansClaraMONTH.csv')    
write.csv(meansUmmeras2,'meansUmmerasMONTH.csv')  

#########
floating<-filter(floating, C.CH4!="NA" & C.CO2!="NA")
floating2<-filter(floating, C.CH4!="NA" & C.CO2!="NA" & DO_mg_l!="NA" & T_water!="NA")
floatingDO<-filter(floating, C.CH4!="NA" & C.CO2!="NA" & DO_mg_l!="NA")
floatingT<-filter(floating, C.CH4!="NA" & C.CO2!="NA" & T_water!="NA")

meansfloating1<-summarise(group_by(floatingT, Year, DOY2, Month, Hour, site, type), count = n(), 
                          CO2_mean=mean(C.CO2), CO2_sd=sd(C.CO2), CO2_se=sd(C.CO2)/sqrt(length(C.CO2)),
                          CH4_mean=mean(C.CH4), CH4_sd=sd(C.CH4), CH4_se=sd(C.CH4)/sqrt(length(C.CH4)),
                          T_water_mean=mean(T_water), T_water_sd=sd(T_water), T_water_se=sd(T_water)/sqrt(length(T_water)),
                         T_air_mean=mean(T_air), T_air_sd=sd(T_air), T_water_se=sd(T_water)/sqrt(length(T_air)),
                          DO_mean=mean(DO_mg_l), DO_sd=sd(DO_mg_l), DO_se=sd(DO_mg_l)/sqrt(length(DO_mg_l))) 

meansfloating2<-summarise(group_by(floatingDO, Year, DOY2, Month, Hour, site, type), count = n(), 
                          CO2_mean=mean(C.CO2), CO2_sd=sd(C.CO2), CO2_se=sd(C.CO2)/sqrt(length(C.CO2)),
                          CH4_mean=mean(C.CH4), CH4_sd=sd(C.CH4), CH4_se=sd(C.CH4)/sqrt(length(C.CH4)),
                          T_water_mean=mean(T_water), T_water_sd=sd(T_water), T_water_se=sd(T_water)/sqrt(length(T_water)),
                          T_air_mean=mean(T_air), T_air_sd=sd(T_air), T_water_se=sd(T_water)/sqrt(length(T_air)),
                          DO_mean=mean(DO_mg_l), DO_sd=sd(DO_mg_l), DO_se=sd(DO_mg_l)/sqrt(length(DO_mg_l))) 

meansfloatingall<-summarise(group_by(floating2, Year, DOY2, Month, Hour, site, type), count = n(), 
                          CO2_mean=mean(C.CO2), CO2_sd=sd(C.CO2), CO2_se=sd(C.CO2)/sqrt(length(C.CO2)),
                          CH4_mean=mean(C.CH4), CH4_sd=sd(C.CH4), CH4_se=sd(C.CH4)/sqrt(length(C.CH4)),
                          T_water_mean=mean(T_water), T_water_sd=sd(T_water), T_water_se=sd(T_water)/sqrt(length(T_water)),
                          T_air_mean=mean(T_air), T_air_sd=sd(T_air), T_water_se=sd(T_water)/sqrt(length(T_air)),
                          DO_mean=mean(DO_mg_l), DO_sd=sd(DO_mg_l), DO_se=sd(DO_mg_l)/sqrt(length(DO_mg_l))) 

write.csv(meansfloating1,'meansfloating1.csv')
write.csv(meansfloating2,'meansfloating2.csv')
write.csv(meansfloatingall,'meansfloatingall.csv')

floatingC<-filter(floating, site!="Ummeras")
floatingU<-filter(floating, site!="Cavemount")

meansfloatingC<-summarise(group_by(floatingC, Year, DOY2, Month, Hour, site, type), count = n(), 
                         CO2_mean=mean(C.CO2), CO2_sd=sd(C.CO2), CO2_se=sd(C.CO2)/sqrt(length(C.CO2)),
                         CH4_mean=mean(C.CH4), CH4_sd=sd(C.CH4), CH4_se=sd(C.CH4)/sqrt(length(C.CH4)),
                         T_air_mean=mean(T_air), T_air_sd=sd(T_air), T_air_se=sd(T_air)/sqrt(length(T_air)),
                         T_water_mean=mean(T_water), T_water_sd=sd(T_water), T_water_se=sd(T_water)/sqrt(length(T_water)),
                         DO_mean=mean(DO_mg_l), DO_sd=sd(DO_mg_l), DO_se=sd(DO_mg_l)/sqrt(length(DO_mg_l))) 

write.csv(meansfloatingC,'meansfloatingC.csv') 
write.csv(meansfloatingU,'meansfloatingU.csv')



T_CH4type<-ggplot(Cavemount.flux1, aes(x=soil_T.average, y=CH4.average))+
  geom_point(size=1.5)+
  stat_smooth(method=loess)+
  scale_y_continuous(breaks = seq(-4, 10, by =1))+
  coord_cartesian(ylim = c(-4, 10), xlim=c(0, 32))+
  scale_x_continuous(breaks = seq(0, 30, by =5))+
  scale_colour_manual(name="", values=c('Bare Peat'="black", 'Birch Scrub'="chocolate", 'Eriophorum'="green2"))+
  labs(title = (expression(paste("Cavemount ", CH[4], " flux by cover type"))), x="Soil temperature (°C)", y = expression(paste ("", CH[4]," flux (nmol ",-CH[4]," ", m^-2 ," ", s^-1, ")", sep="")))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))
