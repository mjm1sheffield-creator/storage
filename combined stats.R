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
str(combined)
a<-subset(combined, site=='Cavemount', select=Plot)
a<-subset(combined, site=='Cavemount', select=type)
summary(a$Plot)
summary(Ummeras.flux$Plot)
summer<-filter(combined, between(Date, as.Date("2021-01-10"), as.Date("2021-04-05")))
summer<-mutate(summer, season="summer")
winter<-mutate(winter, season="winter")
year<-rbind(summer, winter)
year$season <- as.factor(year$season)
write.csv(combined,'combined.csv')
########## save the new file as

Cavemount.flux$Plot <- fct_collapse(Cavemount.flux$Plot, BP8 = c('BP8B', 'BP8C', 'BP8', 'BP8D'),
                                    E5 = c('E5', 'E5B'), E7=c('E7', 'E7B', 'E7C'))
Ummeras.flux$Plot <- fct_collapse(Ummeras.flux$Plot, E1 = c('E1B', 'E1'), 
                                  E2 = c('E2B', 'E2'), M10=c('M10', 'M10B'))
Cavemount.flux$Plot <- recode_factor(Cavemount.flux$Plot, 'E3'="E_3", 'E4'="E_4", 'E5'='E_5', 'E7'='E_7')

Clara.flux<-mutate(Clara.flux, site=type)
Ummeras.flux<-mutate(Ummeras.flux, site=type)
Cavemount.flux<-mutate(Cavemount.flux, site=type)
Cavemount.flux$site <- recode_factor(Cavemount.flux$site, 'Eriophorum'="EriophorumC")
Cavemount.flux$type <- recode_factor(Cavemount.flux$type, 'Eriophorum'="EriophorumC")
combined<-rbind(Ummeras.flux, Clara.flux, Cavemount.flux)
combined$site<-recode(combined$site, "c('Bare Peat', 'EriophorumC', 'Birch Scrub')='Cavemount'; 
                    c('Bare Peat Wet', 'Bare Peat Dry', 'Bare Peat Middle', 'Eriophorum') = 'Ummeras'; 
                      c('Marginal', 'Sub-Central', 'Sub-Marginal', 'Central')='Clara'")

combined<-filter(combined, action!="delete" & action!="delete_flux" & CO2!="NA" & CH4!="NA")
combined[combined$CO2 < 0, "CO2"] <- 0
combined$Year <- as.factor(combined$Year)
combined$Date <- as.Date(combined$Date, "%d/%m/%Y")

combined$Date <- as.factor(combined$Date)
combined$Date <- format(combined$Date, "%d/%m/%Y")
combined$DOY <- as.numeric(combined$DOY)
combinedWT<-filter(combined, WT!="NA")

combined_env<-filter(combined, soil_T!="NA" & moi!="NA")
combined2<-filter(combined, type!="Eriophorum" & type!="EriophorumC")
combinedE<-filter(combined, type=="Eriophorum" | type=="EriophorumC")
combinedCUmeans<-filter(combined2, site!='Clara')

###filter by date
summer<-filter(combined, Date >= "2021-06-01" & Date <= "2021-08-31" | 
                 Date >= "2022-06-01" & Date <= "2022-08-31" )
summer1<-filter(combined_env, Date >= "2021-06-01" & Date <= "2021-08-31" | 
                 Date >= "2022-06-01" & Date <= "2022-08-31" )
summer2<-filter(combined2, Date >= "2021-06-01" & Date <= "2021-08-31" | 
                 Date >= "2022-06-01" & Date <= "2022-08-31" )
summer3<-filter(combinedE, Date >= "2021-06-01" & Date <= "2021-08-31" | 
                  Date >= "2022-06-01" & Date <= "2022-08-31" )
summer4<-filter(combinedCUmeans, Date >= "2021-06-01" & Date <= "2021-08-31" | 
                  Date >= "2022-06-01" & Date <= "2022-08-31" )
winter<-filter(combined, Date >= "2020-12-01" & Date <= "2021-03-01" | 
                 Date >= "2021-12-01" & Date <= "2022-03-01" )
winter1<-filter(combined_env, Date >= "2020-12-01" & Date <= "2021-03-01" | 
                 Date >= "2021-12-01" & Date <= "2022-03-01" )
winter2<-filter(combined2, Date >= "2020-12-01" & Date <= "2021-03-01" | 
                 Date >= "2021-12-01" & Date <= "2022-03-01" )
winter3<-filter(combinedE, Date >= "2020-12-01" & Date <= "2021-02-01" | 
                  Date >= "2021-12-01" & Date <= "2022-03-01" )
winter4<-filter(combinedCUmeans, Date >= "2020-12-01" & Date <= "2021-02-01" | 
                  Date >= "2021-12-01" & Date <= "2022-03-01" )

winter$Date <- as.factor(winter$Date)
winter1$Date <- as.factor(winter1$Date)
winter2$Date <- as.factor(winter2$Date)
winter3$Date <- as.factor(winter3$Date)
winter4$Date <- as.factor(winter4$Date)
summer$Date <- as.factor(summer$Date)
summer1$Date <- as.factor(summer1$Date)
summer2$Date <- as.factor(summer2$Date)
summer3$Date <- as.factor(summer3$Date)
summer4$Date <- as.factor(summer4$Date)
#####################

summer_WT<-filter(summer, WT!="NA")
summer_mean<-summarise(group_by(summer, Plot, type, site), meanCH4=mean(CH4), meanCO2=mean(CO2))
summer_mean1<-summarise(group_by(summer1, Plot, type, site), meanCH4=mean(CH4), meanCO2=mean(CO2),
                         meanT=mean(soil_T), meanmoi=mean(moi))
summer_mean4<-summarise(group_by(summer4, Plot, type, site), meanCH4=mean(CH4), meanCO2=mean(CO2))
summer_meanWT<-summarise(group_by(summer_WT, Plot, type, site), meanCH4=mean(CH4), meanCO2=mean(CO2),
                    meanWT=mean(WT))

summer_mean <- summer_mean[-c(52), ]
mod2<-lm(meanCH4~site, data=summer_mean4)
mod<-lm(meanCO2~site, data=summer_mean)
anova(mod)
summary(glht(mod, linfct=mcp(site="Tukey")))

par(mfrow=c(2,2))
plot(mod)
#1. site (CH4 looks bad) 2. type (Tukey) (sans Clara), E sep. -> CH4 only? (looks bad)

#mod<-lmer(CO2~site+type+(1|Plot)+(1|Date), data=combined, REML=FALSE)
###fixed-effect model matrix is rank deficient so dropping 2 columns / coefficients
###exclude E from analysis/separate analysis? 
#nested design?
##Model failed to converge

summer$newCO2a<-log(summer$CO2+1)
winter$newCO2a<-log(winter$CO2+1)

mods<-lmer(newCO2a~site+(1|type/Plot)+(1|Date), data=summer, REML=FALSE)
mod1s<-lmer(newCO2a~1+(1|type/Plot)+(1|Date), data=summer, REML=FALSE)
modw<-lmer(newCO2a~site+(1|type/Plot)+(1|Date), data=winter, REML=FALSE)
mod1w<-lmer(newCO2a~1+(1|type/Plot)+(1|Date), data=winter, REML=FALSE)

modt1s<-lmer(newCO2a~type+(1|Plot)+(1|Date), data=summer, REML=FALSE)
modt2s<-lmer(newCO2a~1+(1|Plot)+(1|Date), data=summer, REML=FALSE)
modt1w<-lmer(newCO2~type+(1|Plot)+(1|Date), data=winter, REML=FALSE)
modt2w<-lmer(newCO2~1+(1|Plot)+(1|Date), data=winter, REML=FALSE)

mod2s<-lmer(CH4~site+(1|type/Plot)+(1|Date), data=summer, REML=FALSE)
mod3s<-lmer(CH4~1+(1|type/Plot)+(1|Date), data=summer, REML=FALSE)
mod2w<-lmer(CH4~site+(1|type/Plot)+(1|Date), data=winter, REML=FALSE)
mod3w<-lmer(CH4~1+(1|type/Plot)+(1|Date), data=winter, REML=FALSE)

mod1<-lmer(CH4~type+(1|Plot)+(1|Date), data=combined, REML=FALSE)
mod3<-lmer(CH4~1+(1|Plot)+(1|Date), data=combined, REML=FALSE)

anova(modw, mod1w)

diagnostics<- fortify(modw)
p1 <- ggplot(diagnostics, aes(x = .fitted, y = .resid))+
  geom_point()+geom_hline(yintercept = 0, linetype = "dashed")+
  theme_bw()

p2 <- ggplot(diagnostics, aes(sample = .scresid))+
  stat_qq()+
  geom_abline()+
  theme_bw()
grid.arrange(p1,p2, ncol = 2)

summary(glht(modw, linfct=mcp(site="Tukey")))

sum<-summarise(group_by(combined, site), mean_bio1=mean(CO2))
