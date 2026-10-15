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
str(Cavemount.flux)
summary(Cavemount.flux)
Cavemount.flux[, "Date"]
Cavemount.flux[, "newCH4"]
Clara.flux[, "CO2"]

write.csv(Cavemount.flux,'11.csv')
########## save the new file as

Cavemount.flux$Date <- as.factor(Cavemount.flux$Date)
Cavemount.flux$DOY <- as.numeric(Cavemount.flux$DOY)
Cavemount.flux$DOY2 <- as.factor(Cavemount.flux$DOY2)
Cavemount.flux$Year<-as.factor(Cavemount.flux$Year)
Cavemount.flux<-filter(Cavemount.flux, CO2!="NA" & CH4!="NA")
Cavemount.fluxWT<-filter(Cavemount.flux, WT!="NA")
Cavemount.flux<-filter(Cavemount.flux, soil_T!="NA" & moi!="NA")

Cavemount.flux$Plot <- fct_collapse(Cavemount.flux$Plot, BP8 = c('BP8B', 'BP8C', 'BP8', 'BP8D'),
                                    E5 = c('E5', 'E5B'), E7=c('E7', 'E7B', 'E7C'))
Cavemount.flux_mean<-Cavemount.flux
CH4meanC<-summarise(group_by(Cavemount.flux_mean, Plot, type), meanCH4=mean(CH4), meanCO2=mean(CO2),
meanT=mean(soil_T), meanmoi=mean(moi))

mod<-lm(meanCH4~meanmoi + meanT+ type, data=CH4meanC)

par(mfrow=c(2,2))
plot(mod)
###################

Clara.flux<-filter(Clara.flux, CO2!="NA" & CH4!="NA")
Clara.flux[Clara.flux$CO2 < 0, "CO2"] <- 0
Clara.flux['CO2'][Clara.flux['CO2'] < 0] <- 0
Clara.flux$Date <- as.factor(Clara.flux$Date)
Clara.flux$DOY2 <- as.factor(Clara.flux$DOY2)
Clara.flux$Year<-as.factor(Clara.flux$Year)
Clara.fluxWT<-filter(Clara.flux, action!="delete" & action!="delete_flux")
Clara.flux<-filter(Clara.flux, action!="delete" & action!="delete_flux" & soil_T!="NA" & moi!="NA")
Clara.fluxCH4<-filter(Clara.flux, action!="delete" & action!="ch4h" & action!="delete_flux" & soil_T!="NA" & moi!="NA")


CH4meanCl<-summarise(group_by(Clara.flux, Plot, type), meanCH4=mean(CH4),
                    meanT=mean(soil_T), meanmoi=mean(moi))
CH4meanCl <- CH4meanCl[-c(2, 3, 5), ]

mod<-lm(meanCH4~meanmoi + meanT+ type, data=CH4meanCl)

par(mfrow=c(2,2))
plot(mod)

Ummeras.flux$Date <- as.factor(Ummeras.flux$Date)
Ummeras.flux$DOY2 <- as.factor(Ummeras.flux$DOY2)
Ummeras.flux$Year<-as.factor(Ummeras.flux$Year)
Ummeras.fluxWT<-filter(Ummeras.flux, WT!="NA" & action!="delete")
Ummeras.flux<-filter(Ummeras.flux, action!="delete" & soil_T!="NA" & moi!="NA")

Ummeras.flux_mean<-Ummeras.flux
Ummeras.flux_mean$Plot <- recode_factor(Ummeras.flux_mean$Plot, 'E1B' = "E1", 'E2B'='E2', 
                                          'M10B'= 'M10')
CH4meanU<-summarise(group_by(Ummeras.flux_mean, Plot, type), meanCH4=mean(CH4), meanCO2=mean(CO2),
                    meanT=mean(soil_T), meanmoi=mean(moi))

mod<-lm(meanCH4~meanmoi + meanT+ type, data=CH4meanU)

par(mfrow=c(2,2))
plot(mod)

mod00<-lm(CO2~moi+soil_T+type, data=Cavemount.flux)
vif(lm(CO2~soil_T+moi, data=Cavemount.flux))
vif(mod)
vif(modCH4)
vif(mod2)

mod<-lmer(CO2~moi + soil_T+ type+ (1|Plot)+(1|Date), data=Cavemount.flux, REML=FALSE)
mod0<-lmer(newCO2~moi + soil_T+ type+ (1|Plot)+(1|Date), data=Cavemount.flux, REML=FALSE)
mod2<-lmer(newCO2c~moi + soil_T+ type+ (1|Plot)+(1|Date), data=Cavemount.flux, REML=FALSE)
mod<-lmer(CO2~moi + soil_T+ type+ (1|Plot)+(1|Date), data=Cavemount.flux, REML=FALSE)
modCH4<-lmer(CH4~moi + soil_T+ type+ (1|Plot)+(1|Date), data=Cavemount.flux, REML=FALSE)
modCH41<-lmer(newCH4c~moi + soil_T+ type+ (1|Plot)+(1|Date), data=Cavemount.flux, REML=FALSE)
modCH42<-lmer(newCH4~moi + soil_T+ type+ (1|Plot)+(1|Date), data=Cavemount.flux, REML=FALSE)

Cavemount.flux$newCO2<-log(Cavemount.flux$CO2+1)
Cavemount.flux$newCO2b<-sqrt(Cavemount.flux$CO2)
Cavemount.flux$newCO2c<-1/(Cavemount.flux$CO2+1)

Cavemount.flux$newCH4<-log(Cavemount.flux$CH4+10)
Cavemount.flux$newCH4b<-sqrt(Cavemount.flux$CH4+10)
Cavemount.flux$newCH4c<-1/(Cavemount.flux$CH4+10)

mod<-lmer(CH4~moi + soil_T+ type+ (1|Plot)+(1|Date), data=Clara.fluxCH4, REML=FALSE)
mod2<-lmer(CnewCH4c~moi + soil_T+ type+ (1|Plot)+(1|Date), data=Clara.fluxCH4, REML=FALSE)

Clara.fluxCH4$CnewCH4<-log(Clara.fluxCH4$CH4+100)
Clara.fluxCH4$CnewCH4b<-sqrt(Clara.fluxCH4$CH4+100)
Clara.fluxCH4$CnewCH4c<-1/(Clara.fluxCH4$CH4+100)

Clara.flux$CnewCO2<-log(Clara.flux$CO2+1)
Clara.flux$CnewCO2b<-sqrt(Clara.flux$CO2)
Clara.flux$CnewCO2c<-1/(Clara.flux$CO2+1)

modu<-lmer(CO2~moi + soil_T+ type+ (1|Plot)+(1|Date), data=Ummeras.flux, REML=FALSE)
modu2<-lmer(UnewCO2a~moi + soil_T+ type+ (1|Plot)+(1|Date), data=Ummeras.flux, REML=FALSE)

Ummeras.flux$UnewCO2a<-log(Ummeras.flux$CO2+1)
Ummeras.flux$UnewCO2b<-sqrt(Ummeras.flux$CO2)
Ummeras.flux$UnewCO2c<-1/(Ummeras.flux$CO2+1)

Ummeras.flux$UnewCH4a<-log(Ummeras.flux$CH4+1)
Ummeras.flux$UnewCH4b<-sqrt(Ummeras.flux$CH4+1)
Ummeras.flux$UnewCH4c<-1/(Ummeras.flux$CH4+1)

###############################
diagnostics<- fortify(modCH42)
p1 <- ggplot(diagnostics, aes(x = .fitted, y = .resid))+
  geom_point()+geom_hline(yintercept = 0, linetype = "dashed")+
  theme_bw()

p2 <- ggplot(diagnostics, aes(sample = .scresid))+
  stat_qq()+
  geom_abline()+
  theme_bw()
grid.arrange(p1,p2, ncol = 2)

#### p1 can also be presented as >plot(fitted(mod0),residuals(mod0))

plot(modCH41, resid(., scaled=TRUE) ~ moi, abline = 0,pch=1, type=c("p", "smooth"),
     xlab="moi",ylab="Standardised residuals")
plot(modCH41, resid(., scaled=TRUE) ~ soil_T, abline = 0,pch=1,type=c("p", "smooth"),
     xlab="soil T",ylab="Standardised residuals")
plot(modCH41, resid(., scaled=TRUE) ~ fitted(.), type=c("p", "smooth"), abline = 0,
     pch=1, xlab="Fitted values",ylab="Standardised residuals")
#### plotting the residuals against each explanatory variable
xyplot(resid(modCH41)~type, data=Cavemount.flux, abline=0, type=c("smooth", "p", "a", "g"),
       jitter.x=TRUE, )
###when the explanatory variable is not-continuous
lev<-hat(model.matrix(modCH41))
plot(resid(modCH41, type="pearson")~lev,las=1, ylab='Standardised residuals', xlab="Leverage")
###leverage
cd<-cooks.distance(modCH41)
summary(cd)
plot(cd)
###Cook's distance 

hist(Cavemount.flux$CO2, breaks=15)
lines(Cavemount.flux$CO2)

hist(Cavemount.flux$newCH4, breaks=15)
lines(Cavemount.flux$newCH4)

par(mfrow=c(2,2))
plot(mod)

par(mfrow=c(1,1))
qqPlot(mod00, labels=row.names(means), envelope=.95, main="Q-Q Plot")

qqp(Cavemount.flux$newCH4b, "norm")
qqp(Cavemount.flux$newCH4, "lnorm")
gamma<-fitdistr(Cavemount.flux$CH4, "gamma")
qqp(Cavemount.flux$CH4, "gamma", shape=gamma$estimate[[1]], rate=gamma$estimate[[2]])
#########################################

mod0<-lmer(newCO2c~moi + soil_T+ type+ (1|Plot)+(1|Date), data=Cavemount.flux, REML=FALSE)
mod0a<-lmer(newCO2c~ soil_T+ type+ (1|Plot)+(1|Date), data=Cavemount.flux, REML=FALSE)
mod0b<-lmer(newCO2c~moi + type+ (1|Plot)+(1|Date), data=Cavemount.flux, REML=FALSE)
mod0c<-lmer(newCO2c~moi + soil_T+ (1|Plot)+(1|Date), data=Cavemount.flux, REML=FALSE)
mod0d<-lmer(newCO2c~moi + soil_T+ type+ type:soil_T+ type:moi+(1|Plot)+(1|Date), data=Cavemount.flux, REML=FALSE)
mod0e<-lmer(newCO2c~moi + soil_T+ type+ type:soil_T+ (1|Plot)+(1|Date), data=Cavemount.flux, REML=FALSE)
mod0f<-lmer(newCO2c~moi + soil_T+ type+ type:moi+(1|Plot)+(1|Date), data=Cavemount.flux, REML=FALSE)

anova(mod0, mod0a)
anova(mod0, mod0b)
anova(mod0, mod0c)
anova(mod0d, mod0e)
anova(mod0d, mod0f)

summary(glht(mod0, linfct=mcp(type="Tukey")))

mod2<-lmer(CnewCO2c~moi + soil_T+ type+ (1|Plot)+(1|Date), data=Clara.flux, REML=FALSE)
mod2a<-lmer(CnewCO2c~ soil_T+ type+ (1|Plot)+(1|Date), data=Clara.flux, REML=FALSE)
mod2b<-lmer(CnewCO2c~moi + type+ (1|Plot)+(1|Date), data=Clara.flux, REML=FALSE)
mod2c<-lmer(CnewCO2c~moi + soil_T+ (1|Plot)+(1|Date), data=Clara.flux, REML=FALSE)
mod2d<-lmer(CnewCO2c~moi + soil_T+ type+ type:soil_T+ type:moi+(1|Plot)+(1|Date), data=Clara.flux, REML=FALSE)
mod2e<-lmer(CnewCO2c~moi + soil_T+ type+ type:soil_T+ (1|Plot)+(1|Date), data=Clara.flux, REML=FALSE)
mod2f<-lmer(CnewCO2c~moi + soil_T+ type+ type:moi+(1|Plot)+(1|Date), data=Clara.flux, REML=FALSE)

anova(mod2, mod2a)
anova(mod2, mod2b)
anova(mod2, mod2c)
anova(mod2d, mod2e)
anova(mod2d, mod2f)

summary(glht(mod2, linfct=mcp(type="Tukey")))

mod3<-lmer(UnewCO2c~moi + soil_T+ type+(1|Plot)+(1|Date), data=Ummeras.flux, REML=FALSE)
mod3a<-lmer(UnewCO2c~ soil_T+ type+ (1|Plot)+(1|Date), data=Ummeras.flux, REML=FALSE)
mod3b<-lmer(UnewCO2c~moi + type+ (1|Plot)+(1|Date), data=Ummeras.flux, REML=FALSE)
mod3c<-lmer(UnewCO2c~moi + soil_T+ (1|Plot)+(1|Date), data=Ummeras.flux, REML=FALSE)
mod3d<-lmer(UnewCO2c~moi + soil_T+ type+ type:soil_T+ type:moi+(1|Plot)+(1|Date), data=Ummeras.flux, REML=FALSE)
mod3e<-lmer(UnewCO2c~moi + soil_T+ type+ type:soil_T+ (1|Plot)+(1|Date), data=Ummeras.flux, REML=FALSE)
mod3f<-lmer(UnewCO2c~moi + soil_T+ type+ type:moi+(1|Plot)+(1|Date), data=Ummeras.flux, REML=FALSE)

anova(mod3, mod3a)
anova(mod3, mod3b)
anova(mod3, mod3c)
anova(mod3d, mod3e)
anova(mod3d, mod3f)

summary(glht(mod3, linfct=mcp(type="Tukey")))

means<-summarise(group_by(Cavemount.flux, type), meanenv=mean(CH4))
#################################







chisq.test(Cavemount.flux$moi, Cavemount.flux$soil_T)
######no use? 

mod0<-lmer(moi~soil_T*type + (|Plot), data=Cavemount.flux_moi)
mod00<-lmer(soil_T~type + (|Plot), data=Cavemount.flux_moi)

mod1<-lmer(CO2~type + (Date|Plot), data=Cavemount.flux_moi)
mod1<-lmer(CO2~type + (DOY2|Plot), data=Cavemount.flux_moi)
mod2<-lmer(CO2~type + (1|Plot), data=Cavemount.flux_moi)
mod9<-lmer(CO2~type + (1|DOY2)+ (1|Plot), data=Cavemount.flux_moi)

mod10<-lmer(CO2~type + (1+type|DOY2)+ (1+type|Plot), data=Cavemount.flux_moi)
###boundary (singular) fit: see ?isSingular


sum_CH4<-summarise(group_by(sum0, Chamber), meanCH4=mean(CH4))
sum_CH4<-summarise(group_by(sum0, Chamber, Water.table, Planting), meanCH4=mean(CH4))
sumD<-summarise(group_by(GPPonly, Core), meanCO2d=mean(GPP))
sumCH4<-summarise(group_by(sum0, Core), meanCH4=mean(CH4))
sumCO2<-summarise(group_by(unplanted, Core), meanCO2=mean(CO2))
sumCO2<-summarise(group_by(unplanted, Chamber.temperature, Water.table), meanCO2=mean(CO2))
sumCO2p<-summarise(group_by(planted, Core), meanCO2=mean(CO2))
sumM<-summarise(group_by(sum0, Core), meanM=mean(Moisture...12cm.....))
sumT<-summarise(group_by(sum0, Core), meanT=mean(Soil.temperature...10cm....C.))
sum0<-filter(sum0, Soil.temperature...10cm....C.!="NA")
sumCO2<-summarise(group_by(unplanted, Water.table,Chamber.temperature), meanCO2=mean(CO2))
###############

sum0$Water.table<-as.factor(sum0$Water.table)
sum0$Chamber.temperature<-as.factor(sum0$Chamber.temperature)
sum0$week<-as.integer(sum0$week)
planted<-filter(sum0, Planting=="Planted")
unplanted0<-filter(sum0, Planting=="Not planted")

sum$Water.table<-as.factor(sum$Water.table)
sum$Chamber.temperature<-as.factor(sum$Chamber.temperature)
sum$week<-as.integer(sum$week)
unplanted<-filter(sum, Planting=="Not planted")
GPPonly<-filter(sum0, GPP!="NA" & Planting=="Planted")

unplanted <- unplanted[-c(206, 207),]
sum0 <- sum0[-c(504, 440),]
sum0$newCH4<-sign(sum0$CH4)*log(abs(sum0$CH4))

means$Water.table<-as.factor(means$Water.table)
means$Chamber<-as.factor(means$Chamber)
meansCO2<-filter(means, meanCO2!="NA" & Planting=="Not Planted")
meansPL<-filter(means, GPP!="NA")

means$newCH4<-sign(means$meanCH4)*log(abs(means$meanCH4))
means2 <- means[-c(49, 48, 59),]
#############################
diagnostics<- fortify(model6)
p1 <- ggplot(diagnostics, aes(x = .fitted, y = .resid))+
  geom_point()+geom_hline(yintercept = 0, linetype = "dashed")+
  theme_bw()

p2 <- ggplot(diagnostics, aes(sample = .scresid))+
  stat_qq()+
  geom_abline()+
  theme_bw()
grid.arrange(p1,p2, ncol = 2)

hist(meansPL$GPP, breaks=15)
lines(density(meansPL$GPP)) 

par(mfrow=c(2,2))
plot(lmNO3b)

qqPlot(lm6, labels=row.names(means), envelope=.95, main="Q-Q Plot")
qqPlot(lm5)
ncvTest(lm1)

#####################
lm1<-lm(meanCO2~ Water.table*Chamber, data=meansCO2)
lm2<-lm(meanCO2~ Water.table*Chamber, data=meansPL)
lm3<-lm(GPP~ Water.table*Chamber, data=meansPL)
lm4<-lm(meanDarkCO2~ Water.table*Chamber, data=meansPL)
lm5<-lm(meanCH4~ Water.table*Chamber*Planting, data=means)
lm5a<-lm(newCH4~ Water.table*Chamber*Planting, data=means)
lm5b<-lm(meanCH4~ Water.table*Chamber*Planting, data=means2)
lm6<-lm(Moisture~Water.table+Chamber+Planting, data=means)
lm7<-lm(meanCH4~Moisture, data=means2)
lm8<-lm(Soil.T~Water.table+Planting, data=means)
lm8<-lm(Soil.T~Moisture, data=means)
lm9<-lm(GPP~Moisture, data=meansPL)
lm10<-lm(meanDarkCO2~ Moisture, data=meansPL)
lm11<-lm(meanCO2~ Moisture, data=meansPL)
lm12<-lm(meanCO2~ Moisture, data=meansCO2)
lm13<-lm(meanCH4~ Moisture, data=means2)
#######################
lm1<-lm(meanCO2~ Water.table+Chamber, data=meansCO2)
lm2<-lm(meanCO2~ Water.table+Chamber, data=meansPL)
lm3<-lm(GPP~ Water.table+Chamber, data=meansPL)
lm4<-lm(meanDarkCO2~ Water.table+Chamber, data=meansPL)
lm5<-lm(meanCH4~ Water.table+Chamber+Planting, data=means)
###############for contrasts remove interactions?

lm1<-aov(meanCO2~ Water.table*Chamber, data=meansCO2)
TukeyHSD(lm4)

print(lsmeans(lm4, list(pairwise~ Water.table)), adjust = c("tukey"))
## for unbalanced

summary(glht(lm6, linfct=mcp(Water.table="Tukey", interaction_average = TRUE)))
summary(glht(model3, linfct=mcp(Water.table="Tukey")))

########################lme4
summary(glht(model5, linfct=mcp(Water.table="Tukey")))
lsmeans(model1, pairwise~Water.table)
summary(model1)
########################

mod1<-lme(CO2~Water.table+Chamber.temperature, random=~ week|Core, data =unplanted)
mod1bis<-lme(CO2~relevel(Water.table,"50")+Chamber.temperature, random=~ week|Core, data =unplanted)
summary(mod1bis)

model1<-lmer(CO2~Water.table + Chamber.temperature + (week|Core), data=unplanted)


mod2<-lme(CO2~Water.table*Chamber.temperature, random=~ week|Core, data=planted)

model2<-lmer(CO2~Water.table + Chamber.temperature + (week|Core), data=planted)


mod3<-lme(newCH4~Water.table*Chamber.temperature, random=~ week|Core, data=sum0)

model3<-lmer(newCH4~Water.table + Chamber.temperature + Planting + (week|Core), data=sum0)

model4<-lmer(GPP~Water.table + Chamber.temperature + (week|Core), data=GPPonly)

model5<-lmer(CO2.Dark~Water.table + Chamber.temperature + (week|Core), data=GPPonly)

model6<-lmer(newCH4~Moisture...12cm..... +(week|Core), data=sum0)

model7<-lmer(GPP~Moisture...12cm..... + (week|Core), data=GPPonly)

model8<-lmer(CO2.Dark~Moisture...12cm..... + (week|Core), data=planted)

model00<-lmer(CO2~Moisture...12cm..... + (week|Core), data=unplanted)

model0a<-lmer(CO2~week + (1|Core), data=unplanted)

model9<-lmer(Moisture...12cm.....~Chamber.temperature + Planting + Water.table + 
               (week|Core), data=sum0)

model10<-lmer(Soil.temperature...10cm....C.~Planting + (week|Core), data=sum0)


model1a<-lmer(CO2~Water.table + (week|Core), data=unplanted)
model1b<-lmer(CO2~Chamber.temperature + (week|Core), data=unplanted)
model2a<-lmer(CO2~Water.table + (week|Core), data=planted)
model2b<-lmer(CO2~Chamber.temperature + (week|Core), data=planted)
model3a<-lmer(newCH4~Water.table + Chamber.temperature + (week|Core), data=sum0)
model3b<-lmer(newCH4~Water.table + Planting + (week|Core), data=sum0)
model3c<-lmer(newCH4~Chamber.temperature + Planting + (week|Core), data=sum0)
model4a<-lmer(GPP~Water.table + (week|Core), data=GPPonly)
model4b<-lmer(GPP~Chamber.temperature + (week|Core), data=GPPonly)
model5a<-lmer(CO2.Dark~Water.table + (week|Core), data=GPPonly)
model5b<-lmer(CO2.Dark~Chamber.temperature + (week|Core), data=GPPonly)
model6a<-lmer(newCH4~Soil.temperature..10cm + (week|Core), data=sum0)
model6b<-lmer(newCH4~1 + (week|Core), data=sum0)
model7a<-lmer(GPP~Soil.temperature..10cm + (week|Core), data=GPPonly)
model7b<-lmer(GPP~1 + (week|Core), data=GPPonly)
model8a<-lmer(CO2.Dark~ 1 + (week|Core), data=planted)
model8b<-lmer(CO2.Dark~Soil.temperature..10cm + (week|Core), data=planted)
model9a<-lmer(Moisture...12cm.....~Chamber.temperature + Planting + (week|Core), data=sum0)
model9b<-lmer(Moisture...12cm.....~Chamber.temperature + Water.table + (week|Core), data=sum0)
model9c<-lmer(Moisture...12cm.....~Planting + Water.table + (week|Core), data=sum0)
model10a<-lmer(Soil.temperature...10cm....C.~ 1 + (week|Core), data=sum0)
model00a<-lmer(CO2~1 + (week|Core), data=unplanted)

anova(model1, model1a)
anova(model1, model1b)
anova(model2, model2a)
anova(model2, model2b)
anova(model3, model3a)
anova(model3, model3b)
anova(model3, model3c)
anova(model4, model4a)
anova(model4, model4b)
anova(model5, model5a)
anova(model5, model5b)
anova(model6, model6a)
anova(model6, model6b)
anova(model7, model7a)
anova(model7, model7b)
anova(model8, model8a)
anova(model9, model9a)
anova(model9, model9b)
anova(model9, model9c)
anova(model10, model10a)
anova(model00, model00a)
########################
model11<-lmer(CO2~Water.table + Chamber.temperature + Water.table:Chamber.temperature +
                (week|Core), data=unplanted)

model22<-lmer(CO2~Water.table + Chamber.temperature + Water.table:Chamber.temperature + 
                (week|Core), data=planted)

model33<-lmer(newCH4~Water.table + Chamber.temperature + Planting + Water.table:Chamber.temperature +
                Water.table:Planting + Chamber.temperature:Planting + 
                (week|Core), data=sum0)

model33a<-lmer(newCH4~Water.table + Chamber.temperature + Planting + Water.table:Chamber.temperature +
                Water.table:Planting + (week|Core), data=sum0)

model33b<-lmer(newCH4~Water.table + Chamber.temperature + Planting + Water.table:Chamber.temperature +
               Chamber.temperature:Planting + 
                (week|Core), data=sum0)

model33c<-lmer(newCH4~Water.table + Chamber.temperature + Planting +
                Water.table:Planting + Chamber.temperature:Planting + 
                (week|Core), data=sum0)

model44<-lmer(GPP~Water.table + Chamber.temperature + Water.table:Chamber.temperature +
                (week|Core), data=GPPonly)

model55<-lmer(CO2.Dark~Water.table + Chamber.temperature + Water.table:Chamber.temperature +
                (week|Core), data=GPPonly)

anova(model11, model1)
anova(model11, model1)
anova(model22, model2)
anova(model22, model2)
anova(model33, model33a)
anova(model33, model33b)
anova(model33, model33c)
anova(model44, model4)
anova(model44, model4)
anova(model55, model5)
anova(model55, model5)


m6<-lm(newCH4~Moisture...12cm..... + Soil.temperature..10cm, data=sum0)
m7<-lm(GPP~Moisture...12cm..... + Soil.temperature..10cm, data=GPPonly)
vif(m)


Clara.flux<-filter(Clara.flux, CO2!="NA" & CH4!="NA" & action!="delete" & action!="delete_flux")
Clara.flux[Clara.flux$CO2 < 0, "CO2"] <- 0
Clara.flux$Date <- as.factor(Clara.flux$Date)
Clara.flux<-filter(Clara.flux, soil_T!="NA" & moi!="NA")
Clara.flux$CUBtrCO2<-sign(Clara.flux$CO2) * abs(Clara.flux$CO2)^(1/3)
Clara.flux$CUBtrCH4<-sign(Clara.flux$CH4) * abs(Clara.flux$CH4)^(1/3)

modCH4<-lmer(CUBtrCH4~ type+soil_T+(1|Plot), data=Clara.flux, REML=FALSE)
modCO2<-lmer(CUBtrCO2~ type+soil_T+(1|Plot), data=Clara.flux, REML=FALSE)



diagnostics<- fortify(modCO2)
p1 <- ggplot(diagnostics, aes(x = .fitted, y = .resid))+
  geom_point()+geom_hline(yintercept = 0, linetype = "dashed")+
  theme_bw()

p2 <- ggplot(diagnostics, aes(sample = .scresid))+
  stat_qq()+
  geom_abline()+
  theme_bw()
grid.arrange(p1,p2, ncol = 2)



