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

str(floating)
summary(floating)
levels(dat2$type)
rm(list=ls())
sum1<-summarise(group_by(floating_w, Point), mean_bio1=mean(C.CO2))
sumDOC<-summarise(group_by(DOC, site), mean_bio1=mean(DOC.mg.l))
sumCH4<-summarise(group_by(datw1, site), mean_bio1=mean(meanCH4))

#dat<-cbind(dat, (mutate(dat, Point2=Point)))
#######merges the whole frame

#(floating_Cavemount,'11.csv')
####### save a combined file from all sites (after successful mutation)
#co2, ch4 - site, T water, DO (water) (check col-DO, T water)
#cavemount ch4 - ch4-type, 

floating$Point <- recode_factor(floating$Point, '2a' = "2", '3a'='3')
floating <- floating[-26, ]
####-62 CH4 outlier removed
DOC<-filter(floating, DOC.mg.l!="NA" & type!='Eriophorum')
floating2<-filter(floating, C.CO2!="NA" & C.CH4!="NA" & T_water!="NA" & type!='Eriophorum')
floating3<-filter(floating2, DO_mg_l !="NA")
floating_Cavemount<-filter(floating, site=="Cavemount" & C.CH4!="NA")

floating_we<-filter(floating, C.CO2!="NA" & C.CH4!="NA")
floating_w<-filter(floating, C.CO2!="NA" & C.CH4!="NA" & type!='Eriophorum')
datwe<-summarise(group_by(floating_we, Point, site), meanCH4=mean(C.CH4), meanCO2=mean(C.CO2))
datw<-summarise(group_by(floating_w, Point, site), meanCH4=mean(C.CH4), meanCO2=mean(C.CO2))
datwe1<-mutate(datwe, type=Point)
datw1<-mutate(datw, type=Point)

datwe1$type <- recode_factor(datwe1$Point, '1' = "water", '2' = "water", '3'='water', '4'='water',
'5'='water', '6'='water', GR='water', GL='water', 'T'='water', NW='water', SRW='water',
SLW='water',SLE='Eriophorum', NE='Eriophorum', SRE='Eriophorum')
datw1$type <- recode_factor(datw1$Point, '1' = "water", '2' = "water", '3'='water', '4'='water', '5'='water', '6'='water', 
GR='water', GL='water', 'T'='water', NW='water', SRW='water', SLW='water')
#method 1

datwe1$type<-recode(datwe1$Point, "c('1', '2', '3', '4', '5', '6', 'GR', 'GL', 'T', 'NW', 'SRW', 'SLW')='water'; 
                    c('SLE', 'NE', 'SRE') = 'Eriophorum'")
datw1$type<-recode(datw1$Point, "c('1', '2', '3', '4', '5', '6', 
                    'GR', 'GL', 'T', 'NW', 'SRW', 'SLW')='water';")
#method 2

datwe1$type <- fct_collapse(datwe1$Point, water = c("1", "2", "3", "4", "5", "6", "NW", "T", "GR", "GL", "SRW", "SLW"),
Eriophorum = c("SLE","NE", "SRE"))
datw1$type <- fct_collapse(datw1$Point, water = c("1", "2", "3", "4", "5", "6", 
"NW", "T", "GR", "GL", "SRW", "SLW"))
#method 3 in forcats


t.test(meanCH4~site, data=datw1)
t.test(meanCO2~site, data=datw1)
#t-test - sites
#############################

mod1<-lmer(C.CO2~DO_mg_l+T_water+site+(1|Point)+(1|Date), data=floating_CO2, REML=FALSE)
vif(mod1)
######### DO_mg_l+T_water

modDOC1<-lmer(DOC.mg.l~T_water+site+(1|Point)+(1|Date), data=DOC, REML=FALSE)
modDOC2<-lmer(DOC.mg.l~T_water+site+T_water:site+(1|Point)+(1|Date), data=DOC, REML=FALSE)
modDOC3<-lmer(DOC.mg.l~T_water+(1|Point)+(1|Date), data=DOC, REML=FALSE)
modDOC4<-lmer(DOC.mg.l~site+(1|Point)+(1|Date), data=DOC, REML=FALSE)
anova(modDOC1, modDOC2)
anova(modDOC1, modDOC3)
############

CO2a<-lmer(C.CO2c~T_water+site+(1|Point)+(1|Date), data=floating2, REML=FALSE)
CO2b<-lmer(C.CO2~T_water+site+T_water:site+(1|Point)+(1|Date), data=floating2, REML=FALSE)
CO2c<-lmer(C.CO2a~T_water+(1|Point)+(1|Date), data=floating2, REML=FALSE)
CO2d<-lmer(C.CO2~site+(1|Point)+(1|Date), data=floating2, REML=FALSE)

C.CH4a<-lmer(C.CH4a~T_water+DO_mg_l+site+(1|Point)+(1|Date), data=floating3, REML=FALSE)
C.CH4b<-lmer(C.CH4a~T_water+DO_mg_l+site+T_water:site+(1|Point)+(1|Date), data=floating3, REML=FALSE)
C.CH4b2<-lmer(C.CH4a~T_water+DO_mg_l+site+DO_mg_l:site+(1|Point)+(1|Date), data=floating3, REML=FALSE)
C.CH4c<-lmer(C.CH4a~T_water+site+(1|Point)+(1|Date), data=floating3, REML=FALSE)
C.CH4d<-lmer(C.CH4a~DO_mg_l+site+(1|Point)+(1|Date), data=floating3, REML=FALSE)
C.CH4e<-lmer(C.CH4a~DO_mg_l+T_water+(1|Point)+(1|Date), data=floating3, REML=FALSE)

CH4_C1<-lmer(C.CH4a~type+(1|Point)+(1|Date), data=floating_Cavemount, REML=FALSE)
CH4_C2<-lmer(C.CH4a~1+(1|Point)+(1|Date), data=floating_Cavemount, REML=FALSE)
anova(CH4_C1, CH4_C2)


floating2$C.CO2a<-log(floating2$C.CO2+1)
floating2$C.CO2b<-sqrt(floating2$C.CO2+1)
floating2$C.CO2c<-1/(floating2$C.CO2+1)
floating2$CUBtrCO2<- (floating2$C.CO2+1)^(1/3)

floating3$C.CH4a<-log(floating3$C.CH4+2)
floating3$C.CH4b<-sqrt(floating3$C.CH4+2)
floating3$C.CH4c<-1/(floating3$C.CH4+2)

floating_Cavemount$C.CH4a<-log(floating_Cavemount$C.CH4+2)
floating_Cavemount$C.CH4b<-sqrt(floating_Cavemount$C.CH4+2)
floating_Cavemount$C.CH4c<-1/(floating_Cavemount$C.CH4+2)

####################
diagnostics<- fortify(CO2c)
p1 <- ggplot(diagnostics, aes(x = .fitted, y = .resid))+
  geom_point()+geom_hline(yintercept = 0, linetype = "dashed")+
  theme_bw()

p2 <- ggplot(diagnostics, aes(sample = .scresid))+
  stat_qq()+
  geom_abline()+
  theme_bw()
grid.arrange(p1,p2, ncol = 2)


hist(floating_CO2$C.CO2c, breaks=15)
lines(floating_CO2$C.CO2c)

hist(floating_Cavemount$C.CH4a, breaks=10)
lines(floating_Cavemount$C.CH4a)

par(mfrow=c(2,2))
plot(mod)

par(mfrow=c(1,1))
qqPlot(mod, labels=row.names(means), envelope=.95, main="Q-Q Plot")

cd<-cooks.distance(CH4_C1)
summary(cd)
plot(cd)

lev<-hat(model.matrix(CH4_C1))
plot(resid(CH4_C1, type="pearson")~lev,las=1, ylab='Standardised residuals', xlab="Leverage")

#############

E.<-filter(meansfloatingall, type=="Eriophorum")
W.<-filter(meansfloatingall, type!="Eriophorum")

CUBtrCH4all<-sign(meansfloatingall$CH4_mean) * abs(meansfloatingall$CH4_mean)^(1/3)
CUBtrCO2all<-sign(meansfloatingall$CO2_mean) * abs(meansfloatingall$CO2_mean)^(1/3)
CUBtrCH4E<-sign(E.$CH4_mean) * abs(E.$CH4_mean)^(1/3)
CUBtrCO2E<-sign(E.$CO2_mean) * abs(E.$CO2_mean)^(1/3)
CUBtrCH4W<-sign(W.$CH4_mean) * abs(W.$CH4_mean)^(1/3)
CUBtrCO2W<-sign(W.$CO2_mean) * abs(W.$CO2_mean)^(1/3)


Ch4model1<-lm(CUBtrCH4W  ~ site + T_water_mean, data=W.)
Ch4model2<-lm(CUBtrCH4E ~ T_water_mean, data=E.)
Ch4model3<-lm(CUBtrCH4all  ~ site + DO_mean, data=meansfloatingall)
Co2model<-lm(CUBtrCO2W  ~ site, data=W.)

par(mfrow=c(2,2))
plot(Ch4model3)

#######################

floating$Point <- recode_factor(floating$Point, '2a' = "2", '3a'='3')
floating<-filter(floating, C.CO2!="NA" & C.CH4!="NA" & Point!="T")
floatingDO<-filter(floating, DO_mg_l!="NA")
floatingT<-filter(floating, T_water!="NA")
floating2<-filter(floating, DO_mg_l!="NA" & T_water!="NA")
W.<-filter(floating2, type!="Eriophorum")
E.<-filter(floating2, type=="Eriophorum")

CUBtrCH4_2<-sign(floating2$C.CH4) * abs(floating2$C.CH4)^(1/3)
CUBtrCO2_2<-sign(floating2$C.CO2) * abs(floating2$C.CO2)^(1/3)
CUBtrCH4_E<-sign(E.$C.CH4) * abs(E.$C.CH4)^(1/3)
CUBtrCO2_E<-sign(E.$C.CO2) * abs(E.$C.CO2)^(1/3)
W.$CUBtrCH4_W<-sign(W.$C.CH4) * abs(W.$C.CH4)^(1/3)
W.$CUBtrCO2_W<-sign(W.$C.CO2) * abs(W.$C.CO2)^(1/3)


#modCH4<-lmer(CUBtrCH4_2~ type+ (1|site/Point), data=floating2, REML=FALSE)

#flux estimation: 
#1 predict - correlation T air - T water -> tower T air data to predict water fluxes
#1a r2
#1b correlation used : T air - T water
#1c prediction might not work for E - PAR from tower ?
#2 interpolation 

cor(x, y, method = c("pearson"))

modCH4W<-lmer(CUBtrCH4_W~ DO_mg_l+T_water+site+(1|Point)+(1|Date), data=W., REML=FALSE)
modCH4E<-lmer(CUBtrCH4_E~ T_water+ (1|Point)+(1|Date), data=E., REML=FALSE)
modCH4<-lmer(CUBtrCH4_W~ DO_mg_l+T_water+(1|Point), data=W., REML=FALSE)

modCO2W<-lmer(CUBtrCO2_W~ DO_mg_l+T_water+site+(1|Point)+(1|Date), data=W., REML=FALSE)
modCO2E<-lmer(CUBtrCO2_E~ T_water+(1|Point), data=E., REML=FALSE)
modCO2<-lmer(CUBtrCO2_W~ DO_mg_l+T_water+(1|Point)+(1|Date), data=W., REML=FALSE)

modCH4W2<-lmer(CUBtrCH4_W~ DO_mg_l+T_water+(1|Point)+(1|Date), data=W., REML=FALSE)
modCH4W3<-lmer(CUBtrCH4_W~ DO_mg_l+site+(1|Point)+(1|Date), data=W., REML=FALSE)
modCH4W4<-lmer(CUBtrCH4_W~ T_water+site+(1|Point)+(1|Date), data=W., REML=FALSE)
modCH4E2<-lmer(CUBtrCH4_E~ T_water+(1|Point)+(1|Date), data=E., REML=FALSE)
modCH4E3<-lmer(CUBtrCH4_E~ DO_mg_l+(1|Point)+(1|Date), data=E., REML=FALSE)
modCO2W2<-lmer(CUBtrCO2_W~ DO_mg_l+T_water+(1|Point)+(1|Date), data=W., REML=FALSE)
modCO2W3<-lmer(CUBtrCO2_W~ DO_mg_l+site+(1|Point)+(1|Date), data=W., REML=FALSE)
modCO2W4<-lmer(CUBtrCO2_W~ T_water+site+(1|Point)+(1|Date), data=W., REML=FALSE)
modCO2E2<-lmer(CUBtrCO2_E~ DO_mg_l+(1|Point)+(1|Date), data=E., REML=FALSE)
modCO2E3<-lmer(CUBtrCO2_E~ T_water+(1|Point)+(1|Date), data=E., REML=FALSE)

anova(modCH4W,)
anova(modCH4E,)
anova(modCO2W, )
anova(modCO2E, )


diagnostics<- fortify(modCO2E)
p1 <- ggplot(diagnostics, aes(x = .fitted, y = .resid))+
  geom_point()+geom_hline(yintercept = 0, linetype = "dashed")+
  theme_bw()

p2 <- ggplot(diagnostics, aes(sample = .scresid))+
  stat_qq()+
  geom_abline()+
  theme_bw()
grid.arrange(p1,p2, ncol = 2)


hist(W.$CUBtrCH4_W, breaks=15)
lines(W.$CUBtrCH4_W)

vif(modCH4)






C.CH4a<-lmer(C.CH4~T_water+site+type+(1|Point)+(1|Date), data=floating_we, REML=FALSE)
C.CH4a<-lm(CUBtrCH4~water_T_mean+site+type, data=meansfloating)
C.CO2a<-lm(CUBtrCO2~water_T_mean+site+type, data=meansfloating)

str(floating_we)
summary(floating_we)

floating_we<-filter(floating, C.CO2!="NA" & C.CH4!="NA" & T_water!="NA")
