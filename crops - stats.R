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

str(co2)
summary(co2)
co2$day <- as.factor(co2$day)

###########################

full<-lmer(CO2~WFPS+ ST+ MBC+(1|Rep2)+(1|day), data=co2, REML=FALSE)

mod1<-lmer(CO2~WFPS+ ST+ MBC+(1|Rep2), data=co2, REML=FALSE)
mod1a<-lmer(CO2~WFPS+ MBC+(1|Rep2), data=co2, REML=FALSE)
mod1b<-lmer(CO2~WFPS+ ST+(1|Rep2), data=co2, REML=FALSE)
mod1c<-lmer(CO2~ST+ MBC+(1|Rep2), data=co2, REML=FALSE)
anova(mod1, mod1a)
anova(mod1, mod1b)
anova(mod1, mod1c)

diagnostics<- fortify(mod1)
p1 <- ggplot(diagnostics, aes(x = .fitted, y = .resid))+
  geom_point()+geom_hline(yintercept = 0, linetype = "dashed")+
  theme_bw()

p2 <- ggplot(diagnostics, aes(sample = .scresid))+
  stat_qq()+
  geom_abline()+
  theme_bw()
grid.arrange(p1,p2, ncol = 2)

vif(mod1)
###############

full<-lmer(N2O~WFPS+ ST+ MBC+(1|Rep2)+(1|day), data=co2, REML=FALSE)

mod2<-lmer(N2O~WFPS+ ST+ MBC+(1|Rep2), data=co2, REML=FALSE)
mod2a<-lmer(N2O~WFPS+ MBC+(1|Rep2), data=co2, REML=FALSE)
mod2b<-lmer(N2O~WFPS+ ST+(1|Rep2), data=co2, REML=FALSE)
mod2c<-lmer(N2O~ST+ MBC+(1|Rep2), data=co2, REML=FALSE)
anova(mod2, mod2a)
anova(mod2, mod2b)
anova(mod2, mod2c)

diagnostics<- fortify(mod2)
p1 <- ggplot(diagnostics, aes(x = .fitted, y = .resid))+
  geom_point()+geom_hline(yintercept = 0, linetype = "dashed")+
  theme_bw()

p2 <- ggplot(diagnostics, aes(sample = .scresid))+
  stat_qq()+
  geom_abline()+
  theme_bw()
grid.arrange(p1,p2, ncol = 2)

vif(mod2)

###########################

a<-lmer(CO2~Crop+Treatment +WFPS+ST+MBC+
             (1|Crop:Rep2) +(1|day), data=co2, REML=FALSE)


full<-lmer(CO2~Crop*Treatment + WFPS + WFPS:Crop +WFPS:Treatment+ ST +ST:Crop+ 
             ST:Treatment+ MBC+ MBC:Crop + MBC:Treatment + 
             (1|Crop:Rep2) +(1|day), data=co2, REML=FALSE)


modMBC<-lmer(CO2~Crop*Treatment + WFPS + WFPS:Crop +WFPS:Treatment+ ST +ST:Crop+ 
             ST:Treatment+ MBC+ 
             (1|Crop:Rep2) +(1|day), data=co2, REML=FALSE)
modMBC2<-lmer(CO2~Crop*Treatment + WFPS + WFPS:Crop +WFPS:Treatment+ ST +ST:Crop+ 
               ST:Treatment+ 
               (1|Crop:Rep2) +(1|day), data=co2, REML=FALSE)
modST<-lmer(CO2~Crop*Treatment + WFPS + WFPS:Crop +WFPS:Treatment+ ST + 
              MBC+ MBC:Crop + MBC:Treatment + 
              (1|Crop:Rep2) +(1|day), data=co2, REML=FALSE)
modST2<-lmer(CO2~Crop*Treatment + WFPS + WFPS:Crop +WFPS:Treatment+ 
               MBC+ MBC:Crop + MBC:Treatment + 
              (1|Crop:Rep2) +(1|day), data=co2, REML=FALSE)
modWFPS<-lmer(CO2~Crop*Treatment + WFPS + ST +ST:Crop+ 
               ST:Treatment+ MBC+ MBC:Crop + MBC:Treatment + 
               (1|Crop:Rep2) +(1|day), data=co2, REML=FALSE)
modWFPS2<-lmer(CO2~Crop*Treatment + ST + ST:Crop + ST:Treatment+ MBC+ MBC:Crop + MBC:Treatment + 
                 (1|Crop:Rep2) +(1|day), data=co2, REML=FALSE)

modCrop<-lmer(CO2~Crop+Treatment + WFPS + WFPS:Treatment+ ST + ST:Treatment+ 
                MBC+ MBC:Treatment + (1|Crop:Rep2) +(1|day), data=co2, REML=FALSE)
modCrop2<-lmer(CO2~Treatment + WFPS + WFPS:Treatment+ ST + ST:Treatment+ 
                 MBC+ MBC:Treatment + (1|Crop:Rep2) +(1|day), data=co2, REML=FALSE)
modTreatment<-lmer(CO2~Crop+Treatment + WFPS + WFPS:Crop +ST +ST:Crop+ MBC+ MBC:Crop + 
               (1|Crop:Rep2) +(1|day), data=co2, REML=FALSE)
modTreatment2<-lmer(CO2~Crop + WFPS + WFPS:Crop +ST +ST:Crop+ MBC+ MBC:Crop + 
                     (1|Crop:Rep2) +(1|day), data=co2, REML=FALSE)

#############################################################
mod00<-lmer(CO2~Crop+Treatment + WFPS + ST + MBC+ 
              (1|Crop:Rep2) +(1|day), data=co2, REML=FALSE)

mod002<-lmer(CO2~Crop+Treatment + WFPS + ST + MBC+ 
              (1|Crop:Rep2), data=co2, REML=FALSE)

mod00b<-lmer(CO2~Crop+Treatment + WFPS  + MBC+ 
              (1|Crop:Rep2), data=co2, REML=FALSE)

mod00c<-lmer(CO2~Crop+Treatment + WFPS  + ST +
               (1|Crop:Rep2), data=co2, REML=FALSE)

mod00d<-lmer(CO2~Crop+Treatment + ST + MBC +
               (1|Crop:Rep2), data=co2, REML=FALSE)

mod00e<-lmer(CO2~Treatment + WFPS + ST + MBC+ 
               (1|Crop:Rep2) , data=co2, REML=FALSE)

mod00f<-lmer(CO2~Crop + WFPS + ST + MBC+ 
               (1|Crop:Rep2) , data=co2, REML=FALSE)

anova(mod002, mod00b)
anova(mod002, mod00c)
anova(mod002, mod00d)
anova(mod002, mod00e)
anova(mod002, mod00f)

full<-lmer(CO2~Crop*Treatment + WFPS + WFPS:Crop +WFPS:Treatment+ ST +ST:Crop+ 
             ST:Treatment+ MBC+ MBC:Crop + MBC:Treatment + 
             (1|Crop:Rep2), data=co2, REML=FALSE)

modCrop<-lmer(CO2~Crop+Treatment + WFPS + WFPS:Crop +WFPS:Treatment+ ST +ST:Crop+ 
             ST:Treatment+ MBC+ MBC:Crop + MBC:Treatment + 
             (1|Crop:Rep2), data=co2, REML=FALSE)

modMBC1<-lmer(CO2~Crop*Treatment + WFPS + WFPS:Crop +WFPS:Treatment+ ST +ST:Crop+ 
             ST:Treatment+ MBC+ MBC:Crop + 
             (1|Crop:Rep2), data=co2, REML=FALSE)

modMBC2<-lmer(CO2~Crop*Treatment + WFPS + WFPS:Crop +WFPS:Treatment+ ST +ST:Crop+ 
             ST:Treatment+ MBC+ MBC:Treatment + 
             (1|Crop:Rep2), data=co2, REML=FALSE)

modST1<-lmer(CO2~Crop*Treatment + WFPS + WFPS:Crop +WFPS:Treatment+ ST +ST:Crop+ 
          MBC+ MBC:Crop + MBC:Treatment + 
             (1|Crop:Rep2), data=co2, REML=FALSE)

modST2<-lmer(CO2~Crop*Treatment + WFPS + WFPS:Crop +WFPS:Treatment+ ST +
             ST:Treatment+ MBC+ MBC:Crop + MBC:Treatment + 
             (1|Crop:Rep2), data=co2, REML=FALSE)

modWFPS1<-lmer(CO2~Crop*Treatment + WFPS + WFPS:Crop +ST +ST:Crop+ 
             ST:Treatment+ MBC+ MBC:Crop + MBC:Treatment + 
             (1|Crop:Rep2), data=co2, REML=FALSE)

modWFPS2<-lmer(CO2~Crop*Treatment + WFPS + WFPS:Treatment+ ST +ST:Crop+ 
             ST:Treatment+ MBC+ MBC:Crop + MBC:Treatment + 
             (1|Crop:Rep2), data=co2, REML=FALSE)

anova(full, modCrop)
anova(full, modMBC1)
anova(full, modMBC2)
anova(full, modST1)
anova(full, modST2)
anova(full, modWFPS1)
anova(full, modWFPS2)

diagnostics<- fortify(full)
p1 <- ggplot(diagnostics, aes(x = .fitted, y = .resid))+
  geom_point()+geom_hline(yintercept = 0, linetype = "dashed")+
  theme_bw()

p2 <- ggplot(diagnostics, aes(sample = .scresid))+
  stat_qq()+
  geom_abline()+
  theme_bw()
grid.arrange(p1,p2, ncol = 2)

vif(mod002)

summary(glht(mod002, linfct=mcp(Treatment="Tukey")))

summary(glht(full, linfct=mcp(Crop="Tukey", interaction_average = TRUE)))
TukeyHSD(full)

summary(glht(model, lsm(pairwise ~ factorB | factorA)))

lsmeans(mod002, pairwise~Treatment)



#############################

modN<-lmer(N2O~Crop+Treatment +WFPS+ST+MBC+
          (1|Crop:Rep2), data=co2, REML=FALSE)

modNb<-lmer(N2O~Crop+Treatment +WFPS+ST+
             (1|Crop:Rep2), data=co2, REML=FALSE)

modNc<-lmer(N2O~Crop+Treatment +WFPS+MBC+
             (1|Crop:Rep2), data=co2, REML=FALSE)

modNd<-lmer(N2O~Crop+Treatment +ST+MBC+
             (1|Crop:Rep2), data=co2, REML=FALSE)

modNe<-lmer(N2O~Treatment +ST+MBC+ WFPS +
              (1|Crop:Rep2), data=co2, REML=FALSE)

modNf<-lmer(N2O~Crop+ST+MBC+ WFPS+
              (1|Crop:Rep2), data=co2, REML=FALSE)

anova(modN, modNb)
anova(modN, modNc)
anova(modN, modNd)
anova(modN, modNe)
anova(modN, modNf)

fullN<-lmer(N2O~Crop*Treatment + WFPS + WFPS:Crop +WFPS:Treatment+ ST +ST:Crop+ 
             ST:Treatment+ MBC+ MBC:Crop + MBC:Treatment + 
             (1|Crop:Rep2), data=co2, REML=FALSE)

modCropN<-lmer(N2O~Crop+Treatment + WFPS + WFPS:Crop +WFPS:Treatment+ ST +ST:Crop+ 
                ST:Treatment+ MBC+ MBC:Crop + MBC:Treatment + 
                (1|Crop:Rep2), data=co2, REML=FALSE)

modMBC1N<-lmer(N2O~Crop*Treatment + WFPS + WFPS:Crop +WFPS:Treatment+ ST +ST:Crop+ 
                ST:Treatment+ MBC+ MBC:Crop + 
                (1|Crop:Rep2), data=co2, REML=FALSE)

modMBC2N<-lmer(N2O~Crop*Treatment + WFPS + WFPS:Crop +WFPS:Treatment+ ST +ST:Crop+ 
                ST:Treatment+ MBC+ MBC:Treatment + 
                (1|Crop:Rep2), data=co2, REML=FALSE)

modST1N<-lmer(N2O~Crop*Treatment + WFPS + WFPS:Crop +WFPS:Treatment+ ST +ST:Crop+ 
               MBC+ MBC:Crop + MBC:Treatment + 
               (1|Crop:Rep2), data=co2, REML=FALSE)

modST2N<-lmer(N2O~Crop*Treatment + WFPS + WFPS:Crop +WFPS:Treatment+ ST +
               ST:Treatment+ MBC+ MBC:Crop + MBC:Treatment + 
               (1|Crop:Rep2), data=co2, REML=FALSE)

modWFPS1N<-lmer(N2O~Crop*Treatment + WFPS + WFPS:Crop +ST +ST:Crop+ 
                 ST:Treatment+ MBC+ MBC:Crop + MBC:Treatment + 
                 (1|Crop:Rep2), data=co2, REML=FALSE)

modWFPS2N<-lmer(N2O~Crop*Treatment + WFPS + WFPS:Treatment+ ST +ST:Crop+ 
                 ST:Treatment+ MBC+ MBC:Crop + MBC:Treatment + 
                 (1|Crop:Rep2), data=co2, REML=FALSE)

anova(fullN, modCropN)
anova(fullN, modMBC1N)
anova(fullN, modMBC2N)
anova(fullN, modST1N)
anova(fullN, modST2N)
anova(fullN, modWFPS1N)
anova(fullN, modWFPS2N)

diagnostics<- fortify(fullN)
p1 <- ggplot(diagnostics, aes(x = .fitted, y = .resid))+
  geom_point()+geom_hline(yintercept = 0, linetype = "dashed")+
  theme_bw()

p2 <- ggplot(diagnostics, aes(sample = .scresid))+
  stat_qq()+
  geom_abline()+
  theme_bw()
grid.arrange(p1,p2, ncol = 2)

vif(modN)

summary(glht(modN, linfct=mcp(Treatment="Tukey")))

######################

mod1<-lmer(CO2~Crop*Treatment + WFPS + WFPS:Crop +WFPS:Treatment+ ST +ST:Crop+ 
             ST:Treatment+ MBC+ MBC:Crop + 
             (1|Crop:Rep2) +(1|day), data=co2, REML=FALSE)
mod2<-lmer(CO2~Crop*Treatment + WFPS + WFPS:Crop +WFPS:Treatment+ ST +ST:Crop+ 
             ST:Treatment+ MBC+ MBC:Treatment + 
             (1|Crop:Rep2) +(1|day), data=co2, REML=FALSE)
mod3<-lmer(CO2~Crop*Treatment + WFPS + WFPS:Crop +WFPS:Treatment+ ST +ST:Crop+ 
             MBC+ MBC:Crop + MBC:Treatment + 
             (1|Crop:Rep2) +(1|day), data=co2, REML=FALSE)
mod4<-lmer(CO2~Crop*Treatment + WFPS + WFPS:Crop +WFPS:Treatment+ ST + 
             ST:Treatment+ MBC+ MBC:Crop + MBC:Treatment + 
             (1|Crop:Rep2) +(1|day), data=co2, REML=FALSE)
mod5<-lmer(CO2~Crop*Treatment + WFPS + WFPS:Treatment+ ST +ST:Crop+ 
             ST:Treatment+ MBC+ MBC:Crop + MBC:Treatment + 
             (1|Crop:Rep2) +(1|day), data=co2, REML=FALSE)
mod6<-lmer(CO2~Crop*Treatment + WFPS + WFPS:Crop +ST +ST:Crop+ 
             ST:Treatment+ MBC+ MBC:Crop + MBC:Treatment + 
             (1|Crop:Rep2) +(1|day), data=co2, REML=FALSE)
mod7<-lmer(CO2~Crop+Treatment + WFPS + WFPS:Crop +WFPS:Treatment+ ST +ST:Crop+ 
             ST:Treatment+ MBC+ MBC:Crop + MBC:Treatment + 
             (1|Crop:Rep2) +(1|day), data=co2, REML=FALSE)


anova(full, mod1)
anova(full, mod2)
anova(full, mod3)
anova(full, mod4)
anova(full, mod5)
anova(full, mod6)
anova(full, mod7)

anova(modMBC, modMBC2)
anova(modST, modST2)
anova(modWFPS, modWFPS2)
anova(modCrop, modCrop2)
anova(modTreatment, modTreatment2)



diagnostics<- fortify(modST)
p1 <- ggplot(diagnostics, aes(x = .fitted, y = .resid))+
  geom_point()+geom_hline(yintercept = 0, linetype = "dashed")+
  theme_bw()

p2 <- ggplot(diagnostics, aes(sample = .scresid))+
  stat_qq()+
  geom_abline()+
  theme_bw()
grid.arrange(p1,p2, ncol = 2)

vif(a)

########################################

diagnostics<- fortify(full2)
p1 <- ggplot(diagnostics, aes(x = .fitted, y = .resid))+
  geom_point()+geom_hline(yintercept = 0, linetype = "dashed")+
  theme_bw()

p2 <- ggplot(diagnostics, aes(sample = .scresid))+
  stat_qq()+
  geom_abline()+
  theme_bw()
grid.arrange(p1,p2, ncol = 2)

vif(b)

b<-lmer(N2O~Crop+Treatment +WFPS+ST+MBC+
          (1|Crop:Rep2), data=co2, REML=FALSE)

full2a<-lmer(N2O~Crop*Treatment + WFPS + WFPS:Crop +WFPS:Treatment+ ST +ST:Crop+ 
              ST:Treatment+ MBC+ MBC:Crop + MBC:Treatment + 
              (1|Crop:Rep2) +(1|day), data=co2, REML=FALSE)

full2<-lmer(N2O~Crop*Treatment + WFPS + WFPS:Crop +WFPS:Treatment+ ST +ST:Crop+ 
             ST:Treatment+ MBC+ MBC:Crop + MBC:Treatment + 
             (1|Crop:Rep2) , data=co2, REML=FALSE)


N2OWFPS<-lmer(N2O~Crop*Treatment + WFPS + ST +ST:Crop+ ST:Treatment+ MBC+ MBC:Crop + 
                MBC:Treatment + (1|Crop:Rep2) , data=co2, REML=FALSE)

N2OWFPS2<-lmer(N2O~Crop*Treatment + ST +ST:Crop+ ST:Treatment+ MBC+ MBC:Crop + 
                 MBC:Treatment + (1|Crop:Rep2) , data=co2, REML=FALSE)

N2OST<-lmer(N2O~Crop*Treatment + WFPS + WFPS:Crop +WFPS:Treatment+ ST + 
              MBC+ MBC:Crop + MBC:Treatment + (1|Crop:Rep2) , data=co2, REML=FALSE)

N2OST2<-lmer(N2O~Crop*Treatment + WFPS + WFPS:Crop +WFPS:Treatment+ 
              MBC+ MBC:Crop + MBC:Treatment + (1|Crop:Rep2) , data=co2, REML=FALSE)

N2OMBC<-lmer(N2O~Crop*Treatment + WFPS + WFPS:Crop +WFPS:Treatment+ ST +ST:Crop+ 
              ST:Treatment+ MBC+ (1|Crop:Rep2) , data=co2, REML=FALSE)

N2OMBC2<-lmer(N2O~Crop*Treatment + WFPS + WFPS:Crop +WFPS:Treatment+ ST +ST:Crop+ 
               ST:Treatment+ (1|Crop:Rep2) , data=co2, REML=FALSE)

N2OCrop<-lmer(N2O~Crop+Treatment + WFPS + WFPS:Treatment+ ST + ST:Treatment+ 
                MBC+ MBC:Treatment + (1|Crop:Rep2), data=co2, REML=FALSE)
N2OCrop2<-lmer(N2O~Treatment + WFPS + WFPS:Treatment+ ST + ST:Treatment+ 
                 MBC+ MBC:Treatment + (1|Crop:Rep2), data=co2, REML=FALSE)
N2OTreatment<-lmer(N2O~Crop+Treatment + WFPS + WFPS:Crop +ST +ST:Crop+ MBC+ MBC:Crop + 
                     (1|Crop:Rep2), data=co2, REML=FALSE)
N2OTreatment2<-lmer(N2O~Crop + WFPS + WFPS:Crop +ST +ST:Crop+ MBC+ MBC:Crop + 
                      (1|Crop:Rep2), data=co2, REML=FALSE)

N2O1<-lmer(N2O~Crop*Treatment + WFPS + WFPS:Crop +WFPS:Treatment+ ST +ST:Crop+ 
             ST:Treatment+ MBC+ MBC:Crop + (1|Crop:Rep2), data=co2, REML=FALSE)
N2O2<-lmer(N2O~Crop*Treatment + WFPS + WFPS:Crop +WFPS:Treatment+ ST +ST:Crop+ 
             ST:Treatment+ MBC+ MBC:Treatment + (1|Crop:Rep2), data=co2, REML=FALSE)
N2O3<-lmer(N2O~Crop*Treatment + WFPS + WFPS:Crop +WFPS:Treatment+ ST +ST:Crop+ 
             MBC+ MBC:Crop + MBC:Treatment + (1|Crop:Rep2), data=co2, REML=FALSE)
N2O4<-lmer(N2O~Crop*Treatment + WFPS + WFPS:Crop +WFPS:Treatment+ ST + 
             ST:Treatment+ MBC+ MBC:Crop + MBC:Treatment + 
             (1|Crop:Rep2), data=co2, REML=FALSE)
N2O5<-lmer(N2O~Crop*Treatment + WFPS + WFPS:Treatment+ ST +ST:Crop+ 
             ST:Treatment+ MBC+ MBC:Crop + MBC:Treatment + 
             (1|Crop:Rep2), data=co2, REML=FALSE)
N2O6<-lmer(N2O~Crop*Treatment + WFPS + WFPS:Crop +ST +ST:Crop+ ST:Treatment+ 
             MBC+ MBC:Crop + MBC:Treatment + (1|Crop:Rep2), data=co2, REML=FALSE)
N2O7<-lmer(N2O~Crop+Treatment + WFPS + WFPS:Crop +WFPS:Treatment+ ST +ST:Crop+ 
             ST:Treatment+ MBC+ MBC:Crop + MBC:Treatment + 
             (1|Crop:Rep2), data=co2, REML=FALSE)

anova(full2, N2O1)
anova(full2, N2O2)
anova(full2, N2O3)
anova(full2, N2O4)
anova(full2, N2O5)
anova(full2, N2O6)
anova(full2, N2O7)

anova(N2OMBC, N2OMBC2)
anova(N2OST, N2OST2)
anova(N2OWFPS, N2OWFPS2)
anova(N2OCrop, N2OCrop2)
anova(N2OTreatment, N2OTreatment2)

############################

summary(glht(modCrop, linfct=mcp(Crop="Tukey")))
summary(glht(N2OTreatment, linfct=mcp(Treatment="Tukey")))

