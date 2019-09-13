#03_analysis

#Loading Libraries
library(openxlsx)
library(ggplot2)
library(nlme)
library(car)
library(lsmeans)
library(ez)
library(multcomp)

#Setting working directory
setwd("X:/CommonPrograms/Oral Health Services Research Core/Spiro/DuneganKara")

#Absorption data

absorptiondat <- readRDS("absorptiondat.rds")

##ANCOVA
ggplot(absorptiondat, aes(x=PreMass, y=PostMass, color=Method)) +
  geom_point() +
  #geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  xlab("Absorption Before (g)")+
  ylab("Absorption After (g)")

mod.11 = lm(PostMass ~ PreMass + Method + PreMass:Method,
              data = absorptiondat)

Anova(mod.11, type="II")

summary(mod.11)

mod.12 = lm(PostMass ~ PreMass + Method,
              data = absorptiondat)

Anova(mod.12, type="II")

summary(mod.12)

ggplot(absorptiondat, aes(x=PreMass, y=PostMass, color=Method)) +
  geom_point() +
  geom_abline(intercept = -0.022382+-0.005443, slope = 1.075758+-0.002866, color="blue")+
  geom_abline(intercept = -0.022382, slope = 1.075758,color="red")+
  xlab("Absorption Before (g)")+
  ylab("Absorption After (g)")

par(mfrow=c(1,2))
plot(mod.12)[2]
par(mfrow=c(1,1))

plot(fitted(mod.12), 
     residuals(mod.12))

#HV data
hvdat <- readRDS("hvdat.rds")

hvdat$Time <- as.numeric(hvdat$Replicate)
mod.21 <- aov(HV ~ Method + Error(ID/Method), data = hvdat)
summary(mod.21)

pairwise.t.test(hvdat$HV, hvdat$Method, 
                paired = TRUE, 
                p.adjust.method = "bonferroni")

mod.22 <- lme(HV ~ Method,
              random = ~ 1 | ID,
              data=hvdat,
              method="ML")
summary(mod.22)

multils <- lsmeans(mod.22, ~ Method, 
                   mult.name = "response", 
                   adjust="bonferroni")
test(contrast(multils, "pairwise"),  adjust = "bonferroni")
