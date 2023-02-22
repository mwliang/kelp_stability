rm(list=ls())
#setwd(" ")

# loading packages
library(nlme)
library(MuMIn)

########################################
# biodiversity at the local transect level
########################################################################################
KFCD_local.data <- read.csv('KFCD_div_sta_local_ln.csv',header=T)
variable.names(KFCD_local.data)
summary(KFCD_local.data$times)

div_local.data <- KFCD_local.data[grep("7-year", KFCD_local.data$year_intervals),]

Fish.data <- div_local.data[grep("FISH", div_local.data$groups),]
Se_inv.data <- div_local.data[grep("SESSILE INVERT", div_local.data$groups),]
Mo_inv.data <- div_local.data[grep("MOBILE INVERT", div_local.data$groups),]
Alga.data <- div_local.data[grep("UNDERSTORY ALGAE", div_local.data$groups),]

# species richness
# 1. fit the model examining the effects of heatwave on alpha diversity (based on species richness at transect level)
# 1.1 fish 
alphaD_fish.fit <- lme(alphaD ~ time, random=list(~1|sites), na.action = na.omit, data = Fish.data)
summary(alphaD_fish.fit)
anova(alphaD_fish.fit)
r.squaredGLMM(alphaD_fish.fit)
# test homoscedasticity
fligner.test(resid(alphaD_fish.fit), Fish.data$time)
# Assess normality of residuals
hist(alphaD_fish.fit$residuals)
# Inspect the model diagnostic metrics
plot(alphaD_fish.fit)

# 1.2 Sessile invertebrates 
alphaD_Se_inv.fit <- lme(alphaD ~ time, random=list(~1|sites), na.action = na.omit, data = Se_inv.data)
summary(alphaD_Se_inv.fit)
anova(alphaD_Se_inv.fit)
r.squaredGLMM(alphaD_Se_inv.fit)
# test homoscedasticity
fligner.test(resid(alphaD_Se_inv.fit), Se_inv.data$time)
# Assess normality of residuals
hist(alphaD_Se_inv.fit$residuals)
# Inspect the model diagnostic metrics
plot(alphaD_Se_inv.fit)

# 1.3 Mobile invertebrates 
alphaD_Mo_inv.fit <- lme(alphaD ~ time, random=list(~1|sites), na.action = na.omit, data = Mo_inv.data)
summary(alphaD_Mo_inv.fit)
anova(alphaD_Mo_inv.fit)
r.squaredGLMM(alphaD_Mo_inv.fit)
# test homoscedasticity
fligner.test(resid(alphaD_Mo_inv.fit), Mo_inv.data$time)
# Assess normality of residuals
hist(alphaD_Mo_inv.fit$residuals)
# Inspect the model diagnostic metrics
plot(alphaD_Mo_inv.fit)

# 1.4 Understory algae 
alphaD_Alga.fit <- lme(alphaD ~ time, random=list(~1|sites), na.action = na.omit, data = Alga.data)
summary(alphaD_Alga.fit)
anova(alphaD_Alga.fit)
r.squaredGLMM(alphaD_Alga.fit)
# test homoscedasticity
fligner.test(resid(alphaD_Alga.fit), Alga.data$time)
# Assess normality of residuals
hist(alphaD_Alga.fit$residuals)
# Inspect the model diagnostic metrics
plot(alphaD_Alga.fit)


# species diversity
# 2. fit the model examining the effects of heatwave on alpha diversity (based on 1/Simpson at transect level)
# 2.1 fish 
alphaD2_fish.fit <- lme(alphaD2 ~ time, random=list(~1|sites), na.action = na.omit, data = Fish.data)
summary(alphaD2_fish.fit)
anova(alphaD2_fish.fit)
r.squaredGLMM(alphaD2_fish.fit)
# test homoscedasticity
fligner.test(resid(alphaD2_fish.fit), Fish.data$time)
# Assess normality of residuals
hist(alphaD2_fish.fit$residuals)
# Inspect the model diagnostic metrics
plot(alphaD2_fish.fit)

# 2.2 Sessile invertebrates 
alphaD2_Se_inv.fit <- lme(alphaD2 ~ time, random=list(~1|sites), na.action = na.omit, data = Se_inv.data)
summary(alphaD2_Se_inv.fit)
anova(alphaD2_Se_inv.fit)
r.squaredGLMM(alphaD2_Se_inv.fit)
# test homoscedasticity
fligner.test(resid(alphaD2_Se_inv.fit), Se_inv.data$time)
# Assess normality of residuals
hist(alphaD2_Se_inv.fit$residuals)
# Inspect the model diagnostic metrics
plot(alphaD2_Se_inv.fit)

# 2.3 Mobile invertebrates 
alphaD2_Mo_inv.fit <- lme(alphaD2 ~ time, random=list(~1|sites), na.action = na.omit, data = Mo_inv.data)
summary(alphaD2_Mo_inv.fit)
anova(alphaD2_Mo_inv.fit)
r.squaredGLMM(alphaD2_Mo_inv.fit)
# test homoscedasticity
fligner.test(resid(alphaD2_Mo_inv.fit), Mo_inv.data$time)
# Assess normality of residuals
hist(alphaD2_Mo_inv.fit$residuals)
# Inspect the model diagnostic metrics
plot(alphaD2_Mo_inv.fit)

# 2.4 Understory algae 
alphaD2_Alga.fit <- lme(alphaD2 ~ time, random=list(~1|sites), na.action = na.omit, data = Alga.data)
summary(alphaD2_Alga.fit)
anova(alphaD2_Alga.fit)
r.squaredGLMM(alphaD2_Alga.fit)
# test homoscedasticity
fligner.test(resid(alphaD2_Alga.fit), Alga.data$time)
# Assess normality of residuals
hist(alphaD2_Alga.fit$residuals)
# Inspect the model diagnostic metrics
plot(alphaD2_Alga.fit)



########################################
# biodiversity at the broader spatial scales 
########################################################################################
KFCD_regional.data <- read.csv('KFCD_div_sta_regional_ln.csv',header=T)
variable.names(KFCD_regional.data)
summary(KFCD_regional.data$time)

div_regional.data <- KFCD_regional.data[grep("7-year", KFCD_regional.data$year_intervals),]

Fish_regional.data <- div_regional.data[grep("FISH", div_regional.data$groups),]
Se_inv_regional.data <- div_regional.data[grep("SESSILE INVERT", div_regional.data$groups),]
Mo_inv_regional.data <- div_regional.data[grep("MOBILE INVERT", div_regional.data$groups),]
Alga_regional.data <- div_regional.data[grep("UNDERSTORY ALGAE", div_regional.data$groups),]

# gamma diversity, 
# 1. based on richness indices (species richness at site level)
summary(aov(gammaD ~ time, Fish_regional.data))
summary(aov(gammaD ~ times, Se_inv_regional.data))
summary(aov(gammaD ~ time, Mo_inv_regional.data))
summary(aov(gammaD ~ time, Alga_regional.data))

# 2. based on diversity indices (1/Simpson at site level)
summary(aov(gammaD2 ~ time, Fish_regional.data))
summary(aov(gammaD2 ~ time, Se_inv_regional.data))
summary(aov(gammaD2 ~ time, Mo_inv_regional.data))
summary(aov(gammaD2 ~ time, Alga_regional.data))


# beta diversity, 
# 1. based on richness indices (beta = gamma richness/alpha richness)
summary(aov(betaD ~ time, Fish_regional.data))
summary(aov(betaD ~ time, Se_inv_regional.data))
summary(aov(betaD ~ time, Mo_inv_regional.data))
summary(aov(betaD ~ time, Alga_regional.data))

# 2. based on diversity indices (beta = gamma diversity/alpha diversity)
summary(aov(betaD2 ~ time, Fish_regional.data))
summary(aov(betaD2 ~ time, Se_inv_regional.data))
summary(aov(betaD2 ~ time, Mo_inv_regional.data))
summary(aov(betaD2 ~ time, Alga_regional.data))
