rm(list=ls())
#setwd("")

# loading packages
library(nlme)
library(MuMIn)

########################################
# stability at the local transect level
########################################################################################
KFCD_local.data <- read.csv('KFCD_div_sta_local_ln.csv',header=T)
variable.names(KFCD_local.data)
summary(KFCD_local.data$times)

sta_local.data <- KFCD_local.data[grep("7-year", KFCD_local.data$year_intervals),]

Fish.data <- sta_local.data[grep("FISH", sta_local.data$groups),]
Se_inv.data <- sta_local.data[grep("SESSILE INVERT", sta_local.data$groups),]
Mo_inv.data <- sta_local.data[grep("MOBILE INVERT", sta_local.data$groups),]
Alga.data <- sta_local.data[grep("UNDERSTORY ALGAE", sta_local.data$groups),]


########################################
# stability at the broader spatial scales 
########################################################################################
KFCD_regional.data <- read.csv('KFCD_div_sta_regional_ln.csv',header=T)
variable.names(KFCD_regional.data)
summary(KFCD_regional.data$time)

sta_regional.data <- KFCD_regional.data[grep("7-year", KFCD_regional.data$year_intervals),]

Fish_regional.data <- sta_regional.data[grep("FISH", sta_regional.data$groups),]
Se_inv_regional.data <- sta_regional.data[grep("SESSILE INVERT", sta_regional.data$groups),]
Mo_inv_regional.data <- sta_regional.data[grep("MOBILE INVERT", sta_regional.data$groups),]
Alga_regional.data <- sta_regional.data[grep("UNDERSTORY ALGAE", sta_regional.data$groups),]


########################################################################################
########################################################################################
# 1. fit the model examining changes in fish stability before vs. after the heatwave
# 1.1 population stability
spe_sta_fish.fit <- lme(spe_sta ~ time, random=list(~1|sites), na.action = na.omit, data = Fish.data)
summary(spe_sta_fish.fit)
anova(spe_sta_fish.fit)
r.squaredGLMM(spe_sta_fish.fit)
# test homoscedasticity
fligner.test(resid(spe_sta_fish.fit), Fish.data$time)
# Assess normality of residuals
hist(spe_sta_fish.fit$residuals)
# Inspect the model diagnostic metrics
plot(spe_sta_fish.fit)

# 1.2 species asynchrony
spe_asy_fish.fit <- lme(spe_asy ~ time, random=list(~1|sites), na.action = na.omit, data = Fish.data)
summary(spe_asy_fish.fit)
anova(spe_asy_fish.fit)
r.squaredGLMM(spe_asy_fish.fit)
# test homoscedasticity
fligner.test(resid(spe_asy_fish.fit), Fish.data$time)
# Assess normality of residuals
hist(spe_asy_fish.fit$residuals)
# Inspect the model diagnostic metrics
plot(spe_asy_fish.fit)

# 1.3 alpha stability
alpha_sta_fish.fit <- lme(alpha_sta ~ time, random=list(~1|sites), na.action = na.omit, data = Fish.data)
summary(alpha_sta_fish.fit)
anova(alpha_sta_fish.fit)
r.squaredGLMM(alpha_sta_fish.fit)
# test homoscedasticity
fligner.test(resid(alpha_sta_fish.fit), Fish.data$time)
# Assess normality of residuals
hist(alpha_sta_fish.fit$residuals)
# Inspect the model diagnostic metrics
plot(alpha_sta_fish.fit)

# 1.4 spatial asynchrony
summary(aov(spa_asy ~ time, Fish_regional.data))

# 1.5 gamma stability
summary(aov(gamma_sta ~ time, Fish_regional.data))


# 2. fit the model examining changes in sessile invertebrates stability before vs. after the heatwave
# 2.1 population stability
spe_sta_Se_inv.fit <- lme(spe_sta ~ time, random=list(~1|sites), na.action = na.omit, data = Se_inv.data)
summary(spe_sta_Se_inv.fit)
anova(spe_sta_Se_inv.fit)
r.squaredGLMM(spe_sta_Se_inv.fit)
# test homoscedasticity
fligner.test(resid(spe_sta_Se_inv.fit), Se_inv.data$time)
# Assess normality of residuals
hist(spe_sta_Se_inv.fit$residuals)
# Inspect the model diagnostic metrics
plot(spe_sta_Se_inv.fit)

# 2.2 species asynchrony
spe_asy_Se_inv.fit <- lme(spe_asy ~ time, random=list(~1|sites), na.action = na.omit, data = Se_inv.data)
summary(spe_asy_Se_inv.fit)
anova(spe_asy_Se_inv.fit)
r.squaredGLMM(spe_asy_Se_inv.fit)
# test homoscedasticity
fligner.test(resid(spe_asy_Se_inv.fit), Se_inv.data$time)
# Assess normality of residuals
hist(spe_asy_Se_inv.fit$residuals)
# Inspect the model diagnostic metrics
plot(spe_asy_Se_inv.fit)

# 2.3 alpha stability
alpha_sta_Se_inv.fit <- lme(alpha_sta ~ time, random=list(~1|sites), na.action = na.omit, data = Se_inv.data)
summary(alpha_sta_Se_inv.fit)
anova(alpha_sta_Se_inv.fit)
r.squaredGLMM(alpha_sta_Se_inv.fit)
# test homoscedasticity
fligner.test(resid(alpha_sta_Se_inv.fit), Se_inv.data$time)
# Assess normality of residuals
hist(alpha_sta_Se_inv.fit$residuals)
# Inspect the model diagnostic metrics
plot(alpha_sta_Se_inv.fit)

# 2.4 spatial asynchrony
summary(aov(spa_asy ~ time, Se_inv_regional.data))

# 2.5 gamma stability
summary(aov(gamma_sta ~ time, Se_inv_regional.data))


# 3. fit the model examining changes in mobile invertebrates stability before vs. after the heatwave
# 3.1 population stability
spe_sta_Mo_inv.fit <- lme(spe_sta ~ time, random=list(~1|sites), na.action = na.omit, data = Mo_inv.data)
summary(spe_sta_Mo_inv.fit)
anova(spe_sta_Mo_inv.fit)
r.squaredGLMM(spe_sta_Mo_inv.fit)
# test homoscedasticity
fligner.test(resid(spe_sta_Mo_inv.fit), Mo_inv.data$time)
# Assess normality of residuals
hist(spe_sta_Mo_inv.fit$residuals)
# Inspect the model diagnostic metrics
plot(spe_sta_Mo_inv.fit)

# 3.2 species asynchrony
spe_asy_Mo_inv.fit <- lme(spe_asy ~ time, random=list(~1|sites), na.action = na.omit, data = Mo_inv.data)
summary(spe_asy_Mo_inv.fit)
anova(spe_asy_Mo_inv.fit)
r.squaredGLMM(spe_asy_Mo_inv.fit)
# test homoscedasticity
fligner.test(resid(spe_asy_Mo_inv.fit), Mo_inv.data$time)
# Assess normality of residuals
hist(spe_asy_Mo_inv.fit$residuals)
# Inspect the model diagnostic metrics
plot(spe_asy_Mo_inv.fit)

# 3.3 alpha stability
alpha_sta_Mo_inv.fit <- lme(alpha_sta ~ time, random=list(~1|sites), na.action = na.omit, data = Mo_inv.data)
summary(alpha_sta_Mo_inv.fit)
anova(alpha_sta_Mo_inv.fit)
r.squaredGLMM(alpha_sta_Mo_inv.fit)
# test homoscedasticity
fligner.test(resid(alpha_sta_Mo_inv.fit), Mo_inv.data$time)
# Assess normality of residuals
hist(alpha_sta_Mo_inv.fit$residuals)
# Inspect the model diagnostic metrics
plot(alpha_sta_Mo_inv.fit)

# 3.4 spatial asynchrony
summary(aov(spa_asy ~ time, Mo_inv_regional.data))

# 3.5 gamma stability
summary(aov(gamma_sta ~ time, Mo_inv_regional.data))



# 4. fit the model examining changes in Understory algae stability before vs. after the heatwave
# 4.1 population stability
spe_sta_Alga.fit <- lme(spe_sta ~ time, random=list(~1|sites), na.action = na.omit, data = Alga.data)
summary(spe_sta_Alga.fit)
anova(spe_sta_Alga.fit)
r.squaredGLMM(spe_sta_Alga.fit)
# test homoscedasticity
fligner.test(resid(spe_sta_Alga.fit), Alga.data$time)
# Assess normality of residuals
hist(spe_sta_Alga.fit$residuals)
# Inspect the model diagnostic metrics
plot(spe_sta_Alga.fit)

# 4.2 species asynchrony
spe_asy_Alga.fit <- lme(spe_asy ~ time, random=list(~1|sites), na.action = na.omit, data = Alga.data)
summary(spe_asy_Alga.fit)
anova(spe_asy_Alga.fit)
r.squaredGLMM(spe_asy_Alga.fit)
# test homoscedasticity
fligner.test(resid(spe_asy_Alga.fit), Alga.data$time)
# Assess normality of residuals
hist(spe_asy_Alga.fit$residuals)
# Inspect the model diagnostic metrics
plot(spe_asy_Alga.fit)

# 4.3 alpha stability
alpha_sta_Alga.fit <- lme(alpha_sta ~ time, random=list(~1|sites), na.action = na.omit, data = Alga.data)
summary(alpha_sta_Alga.fit)
anova(alpha_sta_Alga.fit)
r.squaredGLMM(alpha_sta_Alga.fit)
# test homoscedasticity
fligner.test(resid(alpha_sta_Alga.fit), Alga.data$time)
# Assess normality of residuals
hist(alpha_sta_Alga.fit$residuals)
# Inspect the model diagnostic metrics
plot(alpha_sta_Alga.fit)

# 4.4 spatial asynchrony
summary(aov(spa_asy ~ time, Alga_regional.data))

# 4.5 gamma stability
summary(aov(gamma_sta ~ time, Alga_regional.data))
