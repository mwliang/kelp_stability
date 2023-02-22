rm(list=ls())
#setwd("")

############################################# Local scale ####################################################          
##############################################################################################################
KFCD_local.data <- read.csv('KFCD_div_sta_local_ln.csv',header=T)
variable.names(KFCD_local.data)
summary(KFCD_local.data$times)

year_7_local.data <- KFCD_local.data[grep("7-year", KFCD_local.data$year_intervals),]

Fish.data <- year_7_local.data[grep("FISH", year_7_local.data$groups),]
Se_inv.data <- year_7_local.data[grep("SESSILE INVERT", year_7_local.data$groups),]
Mo_inv.data <- year_7_local.data[grep("MOBILE INVERT", year_7_local.data$groups),]
Alga.data <- year_7_local.data[grep("UNDERSTORY ALGAE", year_7_local.data$groups),]

##############################################################################################
# richness based
# 1. comparing the effects of heatwave on the diversity-stability relationships at local scale
# 1.1 Fish 
###################################################################### population stability
Fish_pop_sta_rich.aov <- aov(spe_sta ~ alphaD*times, data = Fish.data)
summary(Fish_pop_sta_rich.aov)
# Assess normality of residuals
hist(Fish_pop_sta_rich.aov$residuals)
# Inspect the model diagnostic metrics
par(mfrow=c(2,2))
plot(Fish_pop_sta_rich.aov, which=1:4)

######################################################################  species asynchrony
Fish_spe_asy_rich.aov <- aov(spe_asy ~ alphaD*times, data = Fish.data)
summary(Fish_spe_asy_rich.aov)
# Assess normality of residuals
hist(Fish_spe_asy_rich.aov$residuals)
# Inspect the model diagnostic metrics
par(mfrow=c(2,2))
plot(Fish_spe_asy_rich.aov, which=1:4)

######################################################################  alpha stability
Fish_alpha_sta_rich.aov <- aov(alpha_sta ~ alphaD*times, data = Fish.data)
summary(Fish_alpha_sta_rich.aov)
# Assess normality of residuals
hist(Fish_alpha_sta_rich.aov$residuals)
# Inspect the model diagnostic metrics
par(mfrow=c(2,2))
plot(Fish_alpha_sta_rich.aov, which=1:4)

# 1.2 Sessile invertebrates 
######################################################################  population stability
Se_inv_pop_sta_rich.aov <- aov(spe_sta ~ alphaD*times, data = Se_inv.data)
summary(Se_inv_pop_sta_rich.aov)
# Assess normality of residuals
hist(Se_inv_pop_sta_rich.aov$residuals)
# Inspect the model diagnostic metrics
par(mfrow=c(2,2))
plot(Se_inv_pop_sta_rich.aov, which=1:4)

######################################################################  species asynchrony
Se_inv_spe_asy_rich.aov <- aov(spe_asy ~ alphaD*times, data = Se_inv.data)
summary(Se_inv_spe_asy_rich.aov)
# Assess normality of residuals
hist(Se_inv_spe_asy_rich.aov$residuals)
# Inspect the model diagnostic metrics
par(mfrow=c(2,2))
plot(Se_inv_spe_asy_rich.aov, which=1:4)

######################################################################  alpha stability
Se_inv_alpha_sta_rich.aov <- aov(alpha_sta ~ alphaD*times, data = Se_inv.data)
summary(Se_inv_alpha_sta_rich.aov)
# Assess normality of residuals
hist(Se_inv_alpha_sta_rich.aov$residuals)
# Inspect the model diagnostic metrics
par(mfrow=c(2,2))
plot(Se_inv_alpha_sta_rich.aov, which=1:4)

# 1.3 Mobile invertebrates 
######################################################################  population stability
Mo_inv_pop_sta_rich.aov <- aov(spe_sta ~ alphaD*times, data = Mo_inv.data)
summary(Mo_inv_pop_sta_rich.aov)
# Assess normality of residuals
hist(Mo_inv_pop_sta_rich.aov$residuals)
# Inspect the model diagnostic metrics
par(mfrow=c(2,2))
plot(Mo_inv_pop_sta_rich.aov, which=1:4)

######################################################################  species asynchrony
Mo_inv_spe_asy_rich.aov <- aov(spe_asy ~ alphaD*times, data = Mo_inv.data)
summary(Mo_inv_spe_asy_rich.aov)
# Assess normality of residuals
hist(Mo_inv_spe_asy_rich.aov$residuals)
# Inspect the model diagnostic metrics
par(mfrow=c(2,2))
plot(Mo_inv_spe_asy_rich.aov, which=1:4)

######################################################################  alpha stability
Mo_inv_alpha_sta_rich.aov <- aov(alpha_sta ~ alphaD*times, data = Mo_inv.data)
summary(Mo_inv_alpha_sta_rich.aov)
# Assess normality of residuals
hist(Mo_inv_alpha_sta_rich.aov$residuals)
# Inspect the model diagnostic metrics
par(mfrow=c(2,2))
plot(Mo_inv_alpha_sta_rich.aov, which=1:4)

# 1.4 Understory Algae 
######################################################################  population stability
Alga_pop_sta_rich.aov <- aov(spe_sta ~ alphaD*times, data = Alga.data)
summary(Alga_pop_sta_rich.aov)
# Assess normality of residuals
hist(Alga_pop_sta_rich.aov$residuals)
# Inspect the model diagnostic metrics
par(mfrow=c(2,2))
plot(Alga_pop_sta_rich.aov, which=1:4)

######################################################################  species asynchrony
Alga_spe_asy_rich.aov <- aov(spe_asy ~ alphaD*times, data = Alga.data)
summary(Alga_spe_asy_rich.aov)
# Assess normality of residuals
hist(Alga_spe_asy_rich.aov$residuals)
# Inspect the model diagnostic metrics
par(mfrow=c(2,2))
plot(Alga_spe_asy_rich.aov, which=1:4)

######################################################################  alpha stability
Alga_alpha_sta_rich.aov <- aov(alpha_sta ~ alphaD*times, data = Alga.data)
summary(Alga_alpha_sta_rich.aov)
# Assess normality of residuals
hist(Alga_alpha_sta_rich.aov$residuals)
# Inspect the model diagnostic metrics
par(mfrow=c(2,2))
plot(Alga_alpha_sta_rich.aov, which=1:4)

##############################################################################################
# 1/Simpson based
# 1. comparing the effects of heatwave on the diversity-stability relationships at local scale
# 1.1 Fish 
#######################################################################  population stability
Fish_pop_sta_simp.aov <- aov(spe_sta ~ alphaD2*times, data = Fish.data)
summary(Fish_pop_sta_simp.aov)
# Assess normality of residuals
hist(Fish_pop_sta_simp.aov$residuals)
# Inspect the model diagnostic metrics
par(mfrow=c(2,2))
plot(Fish_pop_sta_simp.aov, which=1:4)

#######################################################################  species asynchrony
Fish_spe_asy_simp.aov <- aov(spe_asy ~ alphaD2*times, data = Fish.data)
summary(Fish_spe_asy_simp.aov)
# Assess normality of residuals
hist(Fish_spe_asy_simp.aov$residuals)
# Inspect the model diagnostic metrics
par(mfrow=c(2,2))
plot(Fish_spe_asy_simp.aov, which=1:4)

#######################################################################  alpha stability
Fish_alpha_sta_simp.aov <- aov(alpha_sta ~ alphaD2*times, data = Fish.data)
summary(Fish_alpha_sta_simp.aov)
# Assess normality of residuals
hist(Fish_alpha_sta_simp.aov$residuals)
# Inspect the model diagnostic metrics
par(mfrow=c(2,2))
plot(Fish_alpha_sta_simp.aov, which=1:4)

# 1.2 Sessile invertebrates 
#######################################################################  population stability
Se_inv_pop_sta_simp.aov <- aov(spe_sta ~ alphaD2*times, data = Se_inv.data)
summary(Se_inv_pop_sta_simp.aov)
# Assess normality of residuals
hist(Se_inv_pop_sta_simp.aov$residuals)
# Inspect the model diagnostic metrics
par(mfrow=c(2,2))
plot(Se_inv_pop_sta_simp.aov, which=1:4)

#######################################################################  species asynchrony
Se_inv_spe_asy_simp.aov <- aov(spe_asy ~ alphaD2*times, data = Se_inv.data)
summary(Se_inv_spe_asy_simp.aov)
# Assess normality of residuals
hist(Se_inv_spe_asy_simp.aov$residuals)
# Inspect the model diagnostic metrics
par(mfrow=c(2,2))
plot(Se_inv_spe_asy_simp.aov, which=1:4)

#######################################################################  alpha stability
Se_inv_alpha_sta_simp.aov <- aov(alpha_sta ~ alphaD2*times, data = Se_inv.data)
summary(Se_inv_alpha_sta_simp.aov)
# Assess normality of residuals
hist(Se_inv_alpha_sta_simp.aov$residuals)
# Inspect the model diagnostic metrics
par(mfrow=c(2,2))
plot(Se_inv_alpha_sta_simp.aov, which=1:4)

# 1.3 Mobile invertebrates 
#######################################################################  population stability
Mo_inv_pop_sta_simp.aov <- aov(spe_sta ~ alphaD2*times, data = Mo_inv.data)
summary(Mo_inv_pop_sta_simp.aov)
# Assess normality of residuals
hist(Mo_inv_pop_sta_simp.aov$residuals)
# Inspect the model diagnostic metrics
par(mfrow=c(2,2))
plot(Mo_inv_pop_sta_simp.aov, which=1:4)

#######################################################################  species asynchrony
Mo_inv_spe_asy_simp.aov <- aov(spe_asy ~ alphaD2*times, data = Mo_inv.data)
summary(Mo_inv_spe_asy_simp.aov)
# Assess normality of residuals
hist(Mo_inv_spe_asy_simp.aov$residuals)
# Inspect the model diagnostic metrics
par(mfrow=c(2,2))
plot(Mo_inv_spe_asy_simp.aov, which=1:4)

#######################################################################  alpha stability
Mo_inv_alpha_sta_simp.aov <- aov(alpha_sta ~ alphaD2*times, data = Mo_inv.data)
summary(Mo_inv_alpha_sta_simp.aov)
# Assess normality of residuals
hist(Mo_inv_alpha_sta_simp.aov$residuals)
# Inspect the model diagnostic metrics
par(mfrow=c(2,2))
plot(Mo_inv_alpha_sta_simp.aov, which=1:4)

# 1.4 Understory Algae 
#######################################################################  population stability
Alga_pop_sta_simp.aov <- aov(spe_sta ~ alphaD2*times, data = Alga.data)
summary(Alga_pop_sta_simp.aov)
# Assess normality of residuals
hist(Alga_pop_sta_simp.aov$residuals)
# Inspect the model diagnostic metrics
par(mfrow=c(2,2))
plot(Alga_pop_sta_simp.aov, which=1:4)

#######################################################################  species asynchrony
Alga_spe_asy_simp.aov <- aov(spe_asy ~ alphaD2*times, data = Alga.data)
summary(Alga_spe_asy_simp.aov)
# Assess normality of residuals
hist(Alga_spe_asy_simp.aov$residuals)
# Inspect the model diagnostic metrics
par(mfrow=c(2,2))
plot(Alga_spe_asy_simp.aov, which=1:4)

#######################################################################  alpha stability
Alga_alpha_sta_simp.aov <- aov(alpha_sta ~ alphaD2*times, data = Alga.data)
summary(Alga_alpha_sta_simp.aov)
# Assess normality of residuals
hist(Alga_alpha_sta_simp.aov$residuals)
# Inspect the model diagnostic metrics
par(mfrow=c(2,2))
plot(Alga_alpha_sta_simp.aov, which=1:4)


############################################# Regional scale ################################################# 
##############################################################################################################
KFCD_regional.data <- read.csv('KFCD_div_sta_regional_ln.csv',header=T)
variable.names(KFCD_regional.data)
summary(KFCD_regional.data$time)

year_7_regional.data <- KFCD_regional.data[grep("7-year", KFCD_regional.data$year_intervals),]

Fish_regional.data <- year_7_regional.data[grep("FISH", year_7_regional.data$groups),]
Se_inv_regional.data <- year_7_regional.data[grep("SESSILE INVERT", year_7_regional.data$groups),]
Mo_inv_regional.data <- year_7_regional.data[grep("MOBILE INVERT", year_7_regional.data$groups),]
Alga_regional.data <- year_7_regional.data[grep("UNDERSTORY ALGAE", year_7_regional.data$groups),]
##############################################################################################################

# 2. comparing the effects of heatwave on the diversity-stability relationships at broad scale
# 2.1 Fish 
#######################################################################  spatial asynchrony
Fish_spa_asy_rich.aov <- aov(spe_asy ~ betaD*times, data = Fish_regional.data)
summary(Fish_spa_asy_rich.aov)
# Assess normality of residuals
hist(Fish_spa_asy_rich.aov$residuals)
# Inspect the model diagnostic metrics
par(mfrow=c(2,2))
plot(Fish_spa_asy_rich.aov, which=1:4)

#######################################################################  gamma stability
Fish_gamma_sta_rich.aov <- aov(gamma_sta ~ gammaD*times, data = Fish_regional.data)
summary(Fish_gamma_sta_rich.aov)
# Assess normality of residuals
hist(Fish_gamma_sta_rich.aov$residuals)
# Inspect the model diagnostic metrics
par(mfrow=c(2,2))
plot(Fish_gamma_sta_rich.aov, which=1:4)

# 2.2 Sessile invertebrates
#######################################################################  spatial asynchrony
Se_inv_spa_asy_rich.aov <- aov(spe_asy ~ betaD*times, data = Se_inv_regional.data)
summary(Se_inv_spa_asy_rich.aov)
# Assess normality of residuals
hist(Se_inv_spa_asy_rich.aov$residuals)
# Inspect the model diagnostic metrics
par(mfrow=c(2,2))
plot(Se_inv_spa_asy_rich.aov, which=1:4)

#######################################################################  gamma stability
Se_inv_gamma_sta_rich.aov <- aov(gamma_sta ~ gammaD*times, data = Se_inv_regional.data)
summary(Se_inv_gamma_sta_rich.aov)
# Assess normality of residuals
hist(Se_inv_gamma_sta_rich.aov$residuals)
# Inspect the model diagnostic metrics
par(mfrow=c(2,2))
plot(Se_inv_gamma_sta_rich.aov, which=1:4)

# 2.3 Mobile invertebrates
#######################################################################  spatial asynchrony
Mo_inv_spa_asy_rich.aov <- aov(spe_asy ~ betaD*times, data = Mo_inv_regional.data)
summary(Mo_inv_spa_asy_rich.aov)
# Assess normality of residuals
hist(Mo_inv_spa_asy_rich.aov$residuals)
# Inspect the model diagnostic metrics
par(mfrow=c(2,2))
plot(Mo_inv_spa_asy_rich.aov, which=1:4)

#######################################################################  gamma stability
Mo_inv_gamma_sta_rich.aov <- aov(gamma_sta ~ gammaD*times, data = Mo_inv_regional.data)
summary(Mo_inv_gamma_sta_rich.aov)
# Assess normality of residuals
hist(Mo_inv_gamma_sta_rich.aov$residuals)
# Inspect the model diagnostic metrics
par(mfrow=c(2,2))
plot(Mo_inv_gamma_sta_rich.aov, which=1:4)

# 2.4 Understory Algae  
#######################################################################  spatial asynchrony
Alga_spa_asy_rich.aov <- aov(spe_asy ~ betaD*times, data = Alga_regional.data)
summary(Alga_spa_asy_rich.aov)
# Assess normality of residuals
hist(Alga_spa_asy_rich.aov$residuals)
# Inspect the model diagnostic metrics
par(mfrow=c(2,2))
plot(Alga_spa_asy_rich.aov, which=1:4)

#######################################################################  gamma stability
Alga_gamma_sta_rich.aov <- aov(gamma_sta ~ gammaD*times, data = Alga_regional.data)
summary(Alga_gamma_sta_rich.aov)
# Assess normality of residuals
hist(Alga_gamma_sta_rich.aov$residuals)
# Inspect the model diagnostic metrics
par(mfrow=c(2,2))
plot(Alga_gamma_sta_rich.aov, which=1:4)

##############################################################################################
# 1/Simpson based
# 2. comparing the effects of heatwave on the diversity-stability relationships at broad scale
# 2.1 Fish 
#######################################################################  spatial asynchrony
Fish_spa_asy_simp.aov <- aov(spe_asy ~ betaD2*times, data = Fish_regional.data)
summary(Fish_spa_asy_simp.aov)
# Assess normality of residuals
hist(Fish_spa_asy_simp.aov$residuals)
# Inspect the model diagnostic metrics
par(mfrow=c(2,2))
plot(Fish_spa_asy_simp.aov, which=1:4)

#######################################################################  gamma stability
Fish_gamma_sta_simp.aov <- aov(gamma_sta ~ gammaD2*times, data = Fish_regional.data)
summary(Fish_gamma_sta_simp.aov)
# Assess normality of residuals
hist(Fish_gamma_sta_simp.aov$residuals)
# Inspect the model diagnostic metrics
par(mfrow=c(2,2))
plot(Fish_gamma_sta_simp.aov, which=1:4)

# 2.2 Sessile invertebrates
#######################################################################  spatial asynchrony
Se_inv_spa_asy_simp.aov <- aov(spe_asy ~ betaD2*times, data = Se_inv_regional.data)
summary(Se_inv_spa_asy_simp.aov)
# Assess normality of residuals
hist(Se_inv_spa_asy_simp.aov$residuals)
# Inspect the model diagnostic metrics
par(mfrow=c(2,2))
plot(Se_inv_spa_asy_simp.aov, which=1:4)

#######################################################################  gamma stability
Se_inv_gamma_sta_simp.aov <- aov(gamma_sta ~ gammaD2*times, data = Se_inv_regional.data)
summary(Se_inv_gamma_sta_simp.aov)
# Assess normality of residuals
hist(Se_inv_gamma_sta_simp.aov$residuals)
# Inspect the model diagnostic metrics
par(mfrow=c(2,2))
plot(Se_inv_gamma_sta_simp.aov, which=1:4)

# 2.3 Mobile invertebrates
#######################################################################  spatial asynchrony
Mo_inv_spa_asy_simp.aov <- aov(spe_asy ~ betaD2*times, data = Mo_inv_regional.data)
summary(Mo_inv_spa_asy_simp.aov)
# Assess normality of residuals
hist(Mo_inv_spa_asy_simp.aov$residuals)
# Inspect the model diagnostic metrics
par(mfrow=c(2,2))
plot(Mo_inv_spa_asy_simp.aov, which=1:4)

#######################################################################  gamma stability
Mo_inv_gamma_sta_simp.aov <- aov(gamma_sta ~ gammaD2*times, data = Mo_inv_regional.data)
summary(Mo_inv_gamma_sta_simp.aov)
# Assess normality of residuals
hist(Mo_inv_gamma_sta_simp.aov$residuals)
# Inspect the model diagnostic metrics
par(mfrow=c(2,2))
plot(Mo_inv_gamma_sta_simp.aov, which=1:4)

# 2.4 Understory Algae  
#######################################################################  spatial asynchrony
Alga_spa_asy_simp.aov <- aov(spe_asy ~ betaD2*times, data = Alga_regional.data)
summary(Alga_spa_asy_simp.aov)
# Assess normality of residuals
hist(Alga_spa_asy_simp.aov$residuals)
# Inspect the model diagnostic metrics
par(mfrow=c(2,2))
plot(Alga_spa_asy_simp.aov, which=1:4)

#######################################################################  gamma stability
Alga_gamma_sta_simp.aov <- aov(gamma_sta ~ gammaD2*times, data = Alga_regional.data)
summary(Alga_gamma_sta_simp.aov)
# Assess normality of residuals
hist(Alga_gamma_sta_simp.aov$residuals)
# Inspect the model diagnostic metrics
par(mfrow=c(2,2))
plot(Alga_gamma_sta_simp.aov, which=1:4)



