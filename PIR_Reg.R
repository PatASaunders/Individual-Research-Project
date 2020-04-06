library(AER)
library("readxl")
library(stargazer)
library(ggplot2)
library(olsrr)
library(mlogit)
library(het.test)
library(ggpubr)
library(knitr)
library(GGally)
library(dplyr)
library(ggpubr)
library(mvnormtest)
library(robustbase)
library(e1071)

data = read_excel("C:/Users/Patrice/Desktop/Classes/PIR/PIR/Dataset_V1.2.xlsx")

data$Essence<- as.factor(data$Essence)
data$Trans = factor(data$Trans)



###############################################################RESTART###################################################

#BASIC REG (Auto, Manual, Deux_Roues, Forced_Induction)

First_RegM <- lm(log(Mix_Conso) ~ EngCyl + Truck + Essence + Manual + Forced_Induction + Stop_Start + Deux_Roues, data=data)

hist(residuals(First_RegM), freq=FALSE)
curve(dnorm(x, mean=0, sd = sqrt(var(residuals(First_RegM)))), col="red", lwd=2, add=TRUE)
#remarque une distribution assez normale
shapiro.test(residuals(First_RegM))
#p-value rejeté mais car nous avons un grand nombre d'observations nous pouvons ignorer ce résultat

summary(First_RegM)

bptest(First_RegM)

First_RegMR <- lm(log(Mix_Conso) ~ EngCyl + Truck + Essence + Auto + Forced_Induction + Stop_Start + Deux_Roues, data=data)
summary(First_RegMR)
vif(First_RegMR)


#######SECOND#######################################################################

Second_RegM <- lm(log(Mix_Conso) ~ EngCyl + Essence + AM_ALL + CVT_ALL + SA + A + Forced_Induction + MSFI + SIDPI + CRDDI + Stop_Start + DriveFWD + DriveRWD, data=data)
summary(Second_RegM)

df_essence <- subset(data, Essence == 1)

Second_RegM <- lm(log(Mix_Conso) ~ EngCyl + AM_ALL + CVT_ALL + SA + A + Forced_Induction + MSFI + SIDPI + Stop_Start + DriveFWD + DriveRWD, data=df_essence)

hist(residuals(Second_RegM), freq=FALSE)
curve(dnorm(x, mean=0, sd = sqrt(var(residuals(Second_RegM)))), col="red", lwd=2, add=TRUE)

bptest(Second_RegM)
summary(Second_RegM)

Second_RegMR <- lm(log(Mix_Conso) ~ EngCyl + AM_ALL + CVT_ALL + SA + A + Forced_Induction + MSFI + SIDPI + Stop_Start + DriveFWD + DriveRWD, data=df_essence)

vif(Second_RegMR)

summary(Second_RegMR)
#gagné pas mal de R2 et R2a


###################THIRD###############################################################################

Third_RegM <- lm(log(Mix_Conso) ~ EngCyl + BHP_KG + AM_ALL + CVT_ALL + SA + A + TC + SC + MSFI + SIDPI + Stop_Start + DriveFWD + DriveRWD, data=df_essence)

hist(residuals(Third_RegM), freq=FALSE)
curve(dnorm(x, mean=0, sd = sqrt(var(residuals(Third_RegM)))), col="red", lwd=2, add=TRUE)
bptest(Third_RegM)

Third_RegMR <- lm(log(Mix_Conso) ~ EngCyl + BHP + KG + AM_ALL + CVT_ALL + SA + A + TC + SC + MSFI + SIDPI + Stop_Start + DriveFWD + DriveRWD, data=df_essence)

vif(Third_RegMR)

summary(Third_RegMR)

#on a gagné un petit peut sur le R2 et R2a en ajoutant les types d'aspiration

###############THIRD MULTIPLE##########################################################################

Third_RegC <- lm(log(City_Conso) ~ EngCyl + KGBHP + AM_ALL + CVT_ALL + SA + A + TC + SC + MSFI + SIDPI + Stop_Start + DriveFWD + DriveRWD, data=df_essence)

hist(residuals(Third_RegC), freq=FALSE)
curve(dnorm(x, mean=0, sd = sqrt(var(residuals(Third_RegC)))), col="red", lwd=2, add=TRUE)
bptest(Third_RegC)

Third_RegH <- lm(log(Hwy_Conso) ~ EngCyl + KGBHP + AM_ALL + CVT_ALL + SA + A + TC + SC + MSFI + SIDPI + Stop_Start + DriveFWD + DriveRWD, data=df_essence)

hist(residuals(Third_RegH), freq=FALSE)
curve(dnorm(x, mean=0, sd = sqrt(var(residuals(Third_RegH)))), col="red", lwd=2, add=TRUE)
bptest(Third_RegH)

Third_RegCR <- lm(log(City_Conso) ~ EngCyl + KG + BHP + AM_ALL + CVT_ALL + SA + A + TC + SC + MSFI + SIDPI + Stop_Start + DriveFWD + DriveRWD, data=df_essence)
Third_RegHR <- lm(log(Hwy_Conso) ~ EngCyl + KG + BHP + AM_ALL + CVT_ALL + SA + A + TC + SC + MSFI + SIDPI + Stop_Start + DriveFWD + DriveRWD, data=df_essence)

vif(Third_RegCR)
vif(Third_RegHR)

summary(Third_RegCR)
summary(Third_RegHR)

df_small <- subset(df_essence, Eng_Disp < 2)
df_nsmall <- subset(df_essence, Eng_Disp >= 2)

thrdRS  <- lm(log(Mix_Conso) ~ EngCyl + KG + BHP + AM_ALL + CVT_ALL + SA + A + TC + MSFI + Stop_Start + DriveFWD + DriveRWD, data=df_small)
thrdRnS <- lm(log(Mix_Conso) ~ EngCyl + KG + BHP + AM_ALL + CVT_ALL + SA + A + TC + SC + MSFI + SIDPI + Stop_Start + DriveFWD + DriveRWD, data=df_nsmall)

summary(thrdRS)
summary(thrdRnS)








########################################################
###########  Mix_Conso = Eng_Disp + e  ###############

model = lm(Mix_Conso~Eng_Disp, data=data)
model
coeftest(model, vcov. = vcovHC, type="HC1")

plot(x=data$Eng_Disp,
     y=data$Mix_Conso,
     pch=20,
     col="steelblue",
     xlab = "Engine Displacememnt",
     ylab= "Consommation en Ville",
     main="Scatterplot of Consommation and Engine Displacement")
abline(model)

summary = summary(model)
summary

########################################################
###########  ln(Mix_Conso) = Trans + e  ##############



model2=lm(log(Mix_Conso)~factor(Trans), data=data)
model2

#####################################################################################
##########  ln(DConso) = Eng_Disp + nCyl + nGears + Stop_Start + Essence + e  ######
##########  aussi avec ln(City_Conso) et ln(Hwy_Conso)  #############################

model_DConso=lm(log(DConso)~Eng_Disp+nCyl+nGears+Stop_Start+Essence, data=data)
model_DConso
coeftest(model_DConso, vcov.=vcovHC, type="HC1")

model_CityConso=lm(log(City_Conso)~Eng_Disp+nCyl+nGears+Stop_Start+Essence, data=data)
model_CityConso
coeftest(model_CityConso, vcov. = vcovHC, type="HC1")
model_HwyConso=lm(log(Hwy_Conso)~Eng_Disp+nCyl+nGears+Stop_Start+Essence, data=data)
model_HwyConso
model_MixConso=lm(log(Mix_Conso)~Eng_Disp+nCyl+nGears+Stop_Start+Essence, data=data)
model_MixConso
coeftest(model_MixConso, vcov. = vcovHC, type="HC1")

coeftest(model_HwyConso, vcov. = vcovHC, type="HC1")
coeftest(model_CityConso, vcov. = vcovHC, type="HC1")

ols_test_breusch_pagan(model_MixConso)

#####################################################################
#############  ln(Hwy/City_Conso) = AMS+AM+SA+M+A+SCV+CVT+Essence  ##
#############  - Enlever une des Trans pour la comparaison  #########

model_CityConso2=lm(log(Hwy_Conso)~AMS+AM+SA+M+A+SCV+CVT+Essence, data=data)
model_CityConso2

model_HwyConso2=lm(log(City_Conso)~AMS+AM+SA+M+A+SCV+CVT, data=data)
model_HwyConso2

qplot(Trans, City_Conso,data=data)


# CO2 ET CONSO

#creating 2 different subsets for petrol and diesel

df_essence <- subset(data, Essence == 1)
df_diesel  <- subset(data, Essence == 0)

# all the different City/Mix/Hwy Consos and with either petrol or diesel
model_MixEss = lm(Mix_Conso~CombCO2KM, data=df_essence)
model_MixEss

model_MixDie = lm(Mix_Conso~CombCO2KM, data=df_diesel)
model_MixDie

model_HwyEss  = lm(Hwy_Conso~HCO2KM, data=df_essence)
model_HwyEss

model_HwyDie  = lm(Hwy_Conso~HCO2KM, data=df_diesel)
model_HwyDie

model_CityEss = lm(City_Conso~CCO2KM, data=df_essence)
model_CityEss

model_CityDie = lm(City_Conso~CCO2KM, data=df_diesel)
model_CityDie

#graphs of each

SP_CombC_T <- ggplot(data, aes(x=MixCO2KM, y=Mix_Conso, color=Essence)) + 
  geom_point() + 
  geom_abline(intercept = -0.01614, slope=0.04271) + 
  geom_abline(intercept = -0.22091, slope=0.03803)
SP_CombC_T

SP_HC_T <- ggplot(data, aes(x=HCO2KM, y=Hwy_Conso, color=Essence)) + 
  geom_point() + 
  geom_abline(intercept = -0.004264, slope=0.042691) + 
  geom_abline(intercept = -0.33585, slope=0.03871)
SP_HC_T

SP_CC_T <- ggplot(data, aes(x=CCO2KM, y=City_Conso, color=Essence)) + 
  geom_point() + 
  geom_abline(intercept = -0.009926, slope=0.042716) + 
  geom_abline(intercept = -0.14757, slope=0.03772)
SP_CC_T

# Most interesting regression:

mainmodel_C <- lm(log(City_Conso)~Eng_Disp + nCyl + kg + Intake_Valves + Stop_Start + Exhaust_Valves + Cyl_Deact + Var_Valve_Timing, data=df_small)
mainmodel_C

ols_test_breusch_pagan(mainmodel_C)


#### REGRESSIONS ################################################
#################################################################

DconsoReg <- lm(DConso ~ EngCyl + AMS + AM + SA + A + SCV + CVT + TC + SC + OT + TS + MSFI + SIDPI + DriveAWD + Drive4WD + DrivePTA, data=df_essence)
DconsoReg
anova(DconsoReg)
coeftest(DconsoReg, vcov. = vcovHC, type="HC1")

#### SMALL_REG ##########################
#########################################


small_reg <- lm(log(Mix_Conso) ~ EngCyl + AMS + AM + SA + A + SCV + CVT + Forced_Induction + deuxroues + MSFI+ SIDPI+CRDDI+Stop_Start, data=df_small)
small_reg

ols_test_breusch_pagan(small_reg)
coeftest(small_reg, vcov. = vcovHC, type="HC1")


#Regression entre City et Highway et comparer les coefficients avec le test z
#Regression entre types de transmission
#montrer a quel moyen un turbo reduit la consommation

cor.test(df_small$TC, df_small$City_Conso, method="pearson")

#condition pour regrouper des variables indicatrices, en variables binomiales?
#turbo sur les petites voitures, correlation entre turbo et puissance

summary(data)



