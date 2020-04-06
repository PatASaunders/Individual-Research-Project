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

thrdRCS  <- lm(log(City_Conso) ~ EngCyl + KG + BHP + AM_ALL + CVT_ALL + SA + A + TC + MSFI + Stop_Start + Deux_Roues, data=df_small)
thrdRHS  <- lm(log(Hwy_Conso) ~ EngCyl + KG + BHP + AM_ALL + CVT_ALL + SA + A + TC + MSFI + Stop_Start + Deux_Roues, data=df_small)

vif(thrdRS)

thrdRCnS <- lm(log(City_Conso) ~ EngCyl + KG + BHP + AM_ALL + CVT_ALL + SA + A + TC + SC + MSFI + SIDPI + Stop_Start + Deux_Roues, data=df_nsmall)
thrdRHnS <- lm(log(Mix_Conso) ~ EngCyl + KG + BHP + AM_ALL + CVT_ALL + SA + A + TC + SC + MSFI + SIDPI + Stop_Start + Deux_Roues, data=df_nsmall)

summary(thrdRCS)
summary(thrdRHS)
summary(thrdRCnS)
summary(thrdRHnS)

