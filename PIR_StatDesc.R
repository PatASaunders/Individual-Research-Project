library(AER)
library("readxl")
library(stargazer)
library(ggplot2)
library(olsrr)
library(mlogit)
library(tidyverse)

data = read_excel("C:/Users/Patrice/Desktop/Classes/PIR/Dataset_V1.2.xlsx")
View(data)

# Difference de Consommmation entre Essence et Diesel (City/Highway)

data$Essence <- as.factor(data$Essence)

BP_CC_E <- ggplot(data, aes(x=Essence, y=City_Conso)) + geom_boxplot(coef=6)
BP_CC_E

BP_HC_E <- ggplot(data, aes(x=Essence, y=Hwy_Conso)) + geom_boxplot(coef=6)
BP_HC_E

#d + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.1, na.rm = TRUE)
#d + geom_jitter(shape=16, position=position_jitter(0.5), na.rm = TRUE)


#ajouter percentiles different et les mettres a coter


# TRANS

data$Trans<-as.factor(data$Trans)
BP_HC_T <- ggplot(data, aes(x=Trans, y=City_Conso)) + geom_boxplot(coef=6)
BP_HC_T

BP_CC_T <- ggplot(data, aes(x=Trans, y=Hwy_Conso)) + geom_boxplot(coef=6)
BP_CC_T

# Type d'Injection

data$Fuel_Metering_Sys<-as.factor(data$Fuel_Metering_Sys)
BP_CC_I <- ggplot(data, aes(x=Fuel_Metering_Sys, y=City_Conso)) + geom_boxplot(coef=6)
BP_CC_I

BP_HC_I <- ggplot(data, aes(x=Fuel_Metering_Sys, y=Hwy_Conso)) + geom_boxplot(coef=6)
BP_HC_I

#Rechercher si tout les diesels utilisent CRDDI!!

# TAILLE MOTEUR NCYL






