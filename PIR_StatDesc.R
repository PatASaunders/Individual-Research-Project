library(AER)
library("readxl")
library(stargazer)
library(ggplot2)
library(olsrr)
library(mlogit)
library(tidyverse)
library(plotly)
library(mlogit)


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

# TAILLE MOTEUR NCYL

ggplot(data, aes(x=Eng_Disp, y=City_Conso, z=nCyl), type="scatter3d")+geom_point()

data$nCyl <- as.factor(data$nCyl)
p <- ggplot(data, aes(x=Eng_Disp, y=Mix_Conso, color=nCyl)) + geom_point()
p

d <- plot_ly(data=data, x=~Eng_Disp, y=~City_Conso, type="scatter", mode="markers")
d

#############################

df_small <- data[data$kg <= 1600 & data$bhp <=200,]
df_big   <- data[data$kg <= 1600 & data$bhp <=200,]

SC_MixSmall <- ggplot(df_small, aes(x=Eng_Disp, y=Mix_Conso)) + geom_point()
SC_MixSmall

SC_CitySmall <- ggplot(df_small, aes(x=Eng_Disp, y=City_Conso)) + geom_point()
SC_CitySmall
SC_HwySmall <- ggplot(df_small, aes(x=Eng_Disp, y=Hwy_Conso)) + geom_point()
SC_HwySmall

SC_MixBig   <- ggplot(df_big, aes(x=Eng_Disp, y=Mix_Conso)) + geom_point()
SC_MixBig

SC_CityBig <- ggplot(df_big, aes(x=Eng_Disp, y=City_Conso)) + geom_point()
SC_CityBig
SC_HwyBig <- ggplot(df_big, aes(x=Eng_Disp, y=Hwy_Conso)) + geom_point()
SC_HwyBig

########### Superposition de Hwy_Small et City_Small et pour big
SC_Small <- ggplot(data, aes(x=City_Conso, y=City_Conso, color=(Eng_Disp))) + aes(x=City_Conso, y=Hwy_Conso) + geom_point()
SC_Small
###########

SC_DConsoSmall <- ggplot(df_small, aes(x=Eng_Disp, y=DConso)) + geom_point()
SC_DConsoSmall

SC_DConsoBig <- ggplot(df_big, aes(x=Eng_Disp, y=DConso)) + geom_point()
SC_DConsoBig

# Roues motrices (truck et nontruck)

df_truck <- subset(data, Truck==1)
df_car <- subset(data, Truck==0)

data$Drive_Desc <- as.factor(data$Drive_Desc)

SC_MCWheel <- ggplot(df_car, aes(x=Drive_Desc, y=Mix_Conso, color=(Drive_Desc))) + geom_point()
SC_MCWheel

SC_MTWheel <- ggplot(df_truck, aes(x=Drive_Desc, y=Mix_Conso, color=(Drive_Desc))) + geom_point()
SC_MTWheel
# DATA MINING NECESSARY


# Air Aspiration & Eng_Disp

data$Aspir_Method <- as.factor(data$Aspir_Method)
SC_MAsp <- ggplot(data, aes(x=Eng_Disp, y=Mix_Conso, color=(Aspir_Method))) + geom_point()
SC_MAsp

# NCyl 


# Regression avec 'petites voitures'










