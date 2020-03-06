library(AER)
library("readxl")
library(stargazer)
library(ggplot2)
library(olsrr)
library(mlogit)
library(tidyverse)
library(plotly)
library(gridExtra)
library(plyr)

#data = read_excel("C:/Users/Patrice/Desktop/Classes/PIR/Dataset_V1.2.xlsx")
data =  read_excel("C:/Users/Clément/Desktop/Master MBFA/PIR/Dataset_V1.2.xlsx")
View(data)

###################################################################################################################
################### Difference de Consommmation entre Essence et Diesel (City/Highway) ############################
###################################################################################################################

data$Essence <- as.factor(data$Essence)

BP_CC_E <- ggplot(data, aes(x=Essence, y=City_Conso)) + geom_boxplot(coef=6)

BP_HC_E <- ggplot(data, aes(x=Essence, y=Hwy_Conso)) + geom_boxplot(coef=6)

grid.arrange(BP_CC_E, BP_HC_E, nrow=1, ncol=2)

df_MC_E = data[c("Essence", "Mix_Conso")]
ddply(df_MC_E,~Essence,summarise,mean=mean(Mix_Conso),sd=sd(Mix_Conso))

#d + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.1, na.rm = TRUE)
#d + geom_jitter(shape=16, position=position_jitter(0.5), na.rm = TRUE)


#ajouter percentiles different et les mettres a coter

########### Commentaires
#Tres peu de voitures diesel dans l'echantillon (30)
#Mais techniquement, + de compression dans les moteurs diesels donc - de conso

##############################################################
#################### TRANS et conso ##########################
##############################################################

data$Trans<-as.factor(data$Trans)
BP_HC_T <- ggplot(data, aes(x=Trans, y=City_Conso)) + geom_boxplot(coef=6)

BP_CC_T <- ggplot(data, aes(x=Trans, y=Hwy_Conso)) + geom_boxplot(coef=6)

grid.arrange(BP_HC_T, BP_CC_T, nrow=1, ncol=2)

df_MC_Trans = data[c("Trans","Mix_Conso")]
ddply(df_MC_Trans,~Trans,summarise,mean=mean(Mix_Conso),sd=sd(Mix_Conso))

############ Commentaires
# A > AMS > SA > M > AM > SCV > CVT

###########################################################################
#################### Type d'Injection et Conso ############################
###########################################################################

data$Fuel_Metering_Sys<-as.factor(data$Fuel_Metering_Sys)
BP_CC_I <- ggplot(data, aes(x=Fuel_Metering_Sys, y=City_Conso)) + geom_boxplot(coef=6)

BP_HC_I <- ggplot(data, aes(x=Fuel_Metering_Sys, y=Hwy_Conso)) + geom_boxplot(coef=6)

grid.arrange(BP_HC_I, BP_CC_I, nrow=1, ncol=2)

df_MC_I = data[c("Fuel_Metering_Sys","Mix_Conso")]
ddply(df_MC_I,~Fuel_Metering_Sys,summarise,mean=mean(Mix_Conso),sd=sd(Mix_Conso))

############# Commentaire
#difference tres legere, SIDPI semble le moins consomer (CRDDI est pour les diesels)

############################################################################
##################### TAILLE MOTEUR NCYL et Conso ##########################
############################################################################

ggplot(data, aes(x=Eng_Disp, y=City_Conso, z=nCyl), type="scatter3d")+geom_point()

data$nCyl <- as.factor(data$nCyl)
ggp_MC_E_C <- ggplot(data, aes(x=Eng_Disp, y=Mix_Conso, color=nCyl)) + geom_point()
ggp_MC_E_C

d <- plot_ly(data=data, x=~Eng_Disp, y=~City_Conso, type="scatter", mode="markers")
d

############# Commentaires
#Techniquement, le nombre de cylindre augmente un peu la consommation, car moins de couple a bas regime, plus de pieces en mouvement, plus de frottements ...
#Techniquement, un plus gros moteur consomme plus, il faut plus de carburant pour remplir les cylindres
#Techniquement, pour les voitures, plus gros est le moteur et plus il y a de cylindres, pour des raisons de confort et de conduite
#Ici, je pense que la taille du moteur joue plus sur la conso que le nb de cylindre
#Donc, je pense qu'il soit plus pertinant de regarder la taille du moteur que le nombre de cylindre.

############################################################
##################### Poids et Conso########################
############################################################
ggp_MC_kg <- ggplot(data, aes(x=kg, y=Mix_Conso)) + geom_point()
ggp_MC_kg

############ Commentaires
#L'augmentation semble lineaire

#################################################################
##################### Puissance et Conso ########################
#################################################################

ggp_MC_bhp <- ggplot(data, aes(x=bhp, y=Mix_Conso)) + geom_point()
ggp_MC_bhp

############ Commentaires
#Augmentation moins lineaire

#################################################################
##################### Groupe de voiture #########################
#################################################################

df_lgt = data[data$kg <= 1600, ]
df_hvy = data[data$kg >= 1600, ]

df_nsport = data[data$bhp <= 200, ]
df_sport = data[data$bhp >= 200, ]

df_small <- data[data$kg <= 1600 & data$bhp <=200,]
df_big   <- data[data$kg >= 1600 & data$bhp >=200,]

######### mean_SD DF
mean = c(mean(df_lgt$Mix_Conso), mean(df_hvy$Mix_Conso), mean(df_nsport$Mix_Conso) , mean(df_sport$Mix_Conso), mean(df_small$Mix_Conso), mean(df_big$Mix_Conso))
sd = c(sd(df_lgt$Mix_Conso), sd(df_hvy$Mix_Conso), sd(df_nsport$Mix_Conso), sd(df_sport$Mix_Conso), sd(df_small$Mix_Conso), sd(df_big$Mix_Conso))
DF_mean_SD_grp = data.frame(mean, sd)
rownames(DF_mean_SD_grp) = c("kg < 1600","kg > 1600","bhp < 200", "bhp > 200", "Small", "Big")
#####################

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

grid.arrange(SC_MixSmall, SC_MixBig, nrow=1, ncol=2)

########### Superposition de Hwy_Small et City_Small et pour big
SC_Small <- ggplot(data, aes(x=Eng_Disp, y=City_Conso, color=(City_Conso))) + aes(x=Eng_Disp, y=Hwy_Conso) + geom_point()
SC_Small
###########

SC_DConsoSmall <- ggplot(df_small, aes(x=Eng_Disp, y=DConso)) + geom_point()

SC_DConsoBig <- ggplot(df_big, aes(x=Eng_Disp, y=DConso)) + geom_point()

grid.arrange(SC_DConsoSmall, SC_DConsoBig, nrow=1, ncol=2)

####################################################################
###################### Roues motrices et conso #####################
####################################################################

############# En fonction de Tuck
df_truck <- subset(data, Truck==1)
df_car <- subset(data, Truck==0)

data$Drive_Desc <- as.factor(data$Drive_Desc)

SC_MCWheel <- ggplot(df_car, aes(x=Drive_Desc, y=Mix_Conso)) + geom_point()
SC_MCWheel

SC_MTWheel <- ggplot(df_truck, aes(x=Drive_Desc, y=Mix_Conso)) + geom_point()
SC_MTWheel

grid.arrange(SC_MCWheel, SC_MTWheel, nrow=1, ncol=2)

############## En fonction du poids
SC_MC_Wheel_Lgt = ggplot(df_lgt, aes(x=Drive_Desc, y=Mix_Conso)) + geom_point()
SC_MC_Wheel_hvy = ggplot(df_hvy, aes(x=Drive_Desc, y=Mix_Conso)) + geom_point()

grid.arrange(SC_MC_Wheel_Lgt, SC_MC_Wheel_hvy, nrow=1, ncol=2)

############## En fonction de la puissance
SC_MC_Wheel_nsport = ggplot(df_nsport, aes(x=Drive_Desc, y=Mix_Conso)) + geom_point()
SC_MC_Wheel_sport = ggplot(df_sport, aes(x=Drive_Desc, y=Mix_Conso)) + geom_point()

grid.arrange(SC_MC_Wheel_nsport, SC_MC_Wheel_sport, nrow=1, ncol=2)

############## Commentaires
#Techniquement, le RWD ne joue pas sur la conso, mais est utilisé sur des voitures puissantes ou a vocation sportive pour des raisons mecaniques, qui elles consomment plus
#Techniquement, La transmission integrale augmente la consommation, car : plus de poids, plus de pieces en mouvement, plus de frottements ...
#Recentes innovations qui reduisent la surconso de la transmission integrale ? 

############################################################################
###################### Air Aspiration et Conso #############################
############################################################################

######### Air Aspiration & Eng_Disp

data$Aspir_Method <- as.factor(data$Aspir_Method)
SC_MAsp <- ggplot(data, aes(x=Eng_Disp, y=Mix_Conso, color=(Aspir_Method))) + geom_point()
SC_MAsp

######### en fonction de Small
BP_MC_ASP_Small =  ggplot(df_small, aes(x=Aspir_Method, y=Mix_Conso)) + geom_boxplot(coef=6)
BP_MC_ASP_Small
ddply(df_small,~Aspir_Method,summarise,mean=mean(Mix_Conso),sd=sd(Mix_Conso))
######### en fonction de Big
BP_MC_ASP_big =  ggplot(df_big, aes(x=Aspir_Method, y=Mix_Conso)) + geom_boxplot(coef=6)
BP_MC_ASP_big
ddply(df_big,~Aspir_Method,summarise,mean=mean(Mix_Conso),sd=sd(Mix_Conso))
######### en fonction du poids
ddply(df_lgt,~Aspir_Method,summarise,mean=mean(Mix_Conso),sd=sd(Mix_Conso))
ddply(df_hvy,~Aspir_Method,summarise,mean=mean(Mix_Conso),sd=sd(Mix_Conso))
######### en fonction de BHP
ddply(df_nsport,~Aspir_Method,summarise,mean=mean(Mix_Conso),sd=sd(Mix_Conso))
ddply(df_sport,~Aspir_Method,summarise,mean=mean(Mix_Conso),sd=sd(Mix_Conso))

######### Commentaire
#pour les voitures < 1600 kg ou < 200 bhp ou "small", l'ajout d'un turbo augmente la conso tres legerement
#pour les voitures lourdes ou puissantes ou "big", l'ajout d'un turbo fait baisser la conso (DownSizing ?)
#le compresseur augmente la conso dans tous les groupes 

################################################################
###################### Cyl_Deact et conso ######################
################################################################

df_small$Cyl_Deact <- as.factor(df_small$Cyl_Deact)
BP_MC_CD = ggplot(df_small, aes(x=Cyl_Deact, y=Mix_Conso)) + geom_boxplot(coef=6)
BP_MC_CD 

######### Commentaires
#Les voitures qui utilise ce systeme sont en majorité des voitures qui consomment deja beaucoup (ex : lamborghini, aston martin, acura ...)
#difficile d'en tirer une conclusion ?

#######################################################################
###################### Var_Valve_Timing et conso ######################
#######################################################################

######### Commentaires
#Peu de voitures ne sont pas équipé de ce systeme, peut on quand meme en tirer quelque chose ?

########################################################################
###################### Stop_Start et Conso #############################
########################################################################

data$Stop_Start = as.factor(data$Stop_Start)
BP_MC_SS = ggplot(data, aes(x=Stop_Start, y=Mix_Conso)) + geom_boxplot(coef=6)
BP_MC_SS
ddply(data,~Stop_Start,summarise,mean=mean(Mix_Conso),sd=sd(Mix_Conso))

BP_CC_SS = ggplot(data, aes(x=Stop_Start, y=City_Conso)) + geom_boxplot(coef=6)
BP_CC_SS 
ddply(data,~Stop_Start,summarise,mean=mean(City_Conso),sd=sd(City_Conso))

df_small$Stop_Start = as.factor(df_small$Stop_Start)
BP_CC_SS_small = ggplot(df_small, aes(x=Stop_Start, y=City_Conso)) + geom_boxplot(coef=6)
BP_CC_SS_small
ddply(df_small,~Stop_Start,summarise,mean=mean(City_Conso),sd=sd(City_Conso))

############# Commentaires
#legere difference en ville, negligeable sinon 
