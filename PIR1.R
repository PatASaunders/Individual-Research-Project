library(AER)
library("readxl")
library(stargazer)
library(ggplot2)
library(olsrr)
library(mlogit)

data = read_excel("C:/Users/Patrice/Desktop/Classes/PIR/Dataset_V1.2.xlsx")
view(data)

model = lm(Mix_Conso~Eng_Disp, data=data)
model
coeftest(model, vcov. = vcovHC, type="HC1")

plot(log(City_Conso)~Eng_Disp,
     col="steelblue",
     pch=20,
     data=data,
     main="log of City_conso and Eng_Disp")

plot(x=data$Eng_Disp,
     y=data$City_conso,
     xlab = "Engine Displacememnt",
     ylab= "Consommation en Ville",
     main="Scatterplot of Consommation and Engine Displacement")


abline(model)
summary = summary(model)
summary

transmod = factor(data$Trans)
transmod

lComb=log(data$Comb_conso)

model2=lm(log(Comb_conso)~factor(Trans), data=data)
model2



model_Dconso=lm(log(Dconso)~Eng_Disp+nCyl+nGears+Stop_Start+Fuel_Bin, data=data)
model_Dconso
coeftest(model_Dconso, vcov.=vcovHC, type="HC1")

model_CityConso=lm(log(City_Conso)~Eng_Disp+nCyl+nGears+Stop_Start+Fuel_Bin, data=data)
model_CityConso
coeftest(model_CityConso, vcov. = vcovHC, type="HC1")
model_HwyConso=lm(log(Hwy_Conso)~Eng_Disp+nCyl+nGears+Stop_Start+Fuel_Bin, data=data)
model_HwyConso
model_MixConso=lm(log(Mix_Conso)~Eng_Disp+nCyl+nGears+Stop_Start+Fuel_Bin, data=data)
model_MixConso
coeftest(model_MixConso, vcov. = vcovHC, type="HC1")


coeftest(model_HwyConso, vcov. = vcovHC, type="HC1")
coeftest(model_CityConso, vcov. = vcovHC, type="HC1")

ols_test_breusch_pagan(model_MixConso)

model_CityConso2=lm(log(Hwy_Conso)~AMS+AM+SA+M+A+SCV+CVT+Fuel_Bin, data=data)
model_CityConso2

model_HwyConso2=lm(log(City_Conso)~AMS+AM+SA+M+A+SCV+CVT, data=data)
model_HwyConso2

lCity_Conso = log(data$City_Conso)

qplot(Trans, City_Conso,data=data)

model1<-lm(log(City_Conso)~AMS+AM+SA+A+SCV+CVT, data=data)
model1

# CO2 ET CONSO

df_essence <- subset(data, Essence==1)
df_diesel <- subset(data, Essence == 0)
view(df_essence)

model_MixEss = lm(Mix_Conso~CombCO2KM, data=df_essence)
model_MixEss
model_MixDie = lm(Mix_Conso~CombCO2KM, data=df_diesel)
model_MixDie

data$Essence<- as.factor(data$Essence)
SP_CombC_T <- ggplot(data, aes(x=CombCO2KM, y=Mix_Conso, color=Essence)) + 
  geom_point() + 
  geom_abline(intercept = -0.01614, slope=0.04271) + 
  geom_abline(intercept = -0.22091, slope=0.03803)
SP_CombC_T

model_HwyEss = lm(Hwy_Conso~HCO2KM, data=df_essence)
model_HwyEss
model_HwyDie = lm(Hwy_Conso~HCO2KM, data=df_diesel)
model_HwyDie
model_CityEss = lm(City_Conso~CCO2KM, data=df_essence)
model_CityEss
model_CityDie = lm(City_Conso~CCO2KM, data=df_diesel)
model_CityDie

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

