library(AER)
library("readxl")
library(stargazer)
library(ggplot2)
library(olsrr)
library(mlogit)

data = read_excel("C:/Users/Patrice/Desktop/Classes/PIR/Dataset_V1.2.xlsx")
data_r <- 

data3<-data[complete.cases(data),]
data2
View(data3)

model = lm(log(City_conso)~Eng_Disp, data=data)
model
coeftest(model, vcov. = vcovHC, type="HC1")

plot(log(City_conso)~Eng_Disp,
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


