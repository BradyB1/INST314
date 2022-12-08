library (ggpubr)
library (broom)
library (AICcmodavg)
library (tidyverse)
library(ggplot2)
library(readxl)


###Section 1 
Energy <- read_excel("StateEnergyCO2EmissionsbyFuel.xlsx", skip = 3)
Energy <- head(Energy, -3)
view(Energy)

#assigning variables 
Total <- Energy$Total
state <- Energy$State

#creating a qqplot with qqline
qqnorm(Energy$Total)
qqline(Energy$Total)

#Ho: The mean CO2 emissions for each state are the same 
#ha: the mean CO2 emissions are different in at least one state 

#oneway anova
oneway <- aov(Total ~ state)
summary.aov(oneway)



#---------------------------------------------------------------#
###section 2

sprouts <-  read_tsv("sprouts.txt")
view(sprouts)


#convert temp to factor 
Temperature <- as.factor(sprouts$Temperature)
#convert salinity to factor 
Salinity <- as.factor(sprouts$Salinity)

germ <- sprouts$Pct.Germinated
#I will use a shapiro test to determine normality 
shapiro.test(germ)

#data is not normally distributed, therefore we will transform the data 
transformation <- log10(germ)

hist(sprouts$Sprouts, col ="blue", main ="normal")

hist(transformation, col= 'black', main = "tranformed")

#two-way additive  
additive <- aov(germ ~ salinity +Temperature)
summary(additive)


#two-way interact
interact <- aov(germ ~ salinity * Temperature)
summary(interact)

#aic model
set <- list(additive, interact)
names <- c("additive", "interact")

#aictab
aictab(set, model_names = "names")

#we will reject the null and do a Tqukey test of additive 
TukeyHSD(additive)
