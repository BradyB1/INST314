library(tidyverse)
biontech_adolescents <- read.csv("biontech_adolescents.csv")
view(biontech_adolescents)
###1 Identify the control group and treatment group
  # control group = placebo 
  # treatment group = Vaccinated group

###2 Hypothesis for effect of receiving vaccine:
  #Null Hypothesis- Getting the vaccine has no effect on you chances to get covid-19 
  #Alternative hypothesis- The group getting the vaccine (treatment group) is less likely to get covid-19 then the placebo group 
  #Ho: Pc = Pt
  #Ha: Pc < Pt
  

###3 Test the hypothesis
#placebo group
controlgroup <- biontech_adolescents[biontech_adolescents$group == "placebo", ]
view(controlgroup)

#vaccinated group
treatmentgroup <- biontech_adolescents[biontech_adolescents$group != "placebo", ]
view(treatmentgroup)


#positive in the placebo (=18)
#use as population estimator
positiveControl <- controlgroup[controlgroup$outcome == "COVID-19", ]
view(positiveControl)


#positive in the vaccinated (= 0)  
positiveTreatment <- treatmentgroup[treatmentgroup$outcome == "COVID-19", ]
view(positiveTreatment)

sum(positiveControl$outcome == "COVID-19")
sum(positiveTreatment$outcome == "COVID-19")

#left handed binomial test, because I believe the vaccinated group has lower odds of contracting the virus 
#(num of infected in treatment, total n in treatment, (total infected in control/total n in control), alternative type)
binom.test(0,1131,(18/1129), alternative = "less", conf.level = .95)
#are we running two binomial tests. One for each group, If not what are we suppose
  #to do to compare them?

##results
#p-value = 1.276e-08 < alpha = .05
#Because the P-Value is smaller then the alpha of .05 we have enough evidence to reject the null hypothesis 

###4 
# After running our hypothesis test we can say that the BioNTech vaccine was correlated with reduced risk of getting covid 19
  #because nobody in the treatment group was infected and 18 in the control group were infected

###5
# Yes, we can say this study makes it possible to prove causation because it is a controlled study meaning the study uses a control group
# that is not being affected by the vaccine and a treatment group that is being affected and it tests the results against each other.
#--------------------------------------------------------------------------#

#section 2L:
gshours[is.na(gshours)] = 0
gshours <- read.csv("gpa_study_hours.csv")
gpas <- gshours$gpa
hours <- gshours$study_hours

# Basic scatter plot
ggplot(gshours, aes(x=gpas, y=hours)) + 
  ggtitle("Gpas to Hours studied")+ 
  geom_point(shape = 19)+
  geom_smooth(method=lm)




#histogram for hours studied 

ggplot(gshours, aes(x = hours)) + 
  ggtitle("Study hours")+
  geom_histogram(aes(y = ..density..),
                 colour = 1) +
  geom_density(lwd = 1, colour = 4,
               fill = 4, alpha = 0.25)

