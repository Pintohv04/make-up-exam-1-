

#Re do Exam # 1 
#Hugo Pinto
10/22/2021

covid_dat_use <- Household_Pulse_data
attach(covid_dat_use)
na.omit(covid_dat_use)

library(dplyr)


summary(EEDUC)
summary(INCOME)
EEDUC [complete.cases(EEDUC)]
INCOME [complete.cases(INCOME)]


#question 1
NewDF <-data.frame(covid_dat_use,covid_dat_use)
xtabs(~EEDUC+ RECVDVACC+REGION+INCOME,data = NewDF)
print(xtabs)

str(select(NewDF,EEDUC, RECVDVACC, REGION, INCOME))
    
table(NewDF$RECVDVACC)


EEDUC[complete.cases(EEDUC)]
REGION[complete.cases(REGION)]
RECVDVACC[complete.cases(RECVDVACC)]


 
northeast <- (REGION == "Northeast") & (RECVDVACC == "yes got vaxx") & (EEDUC == "some coll") 
summary(northeast)


northeast <- (REGION == "Northeast") & (RECVDVACC == "yes got vaxx") & (EEDUC == "HS diploma") 
summary(northeast)

northeast <- (REGION == "Northeast") & (RECVDVACC == "yes got vaxx") & (EEDUC == "assoc deg") 
summary(northeast)

northeast <- (REGION == "Northeast") & (RECVDVACC == "yes got vaxx") & (EEDUC == "bach deg") 


summary(northeast)

mid_income <- lm(formula = (NE) ~ (Vaxxed) + (Some_college) + (INCOME == "$30,000 - $49,999"))
summary((mid_income))
summary(as.numeric(INCOME))
NE = rnorm(REGION == "northeast")
Vaxxed = rnorm(RECVDVACC == "yes got vaxx")
Some_college = rnorm(EEDUC == "SOME COLL")
income = rnorm(INCOME == "$30,000 - $59,999")



# in this question a tried to run  different regressions. I notice that people with Associate degree were less likely to get the COVID-19 Vaccine.
# on the other hand, people with Advance degree were more likely to get the vaccine.

# base on my understanding  there are few people with high school diploma that are able to make  50K per year.





#Question #2
#Anova regression 

summary(as.list.numeric_version(Anova_var))
t.test(x,y)


x = (REGION == "West")
x1 = (RECVDVACC == "yes got vaxx")
y = (EEDUC == "Some_college")
y1 = (GENID_DESCRIBE == "Female")

Anova_var<- ((REGION == "West") & (RECVDVACC == "yes got vaxx") & (EEDUC == "Some_college") & (GENID_DESCRIBE == "Female"))



x = (REGION == "West")
x1 = (RECVDVACC == "yes got vaxx")
y = (EEDUC == "bach deg")
y1 = (GENID_DESCRIBE == "transgender")

Anova_var<- ((REGION == "West") & (RECVDVACC == "yes got vaxx") & (EEDUC == "bach deg") & (GENID_DESCRIBE == "transgender"))


x = (REGION == "West")
x1 = (RECVDVACC == "yes got vaxx")
y = (EEDUC == "adv deg")
y1 = (GENID_DESCRIBE == "other")

xtabs <- Anova_var<- ((REGION == "West") & (RECVDVACC == "yes got vaxx") & (EEDUC == "adv deg") & (GENID_DESCRIBE == "other"))




x = (REGION == "West")
x1 = (RECVDVACC == "yes got vaxx")
y = (EEDUC == "less than hs")
y1 = (GENID_DESCRIBE == "male")




xtabs <- Anova_var<- ((REGION == "West") & (RECVDVACC == "yes got vaxx") & (EEDUC == "less than hs") & (GENID_DESCRIBE == "male"))


x = (REGION == "south")
x1 = (RECVDVACC == "yes got vaxx")
y = (EEDUC == "less than hs")
y1 = (GENID_DESCRIBE == "male")


xtabs <- Anova_var<- ((REGION == "south") & (RECVDVACC == "yes got vaxx") & (EEDUC == "less than hs") & (GENID_DESCRIBE == "male"))



x = (REGION == "south")
x1 = (RECVDVACC == "yes got vaxx")
y = (EEDUC == "some coll")
y1 = (GENID_DESCRIBE == "famale")


 xtabs <-Anova_var<- ((REGION == "south") & (RECVDVACC == "yes got vaxx") & (EEDUC == "some coll") & (GENID_DESCRIBE == "famale"))


x = (REGION == "south")
x1 = (RECVDVACC == "yes got vaxx")
y = (EEDUC == "adv college")
y1 = (GENID_DESCRIBE == "other")

xtabs <- Anova_var<- ((REGION == "south") &  (RECVDVACC == "yes got vaxx") & (EEDUC == "adv deg") & (GENID_DESCRIBE == "other"))




x = (REGION == "notheast")
x1 = (RECVDVACC == "yes got vaxx")
y = (EEDUC == "adv college")
y1 = (GENID_DESCRIBE == "other")

xtabs <- Anova_var<- ((REGION == "northeast") &  (RECVDVACC == "yes got vaxx") & (EEDUC == "adv deg") & (GENID_DESCRIBE == "other"))


x = (REGION == "notheast")
x1 = (RECVDVACC == "yes got vaxx")
y = (EEDUC == "adv college")
y1 = (GENID_DESCRIBE == "trasgender")

xtabs <- Anova_var<- ((REGION == "northeast") &  (RECVDVACC == "yes got vaxx") & (EEDUC == "adv deg") & (GENID_DESCRIBE == "trasgender"))



xtabs <- Anova_var<- ((REGION == "") &  (RECVDVACC == "yes got vaxx") & (EEDUC == "adv deg") & (GENID_DESCRIBE == "trasgender"))



 # base on this information, we notice that as higher the education the out for vaccination is also higher.There is a correlation between ewducation and vaccination rate. Also, gender males for instance have a significal hight rate of vaccination  


 # question #3 
 # first we have to break down the date into sub-groups 

attach(Household_Pulse_data)
summary(Household_Pulse_data)

covid_data <- ((INCOME == "HH income $30k -$ 49,999")& (TENURE == "housing owned with mortgage") & (PUBHLTH == "has public health ins") & (RECVDVACC == "yes got vaxx"))
covid_data <- as.logical(covid_data)
covid_dat_use <- subset(Household_Pulse_data,covid_data) 
detach(Household_Pulse_data)
attach(covid_dat_use)
na.omit(covid_dat_use)

# them summarized some of the subgroups 
summary(INCOME)
summary(TENURE)
summary(PUBHLTH)
summary(RECVDVACC)


summary(as.logical(INCOME))
summary(as.logical(TENURE))
summary(as.logical(PUBHLTH))
summary(as.logical(RECVDVACC))


summary(INCOME)
summary(TENURE)
summary(PUBHLTH)
summary(RECVDVACC)


# now we use the Regression Model
regresion1 <- lm(Vaxxed ~ PUBHLTH + rented +Some_college) 
summary(regress1)




library(stargazer)
stargazer(regress1, type = "text")
install.packages("Rmisc")
library(Rmisc)
CI(Vaxxed,ci =a)
CI(x=0.95)(RECVDVACC == "yes got vaxx")


# now we are calculating  the data by using  interval calculations
INCOME <--0-0  
INCOMEr <--0+0 
TENURE <--0-0
TENUREr <--0+0
PUBHLTH <--0-0
PUBHLTHr <--0+0
RECVDVACC <- -0-0
RECVDVACCr <- -
  
  
require(AER)

# Plotting the subset
NNobs <- length(INCOME)
set.seed(12345) 
graph_obs <- (runif(NNobs) < 0.1) 
COVID_dat_graph <-subset(covid_dat_use,graph_obs)  

plot(INCOME ~ jitter(RECVDVACC, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2),  data = COVID_dat_graph)
plot(INCOME ~ jitter(RECVDVACC, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), ylim = c(0,110000), data = COVID_dat_graph)

 # now we are approaching a different type of regression 

to_be_predicted2 <- data.frame(INCOME == "HH income $30k - $49,999", TENURE == "housing rented", PUBHLTH == "has public health ins", RECVDVACC == "yes got vaxx")
to_be_predicted2$yhat <- predict(regress1, newdata = to_be_predicted2)

lines(yhat ~ AGE, data = to_be_predicted2)

