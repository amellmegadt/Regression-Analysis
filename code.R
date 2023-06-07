data1 = read.csv("D:/Porto/Regression/realest.csv")
View(data1)
df = as.data.frame(data1)

#deleting missing values in dataset
library(dplyr)
dfnew = data.frame(data1) %>% 
  filter(!is.na(Space)) %>%
  filter(!is.na(Lot)) %>%
  filter(!is.na(Tax))

#Make regression model with 7 independent variables 
reghouse = lm(Price~Bedroom+Space+Room+Lot+Tax+Bathroom+Garage, data= dfnew)
summary(reghouse)

#Asumptions
#Non Multicolinearity
library(car)
Vifval = vif(reghouse)
Vifval
#Residual normality
res = resid(reghouse)
library(tseries)
jarque.bera.test(res)
#Homoscedasticity
bptest(reghouse)
#No autocorrelation
dwtest(reghouse)

#define all the independent variable
x1 = dfnew$Bedroom
x2 = dfnew$Space
x3 = dfnew$Room
x4 = dfnew$Lot
x5 = dfnew$Tax
x6 = dfnew$Bathroom
x7 = dfnew$Garage

#Make a linear regression model between residual of square and all independent variable
ressqr = res^2
as.data.frame(res1)
bptest = lm(res1 ~ x1+x2+x3+x4+x5+x6+x7)
summary(bptest)


#Remove two variables that are tax and garage
library(lmtest)
reghouse2 = lm(Price~Bedroom+Space+Room+Bathroom+Garage, data= dfnew)

summary(reghouse2)

#Asumption test
#Multicolinearity
library(car)
Vifval2 = vif(reghouse2)
Vifval2
#normal residual
res2 = resid(reghouse2)
library(tseries)
jarque.bera.test(res2)
#Homoscedasticity
bptest(reghouse2)
#Autocorrelation
dwtest(reghouse2)
