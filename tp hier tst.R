
install.packages("vars")
install.packages("stargazer")
install.packages("remotes")
install.packages("tsm")
install.packages("mFilter")
library(tsm)
library(vars)
library(mFilter)
library(remotes)
library(dplyr)
library(urca)
library(dynlm)
library(lmtest)        
library(readxl)
library(tidyverse)
library(stargazer)
library(tseries)
M<- read_excel("C:/Users/PC_/Downloads/Month_Value_1 (3) (1).xlsx")
#FIND MISSING VALUES 

is.na(M)
#eliminate missing values 
m<- na.omit(M)
View(m)


R <-ts(m$Revenue)
S <-ts(m$Sales_quantity)
C <-ts(m$Average_cost)
p <-ts(m$The_average_annual_payroll_of_the_region)
 

plot(cbind(R,S,C,p))

R.ac<- ac(R, main = "output revenue")
S.ac<- acf(S, main = "output sales quantity")
c.ac<- acf(C, main = "output Average_cost ")
p.ac<- acf(p, main = "output Average_payrol ")

r.pf <- pacf(R ,main="output revenue pacf")

plot(cbind(R.ac,r.pf))




#KPSS TEST 
#H0; stationary
R %>%
ur.kpss() %>%
summary()

adf.R <- ur.df(R, type = "trend", selectlags = "AIC")
summary(adf.R)


S %>%
  ur.kpss() %>%
  summary()


C %>%
  ur.kpss() %>%
  summary()

p %>%
  ur.kpss() %>%
  summary()

VARselect(R)

r10<- diff(R,lag = 10)
plot(r10)


r10 %>%
ur.kpss() %>%
summary()

#sales
VARselect(S)

s10<- diff(S,lag = 10)
plot(s10)

#average cost
VARselect(C)
c10<- diff(S,lag = 10)
plot(c10)


VARselect(p)
P10<- diff(p,lag = 10)
plot(P10)

v.bv<-cbind(r10,s10,c10,P10 )

VAR_EQ1 <- dynlm(r10 ~ L(r10, 1:2) + L(s10, 1:2) + L(c1, 1:2) + L(P1, 1:2)) 


VAR_EQ2 <- dynlm(s10 ~ L(s10, 1:2) + L(r10, 1:2) + L(c1, 1:2) + L(P1, 1:2))

VAR_EQ3 <- dynlm(c1 ~ L(c1, 1:2) + L(r10, 1:2) + L(s10, 1:2) + L(P1, 1:2))

VAR_EQ4 <- dynlm(P1 ~ L(P1, 1:2) + L(r10, 1:2) + L(s10, 1:2) + L(c1, 1:2))


VAR_EQ1 <- dynlm(r10 ~ L(r10, 1:2) + L(s10, 1:2) + L(c1, 1:2) + L(P1, 1:2)) 


VAR_EQ2 <- dynlm(s10 ~ L(s10, 1:2) + L(r10, 1:2) + L(c1, 1:2) + L(P1, 1:2))

VAR_EQ3 <- dynlm(c1 ~ L(c1, 1:2) + L(r10, 1:2) + L(s10, 1:2) + L(P1, 1:2))

VAR_EQ4 <- dynlm(P1 ~ L(P1, 1:2) + L(r10, 1:2) + L(s10, 1:2) + L(c1, 1:2))

#rename regressors for better readability
names(VAR_EQ1$coefficients) <- c("Intercept","r10_t-1", 
                                 "r10_t-2", "s10_t-1", "s10_t-2" , "c1_t-1" , "c1_t-2" ,"P1_t-1" , "P1_t-2")
names(VAR_EQ2$coefficients) <- names(VAR_EQ1$coefficients)
names(VAR_EQ3$coefficients) <- names(VAR_EQ1$coefficients)
names(VAR_EQ4$coefficients) <- names(VAR_EQ1$coefficients)


# robust coefficient summaries
coeftest(VAR_EQ1, vcov. = sandwich)

coeftest(VAR_EQ2, vcov. = sandwich)



VARmodel<-VAR(v.bv,type="const", lag.max=10 ,ic="AIC")
VARmodel
#summary(VARmodel)

estim <- -VAR(v.bv, p=10,type="none")
stargazer(VARmodel,type='texte')
stargazer(VARmodel[["varresult"]],type='text')
stargazer(VARmodel[["varresult"]],type='latex')

#stable model 
#roots(VARmodel,modulus = "TRUE")


STBL <- stability(VARmodel, type = "OLS-CUSUM")
plot(STBL)
v.bv

VARmodelserial<-serial.test(VARmodel)
seriall <- serial.test(VARmodel, type = "PT.asymptotic")
bv.serial 

Box.test(VARmodel,lag=10,type="Ljung-Box")
# Granger causality tests:

#causg <- causality(VARmodel, cause = "s10")
#causg

cs <- causality(VARmodel,cause="s10")
#quantity sales cause revenu ?
grangertest(r10~s10,order=2)
grangertest(r10~c10,order=2)
grangertest(r10~P10,order=2)
grangertest(s10~r10,order=2)
grangertest(s10~c10,order=2)
grangertest(s10~P10,order=2)


plot(bv.serial, names = "une")
hetero.arch <- arch.test(VARmodel, lags.multi = 10, multivariate.only = TRUE)
hetero.arch
bv.norm <- normality.test(bv.est, multivariate.only = TRUE)
bv.nor

norm <- normality.test(VARmodel, multivariate.only = TRUE)
norm


bv.cause.gdp <- causality(bv.est, cause = "gdp")
bv.cause.gdp

bv.cause.une <- causality(bv.est, cause = "une")
bv.cause.une

irf.une <- irf(bv.est, impulse = "gdp", response = "une",
               n.ahead = 40, boot = TRUE)
plot(irf.une, ylab = "unemployment", main = "Shock from output")

bv.vardec <- fevd(bv.est, n.ahead = 10)
plot(bv.vardec)
predictions <- predict(bv.est, n.ahead = 8, ci = 0.95)
plot(predictions, names = "gdp")


plot(predictions, names = "une") 
fanchart(predictions, names = "une")

irfr10 <-irf(VARmodel, impulse = "c10", response = "s10", n.ahead = 20, boot = TRUE,run=100,ci=0.95)
plot(irfr10,ylab="r10",main="response r10")



#DECOMPOSITION VARIANCE 
VD <- fevd(VARmodel,n.ahead = 10)
plot(VD)


arch.test(VARmodel)
