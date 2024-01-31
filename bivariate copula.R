setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

#install.packages(c("magic","VGAM","survey","mnormt","Rmpfr"))
#install.packages("SemiParBIVProbit_3.8.tar.gz", repos = NULL, type = "source")


##############################################################
library(gamlss)
install.packages('mvtnorm')

#############################################################
#Final model 

library(SemiParBIVProbit)
library(copula)
library(VineCopula)

library(gamlss)
#install.packages("corrplot")
library(compositions)
library(Hmisc, pos=4)
library(foreign, pos=4)
library(corrplot)
library(ggplot2)
library(readxl)

#getwd()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

newdata<- read.csv("newdata.csv")
head(newdata)
attach(newdata)
library(psych)
describe(cbind(z1,z2))
cor(cbind(z1,z2))
plot(cbind(z1,z2))
cor(z1,z2)
#MODELOS MARGINAl

mod1=gamlss(z1~education1+income,family=BE,data=newdata)
summary(mod1)
fitted(mod1,"sigma")[1]
#logsigma  -0.02294 transform logit  0.4942652
mod2=gamlss( z2 ~ education1,family=BE,data=newdata)
summary(mod2)
fitted(mod2,"sigma")[1]
#logsigma   -0.09549  transform logit  0.4761458

#encontrando sigma
sigma=exp(-0.02294)/(1+exp(-0.02294))
#sigma2
sigma**2

# MODELO 1 VIA COPULAS PARA MODELAR AS MEDIAS E OBTENER VARIANCIAS, THETA E TAU
eq.mu.1 <- z1 ~ education1+income
eq.mu.2 <- z2 ~ education1
eq.sigma2.1 <- ~1
eq.sigma2.2 <- ~1
eq.theta <- ~ 1
fl <- list(eq.mu.1, eq.mu.2, eq.sigma2.1, eq.sigma2.2, eq.theta)
#Gaussian 
outN <- copulaReg(fl, margins = c("BE", "BE"),
                  data = newdata, gamlssfit = TRUE)
summary(outN)
outN$coefficients
#frank copula
outF <- copulaReg(fl, margins = c("BE", "BE"), BivD = "F",
                  data = newdata, gamlssfit = TRUE)
summary(outF)#sample size small 


#Clayton copula
outC0 <- copulaReg(fl, margins = c("BE", "BE"), BivD = "C0",
                      data = newdata, gamlssfit = TRUE)
summary(outC0)# Error: In BiCopPar2Tau: The parameter

# AMH copula  
outAMH <- copulaReg(fl, margins = c("BE", "BE"), BivD = "AMH",
                   data = newdata, gamlssfit = TRUE)
summary(outAMH)

# FGM copula 
outFGM <- copulaReg(fl, margins = c("BE", "BE"), BivD = "FGM",
                    data = newdata, gamlssfit = TRUE)
summary(outFGM)

#gumbel copula 90
outG90<- copulaReg(fl, margins = c("BE", "BE"), BivD = "G90",
                    data = newdata, gamlssfit = TRUE)
summary(outG90)  ####Error: In BiCopPar2Tau: The parameter, G90, G180, G270

# joe copula 90
outJ90 <- copulaReg(fl, margins = c("BE", "BE"), BivD = "J90",
                   data = newdata, gamlssfit = TRUE)
summary(outJ90) #Error: In BiCopPar2Tau: The parameter ,J90, J180, J270


#############################################################
#############################################################
#summary AIC/BIC
AIC(outN,outF,outC0,outAMH,outFGM,outG90,outJ90)
BIC(outN,outF,outC0,outAMH,outFGM,outG90,outJ90)






