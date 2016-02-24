setwd("~/Winter PSU 2016/Expermental Desige 1/Midterm Project")
data <- read.csv("cigconswithregions.csv")
library("car", lib.loc="~/R/win-library/3.2")
library("leaps", lib.loc="~/R/win-library/3.2")
library("MASS", lib.loc="~/R/win-library/3.2")


####################################################################
#########################Full Data##################################
###################################################################

#Full Model
model.full <- lm(Scig ~ Age + Education + Income + perblack + perfem + price, data = data)
summary(model.full)
#Multiple R-squared:  0.3228,	Adjusted R-squared:  0.2304 
#F-statistic: 3.495 on 6 and 44 DF,  p-value: 0.00651
plot(model.w)

null <- lm(Scig ~ 1, data = data)
full <- lm(Scig ~ Age + Education + Income + perblack + perfem + price , data = data)

step(null, scope=list(lower=null, upper=full), direction="forward")
#lm(formula = Scig ~ Income + price + Age, data = data)
step(full, data=data, direction="backward")
#lm(formula = Scig ~ Age + Income + price, data = data)
step(null, scope = list(upper=full), data=data, direction="both")
#lm(formula = Scig ~ Income + price + Age, data = data)

##forward, backward and stepwise selected model 
model.fb <- lm(formula = Scig ~ Income + price + Age, data = data)
summary(model.fb)
##Multiple R-squared:  0.3047,	Adjusted R-squared:  0.2603 
##F-statistic: 6.865 on 3 and 47 DF,  p-value: 0.0006268

boxcox(model.fb) ## Indacates an inverse y may be apprprtate
data$Sciginv <- 1/(data$Scig)
model.fbinv <- lm(formula = Sciginv ~ Income + price + Age, data = data)
summary(model.fbinv)
##Multiple R-squared:  0.3942,	Adjusted R-squared:  0.3555 
##F-statistic:  10.2 on 3 and 47 DF,  p-value: 2.756e-05

rstudent.fbinv <- rstudent(model.fbinv)
rstudent.fbinv
## outliers [12] [31] [34] [45], HI, NH, NV, UT

data.out <- data[-c(12,31,34,45),]
model.fbinvout <- lm(formula = Sciginv ~ Income + price + Age, data = data.out)
summary(model.fbinvout)
##Multiple R-squared:  0.5924,	Adjusted R-squared:  0.564 
##F-statistic: 20.84 on 3 and 43 DF,  p-value: 1.73e-08


#Region Subsetting
data.ne <- subset(data, data$Regon == "Northeast")
data.mw <- subset(data, data$Regon == "Midwest")
data.w <- subset(data, data$Regon == "Pacific" | data$Regon == "Rockey ")
data.s <- subset(data, data$Regon == "Southeast" | data$Regon == "Southwest")



#Model Selection
leaps <- regsubsets(Scig ~ . - State ,  data = data)
plot(leaps, scale="adjr2")
plot(leaps, scale="bic")

######################################################################
################West Coast Analysis################################### Good Model
######################################################################
#Full Model
model.w <- lm(Scig ~ Age + Education + Income + perblack + perfem + price, data = data.w)
summary(model.w)
#Adjusted R-squared:  0.727
#F-statistic: 5.438 on 6 and 4 DF,  p-value: 0.06157
plot(model.w)

#Forward/Backward/Stepwise Model Selection
null.w <- lm(Scig ~ 1, data = data.w)
full.w <- lm(Scig ~ Age + Education + Income + perblack + perfem + price , data = data.w)

step(null.w, scope=list(lower=null.w, upper=full.w), direction="forward")
#lm(formula = Scig ~ Age + perfem + Education + price + Income + perblack, data = data.w)
step(full.w, data=data.w, direction="backward")
#lm(formula = Scig ~ Age + perfem + Education + price + Income + perblack, data = data.w)
step(null.w, scope = list(upper=full.w), data=data.w, direction="both")
#lm(formula = Scig ~ Age + perfem + Education + price + Income + perblack, data = data.w)

#All three picked the full model
#BoxCox Transformation
boxcox(model.w)
#Lambda = -1 selected
#Inverse Model
data.w$Sciginv <- data.w$Scig^(-1)
model.winv <- lm(Sciginv ~ Age + Education + Income + perblack + perfem + price, data = data.w)
summary(model.winv)
#Multiple R-squared:  0.9685,	Adjusted R-squared:  0.9212 <-------Better when not scaled
#F-statistic: 20.49 on 6 and 4 DF,  p-value: 0.005712
plot(model.winv)
#Inverse Model with Income scaled down by its square root
data.w$incroot <- data.w$Income^(.5)
model.winvscale <- lm(Sciginv ~ Age + Education + incroot + perblack + perfem + price, data = data.w)
summary(model.winvscale)
#Multiple R-squared:  0.9672,	Adjusted R-squared:  0.9181 <-------Worse when scaled
#F-statistic: 19.69 on 6 and 4 DF,  p-value: 0.006159


plot(model.winv)
w.rstudent <- rstudent(model.winv)
list(w.rstudent)
##[38] 2.76650849

#Observations [38] removed
data.w38 <- data.w[-8,]
data.w38$Sciginv <- 1/(data.w38$Scig)
model.winv38 <- lm(Sciginv ~ Age + Education + Income + perblack + perfem + price, data = data.w38)
summary(model.winv38)
plot(model.winv38)
#Multiple R-squared:  0.9901,	Adjusted R-squared:  0.9702 <----- Improvment on "best" model 
#F-statistic: 49.81 on 6 and 3 DF,  p-value: 0.004284



#####################################################################
########################South Analysis############################### No Good Model w/ outliers
##################################################################### Good Model w/o Outliers

#Full Model
model.s <- lm(Scig ~ Age + Education + Income + perblack + perfem + price, data = data.s)
summary(model.s)
#Multiple R-squared:  0.7457,	Adjusted R-squared:  0.5761 
#F-statistic: 4.398 on 6 and 9 DF,  p-value: 0.02371
plot(model.s)

#Forward/Backward/Stepwise Model Selection
null.s <- lm(Scig ~ 1, data = data.s)
full.s <- lm(Scig ~ Age + Education + Income + perblack + perfem + price , data = data.s)

step(null.s, scope=list(lower=null.s, upper=full.s), direction="forward")
#lm(formula = Scig ~ price + Age, data = data.s)
step(full.s, data=data.s, direction="backward")
#lm(formula = Scig ~ Income + perblack + perfem + price, data = data.s)
step(null.s, scope = list(upper=full.s), data=data.s, direction="both")
#lm(formula = Scig ~ price + Age, data = data.s)

#Forward/Stepwise Model
model.sfs <- lm(Scig ~ Age + price , data = data.s)
summary(model.sfs)
#Multiple R-squared:  0.6394,	Adjusted R-squared:  0.584 
#F-statistic: 11.53 on 2 and 13 DF,  p-value: 0.00132
boxcox(model.sfs)
data.s$Sciginv.s <- data.s$Scig^(-1)
#Lambda = -1 selected
model.sfsinv <- lm(Sciginv.s ~ Age + price, data = data.s)
summary(model.sfsinv)
#Multiple R-squared:  0.6476,	Adjusted R-squared:  0.5934 
#F-statistic: 11.95 on 2 and 13 DF,  p-value: 0.001136

#Backward Model
model.sb <- lm(formula = Scig ~ Income + perblack + perfem + price, data = data.s)
summary(model.sb)
#Multiple R-squared:  0.7324,	Adjusted R-squared:  0.635 
#F-statistic: 7.525 on 4 and 11 DF,  p-value: 0.003572
boxcox(model.sb)
boxcox(model.sb, seq(-3,2,.1))
#Options include Lambda = -2,-1,-.5

#For Lambda = -1
model.sbinv <- lm(Sciginv.s ~ Income + perblack + perfem + price, data = data.s)
summary(model.sbinv)
#Multiple R-squared:  0.7783,	Adjusted R-squared:  0.6976 
#F-statistic: 9.652 on 4 and 11 DF,  p-value: 0.001333

#################################
#################################
#For Lambda = -2
data.s$Sciginvsq <- data.s$Scig^(-2)
model.sbinvsq <- lm(Sciginvsq ~ Income + perblack + perfem + price, data = data.s)
summary(model.sbinvsq)
#Multiple R-squared:  0.7849,	Adjusted R-squared:  0.7067 <------- BEST MODEL
#F-statistic: 10.04 on 4 and 11 DF,  p-value: 0.001135

plot(model.sbinvsq)
s.rstudent <- rstudent(model.sbinvsq)
list(s.rstudent)
#[41] = -2.72
#[46] = 1.78 <------- Next closest is 1.2

#Observation [41] removed
data.s41 <- subset(data.s, data.s$State != "LA")
Sciginvsq41 <- data.s41$Scig^(-2)
model.sbinvsq41 <- lm(Sciginvsq41 ~ Income + perblack + perfem + price, data = data.s41)
summary(model.sbinvsq41)
#Multiple R-squared:  0.8751,	Adjusted R-squared:  0.8252 
#F-statistic: 17.52 on 4 and 10 DF,  p-value: 0.0001631

#Observations [41] and [46] removed
outliers <- c("LA", "VA")
data.s4146 <- subset(data.s, !(data.s$State %in% outliers))
Sciginvsq4146 <- data.s4146$Scig^(-2)
model.sbinvsq4146 <- lm(Sciginvsq4146 ~ Income + perblack + perfem + price, data = data.s4146)
summary(model.sbinvsq4146)
#Multiple R-squared:  0.9157,	Adjusted R-squared:  0.8782 <----- Best w/o outliers
#F-statistic: 24.43 on 4 and 9 DF,  p-value: 7.528e-05


################################
################################

#For Lambda = -.5
data.s$Scinvroot <- data.s$Scig^(-.5)
model.sbinvroot <- lm(Scinvroot ~ Income + perblack + perfem + price, data = data.s)
summary(model.sbinvroot)
#Multiple R-squared:  0.7706,	Adjusted R-squared:  0.6872 
#F-statistic: 9.237 on 4 and 11 DF,  p-value: 0.001595

#####################################################################
########################Northeast Analysis########################### No Good Model w/ outliers
##################################################################### Ok Model w/o outliers

#Full Model
model.ne <- lm(Scig ~ Age + Education + Income + perblack + perfem + price, data = data.ne)
summary(model.ne)
#Multiple R-squared:  0.6087,	Adjusted R-squared:  0.1392 
#F-statistic: 1.296 on 6 and 5 DF,  p-value: 0.3967
plot(model.ne)

#Forward/Backward/Stepwise Model Selection
null.ne <- lm(Scig ~ 1, data = data.ne)
full.ne <- lm(Scig ~ Age + Education + Income + perblack + perfem + price , data = data.ne)

step(null.ne, scope=list(lower=null.ne, upper=full.ne), direction="forward")
#lm(formula = Scig ~ price, data = data.ne)
step(full.ne, data=data.ne, direction="backward")
#lm(formula = Scig ~ price, data = data.ne)
step(null.ne, scope = list(upper=full.ne), data=data.ne, direction="both")
#lm(formula = Scig ~ price, data = data.ne)

#All 3 picked the same model
model.nereduced <- lm(Scig ~ price, data = data.ne)
summary(model.nereduced)
#Multiple R-squared:  0.4549,	Adjusted R-squared:  0.4004 
#F-statistic: 8.346 on 1 and 10 DF,  p-value: 0.01613
boxcox(model.nereduced, seq(-4,0,.1))
#Options include Lambda = -3,-2

#################################
#################################

#For Lambda = -3
data.ne$ScigInvCube.ne <- data.ne$Scig^(-3)
model.neInvCube <- lm(ScigInvCube.ne ~ price, data = data.ne)
summary(model.neInvCube)
#Multiple R-squared:  0.529,	Adjusted R-squared:  0.482 <----- Best Model w/ outliers
#F-statistic: 11.23 on 1 and 10 DF,  p-value: 0.007345

ne.rstudent <- rstudent(model.neInvCube)
list(ne.rstudent)
#[15] = -2.07
#[17] = 2.50

#Observations [15] and [17] removed
outliers.ne <- c("DE", "MD")
data.ne1517 <- subset(data.ne, !(data.ne$State %in% outliers.ne))
ScigInvCube1517 <- data.ne1517$Scig^(-3)
model.neInvCube1517 <- lm(ScigInvCube1517 ~ price, data = data.ne1517)
summary(model.neInvCube1517)
plot(model.neInvCube1517)
#Multiple R-squared:  0.8463,	Adjusted R-squared:  0.8271 <----- Much better but still just ok
#F-statistic: 44.05 on 1 and 8 DF,  p-value: 0.000163

################################
################################

#For Lambda = -2
ScigInvSq.ne <- data.ne$Scig^(-2)
model.neInvSq <- lm(ScigInvSq.ne ~ price, data = data.ne)
summary(model.neInvSq)
#Multiple R-squared:  0.5265,	Adjusted R-squared:  0.4791 
#F-statistic: 11.12 on 1 and 10 DF,  p-value: 0.007563

######################################################################
################Midwest Analysis###################################### Good Model
######################################################################

#Full Model
model.mw <- lm(Scig ~ Age + Education + Income + perblack + perfem + price, data = data.mw)
summary(model.mw)
##Multiple R-squared:  0.9418,	Adjusted R-squared:  0.872 
##F-statistic: 13.49 on 6 and 5 DF,  p-value: 0.005903
plot(model.mw)

#Forward/Backward/Stepwise Model Selection
null.mw <- lm(Scig ~ 1, data = data.mw)
full.mw <- lm(Scig ~ Age + Education + Income + perblack + perfem + price , data = data.mw)

step(null.mw, scope=list(lower=null.mw, upper=full.mw), direction="forward")
#lm(formula = Scig ~ perblack + price + Income + Age, data = data.mw)
step(full.mw, data=data.mw, direction="backward")
#lm(formula = Scig ~ Age + Education + perblack + perfem + price, data = data.mw)
step(null.mw, scope = list(upper=full.mw), data=data.ne, direction="both")
#lm(formula = Scig ~ perblack + price + Income + Age, data = data.mw)

#Two different models picked

#Forward/Stepwise Model
model.mwFS <- lm(Scig ~ perblack + price + Income + Age, data = data.mw)
summary(model.mwFS)
#Multiple R-squared:  0.9331,	Adjusted R-squared:  0.8948 
#F-statistic:  24.4 on 4 and 7 DF,  p-value: 0.0003306
boxcox(model.mwFS, seq(-4,1,.1))
#Options include Lambda = -3,-2,-1,-.5,0

#For Lambda = -1
data.mw$Scig.mwInv <- data.mw$Scig^(-1)
model.mwInv <- lm(Scig.mwInv ~ perblack + price + Income + Age, data = data.mw)
summary(model.mwInv)
#Multiple R-squared:  0.934,	Adjusted R-squared:  0.8963 <---- Barely any change
#F-statistic: 24.76 on 4 and 7 DF,  p-value: 0.0003153

#For Lambda = -2
data.mw$Scig.mwInvSq <- data.mw$Scig^(-2)
model.mwInvSq <- lm(Scig.mwInvSq ~ perblack + price + Income + Age, data = data.mw)
summary(model.mwInvSq)
#Multiple R-squared:  0.9319,	Adjusted R-squared:  0.893 <----- Barely any change
#F-statistic: 23.96 on 4 and 7 DF,  p-value: 0.0003507

#For Lambda = -.5
data.mw$Scig.mwInvRt <- data.mw$Scig^(-.5)
model.mwInvRt <- lm(Scig.mwInvRt ~ perblack + price + Income + Age, data = data.mw)
summary(model.mwInvRt)
#Multiple R-squared:  0.9344,	Adjusted R-squared:  0.897 <----- Bigger change but not much
#F-statistic: 24.94 on 4 and 7 DF,  p-value: 0.0003084

rstudent.mwInvRt <- rstudent(model.mwInvRt)
rstudent.mwInvRt
data.mw28 <- data.mw[-8,]
model.mwInvRt28 <- lm(Scig.mwInvRt ~ perblack + price + Income + Age, data = data.mw28)
summary(model.mwInvRt28)
##Multiple R-squared:  0.9854,	Adjusted R-squared:  0.9756  <-------- Best model
##F-statistic: 101.1 on 4 and 6 DF,  p-value: 1.237e-05

############################################################################
##########################Final Models######################################
############################################################################

#West Coast 
#removed OR
#model.winv38 <- lm(Sciginv ~ Age + Education + Income + perblack + perfem + price, data = data.w38)
#Multiple R-squared:  0.9901,	Adjusted R-squared:  0.9702 <----- Improvment on "best" model 
#F-statistic: 49.81 on 6 and 3 DF,  p-value: 0.004284


#South 

#Observations [41] and [46] removed
#outliers <- c("LA", "VA")
#model.sbinvsq4146 <- lm(Sciginvsq4146 ~ Income + perblack + perfem + price, data = data.s4146)
#Multiple R-squared:  0.9157,	Adjusted R-squared:  0.8782 <----- Best w/o outliers
#F-statistic: 24.43 on 4 and 9 DF,  p-value: 7.528e-05

#North East 

#Observations [15] and [17] removed
#outliers.ne <- c("DE", "MD")
#model.neInvCube1517 <- lm(ScigInvCube1517 ~ price, data = data.ne1517)
#Multiple R-squared:  0.8463,	Adjusted R-squared:  0.8271 <----- Much better but still just ok
#F-statistic: 44.05 on 1 and 8 DF,  p-value: 0.000163

#MidWest 
# removed NB
#data.mw28 <- data.mw[-8,]
#model.mwInvRt28 <- lm(Scig.mwInvRt ~ perblack + price + Income + Age, data = data.mw28)
##Multiple R-squared:  0.9854,	Adjusted R-squared:  0.9756  <-------- Best model
##F-statistic: 101.1 on 4 and 6 DF,  p-value: 1.237e-05




