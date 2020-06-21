library(e1071)
library(car)

Data <- read.csv("ToyotaCorolla.csv")
Corolla<-Data[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]

attach(Corolla)

# First Moment Business Decision
summary(Corolla)

# Second Moment Business Decision
sd(Price)
sd(Age_08_04)
sd(KM)
sd(HP)
sd(cc)
sd(Doors)
sd(Gears)
sd(Quarterly_Tax)
sd(Weight)
var(Price)
var(Age_08_04)
var(KM)
var(HP)
var(cc)
var(Doors)
var(Gears)
var(Quarterly_Tax)
var(Weight)

# Third Moment Business Decision
skewness(Price)
skewness(Age_08_04)
skewness(KM)
skewness(HP)
skewness(cc)
skewness(Doors)
skewness(Gears)
skewness(Quarterly_Tax)
skewness(Weight)
# Fourth Moment Business Decision
kurtosis(Price)
kurtosis(Age_08_04)
kurtosis(KM)
kurtosis(HP)
kurtosis(cc)
kurtosis(Doors)
kurtosis(Gears)
kurtosis(Quaterly_Tax)
kurtosis(Weight)

plot(Age_08_04, Price)

plot(KM, Price)

plot(HP, Price)

plot(cc, Price)

plot(Doors, Price)

plot(Gears, Price)

plot(Quarterly_Tax, Price)

plot(Weight, Price)

# Cars of Fuels types 
summary(Data$Fuel_Type)

#summary(Data$Model)
summary(Data$Color)

# Find Correlation between input and output
pairs(Corolla)


# Correlation Coefficient matrix - Strength & Direction of Correlation
cor(Corolla)


##Pure Correlation  b/n the varibles
library(corpcor)
cor2pcor(cor(Corolla))


## Building linear regression model
model <- lm(Price ~ ., data = Corolla)
summary(model)


# cc and Doors are influence to each other, predict the model based on individual records
model.carcc <- lm(Price ~ cc)
summary(model.carcc)

model.cardoor <- lm(Price ~ Doors)
summary(model.cardoor)


## Build model with cc and Doors
model.car <- lm(Price ~ cc + Doors)
summary(model.car)

# Find out the influencial record
influence.measures(model.car)

# ploting influential measures
influenceIndexPlot(model.car)

influencePlot(model.car)


# Delete influentails records and build the model
model1 <- lm(Price ~ ., data = Corolla[-c(81),])
summary(model1)


vif(model1)

avPlots(model1)

finalmodel <- lm(Price ~ Age_08_04 + KM + HP + cc + Gears + Quarterly_Tax + Weight, data = Corolla[-c(81),])
summary(finalmodel)

## Evaluate model LINE assumptions 
plot(finalmodel)

#Residual plots,QQplot,std-Residuals Vs Fitted,Cook's Distance 
qqPlot(model)

