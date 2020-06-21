cc.wg <- read.csv(file.choose()) # choose the Calories_Consumed.csv data set
View(cc.wg)

# 14 Observations of 2 variables

# Scatter Diagram (Plot x,y)
plot(cc.wg$Calories.Consumed,cc.wg$Weight.gained..grams.)

# Other Exploratory data analysis and Plots

boxplot(cc.wg)
hist(cc.wg$Calories.Consumed)
hist(cc.wg$Weight.gained..grams.)
summary(cc.wg)
# Correlation coefficient value for Calories Consumes and Weight Gained
cc<- cc.wg$Calories.Consumed
wg <- cc.wg$Weight.gained..grams.
cor(wg,cc)
# If |r| is greater than  0.85 then Co-relation is Strong(Correlation Co-efficient = 0.946991). 
# This has a Strong Co-relation 

# Simple model without using any transformation
reg<-lm(wg~cc)
summary(reg)
# Probability value should be less than 0.05(4.54e-05)
# The multiple-R-Squared Value is 0.8968 which is greater 0.8(In General)
# The Probability Value for F-Statistic is 2.85e-07(Overall Probability Model is also less than 0.05)
confint(reg,level = 0.95) # confidence interval

# The above code will get you 2 equations 
# 1 to caliculate the lower range and other for upper range

# Function to Predict the above model 
predict(reg,interval="predict")
# predict(reg,type="prediction")
# R-squared value for the above model is 0.8968 

# we may have to do transformation of variables for better R-squared value
# Applying transformations

# Logarthmic transformation
reg_log<-lm(wg~log(cc))  # Regression using logarthmic transformation
summary(reg_log)
confint(reg_log,level=0.95)
predict(reg_log,interval="predict")
# R-squared value for the above model is 0.8077
# Adjusted R-squared:  0.7917 

# we may have to do different transformation better R-squared value
# Applying different transformations

# Exponential model 
reg_exp<-lm(log(wg)~cc) # regression using Exponential model
summary(reg_exp)
confint(reg_exp,level=0.95)
exp(predict(reg_exp,interval="predict"))
# R-squared value has increased from 0.8776
# Adjusted R SQuare Value - 0.8674 
# Higher the R-sqaured value - Better chances of getting good model 
# for Calories Consumed and Weight Gained.

# Quadratic model
cc.wg[,"CC_sq"] = cc*cc

# Quadratic model
quad_mod <- lm(wg~cc+I(cc^2),data=cc.wg)
summary(quad_mod)
confint(quad_mod,level=0.95)
predict(quad_mod,interval="predict")
# Adjusted R-Squared = 0.9433
#Multiple R -Squared Value = 0.9521

# Quadratic model
qd_model <- lm(wg~cc+CC_sq,data=cc.wg)
summary(qd_model)
confint(quad_mod,level=0.95)
predict(quad_mod,interval="predict")
# Adjusted R-Squared = 0.9433
#Multiple R -Squared Value = 0.9521

# Cubic model
poly_mod <- lm(wg~cc+I(cc^2)+I(cc^3),data=cc.wg)
summary(poly_mod) # 0.9811

confint(poly_mod,level=0.95)

predict(poly_mod,interval="predict")
# Adjusted R-Squared = 0.9755
#Multiple R -Squared Value = 0.9811

model_R_Squared_values <- list(model=NULL,R_squared=NULL)
model_R_Squared_values[["model"]] <- c("reg","reg_log","reg_exp","quad_mod","poly_mod")
model_R_Squared_values[["R_squared"]] <- c(0.8968,0.7917,0.8674,0.9433,0.9755)
Final <- cbind(model_R_Squared_values[["model"]],model_R_Squared_values[["R_squared"]])
View(model_R_Squared_values)

# Cubic  model gives the best Adjusted R-Squared value
pred_final <- predict(poly_mod)
pred_final
rmse<-sqrt(mean((pred_final-wg)^2))
rmse
plot(poly_mod)
hist(residuals(poly_mod)) # close to normal distribution

