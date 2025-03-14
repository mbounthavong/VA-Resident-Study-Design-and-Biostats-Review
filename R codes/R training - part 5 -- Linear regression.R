################################################################################
# Title:        R tutorial - Linear regression
# By:           Mark Bounthavong
# Output:       R 
# Category:     Biostatistics
# Date:         27 October 2021
# Updated:      14 March 2025
# Updated by:   Mark Bounthavong
################################################################################

#### This tutorial will provide an introduction to performing Linear Regression (e.g., OLS) models



###############################
#### Clear the environment
rm(list = ls())
###############################


###############################
#### Load the libraries
###############################
library("ggplot2")
# install.packages("predict3d")

# if(!require(devtools)) install.packages("devtools")
# devtools::install_github("cardiomoon/predict3d"")
library("predict3d")


###############################
## Import data
###############################
urlfile = "https://raw.githubusercontent.com/mbounthavong/VA-Resident-Study-Design-and-Biostats-Review/refs/heads/main/Data/diabetes.csv"
diabetes.data <- read.csv(url(urlfile))
head(diabetes.data)

### Plot the association between the subject's age and glucose level
ggplot(diabetes.data, aes(x = Age, y = Glucose)) +
  geom_point() +
  stat_smooth()

### Linear regression model (Y = Glucose, X = Age)
linear.model1 <- lm(Glucose ~ Age, data = diabetes.data)
summary(linear.model1)


# Call:
#   lm(formula = Glucose ~ Age, data = diabetes.data)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -126.453  -20.849   -3.058   18.304   86.159 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 97.08016    3.34095   29.06  < 2e-16 ***
#   Age          0.71642    0.09476    7.56 1.15e-13 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 30.86 on 766 degrees of freedom
# Multiple R-squared:  0.06944,	Adjusted R-squared:  0.06822 
# F-statistic: 57.16 on 1 and 766 DF,  p-value: 1.15e-13


#### Visual the linear regression model
ggplot(diabetes.data, aes(x = Age, y = Glucose)) +
  geom_point() +
  stat_smooth(method = "lm")

ggPredict(linear.model1, digits = 1, show.point = FALSE, se = TRUE, xpos = 0.5)


### Question: Is pregnancy associated with glucose levels? 

### Create a grouping variable for pregnancy. 
###### If subject has 0 pregnancy, we code them as zero. 
###### If subject has >0 pregnancy, we code them as one 

#### Generate groups based on pregnancies (Group 1 = 0, Group 2 = 1-5, Group 3 = >5 pregnancies)
diabetes.data$group[diabetes.data$Pregnancies == 0] = 0
diabetes.data$group[diabetes.data$Pregnancies > 0] = 1
diabetes.data$group

### Linear model (Glucose = B0 + B1(Pregnancy))
linear.model2 <- lm(Glucose ~ group, data = diabetes.data)
summary(linear.model2)

#### Interpret the output:
# 
# Call:
#   lm(formula = Glucose ~ group, data = diabetes.data)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -120.539  -21.539   -4.539   19.461   78.461 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  123.000      3.036   40.52   <2e-16 ***
#   group         -2.461      3.282   -0.75    0.454    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 31.98 on 766 degrees of freedom
# Multiple R-squared:  0.0007336,	Adjusted R-squared:  -0.0005709 
# F-statistic: 0.5624 on 1 and 766 DF,  p-value: 0.4535

ggPredict(linear.model2, digits = 2)




### Question: Is pregnancy associated with glucose levels controlling for Age? 
linear.model3 <- lm(Glucose ~ group + Age, data = diabetes.data)
summary(linear.model3)
confint(linear.model3)  ## Generates the 95% CI


#### Interpret the output:

# Call:
#   lm(formula = Glucose ~ group + Age, data = diabetes.data)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -125.715  -20.546   -2.991   17.316   87.734 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 102.00752    3.95100  25.818  < 2e-16 ***
#   group        -7.47264    3.22137  -2.320   0.0206 *  
#   Age           0.76050    0.09638   7.891 1.04e-14 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 30.77 on 765 degrees of freedom
# Multiple R-squared:  0.07594,	Adjusted R-squared:  0.07352 
# F-statistic: 31.43 on 2 and 765 DF,  p-value: 7.592e-14

#### Visual the linear regression model
ggPredict(linear.model3, digits = 2)
ggPredict(linear.model3, digits = 2, show.point = FALSE, se = TRUE, xpos = 0.5)
ggPredict(linear.model3, pred = group, digits = 1, show.point = FALSE, se = TRUE, xpos = 0.5)
ggPredict(linear.model3, pred = Age, digits = 1, show.point = FALSE, se = TRUE, xpos = 0.5)


### Question: Is pregnancy associated with glucose levels controlling for Age, BMI, and BP? 
linear.model4 <- lm(Glucose ~ group + Age + BMI + BloodPressure, data = diabetes.data)
summary(linear.model4)
confint(linear.model4)  ## Generates the 95% CI



