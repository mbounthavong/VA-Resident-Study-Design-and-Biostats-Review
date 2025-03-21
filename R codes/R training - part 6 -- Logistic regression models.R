################################################################################
# Title:        R tutorial - Logistic regression models
# By:           Mark Bounthavong
# Output:       R 
# Category:     Biostatistics
# Date:         04 December 2021
# Updated:      21 March 2025
# Updated by:   Mark Bounthavong
################################################################################

#### This tutorial will provide an introduction to performing one-way ANOVA and Kruskal-Wallis tests

#### Clear the environment
rm(list = ls())


#### Load the libraries
library("ggplot2")
library("gmodels")
library("epitools")
library("psych")


###############################
## Import data
###############################
urlfile = "https://raw.githubusercontent.com/mbounthavong/VA-Resident-Study-Design-and-Biostats-Review/refs/heads/main/Data/diabetes.csv"
diabetes.data <- read.csv(url(urlfile))
head(diabetes.data)




#### Generate groups based on pregnancies (0 = No pregnancy and 1 = history of pregnancy)
diabetes.data$group[diabetes.data$Pregnancies == 0] = 0 
diabetes.data$group[diabetes.data$Pregnancies > 0] = 1 
CrossTable(diabetes.data$group)

### Create factors
data2 <- within(diabetes.data, {
  group <- factor(group, labels = c("No history of pregnancies", "History of pregnancies"))
})

### Cross tabulate
CrossTable(data2$group)


### Crude Logistic Regression Model
logit1 <- glm(group ~ Age, data = data2, family = "binomial"(link = "logit"))
summary(logit1)
confint(logit1)

### Exponentiate the coefficients
exp(coef(logit1))
exp(confint(logit1))
exp(cbind(OR = coef(logit1), confint(logit1)))

### Multivariable logistic regression model
logit2 <- glm(group ~ Age + BMI + Glucose + SkinThickness, data = data2, family = "binomial"(link = "logit"))
summary(logit2)
confint(logit2)

### Exponentiate the coefficients
exp(coef(logit2))
exp(confint(logit2))

exp(cbind(OR = coef(logit2), confint(logit2)))