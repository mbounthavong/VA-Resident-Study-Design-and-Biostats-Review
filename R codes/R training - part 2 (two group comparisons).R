################################################################################
# Title:        R tutorial - Part 2 -- Bivariate comparisons
# By:           Mark Bounthavong
# Output:       R 
# R Version:    4.2.3 "Shortstop Beagle"
# Category:     Biostatistics
# Date:         07 August 2021
# Updated:      27 December 2024
# Updated by:   Mark Bounthavong
################################################################################

# This tutorial will introduce students to using R and R studio.


## Clear environment
rm(list = ls())


###############################
## Libraries
###############################
if (!require("pacman")) install.packages("pacman"); library("pacman")
p_load("ggplot2", 
       "psych", 
       "tidyverse")


###############################
## Import file from GitHub site
###############################
urlfile = "https://raw.githubusercontent.com/mbounthavong/VA-Resident-Study-Design-and-Biostats-Review/refs/heads/main/Data/diabetes.csv"
diabetes.data <- read.csv(url(urlfile))
head(diabetes.data)

###############################
## Data manipulation
###############################
## Add subject id
diabetes.data["subject_id"] <- seq(1, 768)

## Rearrange: subject_id first
diabetes.data <- diabetes.data %>% select(subject_id, everything())  




###############################
## Creating new variables
###############################

#### Method 1: Create a group -- High pregnancy (3.5 or more pregnancies) and Low pregnancy groups (< 3.5 pregnancies)
diabetes.data$High.Pregnancy[diabetes.data$Pregnancies <=3.5] = 0
diabetes.data$High.Pregnancy[diabetes.data$Pregnancies > 3.5] = 1
diabetes.data$High.Pregnancy

#### Method 2: Create a group -- High pregnancy (3.5 or more pregnancies) and Low pregnancy groups (< 3.5 pregnancies)
diabetes.data$High.Pregnancy <- ifelse(diabetes.data$Pregnancies > 3.5, c("High-preg"), c("Low-preg"))
diabetes.data$High.Pregnancy

#### Look at the histogram distributions between High and Low pregnancies
par(mfrow = c(1, 2))  ### Build the matrix template
hist(diabetes.data$Pregnancies[diabetes.data$High.Pregnancy==0])   ## hist for Low-preg group
hist(diabetes.data$Pregnancies[diabetes.data$High.Pregnancy==1])  ## hist for High-preg group

#### Example 1: Create a variable called High thickness (defined as >= 25 mm):
diabetes.data$High.thickness <- ifelse(diabetes.data$SkinThickness >= 25, c("High thickness"), c("Low thickness"))
diabetes.data$High.thickness

par(mfrow = c(1, 2))
hist(diabetes.data$SkinThickness[diabetes.data$High.thickness=="High thickness"])
hist(diabetes.data$SkinThickness[diabetes.data$High.thickness=="Low thickness"])



###############################
## Descriptive analysis
###############################


### By Groups
table(diabetes.data$High.Pregnancy)

### High.Pregnancy = 0 (<= 3.5 pregnancies)
### High.Pregnancy = 1 (> 3.5 pregnancies)
describeBy(diabetes.data$BMI, group = diabetes.data$High.Pregnancy)

### For all variables
describeBy(diabetes.data, group = diabetes.data$High.Pregnancy)




###############################
## Statistical tests
###############################

#### Let's compare BMI between the High.Pregnancy categories

#### View distributions
hist(diabetes.data$BMI)

#### Method #1
par(mfrow = c(1, 2))
hist(diabetes.data$BMI[diabetes.data$High.Pregnancy==0])
hist(diabetes.data$BMI[diabetes.data$High.Pregnancy==1])

#### Method #2
ggplot(diabetes.data, aes(x = BMI)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(High.Pregnancy ~ .)



#### One-sample t-test
# H0: The mean number of pregnancies is not different from 3.5 (H0: mu = 3.5)
# Ha: The mean number of pregnancies is different from 3.5 (Ha: mu != 3.5)
t.test(diabetes.data$Pregnancies, mu = 3.5) 


#### Parametric two-group comparison - Independent t test
# H0: The mean BMI in the group with the High Pregnancy is not 
#     different from the mean BMI in the group with 
#     Low Pregnancy (H0: mu1 = mu2)
# Ha: The mean BMI in the group with the High Pregnancy is  
#     different from the mean BMI in the group with 
#     Low Pregnancy (Ha: mu1 != mu2)
t.test(diabetes.data$BMI ~ diabetes.data$High.Pregnancy, alternative = "two.sided", mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)
t.test(diabetes.data$BMI ~ diabetes.data$High.Pregnancy)


#### Let's compare the mean age between the two groups:
describeBy(diabetes.data$Age, group = diabetes.data$High.Pregnancy)

par(mfrow = c(1, 2))
hist(diabetes.data$Age[diabetes.data$High.Pregnancy==0])
hist(diabetes.data$Age[diabetes.data$High.Pregnancy==1])

t.test(diabetes.data$Age ~ diabetes.data$High.Pregnancy, alternative = "two.sided", mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)


attach(diabetes.data)          ### You can attached the data 
t.test(BMI ~ High.Pregnancy)   ### No need to preface each var with data set
detach(diabetes.data)


#### Non-parametric two-group comparison - Mann-Whitney U test
wilcox.test(diabetes.data$BMI ~ diabetes.data$High.Pregnancy)


#### Test for normality (Kolmogorov-Smirnoff test)
## HO: There is no difference in the distribution of the variable from a normal distribution
## Ha: There is a difference in the distribution of the variable from a normal distribution
ks.test(diabetes.data$BMI, pnorm)
hist(diabetes.data$BMI)

#### Test for normality (Shapiro-Wilk's test)
shapiro.test(diabetes.data$BMI)
shapiro.test(diabetes.data$BMI[diabetes.data$High.Pregnancy==0])
shapiro.test(diabetes.data$BMI[diabetes.data$High.Pregnancy==1])

#### Example 1: Is glucose normally distributed?
hist(diabetes.data$Glucose)
ks.test(diabetes.data$Glucose, pnorm)
shapiro.test(diabetes.data$Glucose)

#### Example 2: Is BP normally distributed?
hist(diabetes.data$BloodPressure)
ks.test(diabetes.data$BloodPressure, pnorm)
shapiro.test(diabetes.data$BloodPressure)

#### EXample 3: Is BMI normally distributed?
hist(diabetes.data$BMI)
ks.test(diabetes.data$BMI, pnorm)
shapiro.test(diabetes.data$BMI)

#### Example 4: Is skin thickness normally distributed?
hist(diabetes.data$SkinThickness)
ks.test(diabetes.data$SkinThickness, pnorm)
shapiro.test(diabetes.data$SkinThickness)



######################################
## Categorical data analysis (2 x 2)
######################################

#### 2 x 2 contingency table
table(diabetes.data$High.thickness, diabetes.data$High.Pregnancy)
table1 <- table(diabetes.data$High.thickness, diabetes.data$High.Pregnancy)
prop.table(table1, margin = 2)

#### Chi square test -- Method #1
# H0: The number of high pregnancies in the group with the Outcome is
#     not different from the group without the Outcome (H0: n1 = n2)
# Ha: The number of high pregnancies in the group with the Outcome is
#     different from the group without the Outcome (Ha: n1 != n2)
chisq.test(diabetes.data$High.thickness, diabetes.data$High.Pregnancy, correct = FALSE)

#### Chi square test -- Method #2
### install.packages("gmodels") ### Code used to install the gmodels package
library("gmodels")

CrossTable(diabetes.data$High.thickness, diabetes.data$High.Pregnancy, chisq = TRUE, missing.include = TRUE)


#### Fisher's exact test
# When there are cells with < 5 observations or when the total observations
# is less than 20; these are just general rules. Users should apply these
# at their discretion.
fisher.test(diabetes.data$High.thickness, diabetes.data$High.Pregnancy)





