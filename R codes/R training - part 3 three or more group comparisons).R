################################################################################
# Title:        R tutorial - Part 3 -- Three or more groups comparisons
# By:           Mark Bounthavong
# Output:       R 
# Category:     Biostatistics
# Date:         29 August 2021
# Updated:      18 November 2022
# Updated by:   Mark Bounthavong
################################################################################

# This tutorial will introduce students to multigroup comparisons using
# one-way Analysis of Variance (ANOVA)

## Clear environment
rm(list = ls())


## Set the working directory 
#### For Mac:
setwd("/Users/mbounthavong/Dropbox/UCSD Folder/Courses/R training/Data/")

#### For Windows:
setwd("C:\\Users\\mbounthavong\\Dropbox\\UCSD Folder\\Courses\\R training\\Data")


## Import file
diabetes.data <- read.csv("diabetes.csv", header = TRUE)
head(diabetes.data)
diabetes.data



###############################
## Libraries
###############################
library("ggplot2")
library("psych")

###############################
## Descriptive analysis
###############################

# GLUCOSE
#### View GLUCOSE distribution
par(mfrow = c(1, 1))
hist(diabetes.data$Glucose)

### Glucose descriptive
summary(diabetes.data$Glucose)


# AGE
#### View AGE distribution
hist(diabetes.data$Age)

### Age descriptive
summary(diabetes.data$Age)


###############################
## Create Age Categories 
## (Age.cat: 20 to 29 years, 
##           30 to 39 years, 
##           and 40 + years)
###############################

#### Create Age categories (Age.cat)
diabetes.data$Age.cat[diabetes.data$Age >= 20 & diabetes.data$Age < 30] = "20 to 29 years"
diabetes.data$Age.cat[diabetes.data$Age >= 30 & diabetes.data$Age < 40] = "30 to 39 years"
diabetes.data$Age.cat[diabetes.data$Age >= 40] = "40 +  years"
diabetes.data$Age.cat

##### Order factor levels to make output easier to read
diabetes.data$Age.cat = factor(diabetes.data$Age.cat,
                       levels=c("20 to 29 years", "30 to 39 years", "40 +  years"))



#####################################################
# Compare the mean Glucose between the Age groups
#####################################################
describeBy(diabetes.data$Glucose, group = diabetes.data$Age.cat)
IQR(diabetes.data$Glucose, na.rm = FALSE, type = 7)

#### Look at the glucose histogram distributions between Age.cat
par(mfrow = c(1, 3))  ### Build the matrix template
hist(diabetes.data$Glucose[diabetes.data$Age.cat=="20 to 29 years"])   
hist(diabetes.data$Glucose[diabetes.data$Age.cat=="30 to 39 years"])   
hist(diabetes.data$Glucose[diabetes.data$Age.cat=="40 +  years"])   

#### Box plots of mean glucose across age categories
par(mfrow = c(1, 1))  ### Build the matrix template
boxplot(diabetes.data$Glucose ~ diabetes.data$Age.cat)


#### Normality test
shapiro.test(diabetes.data$Glucose)
shapiro.test(diabetes.data$Glucose[diabetes.data$Age.cat=="20 to 29 years"])
shapiro.test(diabetes.data$Glucose[diabetes.data$Age.cat=="30 to 39 years"])
shapiro.test(diabetes.data$Glucose[diabetes.data$Age.cat=="40 +  years"])


###############################
## Statistical tests
###############################

#### One-way ANOVA

#### Parametric three-group comparison - One-way ANOVA
# H0: The mean glucose levels are not different across the age categories
#     no Outcomes (H0: mu1 = mu2 = mu3) 
# Ha: The mean glucose levels are different across the age categories
#     no Outcomes (Ha: mu1 != mu2 != mu3) 
summary(aov(diabetes.data$Glucose ~ diabetes.data$Age.cat))


#### Post hoc test
TukeyHSD(aov(diabetes.data$Glucose ~ diabetes.data$Age.cat))




#### Kruskal-Wallis test

#### Non-Parametric three-group comparison - Kruskal-Wallis test
# H0: The median glucose levels are not different across the age categories
#     no Outcomes (H0: mu1 = mu2 = mu3) 
# H0: The median glucose levels are different across the age categories
#     no Outcomes (Ha: mu1 != mu2 != mu3) 
kruskal.test(diabetes.data$Glucose ~ diabetes.data$Age.cat)


#### Post hoc test
pairwise.wilcox.test(diabetes.data$Glucose, diabetes.data$Age.cat, 
                     p.adjust.method = "BH") ### Benjamini & Hochberg adjustment method

