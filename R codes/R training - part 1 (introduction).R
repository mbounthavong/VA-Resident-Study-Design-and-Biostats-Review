###########################################################################################
# Title:        R tutorial - Part 1
# By:           Mark Bounthavong
# Output:       R 
# R version:    4.2.3 "Shortstop Beagle"
# Category:     Biostatistics
# Date:         16 September 2022
# Updated:      08 November 2024
# Updated by:   Mark Bounthavong
###########################################################################################

# This tutorial will introduce students to using R and R studio.


## Clear environment
rm(list = ls())



###############################
#### Load libraries
###############################
# install.packages("tidyverse")
# install.packages("Hmisc")
# install.packages("gmodels")
# install.packages("psych")
library("tidyverse")
library("Hmisc")
library("gmodels")
library("psych")
library("readr")


###############################
#### R basics
###############################
## Syntax
a <- 10
b <- 2
a / b


###############################
## Import data
###############################
urlfile = "https://raw.githubusercontent.com/mbounthavong/VA-Resident-Study-Design-and-Biostats-Review/refs/heads/main/Data/diabetes.csv"
diabetes.data <- read.csv(url(urlfile))
head(diabetes.data)


## Describe the data
describe(diabetes.data)
diabetes.data %>% summarise_all(typeof) %>% gather


###############################
## Data manipulation
###############################
## Add subject id
diabetes.data["subject_id"] <- seq(1, 768)

## Rearrange: subject_id first
diabetes.data <- diabetes.data %>% select(subject_id, everything())  


#### Create a group -- High pregnancy (3.5 or more pregnancies) and Low pregnancy groups (< 3.5 pregnancies)
diabetes.data$High.Pregnancy[diabetes.data$Pregnancies <=3.5] = 0
diabetes.data$High.Pregnancy[diabetes.data$Pregnancies > 3.5] = 1
diabetes.data$High.Pregnancy

#### Create multiple group
diabetes.data$High.Pregnancy3[diabetes.data$Pregnancies < 2] = 0
diabetes.data$High.Pregnancy3[diabetes.data$Pregnancies >= 2 & diabetes.data$Pregnancies < 4] = 1
diabetes.data$High.Pregnancy3[diabetes.data$Pregnancies >= 4 & diabetes.data$Pregnancies < 6] = 2
diabetes.data$High.Pregnancy3[diabetes.data$Pregnancies >= 6 & diabetes.data$Pregnancies < 8] = 3
diabetes.data$High.Pregnancy3[diabetes.data$Pregnancies >= 8 & diabetes.data$Pregnancies < 10] = 4
diabetes.data$High.Pregnancy3[diabetes.data$Pregnancies >= 10] = 5
diabetes.data$High.Pregnancy3


## Relocate "High.Pregnancy" to after "subject_id"
diabetes.data <- diabetes.data %>% relocate(High.Pregnancy, .after = subject_id)



###############################
## Plots
###############################
#### Method #1
plot(x = diabetes.data$Pregnancies, y = diabetes.data$Glucose)


#### Method #2
#install.packages("ggplot2")     ### Install R Package (Once installed, you can comment this out)
library("ggplot2")              ### Load library

ggplot(data = diabetes.data, aes(x = Pregnancies, y = Glucose)) + 
  geom_point(data = diabetes.data, aes(y = Glucose), colour = 'blue', size = 3)



###############################
## Descriptive analysis
###############################
#### Histogram
hist(diabetes.data$Pregnancies)

#### Method #1
par(mfrow = c(1, 2))
hist(diabetes.data$Pregnancies[diabetes.data$Outcome==0])
hist(diabetes.data$Pregnancies[diabetes.data$Outcome==1])

#### Method #2
ggplot(diabetes.data, aes(x = Pregnancies)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(Outcome ~ .)



###############################
## Table 1. example
###############################
t.test(diabetes.data$Age ~ diabetes.data$High.Pregnancy)
describeBy(diabetes.data$Age, group = diabetes.data$High.Pregnancy)

t.test(diabetes.data$Glucose ~ diabetes.data$High.Pregnancy)
describeBy(diabetes.data$Glucose, group = diabetes.data$High.Pregnancy)


##############################################################
## Hypothesis test - Student t test
##############################################################
t.test(diabetes.data$BMI ~ diabetes.data$High.Pregnancy)
describeBy(diabetes.data$BMI, group = diabetes.data$High.Pregnancy)

##############################################################
## Hypothesis test - chi square test
##############################################################
CrossTable(diabetes.data$High.Pregnancy, diabetes.data$Outcome, chisq = TRUE, missing.include = TRUE)

