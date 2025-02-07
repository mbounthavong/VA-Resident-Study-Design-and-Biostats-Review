################################################################################
# Title:        R tutorial - Part 4 - Confounding and Interactions
# By:           Mark Bounthavong
# Output:       R 
# Category:     Biostatistics
# Date:         26 September 2021
# Updated:      19 January 2024
# Updated by:   Mark Bounthavong
################################################################################

# This tutorial will center around using the R package "epitools".

### Clear the environment
rm(list = ls())


### Install the package "epitools"
install.packages("epitools")    ### Once installed, you can comment this line
library("epitools")             ### Load the epitools package

### Install the package "lawstat"
install.packages("lawstat")
library("lawstat")

install.packages("epiR")
library("epiR")


##########################################
### Estimate the risk and odds ratios
##########################################
# Step 1: create a matrix
Table1 <- matrix(c(11, 36, 518, 517), nrow = 2, ncol = 2)
Table1

# Step 2: Estimate the RR
riskratio.wald(Table1, rev = c("both"))

# Step 3: Estimate the OR
oddsratio.wald(Table1, rev = c("both"))

### You can double check your work
num <- 11 * 517
denom <- 518 * 36
OR <- num / denom
OR


# Step 4: Estimate the risk difference
risk1 <- 11/ (518 + 11)
risk2 <- 36 / (517 + 36)
RD <- risk1 - risk2
RD

baselinerisk <- 0.087
reducedrisk <- baselinerisk * 0.40
reducedrisk

relativechange <- (baselinerisk - reducedrisk) / baselinerisk
relativechange





##############################################################
# Motivating Example #1 (Does Drug A or B cause Death?)
##############################################################
Table2 <- matrix(c(250, 150, 2000, 1500), nrow = 2, ncol = 2)
Table2

riskratio.wald(Table2, rev = c("both"))
oddsratio.wald(Table2, rev = c("both"))




##########################################
# Confounding
##########################################

### Distribution of Exercise across Drug groups
# Total
Table3_0 <- matrix(c(1750, 500, 850, 800), nrow = 2, ncol = 2)
Table3_0

riskratio.wald(Table3_0, rev = c("both"))
oddsratio.wald(Table3_0, rev = c("both"))

# Among Exercise (N=2600)
Table3_1 <- matrix(c(150, 50, 1600, 800), nrow = 2, ncol = 2)
Table3_1

riskratio.wald(Table3_1, rev = c("both"))
oddsratio.wald(Table3_1, rev = c("both"))

# Among No exercise (N=1300)
Table3_2 <- matrix(c(100, 100, 400, 700), nrow = 2, ncol = 2)
Table3_2

riskratio.wald(Table3_2, rev = c("both"))
oddsratio.wald(Table3_2, rev = c("both"))

### MH RR and OR [use epi.2by2() function]
matrix.array <- array(c(Table3_1, Table3_2), dim = c(2, 2, 2))
matrix.array
epi.2by2(matrix.array, digits = 3)


# Among Drug A (N=2250)
Table3 <- matrix(c(150, 100, 1600, 400), nrow = 2, ncol = 2)
Table3

riskratio.wald(Table3, rev = c("both"))
oddsratio.wald(Table3, rev = c("both"))


# Among Drug B (N=1650)
Table4 <- matrix(c(50, 100, 800, 700), nrow = 2, ncol = 2)
Table4

riskratio.wald(Table4, rev = c("both"))
oddsratio.wald(Table4, rev = c("both"))


### Exercise associated with Death?
# Among those who DONT exercise
Table5<- matrix(c(200, 200, 2400, 1100), nrow = 2, ncol= 2)
Table5

riskratio.wald(Table5, rev = c("both"))
oddsratio.wald(Table5, rev = c("both"))




##########################################
# Interactions or Effect Modification
##########################################
# Among those who exercise (Drug associated with Death)
Table6 <- matrix(c(150, 50, 1600, 800), nrow = 2, ncol= 2)
Table6

riskratio.wald(Table6, rev = c("both"))
oddsratio.wald(Table6, rev = c("both"))

# Among those who DONT exercise (Drug associated with Death)
Table7<- matrix(c(100, 100, 400, 700), nrow = 2, ncol= 2)
Table7

riskratio.wald(Table7, rev = c("both"))
oddsratio.wald(Table7, rev = c("both"))




