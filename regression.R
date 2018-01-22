#LOAD LIBRARIES
library(tidyverse)
library(readxl)

#IMPORT DATA
data <- read_excel("C:/Alice Wei/WFU/MSBA/Mini 4/Marketing Analytics/Assignment 1/Case Assignment 1 Data - Vodka Data.xlsx", sheet = "data")
dataDic <- read_excel("C:/Alice Wei/WFU/MSBA/Mini 4/Marketing Analytics/Assignment 1/Case Assignment 1 Data - Vodka Data.xlsx", sheet = "data dictionary")

#Q1
Q1Regression <- lm(LnSales ~ PriceRerUnit + Print + Outdoor + Broad + LagTotalSales, data)
Q1glm <- glm(LnSales ~ PriceRerUnit + Print + Outdoor + Broad + LagTotalSales, data , family = gaussian)
summary(Q1Regression)
summary(Q1glm)

plot(Q1Regression)
plot(residuals(Q1Regression))


#Q2
Q2Regression <- lm(LnDiff ~ LnLPrice + LnPrint + LnOut + LnBroad, data )
summary(Q2Regression)
plot(Q2Regression)
plot(residuals(Q2Regression))

#Q3
Q3Regression <- lm(LnDiff ~ LnLPrice + LnPrint + LnOut + LnBroad + Tier1 + Tier2, data )
summary(Q3Regression)
plot(Q3Regression)
plot(residuals(Q3Regression))


#4
Q4Regression <- lm(LnDiff ~ LnLPrice + LnPrint + LnOut + LnBroad + Tier1 + Tier2 + LagTotalMinusSales, data )
summary(Q4Regression)
plot(Q4Regression)
plot(residuals(Q4Regression))


#5
Q5Regression <- lm(LnDiff ~ LnLPrice + LnPrint + LnOut + LnBroad + Tier1 + Tier2 + LagTotalMinusSales + Firstintro, data )
summary(Q5Regression)
plot(Q5Regression)
plot(residuals(Q5Regression))


#Compare Models
anova(Q2Regression, Q3Regression, Q4Regression, Q5Regression)

