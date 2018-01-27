#LOAD LIBRARIES
library(tidyverse)
library(readxl)
library(MASS)
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

#Sales are positively related to last year's sales and the print expenditure
#However, as the outdoor expenditure increase, sales will go down. 

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

StepwiseQ5 <- stepAIC(Q5Regression, DIRECTION = "both")
StepwiseQ5$anova
Q5RegressionFinal <- lm(LnDiff ~ LnLPrice + LnPrint  + Tier1 + Tier2 + LagTotalMinusSales + Firstintro, data )
summary(Q5RegressionFinal)


#6
#Compare Models
anova(Q2Regression, Q3Regression, Q4Regression, Q5Regression)
# Comparing models we discovered that as we add in variables, the R square went up and there are corelations
# between different variables. Also, pulling more variable from the Error term in Regression 2 is more accurate becuase, in general, First Intro is very important
# when looking at the sale changes. 
# The stepwies results shows that Print, Tiers, Price, Other brands sales, if it's a new brand
# Price is the only factor that will drag the sales down
# Also, it shows that people are willing to try new brands, since there is a positive relationship between FirstIntro and Change in sales


#7
#Print is the most efficient way to boost sales

