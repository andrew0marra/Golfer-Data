## Project:  STA 215, Fall 2023, Final Project
# Located:   Kline TCNJ Google Drive
# File Name: template
# Date:      2023_11_16
# Who:       Zachary D. Kline



## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(haven)
library(forcats)
library(psych)

# Load data 
data <- read_delim("raw_data.csv")



##################################################################################
############### STEP 1: Table 1    ####################   
##################################################################################
mean(data$Driver_Distance)
mean(data$Tournments_won)
mean(data$`Money `)
mean(data$Age)
table(data$Golf_Clubs)
table(data$`Right_Left `)
table(data$`Active `)
sd(data$Driver_Distance)
sd(data$Tournments_won)
sd(data$`Money `)
sd(data$Age)
min(data$Driver_Distance)
max(data$Driver_Distance)
min(data$Tournments_won)
max(data$Tournments_won)
sd(data$Golf_Clubs)
sd(data$`Right_Left `)
##################################################################################
####################  STEP 2: Table  2             ####################   
##################################################################################
table(data$Golf_Clubs)
table(data$`Right_Left `)
table(data$Golf_Clubs, data$Driver_Distance)
##################################################################################
####################   STEP 3: Chi squared test             ####################   
##################################################################################
chisq.test(data$Golf_Clubs)
chisq.test(data$Driver_Distance)
chisq.test(data$Tournments_won)
chisq.test(data$`Right_Left `)
##################################################################################
####################  STEP 4: ANOVA                ####################   
##################################################################################
anova_adapted <- aov(Driver_Distance ~ Golf_Clubs, data = data)
# Summarize ANOVA results
summary(anova_adapted)

anova_adapted <- aov(Tournments_won ~ `Active `, data = data)
# Summarize ANOVA results
summary(anova_adapted)

anova_adapted <- aov(`Money ` ~ `Right_Left `, data = data)
# Summarize ANOVA results
summary(anova_adapted)
##################################################################################
####################  STEP 5: Correlation                ####################   
##################################################################################
cor(data$Driver_Distance, data$Golf_Clubs)
cor(data$Tournments_won, data$`Money `)
##################################################################################
####################  STEP 6: Linear Regression                ####################   
##################################################################################
linear_relationship <- lm(data$Tournments_won ~ data$Driver_Distance)
summary(linear_relationship)

##################################################################################
####################  STEP 7: Figure 1                                ####################   
##################################################################################
x_mean <- mean(data$Driver_Distance)
y_mean <- mean(data$Tournments_won)
linear_plot <- plot(data$Driver_Distance, data$Tournments_won)

print(linear_relationship)
abline(linear_relationship, col = "red")
abline(h=y_mean, col = "red")
abline(v=x_mean, col = "red")

##################################################################################
####################  STEP 8: Examine residuals                     ####################   
##################################################################################
plot(data$Tournments_won, residuals(linear_relationship))
plot(data$Driver_Distance, residuals(linear_relationship))