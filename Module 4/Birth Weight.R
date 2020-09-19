# Title: Birth Weight Analysis Script 
# Author: Grant A. Allard
# Author's Email: grant@grantallard.com
# Date Created: 2020-09-18

# Purpose:
#A script that provides real world statistical data and interpretation for rejection hypotheses on a logic tree. 


# Set Up####
# Libraries
library(tidyverse)
library(MASS)
library(stargazer)

# Data
birthwt

# Script ####

#Convert Factor Variables####
birthwt$low <- factor(birthwt$low, levels = c(0,1),labels = c("No", "Yes"))
birthwt$race <- factor(birthwt$race, levels = c(1:3), labels=c("white","black","other"))
birthwt$smoke <- factor(birthwt$smoke, levels = c(0,1), labels = c("No", "Yes"))
birthwt$ht <- factor(birthwt$ht, levels = c(0,1), labels = c("No", "Yes"))
birthwt$ui <- factor(birthwt$ui, levels = c(0,1),labels = c("No", "Yes"))
birthwt$ptl <- factor(birthwt$ptl)
birthwt$ftv <- factor(birthwt$ftv)

# Scatter plots 
lwtPlot<-birthwt %>% 
  ggplot(aes(x=lwt, y=bwt))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE, color="red")+
  geom_smooth(method="lm", formula=y~1, se=FALSE)+
  ggtitle("Birth Weight x Mother's Weight during Pregnancy")+
  labs(y="Birth Weight (in grams)",
       x="Mother's Weight during Pregnancy (lbs)")+
  theme(axis.text.x = element_text(size=9),
        axis.text.y = element_text(size=9))
lwtPlot 

# Smoking - Comparison of Means
smoking_plot<-birthwt %>% 
  group_by(smoke) %>% 
  summarize(bwt=sum(bwt), mean_bwt = bwt/n(), sd_bwt=sd(bwt)) %>%
  ggplot(aes(x=smoke, y=mean_bwt, fill=smoke))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=mean_bwt, ymax=sd_bwt))+
  ggtitle("Differences in Mean Birthweigth x Mother's Smoking")+
  labs(y="Mean Birth Weight in grams")
smoking_plot

#Smoking Box
smoking_box<-birthwt %>% 
  ggplot(aes(x=smoke, y=bwt, fill=smoke))+
  geom_boxplot()
smoking_box


  
# Copyright (c) Grant Allard, 2020
