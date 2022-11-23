## setwd("C:/Users/jy33/OneDrive/Desktop/R/female_fitness")

library(ggplot2); theme_set(theme_classic())
library(survival)
library(survminer)
library(tidyverse)
library(ggsci)
library(car)

My_Theme = theme(
  axis.title.x = element_text(size = 18),
  axis.text.x = element_text(size = 16),
  axis.title.y = element_text(size = 18), 
  axis.text.y = element_text(size = 16))

survival <- read.csv("data/longevity.csv", stringsAsFactors = TRUE)

## Survival curves
curve <- survfit(Surv(longevity) ~ treatment, data = survival)

ggsurvplot(curve, data = survival, legend = "none", linetype = 1, size = 1.8, alpha = 0.7, color = "strata", 
           palette = "nejm", font.x = 22, 
           font.y = 22, font.tickslab = 16) + xlab("Day") + ylab("Proportion of females alive")

## Survival model
long_model <- coxph(Surv(longevity) ~ treatment, data = survival)
summary(long_model)
Anova(long_model)
