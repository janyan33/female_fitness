## setwd("C:/Users/jy33/OneDrive/Desktop/R/female_fitness")

library(ggplot2); theme_set(theme_classic())
library(survival)
library(survminer)
library(tidyverse)
library(ggsci)

survival <- read.csv("data/longevity.csv", stringsAsFactors = TRUE)

## Survival curves
curve <- survfit(Surv(longevity) ~ treatment, data = survival)

ggsurvplot(curve, data = survival, legend = "none", linetype = 1, size = 1.8, alpha = 0.7, color = "strata", 
           palette = "nejm", break.x.by = 5) + xlab("Day")

## Survival model
long_model <- coxph(Surv(longevity) ~ treatment, data = survival)
summary(long_model)
