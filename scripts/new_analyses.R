## setwd("C:/Users/jy33/OneDrive/Desktop/R/female_fitness")

library(tidyverse)
library(ggplot2); theme_set(theme_classic())
library(lme4)
library(DHARMa)
library(car)
library(ggsci)
library(lme4)
library(glmmTMB)

My_Theme = theme(
  axis.title.x = element_text(size = 18),
  axis.text.x = element_text(size = 18),
  axis.title.y = element_text(size = 18), 
  axis.text.y = element_text(size = 18))

## NEW MODELS 2023-07-27
weekly_data <- read.csv("data/female_fitness_eggs_week.csv") %>% 
               mutate(treatment_arena = paste(treatment, arena, sep = "")) %>% 
               filter(treatment_arena != "high11" & treatment_arena != "low17") # filter out discarded arenas

# Turn variables into factors
weekly_data$treatment <- as.factor(weekly_data$treatment) 
weekly_data$arena <- as.factor(weekly_data$arena) 

# EGG LAYING RATE MODEL
egg_mod <- glmmTMB(data = weekly_data, eggs ~ treatment + week + offset(log(days_alive)) + 
                 (1|treatment:arena), 
                 family = nbinom1())

plot(simulateResiduals(egg_mod), form = weekly_data[!(is.na(weekly_data$eggs)),]$treatment)

Anova(egg_mod)

# HATCHLING PRODUCTION MODEL
hatchling_mod <- lmer(data = weekly_data, hatchlings ~ treatment + week + (1|treatment:arena))

plotResiduals(hatchling_mod, form = weekly_data$week)

Anova(hatchling_mod)

# HATCH RATE MODEL (THIS MODEL IS WRONG, THERE ARE NAs IN THE EGGS - HATCHLINGS FORMULA THAT NEED TO BE DEALT WITH)
hatch_rate_mod <- glmer(data = weekly_data, cbind(hatchlings, (eggs - hatchlings)) ~
                        treatment + week + (1|treatment:arena), family = binomial())

plot(simulateResiduals(hatch_rate_mod))

Anova(hatch_rate_mod)

