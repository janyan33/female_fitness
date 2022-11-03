## setwd("C:/Users/jy33/OneDrive/Desktop/R/female_fitness")

library(tidyverse)
library(ggplot2); theme_set(theme_classic())
library(lme4)
library(DHARMa)
library(car)
library(ggsci)
library(lme4)
library(MASS)
library(glmmTMB)

### HATCHLINGS ###
# Loading hatchling 
hatchlings <- read.csv("data/cumulative_weekly_hatchlings.csv")

# Hatchling plot
ggplot(data = hatchlings, aes(x = week, y = cumulative, color = treatment)) + geom_point(size = 3) +
  scale_color_nejm() + geom_smooth(se = F, size = 2) + ylab("Cumulative hatchlings produced per treatment") + ylim(0, 2000) 

hatchlings_per_fem <- read.csv("data/hatchlings.csv") %>% 
  filter(treatment == "low" | treatment == "high")

hatchlings_per_fem$treatment <- as.factor(hatchlings_per_fem$treatment)

ggplot(data = hatchlings_per_fem, aes(x = treatment, y = total, fill = treatment)) + geom_boxplot(outlier.colour = NA) + 
  scale_fill_nejm() + ylab("Total number of hatchlings produced per female") + ylim(40, 150) +
  geom_dotplot(binaxis='y', stackdir = 'center', dotsize = 0.5, position=position_dodge(0.8), alpha = 0.5)

# Hatchling model
hatchlings_mod <- glmmTMB(data = hatchlings_per_fem, total ~ treatment, family = nbinom2)

summary(hatchlings_mod)

plot(simulateResiduals(hatchlings_mod))



