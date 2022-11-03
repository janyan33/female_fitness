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

My_Theme = theme(
  axis.title.x = element_text(size = 18),
  axis.text.x = element_text(size = 16),
  axis.title.y = element_text(size = 16), 
  axis.text.y = element_text(size = 16))


### HATCHLINGS ###
# Loading cumulative hatchlings data 
hatchlings <- read.csv("data/cumulative_weekly_hatchlings.csv")

# Cumulative hatchlings plot (don't include in manuscript)
ggplot(data = hatchlings, aes(x = week, y = cumulative, color = treatment)) + geom_point(size = 3) +
  scale_color_nejm() + geom_smooth(se = F, size = 2) + ylab("Cumulative hatchlings produced per treatment") + ylim(0, 2000) 


# Loading hatchlings per female data
hatchlings_per_fem <- read.csv("data/hatchlings.csv") %>% 
                      filter(treatment == "Low" | treatment == "High") #bc there's lots of empty rows

hatchlings_per_fem$treatment <- factor(hatchlings_per_fem$treatment, levels = c("Low", "High")) # re-order factor levels

ggplot(data = hatchlings_per_fem, aes(x = treatment, y = total, fill = treatment)) + geom_boxplot(outlier.colour = NA) + 
  scale_fill_manual(values = c("#0072b5", "#bc3c29")) + ylab("Number of hatchlings produced per female") + ylim(0, 150) +
 # geom_dotplot(binaxis='y', stackdir = 'center', dotsize = 0.5, position=position_dodge(0.8), alpha = 0.8) + My_Theme + 
  xlab("Treatment") #+ theme(legend.position = "none")

# Hatchling model
hatchlings_mod <- glmmTMB(data = hatchlings_per_fem, total ~ treatment, family = nbinom2)
hatchlings_mod <- lm(data = hatchlings_per_fem, total ~ treatment)

summary(hatchlings_mod)

plot(simulateResiduals(hatchlings_mod))



