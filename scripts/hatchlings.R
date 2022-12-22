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

# Loading hatchlings per female data
hatchlings_per_fem <- read.csv("data/hatchlings.csv") %>% 
                      filter(treatment == "Low" | treatment == "High")  #bc there's lots of empty rows 


hatchlings_per_fem$total <- as.numeric(hatchlings_per_fem$total)
hatchlings_per_fem$treatment <- factor(hatchlings_per_fem$treatment, levels = c("Low", "High")) # re-order factor levels

ggplot(data = hatchlings_per_fem, aes(x = treatment, y = total, fill = treatment)) + geom_boxplot(outlier.colour = NA) + 
  scale_fill_manual(values = c("#0072b5", "#bc3c29")) + ylab("Number of hatchlings produced") + ylim(0, 170) +
  geom_dotplot(binaxis='y', stackdir = 'center', dotsize = 0.5, position=position_dodge(0.8), alpha = 0.8) + My_Theme + 
  xlab("Treatment") + theme(legend.position = "none")

# Hatchling model
hatchlings_mod <- glmmTMB(data = hatchlings_per_fem, total ~ treatment, family = nbinom2(link = "log"))
Anova(hatchlings_mod)

plot(simulateResiduals(hatchlings_mod))

mean(hatchlings_per_fem$total, na.rm = TRUE)

# Hatch rate
ggplot(data = hatchlings_per_fem, aes(x = treatment, y = hatch_rate, fill = treatment)) + geom_boxplot(outlier.colour = NA) + 
  scale_fill_manual(values = c("#0072b5", "#bc3c29")) + ylab("Hatch rate per arena") +
  geom_dotplot(binaxis='y', stackdir = 'center', dotsize = 0.5, position=position_dodge(0.8), alpha = 0.8) + My_Theme + 
  xlab("Treatment") + theme(legend.position = "none")


hatch_rate_mod <- lm(data = hatchlings_per_fem, hatch_rate ~ treatment)
plot(hatch_rate_mod)
Anova(hatch_rate_mod)

