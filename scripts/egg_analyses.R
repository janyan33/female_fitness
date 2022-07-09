## setwd("C:/Users/jy33/OneDrive/Desktop/R/female_fitness")

library(tidyverse)
library(ggplot2); theme_set(theme_classic())
library(lme4)
library(DHARMa)
library(car)
library(ggsci)

### Weekly egg laying rate ###
# Loading egg data
eggs <- read.csv("data/female_fitness_eggs_week.csv", stringsAsFactors = TRUE)
eggs$week <- as.factor(eggs$week)

# Egg plots
ggplot(data = eggs, aes(x = week, y = eggs, fill = treatment)) + #geom_bar(stat = "identity", position = "dodge") + 
  ylab("Number of eggs produced per female") + xlab("Week") + geom_boxplot(outlier.colour = NA) + scale_fill_nejm() + 
  geom_dotplot(binaxis='y', stackdir = 'center', dotsize = 0.35, position=position_dodge(0.8), alpha = 0.5)

eggs$week <- as.numeric(eggs$week)

egg_model <- lm(data = eggs, eggs ~ treatment + week)
plot(egg_model)
summary(egg_model)

