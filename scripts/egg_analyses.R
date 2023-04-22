## setwd("C:/Users/jy33/OneDrive/Desktop/R/female_fitness")

library(tidyverse)
library(ggplot2); theme_set(theme_classic())
library(lme4)
library(DHARMa)
library(car)
library(ggsci)
library(emmeans)
library(mdscore)

My_Theme = theme(
  axis.title.x = element_text(size = 18),
  axis.text.x = element_text(size = 16),
  axis.title.y = element_text(size = 18), 
  axis.text.y = element_text(size = 16))

### Weekly egg laying rate ###
# Loading egg data for plot
eggs <- read.csv("data/female_fitness_eggs_week.csv")

eggs$treatment <- as.factor(eggs$treatment)
eggs$week <- as.factor(eggs$week)
eggs$daily_rate <- as.numeric(eggs$daily_rate)

# Weekly egg-laying rate boxplot
ggplot(data = eggs, aes(x = week, y = daily_rate_weighted, fill = treatment)) + #geom_bar(stat = "identity", position = "dodge") + 
  ylab("Average daily egg production") + xlab("Week") + geom_boxplot(outlier.colour = NA) + scale_fill_nejm() + 
  geom_dotplot(binaxis='y', stackdir = 'center', dotsize = 0.35, position=position_dodge(0.8), alpha = 0) + 
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16))


## Loading egg data for model
egg_mod_data <- read.csv("data/hatchlings.csv")

egg_model <- lm(data = egg_mod_data, egg_lay_rate ~ treatment)
summary(egg_model)
Anova(egg_model)


