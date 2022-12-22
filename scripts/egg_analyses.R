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
# Loading egg data
eggs <- read.csv("data/female_fitness_eggs_week.csv")

eggs$treatment <- as.factor(eggs$treatment)
eggs$week <- as.factor(eggs$week)
eggs$daily_rate <- as.numeric(eggs$daily_rate)

# Weekly egg-laying rate boxplot
ggplot(data = eggs, aes(x = week, y = daily_rate_weighted, fill = treatment)) + #geom_bar(stat = "identity", position = "dodge") + 
  ylab("Average daily egg production") + xlab("Week") + geom_boxplot(outlier.colour = NA) + scale_fill_nejm() + 
  geom_dotplot(binaxis='y', stackdir = 'center', dotsize = 0.35, position=position_dodge(0.8), alpha = 0) + 
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16))

eggs$week <- as.numeric(eggs$week)

egg_model <- lm(data = eggs, daily_rate_weighted ~ treatment+week)
summary(egg_model)
Anova(egg_model)

#plot(egg_model)

# Weekly hatch rate boxplot
ggplot(data = eggs, aes(x = week, y = hatch_rate, fill = treatment)) + #geom_bar(stat = "identity", position = "dodge") + 
  ylab("Average weekly egg hatch rate") + xlab("Week") + geom_boxplot(outlier.colour = NA) + scale_fill_nejm() + 
  geom_dotplot(binaxis='y', stackdir = 'center', dotsize = 0.35, position=position_dodge(0.8), alpha = 0) + 
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16))
