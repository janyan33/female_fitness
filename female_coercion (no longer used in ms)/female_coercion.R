## setwd("C:/Users/jy33/OneDrive/Desktop/R/female_fitness")

library(tidyverse)
library(ggplot2); theme_set(theme_classic())
library(lme4)
library(DHARMa)
library(car)
library(ggsci)
library(glmmTMB)

My_Theme = theme(
  axis.title.x = element_text(size = 16),
  axis.text.x = element_text(size = 14),
  axis.title.y = element_text(size = 16), 
  axis.text.y = element_text(size = 16), 
  legend.title = element_text(size = 16))

## USED THIS SECTION OF SCRIPT TO GENERATE THE CLEAN DATA FILE
# Consolidated dataframe 
all_data <- read.csv("female_coercion/female_coercion_all_R.csv")

all_data <- all_data %>% 
           mutate(trial_dur = trial_end - encounter) 

all_data$trial_dur[is.na(all_data$trial_dur)] <- 1200
all_data$running_away_dur[is.na(all_data$running_away_dur)] <- 0
all_data$refusal_posture[is.na(all_data$refusal_posture)] <- 0
all_data$insem_resist[is.na(all_data$insem_resist)] <- 0
all_data$mounts[is.na(all_data$mounts)] <- 0

all_data <- all_data %>%  
            mutate(prop_run = (running_away_dur + refusal_posture)/(trial_dur - insem_dur)) %>% 
            mutate(prop_insem_resist = insem_resist/insem_dur) %>% 
            mutate(insem_lat = insem_start - encounter)

all_data$day <- as.factor(all_data$day)

# Insemination duration boxplot
ggplot(data = all_data, aes(x = day, y = insem_dur, fill = treatment)) + geom_boxplot() + 
       scale_fill_nejm() + ylab("Insemination duration (s)") + xlab("Day") + My_Theme +
       coord_cartesian(ylim = c(0, 150))

# Insemination latency boxplot
ggplot(data = all_data, aes(x = day, y = insem_lat, fill = treatment)) + geom_boxplot() + 
       scale_fill_nejm() + ylab("Insemination latency (s)") + xlab("Day") + My_Theme + 
       coord_cartesian(ylim = c(0, 1000))

# Proportion of trial spent running away boxplot
ggplot(data = all_data, aes(x = day, y = prop_run, fill = treatment)) + geom_boxplot() + 
       scale_fill_nejm() + ylim(0, 0.2) + ylab("Female evasion rate") + 
       xlab("Day") + My_Theme + coord_cartesian(ylim = c(0, 0.2))

## MODELS
focal_data <- read.csv("female_coercion/focal_data.csv")

focal_data$arena <- as.factor(focal_data$arena)

# Insemination duration model
duration_lm <- lmer(data = focal_data, log(con_insem_dur + 60) ~ day + (1|arena))
plot(simulateResiduals(duration_lm)) # looks fine
Anova(duration_lm, test.statistic = "Chisq")

# Insemination latency model
latency_glm <- lmer(data = focal_data, log(con_insem_lat + 130) ~ day + (1|arena))
plot(simulateResiduals(latency_glm)) # looks fine
Anova(latency_glm, test.statistic = "Chisq")

# Proportion of trial spent running away model
running_away_glm <- lmer(data = focal_data, log(con_prop_run_2) ~ day + 
                        (1|arena))
plot(simulateResiduals(running_away_glm))
Anova(running_away_glm, test.statistic = "Chisq")


#### ANALYSES FOR EXPERIMENT 3 WITHOUT CORRECTION FOR DAY EFFECTS
# Insemination duration boxplot
duration_mod <- lmer(data = focal_data, log(insem_dur) ~ day + (1|arena))
plot(simulateResiduals(duration_mod)) # looks good
Anova(duration_mod)

## Insemination latency
latency_mod <- lmer(data = focal_data, log(insem_lat) ~ day + (1|arena))
plot(simulateResiduals(latency_mod)) # looks good
Anova(latency_mod)

# Proportion of trial spent running away model
evasion_mod <- lmer(data = focal_data, asin(sqrt(prop_run)) ~ day + (1|arena))
plot(simulateResiduals(evasion_mod)) # looks good
Anova(evasion_mod)

