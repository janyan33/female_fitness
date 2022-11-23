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
all_data <- read.csv("female_coercion/female_coercion_all.csv")

# Dataframes for individual behaviours exported from BORIS
insem_data <- read.csv("female_coercion/insem.csv")
encounters <- read.csv("female_coercion/encounters.csv")
running_away <- read.csv("female_coercion/running_away.csv")
refusal_post <- read.csv("female_coercion/refusal_posture.csv")
insem_resist <- read.csv("female_coercion/insem_resist.csv")
mounts_num <- read.csv("female_coercion/mounts_num.csv")

# Joining dataframes
all_data <- left_join(all_data, insem_data, by = "trial_num")
all_data <- left_join(all_data, encounters, by = "trial_num")
all_data <- left_join(all_data, running_away, by = "trial_num")
all_data <- left_join(all_data, refusal_post, by = "trial_num")
all_data <- left_join(all_data, insem_resist, by = "trial_num")
all_data <- left_join(all_data, mounts_num, by = "trial_num")

write.csv(all_data, "female_coercion_all_R.csv")

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

# Proportion of insemination duration spent resisting boxplot
ggplot(data = all_data, aes(x = day, y = prop_insem_resist, fill = treatment)) + geom_boxplot() + 
       scale_fill_nejm() + ylab("Proportion of insemination spent resisting") + xlab("Day")

# Number of mounts boxplot
ggplot(data = all_data, aes(x = day, y = mounts, fill = treatment)) + geom_boxplot() + 
       scale_fill_nejm()  + ylab("Number of mounts during trial") + xlab("Day")


## MODELS
focal_data <- read.csv("female_coercion/focal_data.csv")

# Insemination duration model
duration_lm <- glm(data = focal_data, log(con_insem_dur + 60) ~ day)
plot(duration_lm) # looks fine
summary(duration_lm)
Anova(duration_lm, test.statistic = "Wald")

# Insemination latency model
latency_lm <- glm(data = focal_data, log(con_insem_lat + 200) ~ day)
plot((latency_lm)) # looks fine
summary(latency_lm)
Anova(latency_lm, test.statistic = "Wald")

# Proportion of trial spent running away model
running_away_glm <- glm(data = focal_data, log(con_prop_run + 2) ~ day)#, family = Gamma(link = "log"))
plot(running_away_glm)
Anova(running_away_glm, test.statistic = "Wald")


