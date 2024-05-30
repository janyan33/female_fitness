## setwd("C:/Users/jy33/OneDrive/Desktop/R/female_rep_status")

library(tidyverse)
library(ggplot2); theme_set(theme_classic())
library(lme4)
library(DHARMa)
library(stats)
library(car)
library(glmmTMB)
library(ggsci)
library(emmeans)

##### LOADING AND CLEANING DATA
# All observations raw
all_obs <- read.csv("female_insem_status/data/rep_status_all_data.csv") %>% 
           filter(notes != "exclude")

           all_obs$replicate <- as.factor(all_obs$replicate)
           all_obs$treatment <- as.factor(all_obs$treatment)
           all_obs$avoidances <- as.factor(all_obs$avoidances)
           all_obs$avoidance_success <- as.factor(all_obs$avoidance_success)
           all_obs$aborts <- as.factor(all_obs$aborts)
           all_obs$female_ID <- as.factor(all_obs$female_ID)
           all_obs$male_ID <- as.factor(all_obs$male_ID)
           all_obs$behaviour <- as.factor(all_obs$behaviour)

##### STATISTICAL ANALYSES 
# Probability of female avoidance
fem_avoidances_model <- glmer(data = all_obs, avoidances ~ treatment + 
                             (1|replicate:female_ID) + 
                             (1|replicate:male_ID) + (1|replicate),
                              family = binomial(link = "logit"))
summary(fem_avoidances_model) # Diagnostics fine
Anova(fem_avoidances_model)

# Probability of male abort
aborts_model <- glmer(data = all_obs, aborts ~ treatment + (1|replicate:female_ID) + 
                     (1|replicate:male_ID) + (1|replicate), 
                      family = binomial(link = "logit"))
summary(aborts_model) # Diagnostics ok 
Anova(aborts_model)
plot(simulateResiduals(aborts_model))

# Whether mounts led to inseminations
mount_insem_model <- glmer(data = all_obs, behaviour ~ treatment + 
                          (1|replicate) + (1|replicate:female_ID) + (1|replicate:male_ID), 
                           family = binomial(link = "logit"))

summary(mount_insem_model) # Diagnostics fine (Levene Test failed but plot looks fine; verified with Ben Bolker that it's ok)
Anova(mount_insem_model)
plot(simulateResiduals(mount_insem_model))

###################### DATA VISUALIZATION ######## #############
# Data summarized for each female
fem_data <- read.csv("female_insem_status/data/attributes.csv") %>%  
                     filter(sex == "female")

fem_data$replicate <- as.factor(fem_data$replicate)
fem_data$treatment <- as.factor(fem_data$treatment)
fem_data$insem_status <- as.factor(fem_data$insem_status)
fem_data$ID <- as.factor(fem_data$ID)

##### FEMALE AVOIDANCE AND MALE REJECTION OF FEMALES
# Female avoidance rate (not sig; p = 0.32)
ggplot(data = fem_data, aes(x = treatment, y = evade_rate)) + geom_boxplot(outlier.shape = NA, fill = "lightcyan4", alpha = 0.1) + 
       geom_jitter(alpha = 0.4, aes(color = replicate), size = 3, width = 0.1, height = 0) + 
       labs(y = "Attempted avoidance rate", x = "Female mating status") + scale_color_nejm() + 
       theme(text = element_text(size = 20))

# Male abort rate (sig)
ggplot(data = fem_data, aes(x = treatment, y = abort_rate)) + geom_boxplot(outlier.shape = NA, fill = "lightcyan4", alpha = 0.1) + 
       geom_jitter(alpha = 0.4, aes(color = replicate), size = 3, width = 0.1, height = 0) + 
       labs(y = "Male abort rate", x = "Female mating status") + scale_color_nejm() + 
       theme(text = element_text(size = 20))

# Insemination rate (sig)
ggplot(data = fem_data, aes(x = treatment, y = insem_mount_ratio)) + geom_boxplot(outlier.shape = NA, fill = "lightcyan4", alpha = 0.1) + 
       geom_jitter(alpha = 0.4, aes(color = replicate), size = 3, width = 0.1, height = 0) + 
       labs(y = "Proportion of mounts that led to insemination", x = "Female mating status") + scale_color_nejm() + 
       theme(text = element_text(size = 16))

#### EXTRA FIGS IN SUPP 
# Mounts received
ggplot(data = fem_data, aes(x = treatment, y = mounts)) + geom_boxplot(outlier.shape = NA, fill = "lightcyan4", alpha = 0.1) + 
       geom_jitter(alpha = 0.4, aes(color = replicate), size = 2.5, width = 0.15, height = 0) + 
       labs(y = "Mounts", x = "Female mating status") + scale_color_nejm() + 
       theme(text = element_text(size = 20))

# Inseminations received 
ggplot(data = fem_data, aes(x = treatment, y = inseminations)) + geom_boxplot(outlier.shape = NA, fill = "lightcyan4", alpha = 0.1) + 
       geom_jitter(alpha = 0.4, aes(color = replicate), size = 3, width = 0.2, height = 0.) + 
       labs(y = "Inseminations", x = "Female mating status") + scale_color_nejm() + 
       theme(text = element_text(size = 20))







