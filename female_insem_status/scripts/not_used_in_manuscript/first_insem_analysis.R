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

## LOADING AND CLEANING DATA

# Summary of each replicate
sum_data <- read.csv("data/first_insem_summary.csv")
    sum_data$replicate <- as.factor(sum_data$replicate)

# All observations for females up until first insemination
all_obs <- read.csv("data/first_insem_data.csv")

    all_obs$replicate <- as.factor(all_obs$replicate)
    all_obs$treatment <- as.factor(all_obs$treatment)
    all_obs$avoidances <- as.factor(all_obs$avoidances)
    all_obs$avoidance_success <- as.factor(all_obs$avoidance_success)
    all_obs$aborts <- as.factor(all_obs$aborts)
    all_obs$female_ID <- as.factor(all_obs$female_ID)
    all_obs$male_ID <- as.factor(all_obs$male_ID)
    all_obs$behaviour <- as.factor(all_obs$behaviour)
  
# Female attributes (up until first insemination)
fem_attr_first <- read.csv("data/attribute_first_insem.csv") %>% 
                  filter(sex == "female")

    fem_attr_first$replicate <- as.factor(fem_attr_first$replicate)
    fem_attr_first$treatment <- as.factor(fem_attr_first$treatment)

## STATISTICAL ANALYSES
    
# Probability of female avoidance
fem_avoidances_model <- glmer(data = all_obs, avoidances ~ treatment + (1|replicate/female_ID) + 
                              (1|replicate),
                              family = binomial(link = "logit"))

summary(fem_avoidances_model) # Diagnostic ok but can't incoporate male_ID


# Probability of male abort
aborts_model <- glmer(data = all_obs, aborts ~ treatment + (1|replicate) + (1|replicate/female_ID) + 
                      (1|replicate/male_ID),
                      family = binomial(link = "logit"))

summary(aborts_model) # Diagnostic ok

# Number of mounts
mount_model <- glmer.nb(data = fem_attr_first, mounts ~ treatment + (1|replicate))

plot(simulateResiduals(mount_model))
summary(mount_model)

# Insem to mount ratio
insem_mount_ratio_model <- glmer(data = all_obs, behaviour ~ treatment + (1|replicate) +
                                 (1|replicate/male_ID), 
                                 #(1|replicate/female_ID), 
                                 family = binomial(link = "logit"))

plot(simulateResiduals(insem_mount_ratio_model))
summary(insem_mount_ratio_model)


## BOXPLOTS FOR REPLICATE SUMS
# Female avoidance
ggplot(data = sum_data, aes(x = mating_status, y = evade_rate)) + geom_boxplot() + 
  geom_jitter(alpha = 0.8, aes(color = replicate), size = 4, width = 0.02, height = 0) + 
  labs(y = "Proportion of mounts evaded by females", x = "Female mating status") + scale_color_nejm()

# Male aborts
ggplot(data = sum_data, aes(x = mating_status, y = abort_rate)) + geom_boxplot() + 
  geom_jitter(alpha = 0.8, aes(color = replicate), size = 4, width = 0.02, height = 0) + 
  labs(y = "Proportion of mounts aborted by males", x = "Female mating status") + scale_color_nejm()

# Inseminations
ggplot(data = sum_data, aes(x = mating_status, y = inseminations)) + geom_boxplot(fill = "honeydew3", alpha = 0.2) +
  geom_jitter(alpha = 0.8, aes(color = replicate), size = 4, width = 0.02, height = 0) + 
  labs(y = "Inseminations", x = "") + scale_color_nejm() + 
  theme(text = element_text(size = 20))

# Mounts
ggplot(data = sum_data, aes(x = mating_status, y = mounts_received)) + geom_boxplot(fill = "honeydew3", alpha = 0.2, outlier.shape = NA) +
  geom_jitter(alpha = 0.5, aes(color = replicate), size = 4, width = 0.02, height = 0) + 
  labs(y = "Mounts received", x = "") + scale_color_nejm() + 
  theme(text = element_text(size = 20))

# Insemination to mount ratio
ggplot(data = sum_data, aes(x = mating_status, y = insem_mount_ratio)) + geom_boxplot(fill = "honeydew3", alpha = 0.2, outlier.shape = NA) +
  geom_jitter(alpha = 0.5, aes(color = replicate), size = 4, width = 0.02, height = 0) + 
  labs(y = "Proportion of mounts that led \n to insemination", x = "") + scale_color_nejm() + 
  theme(text = element_text(size = 20))

## BOXPLOTS FOR INDIVIDUAL FEMALES

# Mounts
ggplot(data = fem_attr_first, aes(x = treatment, y = mounts)) + geom_boxplot(fill = "honeydew3", alpha = 0.2, outlier.shape = NA) +
  geom_jitter(alpha = 0.5, aes(color = replicate), size = 4, width = 0.3, height = 0) + 
  labs(y = "Mounts received", x = "") + scale_color_nejm() + 
  theme(text = element_text(size = 20))

# Female avoidances
ggplot(data = fem_attr_first, aes(x = treatment, y = evade_rate)) + geom_boxplot(fill = "honeydew3", alpha = 0.2, outlier.shape = NA) +
  geom_jitter(alpha = 0.5, aes(color = replicate), size = 4, width = 0.2, height = 0) + 
  labs(y = "Proportion of mounts evaded by females", x = "") + scale_color_nejm() + 
  theme(text = element_text(size = 20))

# Male aborts
ggplot(data = fem_attr_first, aes(x = treatment, y = abort_rate)) + geom_boxplot(fill = "honeydew3", alpha = 0.2, outlier.shape = NA) +
  geom_jitter(alpha = 0.5, aes(color = replicate), size = 4, width = 0.2, height = 0) + 
  labs(y = "Proportion of mounts aborted by males", x = "") + scale_color_nejm() + 
  theme(text = element_text(size = 16))

# Insemination to mount ratio
ggplot(data = fem_attr_first, aes(x = treatment, y = insem_mount_ratio)) + geom_boxplot(fill = "honeydew3", alpha = 0.2, outlier.shape = NA) +
  geom_jitter(alpha = 0.5, aes(color = replicate), size = 4, width = 0.25, height = 0) + 
  labs(y = "Proportion of mounts that led \n to insemination", x = "") + scale_color_nejm() + 
  theme(text = element_text(size = 16))






