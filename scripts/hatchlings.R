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
  axis.text.x = element_text(size = 18),
  axis.title.y = element_text(size = 18), 
  axis.text.y = element_text(size = 18))

# Loading hatchlings per female data
hatchlings_per_fem <- read.csv("data/hatchlings.csv") %>% 
                      filter(treatment == "Low" | treatment == "High") #bc there's lots of empty rows 
                      

hatchlings_per_fem$total <- as.numeric(hatchlings_per_fem$total)
hatchlings_per_fem$treatment <- factor(hatchlings_per_fem$treatment, levels = c("Low", "High")) # re-order factor levels

ggplot(data = hatchlings_per_fem, aes(x = treatment, y = total, fill = treatment)) + geom_boxplot(outlier.colour = NA) + 
  scale_fill_manual(values = c("#0072b5", "#bc3c29")) + ylab("Number of hatchlings produced") + ylim(0, 170) +
  geom_dotplot(binaxis='y', stackdir = 'center', dotsize = 0.5, position=position_dodge(0.8), alpha = 0.8) + My_Theme + 
  xlab("Treatment") + theme(legend.position = "none")



# New fig for eggs
egg_sums <- as.data.frame(aggregate(eggs ~ treatment, hatchlings_per_fem, sum)) # total eggs laid per treatment

long_data <- read.csv("data/longevity.csv") # load in longevity data
days_alive_sums <- as.data.frame(aggregate(longevity ~ treatment, long_data, sum)) 

egg_sums <- left_join(egg_sums, days_alive_sums, by = "treatment")

egg_sums <- egg_sums %>% 
            mutate(egg_rate = eggs/longevity)


egg_sums$treatment <- factor(egg_sums$treatment, levels = c("Low", "High"))

ggplot(egg_sums, aes(x = treatment, y = egg_rate, fill = treatment)) + geom_bar(stat = "identity") + ylim(0, 2) +
       scale_fill_manual(values = c("#0072b5", "#bc3c29")) + My_Theme + ylab("Eggs laid per day of females being alive \n(pooled by treatment)") +
       xlab("Treatment")



# New fig for hatchlings (barplot just showing total number of hatchlings produced per treatment)
hatchling_sums <- as.data.frame(aggregate(total ~ treatment, hatchlings_per_fem, sum))
hatchling_sums$treatment <- factor(hatchling_sums$treatment, levels = c("Low", "High")) # re-order factor levels

ggplot(hatchling_sums, aes(x = treatment, y = total, fill = treatment)) + geom_bar(stat = "identity") + ylim(0, 2500) + 
       scale_fill_manual(values = c("#0072b5", "#bc3c29")) + My_Theme + ylab("Total number of hatchlings produced \n(pooled by treatment)") +
       xlab("Treatment")






# Hatchling model
hatchlings_mod_2 <- glm(data = hatchlings_per_fem, total ~ treatment, family = poisson())

summary(hatchlings_mod_2)
plot(hatchlings_mod_2)
Anova(hatchlings_mod_2)



hatchlings_mod <- glmmTMB(data = hatchlings_per_fem, total ~ treatment, family = nbinom2(link = "log"))
Anova(hatchlings_mod)
summary(hatchlings_mod)

plot(simulateResiduals(hatchlings_mod))



mean(hatchlings_per_fem$total, na.rm = TRUE)

# Hatch rate
ggplot(data = hatchlings_per_fem, aes(x = treatment, y = hatch_rate, fill = treatment)) + geom_boxplot(outlier.colour = NA) + 
  scale_fill_manual(values = c("#0072b5", "#bc3c29")) + ylab("Hatch rate per arena") +
  geom_dotplot(binaxis='y', stackdir = 'center', dotsize = 0.5, position=position_dodge(0.8), alpha = 0.8) + My_Theme + 
  xlab("Treatment") + theme(legend.position = "none")


hatch_rate_mod <- glm(data = hatchlings_per_fem, cbind(total, (eggs - total)) ~ treatment, family = binomial(link = "logit"))

plot(hatch_rate_mod)
Anova(hatch_rate_mod)
summary(hatch_rate_mod)


hist(hatchlings_per_fem$hatch_rate[hatchlings_per_fem$treatment == "Low"], breaks = 15, xlim = c(0.9, 1), ylim = c(0, 10))
hist(hatchlings_per_fem$hatch_rate[hatchlings_per_fem$treatment == "High"], breaks = 15, xlim = c(0.9, 1), ylim = c(0, 10))


