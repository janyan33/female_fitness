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
  axis.text.x = element_text(size = 16),
  axis.title.y = element_text(size = 16), 
  axis.text.y = element_text(size = 16))

### INSEMINATION AND PURSUIT DURATIONS ###
# Insemination durations
durations <- read.csv("female_fitness_2022/ff_mating_durations.csv") %>% 
             filter(notes != "finish time off") %>% 
             filter(notes != "end time off") %>% 
             filter(notes != "no seconds; end time off") %>%
             filter(notes != "no seconds") %>% 
             filter(notes != "mating start is WRONG") %>% 
             filter(date != "05-11-2022") %>% 
             filter(mating_duration != "0") %>% 
             filter(treatment != "") 
             
durations$treatment <- as.factor(durations$treatment)
durations$mating_duration <- as.numeric(durations$mating_duration)
durations$pursuit_duration <- as.numeric(durations$pursuit_duration)
durations$pursuit_start <- as.numeric(durations$pursuit_start)

# Insemination duration boxplot
insem_durations <- durations %>% 
                   filter(mating_duration != "#VALUE!") %>%
                   filter(mating_duration < 600)

ggplot(data = insem_durations, aes(x = treatment, y = mating_duration, color = treatment, fill = treatment)) + 
       geom_boxplot(outlier.colour = NA) + scale_color_nejm() + scale_fill_nejm(alpha = 0.6) +
       geom_jitter(width = 0.3, height = 0, alpha = 0.15, color = "black") + 
       ylab("Insemination duration (s)")

mean(insem_durations$mating_duration[insem_durations$treatment == "high"])
mean(insem_durations$mating_duration[insem_durations$treatment == "low"])

# Pursuit duration (amount of time between initial pursuit and insemination)
pursuit_dur <- durations %>% 
               filter(pursuit_duration != "#VALUE!") %>% 
               filter(pursuit_duration < 3600 & pursuit_duration > 0)

ggplot(data = pursuit_dur, aes(x = treatment, y = pursuit_duration, color = treatment, fill = treatment)) + 
       geom_boxplot(outlier.colour = NA) + scale_color_nejm() + scale_fill_nejm(alpha = 0.6) +
       geom_jitter(width = 0.3, height = 0, alpha = 0.15, color = "black") + 
       ylab("Pursuit duration (s)")

mean(pursuit_dur$pursuit_duration[pursuit_dur$treatment == "high"])
mean(pursuit_dur$pursuit_duration[pursuit_dur$treatment == "low"])

# Pursuit latency (time between trial start and initial pursuit)
pursuit_lat <- durations %>% 
               filter(pursuit_start != "#VALUE!") %>% 
               filter(pursuit_start > 3600) #& pursuit_start > 0)

hist(pursuit_lat$pursuit_start)

ggplot(data = pursuit_lat, aes(x = treatment, y = pursuit_start, color = treatment, fill = treatment)) + 
  geom_boxplot(outlier.colour = NA) + scale_color_nejm() + scale_fill_nejm(alpha = 0.6) +
  geom_jitter(width = 0.3, height = 0, alpha = 0.15, color = "black") + 
  ylab("Pursuit latency (s)")

mean(pursuit_lat$pursuit_start[pursuit_lat$treatment == "high"])
mean(pursuit_lat$pursuit_start[pursuit_lat$treatment == "low"])


