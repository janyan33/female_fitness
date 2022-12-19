## setwd("C:/Users/jy33/OneDrive/Desktop/R/female_rep_status")

library(tidyverse)
library(ggplot2); theme_set(theme_classic())
library(lme4)
library(DHARMa)
library(janitor)
library(wesanderson)
library(stats)

sum_data <- read.csv("data/rep_summary.csv", stringsAsFactors = TRUE) %>% 
            remove_empty(which = c("rows", "cols"))

sum_data$replicate <- as.factor(sum_data$replicate)

## Mounts evaded by females boxplot
ggplot(data = sum_data, aes(x = mating_status, y = mounts_evaded)) + geom_boxplot() + 
       geom_jitter(aes(color = replicate), size = 3, alpha = 0.8, width = 0.02) + 
       scale_color_brewer(palette = "Dark2") + labs(y = "Proportion of mounts evaded by females", x = "Female mating status")

## Mounts aborted by males boxplot
ggplot(data = sum_data, aes(x = mating_status, y = mounts_aborted)) + geom_boxplot() + 
       geom_jitter(aes(color = replicate), size = 3, alpha = 0.8, width = 0.02) + 
       scale_color_brewer(palette = "Dark2") + labs(y = "Proportion of mounts aborted by males", x = "Female mating status")

## Wilcoxin tests
# Mounts evaded
wilcox.test(data = sum_data, mounts_evaded ~ mating_status)

# Mounts aborted
wilcox.test(data = sum_data, mounts_aborted ~ mating_status)

