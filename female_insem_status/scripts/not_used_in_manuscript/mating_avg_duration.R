library(tidyverse)
library(ggplot2); theme_set(theme_classic())
library(janitor)
library(stats)


matings <- read.csv("data/MS_matings.csv") %>% 
           select("seconds") %>% 
           filter(seconds != "#VALUE!") %>% 
           filter(seconds != 0)

matings$seconds <- as.numeric(matings$seconds)

hist(matings$seconds, breaks = 20)
mean(matings$seconds) # 101.35
median(matings$seconds) # 90 seconds
length(matings$seconds) # 195 matings

sd(matings$seconds)

mean(matings$seconds) - 2*(sd(matings$seconds))

