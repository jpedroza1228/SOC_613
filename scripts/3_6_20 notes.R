library(tidyverse)
library(readstata13)
library(psych)
library(lme4)
library(lmerTest)
library(ggeffects)

options(max.print = 99999)
options(scipen = 999)

getwd()

set.seed(20200306)

gpa_data <- read.dta13("E:/UO/R Projects/SOC_613/data/gpa.dta")

colnames(gpa_data)

long_gpa <- gpa_data %>% 
  gather(key = wave, value = gpa_values, c(-1, -14:-16)) %>% 
  separate()


long_gpa <- long_gpa %>% 
  mutate(time = recode(wave, 'gpa1' = '1',
                       'gpa2' = '2',
                       'gpa3' = '3',
                       'gpa4' = '4',
                       'gpa5' = '5',
                       'gpa6' = '6')) %>% 
  mutate(time = as.numeric(time))

colnames(long_gpa)

fit <- lm(gpa_values ~ time + )