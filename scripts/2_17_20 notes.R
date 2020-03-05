library(tidyverse)
library(readstata13)
library(lme4)
library(psych)
library(optimx)
library(lmerTest)
library(dfoptim)

options(max.print = 99999)
options(scipen = 999)

getwd()

set.seed(232020)

data <- read.dta13("E:/UO/R Projects/SOC 613/data/AddHealth.dta")

colnames(data)

data %>% 
  count(gen_health)

data <- data %>% 
  mutate(gen_health = recode(gen_health, '1' = '1',
                             '2' = '1',
                             '3' = '1',
                             '4' = '0',
                             '5' = '0'),
         poor = case_when(multpov_w1 <= 1 ~ 1,
                          multpov_w1 > 1 ~ 0),
         poor = recode(poor, '1' = 'poor',
                       '0' = 'not_poor'),
         poor = as.factor(poor),
         sex = as.factor(sex))

data$gen_health <- as.numeric(data$gen_health)

mmodel <- glmer(gen_health ~ sex + poor + (1 |schoolid), 
                data = data, 
                family = binomial(link = "logit"), 
                control = lmerControl(optimizer = 'Nelder_Mead'))
summary(model)                  
