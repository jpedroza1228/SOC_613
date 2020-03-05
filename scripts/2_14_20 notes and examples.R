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

set.seed(2142020)

data <- read.dta13("E:/UO/R Projects/SOC 613/data/AddHealth.dta")

data <- data %>% 
  dplyr::select(aid, 
                schoolid,
                sex,
                cesd,
                age_w1)

str(data)

data <- data %>% 
  mutate(sex = as.factor(sex),
         sex = recode(sex, '1' = 'male',
                      '2' = 'female'))

data_sub <- cbind(data, dummy.code(data$sex))

colnames(data_sub)

model_1 <- lmer(cesd ~ female + (female || schoolid),
                REML = FALSE,
              data = data_sub)
summary(model_1)

model_2 <- lmer(cesd ~ female + age_w1 + (female + age_w1 || schoolid),
              data = data_sub)
summary(model_2)
# summary(allFit(model))



