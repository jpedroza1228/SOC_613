library(tidyverse)
library(readstata13)
library(lme4)
library(psych)

# install.packages('optimx')
library(optimx)

# install.packages('lmerTest')
library(lmerTest)

# https://www.theanalysisfactor.com/unstructured-covariance-matrix-when-it-does-and-doesn%E2%80%99t-work/

getwd()

set.seed(12420)
data <- read.dta13("E:/UO/R Projects/SOC 613/data/jsp2.dta")
data <- read.dta13("/Volumes/JPEDROZA/UO/R Projects/SOC 613/data/jsp2.dta")

colnames(data)

mod1 <- lmer(math5 ~ math3 + (1 | school), data = data, REML = FALSE)
summary(mod1)
confint(mod1)

mod2 <- lmer(math5 ~ math3 + (math3 || school), data = data, REML = FALSE,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(mod2)
confint(mod2)

mod3 <- lmer(math5 ~ math3 + (math3 | school), data = data, REML = FALSE,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(mod3)

# confidence intervals are based on the standard deviations
# make sure to square these scores to represent the values in STATA
# confint(mod3, method = 'Wald', level = .95)
confint(mod3, method = 'profile', level = .95)
as.data.frame(VarCorr(mod3))
# ranova(mod3)

anova(mod1, mod2)
anova(mod2, mod3)
