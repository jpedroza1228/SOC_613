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

data <- read.dta13("E:/UO/R Projects/SOC 613/data/police.dta")

colnames(data)

sub <- police %>% 
  filter(crime == 4) %>% 
  mutate(eth = as.factor(eth),
         pop = as.numeric(pop),
         eth = recode(eth, '1' = 'black',
                      '2' = 'hispanic',
                      '3' = 'white'),
         eth = relevel(as.factor(eth), ref = 'white'))

str(sub)


# https://stat.ethz.ch/pipermail/r-sig-mixed-models/2007q1/000059.html

ex <- lme4::glmer(stops ~ eth + (1 | precinct) + offset(log(pop)),
                     family = poisson(link = 'log'), data = sub)
summary(ex)

# cc_model1 <- confint(model1, parm="beta_") 
cc_model1_wald <- confint(ex, parm = "beta_", method = "Wald")
# ctab_model1 <- cbind(est=fixef(model1), cc_model1)
ctab_model1_wald <- cbind(est = fixef(ex), cc_model1_wald)

ctab_model1_wald

rtab_model1_wald <- exp(ctab_model1_wald)

# Answer: Odds & Odds Ratios
print(rtab_model1_wald, digits = 3)



