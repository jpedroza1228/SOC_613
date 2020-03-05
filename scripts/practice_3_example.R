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

set.seed(12720)
data <- read.dta13("E:/UO/R Projects/SOC 613/data/jsp2.dta")

colnames(data)

data$school <- as.factor(data$school)

data <- data %>% 
  group_by(school) %>% 
  mutate(n_per = n())

data_add <- data %>% 
  group_by(school) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  count(school)

data <- left_join(data, data_add, by = 'school')

data <- data %>% 
  group_by(school) %>%
  mutate(one_school = !duplicated(n))

colnames(data)
data <- data %>% 
  mutate(one_school = ifelse(one_school == "TRUE", 1, 0))






single <- lm(math5 ~ school, data = data)
summary(single)

data$fixed_value <- predict(single, newdata = data)
data$fixed_value <- as.numeric(data$fixed_value)

int <- lmer(math5 ~ 1 + (1 | school), data = data, REML = FALSE)
summary(int)


# install.packages('insight')
# library(insight)
get_variance_random(int)
get_variance_distribution(int)
get_variance_intercept(int)
# intercept for fixed model
get_parameters(int)

data %>% 
  group_by(school) %>% 
  mutate(mu_diff = int$mu)

ex <- data %>% 
  mutate(mu0 = 30.61 + random_value | 30.61 - random_value)

# dat <- get_data(int)
# pred <- find_predictors(int, flatten = TRUE)

# install.packages('ggeffects')
# install.packages('merTools')
library(merTools)

pre_random <- predictInterval(merMod = int,
                              newdata = data,
                              level = .95,
                              n.sims = 1000,
                              stat = 'mean',
                              type = 'linear.prediction',
                              include.resid.var = TRUE)

data$pre_random <- as.numeric(pre_random$fit)
data$pre_random_low <- as.numeric(pre_random$lwr)
data$pre_random_high <- as.numeric(pre_random$upr)


# install.packages('sjPlot')
library(sjmisc)
library(sjPlot)

# data$random_value <- predict(int, newdata = data)
# data$random_value <- as.numeric(data$random_value)

colnames(data)

data %>% 
  filter(one_school == 1) %>% 
  mutate(diff = fixed_value - random_value) %>% 
  dplyr::select(school, n_per, fixed_value, random_value, diff)

library(ggpubr)
plot1 <- data %>% 
  ggplot(aes(fixed_value)) +
  geom_histogram(color = 'white', fill = 'dodgerblue', bins = 20) +
  theme_minimal()

plot2 <- data %>% 
  ggplot(aes(random_value)) +
  geom_histogram(color = 'white', fill = 'dodgerblue', bins = 20) +
  theme_minimal()

ggarrange(plot1, plot2)



data %>% 
  filter(one_school == 1) %>% 
  ggplot(aes(fct_reorder(school, pre_random), pre_random)) +
  geom_errorbar(aes(ymin = pre_random_low,
                    ymax = pre_random_high),
                color = 'gray70') +
  geom_point() +
  theme_minimal()
