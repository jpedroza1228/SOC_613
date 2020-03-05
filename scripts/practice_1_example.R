str(data)

data_s <- data %>% 
  dplyr::select(schoolid,
                age_w1,
                cesd)

ml_1 <- lmer(cesd ~ age_w1 + (1|schoolid), data = data_s)
summary(ml_1)
confint(ml_1)
anova(ml_1)


qf(p = .95, df1 = 1, df2 = 6490)
# 78.98 > 3.84 therefore it is significant at the .05 at the very least
# look up scores on f value table
