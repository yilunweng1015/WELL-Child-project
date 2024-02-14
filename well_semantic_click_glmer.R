library(lme4)
library(dplyr)
data <- read.csv("well_semantic_click_raw.csv")

data <- data %>%
  mutate(click2inst = ifelse(click_region == "ti", 1, 0)) %>%
  as.data.frame()


condition.helmert = matrix(c(-1/2,1/2), ncol = 1)
data$cond <- as.factor(data$cond)
contrasts(data$cond) = condition.helmert

# Click on Relevant Instrument
result <- glmer(click2inst ~ 1 + cond + (1+cond|subject) + (1|trial), family = binomial, data = data)
summary(result)
# Fixed effects:
#               Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   3.5922     0.7141   5.031 4.89e-07 ***
# cond1        -2.8124     0.9285  -3.029  0.00245 ** 