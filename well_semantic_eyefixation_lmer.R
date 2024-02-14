library(lme4)
library(lmerTest)
data <- read.csv("well_semantic_eyefixation_proportion.csv")
data <- data[-c(1)]
## Looks to relevant instrument
rev_inst <- subset(data, look_object!="TA" & look_object!="DA" & look_object!="DI")

# check distribution
#hist(rev_inst$Percentage)
condition.helmert = matrix(c(-1/2,1/2), ncol = 1)
rev_inst$cond <- as.factor(rev_inst$cond)
contrasts(rev_inst$cond) = condition.helmert

## Verb time window
rev_inst_verb <- subset(rev_inst,timewindow==1)
result_verb <- lmer(Percentage ~ 1 + cond + (1|trial),data = rev_inst_verb, REML=FALSE)
summary(result_verb)
# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)  0.227248   0.011510 33.588127  19.744   <2e-16 ***
# cond1       -0.009479   0.023019 33.588127  -0.412    0.683  


## N1 time window
rev_inst_N1 <- subset(rev_inst,timewindow==2)
result_n1 <- lmer(Percentage ~ 1 + cond + (1|trial),data = rev_inst_N1, REML=FALSE)
summary(result_n1)
# Fixed effects:
#             Estimate    Std. Error  df    t value Pr(>|t|)    
# (Intercept)  0.23082    0.01230 36.22772  18.770   <2e-16 ***
# cond1       -0.05463    0.02460 36.22772  -2.221   0.0327 *  

anova(result_n1)
F_to_eta2(4.93, 1, 36.228)


## N2 time window
rev_inst_N2 <- subset(rev_inst,timewindow==3)
result_n2 <- lmer(Percentage ~ 1 + cond + (1|trial),data = rev_inst_N2, REML=FALSE)
summary(result_n2)
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)  0.33730    0.01342 34.72553   25.14   <2e-16 ***
# cond1       -0.03946    0.02684 34.72553   -1.47     0.15
