require(here)
require(dplyr)

delo <- read.csv(here("data", "delomys.csv"))

#numerical exploration----
summary(delo$body_mass)
summary(delo$body_length)

#normality tests
shapiro.test(delo$body_mass)
shapiro.test(delo$body_length)

shapiro.test(resid(fit1))
shapiro.test(resid(fit2))
shapiro.test(resid(fit3))
shapiro.test(resid(fit4))
shapiro.test(resid(fit5))

#linear models

fit1 <- lm(delo$body_length ~ delo$body_mass)
anova(fit1)

fit2 <- lm(delo$body_mass ~ delo$sex) 
anova(fit2)

fit3 <- lm(delo$body_mass ~ delo$binomial)
anova(fit3)

fit4 <- lm(delo$body_mass ~ delo$binomial + delo$sex)
anova(fit4)

fit5 <- lm(delo$body_mass ~ delo$binomial * delo$sex) 
anova(fit5)

##coefficient tables
knitr::kable(coef(summary(fit1)))
knitr::kable(coef(summary(fit2)))
knitr::kable(coef(summary(fit3)))
knitr::kable(coef(summary(fit4)))
knitr::kable(coef(summary(fit5)))

##model comparison
AIC(fit1, fit2, fit3, fit4, fit5)

#graphical exploration----

  ##Q1-4
plot(delo$body_mass, delo$body_length)
hist(delo$body_mass)
hist(delo$body_length)
boxplot(delo$body_mass ~ delo$binomial)
boxplot(delo$body_mass ~ delo$sex)
boxplot(delo$body_mass ~ delo$sex + delo$binomial)

##Q5-6
hist(resid(fit1), breaks = 50)
hist(resid(fit2), breaks = 50)
hist(resid(fit3))
hist(resid(fit4))
hist(resid(fit5))

#questions----
#Q1
# seems to be a positive linear relationship

#Q2
# body mass histogram is mostly normal, length hist is non-normal and right skewed

#Q3
# Failed normality test for both data sets. while they may look it, both are non-normal

#Q4
# boxplots suggest that D. sublineatus has lower average body mass when compared to other sp.

#Q5
# fits 2-5 appear normal, fit 1 does not (significant right skew)
# all fits fail the shapiro test for normality

#Q6
# fit 2 is the least non-normal but still fails 

#Q7
# for every gram increase in body weight, body length will increase by ~0.875 mm

#Q8
# ~163.7 mm

#Q9
# ~76.1 mm

#Q10
# Female

#Q11
#D. dorsalis

#Q12
# males are generally heavier

#Q13
# D. dorsalis is the heavier species

#Q14
# Yes with p-values of <2.2e-16 and 1.942e-7 respectively, both species and sex are significant predictors for body mass

#Q15
# 

#Q16

#Q17
# The models with the lowest AIC scores are the additive and interactive models using species and sex as predictors

#Q18
# I would select the additive model. It is both easier to explain and has a slightly lower score than the interactive model.
