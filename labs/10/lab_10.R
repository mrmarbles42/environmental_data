require(here)
require(palmerpenguins)

#data----
rope = read.csv(here("data", "rope.csv"))

rope$rope.type = as.factor(rope$rope.type)
levels(rope$rope.type)

n_obs = length(rope[,1])
n_groups = length(unique(rope$rope.type))

ss_tot = sum((rope$p.cut - mean(rope$p.cut))**2)
df_tot = n_obs - 1

#aggregated residuals
agg_resids = aggregate(rope$p.cut,
                       by = list(rope$rope.type),
                       FUN = function(x) x - mean(x)
                       )
str(agg_resids)

#aggregated sum-squared residuals
agg_sum_sq_resids = aggregate(rope$p.cut, 
                              by = list(rope$rope.type),
                              FUN = function(x) sum((x - mean(x))**2)
                              )
str(agg_sum_sq_resids)

ss_within = sum(agg_sum_sq_resids$x)

ss_among = ss_tot - ss_within

df_within = (n_obs - n_groups)
df_among = (n_groups - 1)

ms_among  =  (ss_among / (df_among))
ms_within = (ss_within / (df_within))

f_ratio = (ms_among / ms_within)

f_pval = 1 - pf(f_ratio, 
                df_among, 
                df_within)







bartlett.test(agg_resids$x)

sapply(agg_resids$x, shapiro.test)



aov_rope = anova(lm(p.cut ~ rope.type, data = rope))
str(aov_rope)
sum(aov_rope$`Sum Sq`)


#Q1----
#number comparison tolerance
digits_check = 5

# Build the reference model using R functions
fit_1 = lm(p.cut ~ rope.type, data=rope)
anova(fit_1)
anova_fit_1 = anova(fit_1)

# Check degrees of freedom
anova_fit_1$Df == c(df_among, df_within)

# Check sums of squares
round(anova_fit_1$`Sum Sq`, digits = digits_check) == round(c(ss_among, ss_within), digits = digits_check)

# Check mean squares
round(anova_fit_1$`Mean Sq`, digits = digits_check) == round(c(ms_among, ms_within), digits = digits_check)

# Check the F-ratio
round(anova_fit_1$`F value`[1], digits = digits_check) == round(f_ratio, digits = digits_check)

# Check the F test statistic p-value
round(anova_fit_1$`Pr(>F)`[1], digits = digits_check) == round(f_pval, digits = digits_check)
#Q2-4----
bartlett.test(p.cut ~ rope.type, data=rope)

#Q5-7----
factor(rope$rope.type)
fit_rope_1 = lm(p.cut ~ rope.type, data = rope)
summary(fit_rope_1)

#Q8-11----
shapiro.test(residuals(fit_rope_1))


str(agg_resids)

sapply(agg_resids$x, function(x) shapiro.test(x))

#Q12-17----

pen_fem = subset(penguins, sex == "female")

boxplot(body_mass_g ~ species, data = pen_fem)
bartlett.test(body_mass_g ~ species, data = pen_fem)

fit_pen = lm(body_mass_g ~ species, data = pen_fem)
shapiro.test(residuals(fit_pen))

TukeyHSD(aov(fit_pen))
