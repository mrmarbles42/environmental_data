#setup----

require(here)
require(boot)
require(simpleboot)
require(palmerpenguins)
require(ggplot2)
require(dplyr)
require(gridExtra)


cat_rate = read.csv(here("data", "catrate.csv"))
veg = read.csv(here("data", "vegdata.csv"))
disp = read.csv(here("data", "dispersal.csv"))
bird = read.csv(here("data", "bird.sta.csv"))
hab = read.csv(here("data", "hab.sta.csv"))
birdhab = merge(bird, hab, by=c("basin", "sub", "sta"))


#Q1-2 ----

creep_table = table(birdhab$s.edge, birdhab$BRCR > 0)[, 1:2]


creep_table

#Q3-5----
fit_species <- lm(penguins$body_mass_g ~ penguins$species)
fit_sex <- lm(penguins$body_mass_g ~ penguins$sex)
fit_both <- lm(penguins$body_mass_g ~ penguins$species * penguins$species)


#Q6-9----
hist(fit_species$residuals)

boxplot(penguins$body_mass_g ~ penguins$species,
        ylab = "Body mass (g)",
        xlab = "Penguin Species")

boxplot(body_mass_g ~ sex, data = penguins,
        ylab = "Body mass (g)",
        xlab = "Penguin sex")
  
boxplot(penguins$body_mass_g ~ penguins$sex * penguins$species,
        xlab = "Penguin sex + species",
        ylab = "Body mass (g)",
        names = c("Adelie \n (f)",
                  "Adelie \n (m)",
                  "Chinstrap \n (f)",
                  "Chinstrap \n (m)",
                  "Gentoo \n (f)",
                  "Gentoo \n (m)"))

#Q10-12----
bartlett.test(body_mass_g ~ sex, data = penguins)
bartlett.test(body_mass_g ~ species, data = penguins)

#Q13-14----

pen_groups <- aggregate(body_mass_g ~ sex * species,
                        data = penguins,
                        FUN = c)
af_mass <- pen_groups %>%
  filter(sex == "female",
         species == "Adelie")
am_mass <- pen_groups %>%
  filter(sex == "male",
         species == "Adelie")
cf_mass <- pen_groups %>%
  filter(sex == "female",
         species == "Chinstrap")
cm_mass <- pen_groups %>%
  filter(sex == "male",
         species == "Chinstrap")
gf_mass <- pen_groups %>%
  filter(sex == "female",
         species == "Gentoo")
gm_mass <- pen_groups %>%
  filter(sex == "male",
         species == "Gentoo")

bartlett.test(pen_groups$body_mass_g)

bartlett.test()

#Q15----
dat_fl <- read.csv(here("data", "trees_FL.csv"))

plot_1 <- ggplot(dat_fl, aes(ProbabilityofFailure)) +
  geom_bar() + 
  labs(x = "Probability of Failure")

plot_2 <- dat_fl %>%
  ggplot(aes(Failure_Standardized)) +
  geom_bar() +
  labs(x = "Failure (standardized)")

plot_3 <- dat_fl %>%
  ggplot(aes(DBH_in)) +
  geom_histogram() +
  labs(x = "Diameter at Breast Height (in)")

plot_4 <- dat_fl %>%
  ggplot(aes(DBH_in, HeighttoTop_ft)) +
  geom_point() +
  labs(x = "Diameter at Breast Height (in) \n vs Height to Top (ft)")

grid.arrange(plot_1, plot_2, plot_3, plot_4, ncol = 2, nrow = 2)

#Q16-17----

whole_fail <- dat_fl %>%
  filter(Failure_Standardized == "whole")
no_fail <- dat_fl %>%
  filter(Failure_Standardized == "none")
branch_fail <- dat_fl %>%
  filter(Failure_Standardized == "branch")

ks.test(whole_fail$DBH_in, no_fail$DBH_in)


#Q18-20----
cor.test(dat_fl$DBH_in, 
         dat_fl$HeighttoTop_ft,
         method = "spearman")

#Q21-25----
dat_fl$fail = factor(dat_fl$Failure_Standardized != "none")

levels(dat_fl$fail) = c("No Fail", "Fail")

fl_table_2 = table(
  dat_fl$ProbabilityofFailure,
  dat_fl$fail)

fl_chi <- chisq.test(fl_table_2)

fl_chi$statistic

round(fl_chi$residuals)

round(fl_chi$observed - fl_chi$expected, 0)
