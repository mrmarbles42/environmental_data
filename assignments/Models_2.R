require(here)
require(palmerpenguins)

penguins <- penguins

penguins$species <- factor(penguins$species, levels=c("Adelie", "Chinstrap", "Gentoo"))
penguins



#Q1
label = c("Adelie \n female",
          "Adelie \n male",
          "Chinstrap \n female",
          "Chinstrap \n male",
          "Gentoo \n female",
          "Gentoo \n male")

boxplot(penguins$body_mass_g ~ sex + species, data = penguins,
        ylab = "Body mass",
        names = label)

#Q2
# yes, I think that the male penguins are significantly heavier than females of each species. All males have greater mean weights and their IQRs are distinct than their female counterparts.

#Q3
# Yes, I think that it may improve model fit. the variation between sexes is of differing magnitude based on species.

#Q4
fit_both <- lm(body_mass_g ~species * sex, data = penguins)

summary(fit_both)

#Q5
# the base case is an Adelie female

#Q6

#Q7
# 3527.21

#Q8
#3527.206

chinstrap <- subset(penguins, species == "Chinstrap")
chin_fem <- subset(chinstrap, sex == "female")
mean(chin_fem$body_mass_g)
