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


#Q3

#Q4
fit_both <- lm(body_mass_g ~ sex ** species, data = penguins)

summary(fit_both)

#Q5
# the base case is an Adelie female