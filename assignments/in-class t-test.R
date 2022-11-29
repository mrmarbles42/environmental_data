require(palmerpenguins)
dat_ade = droplevels(subset(penguins, species == "Adelie"))


boxplot(dat_ade$body_mass_g,
        xlab = "Adelie")

dat_ade_m <- subset(dat_ade, sex == "male")

t.test(dat_ade$body_mass_g ~ dat_ade$sex, alternative = c("two.sided"))

t.test(dat_ade_m$meanbody_mass_g > 4000)
