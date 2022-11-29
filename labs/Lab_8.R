library(palmerpenguins)
require(simpleboot)

penguins = droplevels(subset(penguins, species != "Gentoo"))



#T-test
t.test(flipper_length_mm ~ species, data = penguin_dat, alternative = "less")

?two.boot

two.boot()