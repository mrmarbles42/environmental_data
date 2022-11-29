library(here)
library(palmerpenguins)


penguins = data.frame(penguins)


#mean
mean(penguins$body_mass_g, na.rm = T)



#plots
boxplot(penguins$bill_depth_mm)
boxplot(bill_depth_mm ~ sex, data = penguins)


#par(mfrow = c(1, 2))

boxplot(penguins$bill_depth_mm)
boxplot(bill_depth_mm ~ sex, data = penguins)

coplot(body_mass_g ~ bill_depth_mm | sex, data = penguins)
coplot(flipper_length_mm ~ bill_length_mm |species, data = penguins, rows = 1)

hist(penguins$flipper_length_mm)

png(filename = here("basic_scatterplot.png"), width = 800, height = 600)
plot(penguins$body_mass_g, penguins$flipper_length_mm,
     xlab = "Body mass (g)",
     ylab = "Flipper length (mm)",
     main = "Body mass vs Flipper length")
dev.off()

boxplot(penguins$species ~ penguins$island)
