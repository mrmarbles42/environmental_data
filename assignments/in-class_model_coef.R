data("iris")

fit_species = 
  lm(
    Sepal.Length ~ Species,
    data = iris)

summary(fit_species)

setosa <- subset(iris, Species == "setosa")
mean(setosa$Sepal.Length)

boxplot(iris$Sepal.Length ~ iris$Species)
hist(resid(fit_species))
shapiro.test(residuals(fit_species))


#petals

fit_petals <- lm(
  iris$Petal.Length ~ iris$Petal.Width
)
plot(
  Petal.Width ~ Petal.Length,
  data = iris,
  xlab = "Petal Length (cm)",
  ylab = "Petal Width (cm)")

summary(fit_petals)
fit_petals$coefficients

shapiro.test(resid(fit_petals))
