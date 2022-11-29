require(palmerpenguins)
require(dplyr)


##Q1

rm(list = ls())

sse_mean = function(x)
{
  x <- x[!is.na(x)]
  n <- length(x)
   
  sse <- sd(x) / sqrt(n)
  
  return(sse)
}

sse_mean(penguins$body_mass_g)
sse_mean(mtcars$mpg)


##Q2
two_group_resample_diff = function(x, n_1, n_2)
{
  a <- mean(sample(x, size = n_1, replace = T), na.rm = T)
  b <- mean(sample(x, size = n_2, replace = T), na.rm = T)
  
  difference_in_means <- diff(c(a, b))
    
  
  return(difference_in_means) 
}


#Q3
resample <- function(x, num_samp) {
  mean_diff <- c()
  
  for (i in 1:num_samp) {
    mean_diff <- c(mean_diff,
                   two_group_resample_diff(x,
                                           68,
                                           152))
  }
  return(mean_diff)
}

dat_pen = subset(penguins, species != "Gentoo")

#Q4



#Q5
 
# rand_samp_df <- data.frame(rand_samp)
# rand_58 <- filter(abs(rand_samp_df), rand_samp >= 5.8)

#Q6


#Q7

boxplot(dat_pen$bill_depth_mm ~ dat_pen$species)


depth_means <- aggregate(dat_pen$bill_depth_mm ~ dat_pen$species,
          FUN = "mean",
          na.rm = T)

diff_crit <- diff(depth_means[,2])

t_test <- t.test(dat_pen$bill_depth_mm ~ dat_pen$species, 
       alternative = "two.sided")

#two_group_resample_diff(penguins$bill_depth_mm, 68, 152)

depth_resample <- data.frame(abs(resample(dat_pen$bill_depth_mm, 1000)))

big_diff <- depth_resample %>%
  filter(depth_resample[1] >= diff_crit)

hist(as.numeric(depth_resample[1]),
     main = "Bill depth difference in means",
     xlab = "Difference")
