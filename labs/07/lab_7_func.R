require(boot)


boot_mean = function(x, i)
{
  return(mean(x[i], na.rm = TRUE))
}

boot_ci <- function(x, r) {
  r = r
  result <- numeric(r)
  for(i in 1:r)
  {
    result[i] = mean(sample(x, replace=TRUE))
  }

  return(result = quantile(result,c(0.025,0.975)))
}

boot_rep <- function(x, r, statistic) {
  boot <- boot(
    data = x,
    statistic = statistic,
    R = r
  )
  print(boot)
  
}

moths = read.csv(here("data", "moths.csv"))

rarefaction <- function(input, reps) {
  
  input <- as.data.frame(input)
  
  r = reps
  n = nrow(input)
  
  results_out <- matrix(nrow = r,
                        ncol = n)
  for (i in 1:r)
  {
    for (j in 1:n)
    {
      #samples input data row indices w/ replace
      rows_j = sample(n, size = j, replace = TRUE)
      
      #new matrix from resampled rows
      t1 = as.matrix(input[rows_j,])
      
      #calc column sums of new matrix
      t2 = apply(t1, 2, sum)
      
      #count number of columns w/ observations
      results_out[i, j] = sum(t2 > 0)
    }
  }
  return(results_out)
}


rm(list = ls())




rarefaction_sampler = function(input_dat, n_iterations)
{
  n_input_rows = nrow(input_dat)
  
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  
  # The outer loop: runs once for each bootstrap iteration.  index variable is i
  for(i in 1:n_iterations)
  {
    # The inner loop: simulates increasing sampling intensity
    # Sampling intensity ranges from 1 site to the complete count of
    # sites in the input data (n)
    # index variable is j
    for(j in 1:n)
    {
      # sample the input data row indices, with replacement
      rows_j = sample(n, size = j, replace=TRUE)
      
      # Creates a new data matrix
      t1 = input_dat[rows_j, ]
      
      # Calculates the column sums
      t2 = apply(t1, 2, sum)
      
      # Counts the number of columns in which any moths were observed
      results_out[i, j] = sum(t2 > 0)
    }
  }
  return(results_out)
}


boot_ci <- function(x, m = 1000) 
{
result <-  numeric(m)

for(i in 1:m)
{
  result[i] = mean(sample(x, replace=TRUE))
}
print(mean(result))
print(quantile(result,c(0.025,0.975), na.rm = T))
}

