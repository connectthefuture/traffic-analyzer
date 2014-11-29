library('locfit')

parametric_density_estimation <- function (dat, alpha) {
  # This function will perform a parametric analysis of the density
  # of a given data set. The general parametric form is that the underlying 
  # distribution is normal. To model this we simply use the MLE for mu and sigma
  
  n = length(dat$speed)
  # Estimate mu and sigma
  mu = mean(dat$speed)
  sigma = sd(dat$speed)
  # Calculate a confidence interval
  mu_error = qnorm(1 - alpha / 2) * sigma / sqrt(n)
  
  # Print out our results
  print ("Estimated value of mu:")
  print(mu)
  print("Confidence Interval for mu:")
  print(mu - mu_error)
  print(mu + mu_error)
  
  
}

# Load the data
region_data <- read.table('../dat/augmented_region_data.csv', header=TRUE, sep=',')
segment_data <- read.table('../dat/augmented_segment_data.csv', header=TRUE, sep=',')



#### FOR TESTING ####
current_data = dat[region_data$region_id == 13,]

# For the time being we'll look at region 13 (Downtown Loop)
current_data = dat[dat$region_id == 13,]


xs = seq(0, 40, length = 1000)
# For actual analysis we'll build one model for each region of Chicago
for (region in 1:29) {
  current_data = dat[dat$region_id == region,]
  train_data = current_data[current_data$day < 25,]
  test_data = current_data[current_data$day >= 25,]
  
  #plot(current_data$hour, current_data$speed)
  
  # Non parametric density estimation
  plot(density(train_data$speed), col='blue')
  
  # Fit a gaussian
  mu = mean(train_data$speed)
  sigma = sd(train_data$speed)
  lines(xs, dnorm(xs, mean=mu, sd=sigma), type='l')
  
  # Fit a simple linear regression
  fit = lm(speed ~ poly(hour, 3) + weekend * hour, data=train_data)
  predictions = predict(fit, test_data)
  print("Linear Regression")
  print(1 / length(predictions) * sum((predictions- test_data$speed)^2))
  
  # Fit a local linear regression
  fit = locfit(speed ~ hour + weekend * hour, data=train_data)
  # Calculate the error
  predictions = predict(fit, test_data)
  print("Local Linear Regression")
  print(1 / length(predictions) * sum((predictions- test_data$speed)^2))
}
