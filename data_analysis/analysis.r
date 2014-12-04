library('locfit')

PLOT_DIR = '../plots/'
MODEL_DIR = '../models/'

# Load and prepare the data
region_data <- read.table('../dat/augmented_region_data.csv', header=TRUE, sep=',')

# Add a little noise to the data to (we do this since there are a lot of repeated values
# in the data that throw off density estimates)
region_data$speed = region_data$speed + rnorm(nrow(region_data), 0, 0.01)

# Convert all sports games to factors
region_data$any_game = factor(region_data$any_game)
region_data$cubs = factor(region_data$cubs)
region_data$bulls = factor(region_data$bulls)
region_data$blackhawks = factor(region_data$blackhawks)
region_data$bears = factor(region_data$bears)
region_data$whitesox = factor(region_data$whitesox)

parametric_density_estimation <- function (dat, alpha = 0.05, 
                                           save_plots=FALSE, save_prefix='') {
  # This will perform some basic parametric density estimation on a given data set.
  # It assumes that the density is normal, and simply estimates the mean and variance.
  # To examine this assumption a QQ plot is produced.
  # Args:
  #    dat: The data set to be examined. Must have a 'speed' value.
  #    alpha: The alpha to use when calculating confidence intervals.
  #    save_plots: A flag to indicate if plots should be saved or displayed.
  #    save_prefix: A prefix to be prepended to all plots saved by this function.
  n = length(dat$speed)
  mu = mean(dat$speed)
  sigma2 = var(dat$speed)
  mu_error = qnorm(1 - alpha / 2) * sqrt(sigma2) / sqrt(n)
  sigma2_upper = (n - 1) * sigma2 / qchisq((1 - alpha)/2, n - 1) - sigma2
  sigma2_lower = sigma2 - (n - 1) * sigma2 / qchisq((1 - alpha)/2, n - 1, lower.tail = FALSE)
  
  # print the results
  print(sprintf("%.2f $\\pm$ %.2E, %.2f (-%.2E, +%.2E)", mu, mu_error, sigma2, sigma2_lower, sigma2_upper))
  
  # Plotting
  if (save_plots) {png(paste(PLOT_DIR, save_prefix, '-qqplot', sep=''))}
  qqnorm(dat$speed)
  if (save_plots) { dev.off()}
}


parametric_regression <- function (dat, test_percent = 0.25, seed=NULL,
                                   save_plots=FALSE, save_prefix='') {
  # This function will perform a paramateric regression on the given data set. It performs
  # both linear and cubic regressions over several different models and choses the one with
  # the lowest error. Error is calculated by taking a random subset of the data as held out
  # test data.
  # Args:
  #    dat: The data set to be examined. Must have a 'speed' value.
  #    test_percent: How much data should be held out in the test set.
  #    seed: An optional seed for the random number generator to make reproducable runs.
  #    save_plots: A flag to indicate if plots should be saved or displayed.
  #    save_prefix: A prefix to be prepended to all plots saved by this function.
  
  
  select_model = function(models, train_data) {
    # This function will test several models and choose the best model. It does this
    # by computing the AIC for each model and choosing the best one.
    min_aic = Inf
    best_model = NULL
    for (model in models) {
      fit = lm(model, train_data)
      aic_value = AIC(fit)
      if (aic_value < min_aic) {
        min_aic = aic_value
        best_model = model
      }
    }
    return (best_model)
  }
  
  # Make sure we set the seed properly
  if (!is.null(seed)) { set.seed(seed) } 

  # Define models that we will try
  linear_models = c(formula(speed ~ hour + minute),
                    formula(speed ~ hour + minute + day_label),
                    formula(speed ~ hour + minute + day_label + weekend + weekend * hour),
                    # Add in sports games
                    formula(speed ~ hour + minute + day_label + weekend + weekend * hour + any_game),
                    formula(speed ~ hour + minute + day_label + weekend + weekend * hour + blackhawks),
                    formula(speed ~ hour + minute + day_label + weekend + weekend * hour + bears),
                    formula(speed ~ hour + minute + day_label + weekend + weekend * hour + bulls),
                    formula(speed ~ hour + minute + day_label + weekend + weekend * hour + cubs),
                    formula(speed ~ hour + minute + day_label + weekend + weekend * hour + whitesox)
  )
  
  nonlinear_models = c(formula(speed ~ poly(hour, 3)),
                       formula(speed ~ poly(hour, 3) + minute + poly(day_label, 3)),
                       formula(speed ~ poly(hour, 3) + minute + poly(day_label, 3) + weekend + weekend * poly(hour, 3)),
                       # Add in sports games
                       formula(speed ~ poly(hour, 3) + minute * poly(hour, 3) + poly(day_label, 3) + weekend + weekend * poly(day_label, 3) + any_game),
                       formula(speed ~ poly(hour, 3) + minute * poly(hour, 3) + poly(day_label, 3) + weekend + weekend * poly(day_label, 3) + blackhawks),
                       formula(speed ~ poly(hour, 3) + minute * poly(hour, 3) + poly(day_label, 3) + weekend + weekend * poly(day_label, 3) + bears),
                       formula(speed ~ poly(hour, 3) + minute * poly(hour, 3) + poly(day_label, 3) + weekend + weekend * poly(day_label, 3) + bulls),
                       formula(speed ~ poly(hour, 3) + minute * poly(hour, 3) + poly(day_label, 3) + weekend + weekend * poly(day_label, 3) + cubs),
                       formula(speed ~ poly(hour, 3) + minute * poly(hour, 3) + poly(day_label, 3) + weekend + weekend * poly(day_label, 3) + whitesox)
  )
  
  # Create test and train data
  test_rows = sample(1:nrow(dat), nrow(dat) * test_percent, replace=FALSE)
  test_data = dat[test_rows, ]
  train_data = dat[-test_rows, ]
  
  # Find the best linear model
  best_linear_model = select_model(linear_models, train_data)
  best_nonlinear_model = select_model(nonlinear_models, train_data)
  
  # Create the actual fits
  best_linear_fit = lm(best_linear_model, train_data)
  best_nonlinear_fit = lm(best_nonlinear_model, train_data)
  
  # Calculate training and test error for the fits
  linear_training_error = mean(best_linear_fit$residuals^2)
  linear_testing_error = mean((predict(best_linear_fit, test_data) - test_data$speed)^2)
  nonlinear_training_error = mean(best_nonlinear_fit$residuals^2)
  nonlinear_testing_error = mean((suppressWarnings(predict(best_nonlinear_fit, test_data)) - test_data$speed)^2)
  
  # Print out everything of use
  #print(sprintf("Linear, Training error: %f, Testing error: %f", linear_training_error, linear_testing_error))
  #print(sprintf("Nonlinear, Training error: %f, Testing error: %f", nonlinear_training_error, nonlinear_testing_error))
  #print("Best Linear model")
  #print(best_linear_model)
  #print("Best Nonlinear model")
  #print(best_nonlinear_model)
  print(sprintf("%.2f & %.2f, %.2f, %.2f, %.2f, %.2f", 
                linear_training_error, linear_testing_error, summary(best_linear_fit)$r.squared,
                nonlinear_training_error, nonlinear_testing_error, summary(best_nonlinear_fit)$r.squared))
  
  # For standard erros (which can be used to construct confidence intervals)
  # print(summary(best_linear_fit))
  # print(summary(best_nonlinear_fit))
  
  # Save any models/graphs
  if (save_plots) {
    saveRDS(best_linear_fit, paste(MODEL_DIR, save_prefix, '-linear.RData', sep=''))
    saveRDS(best_nonlinear_fit, paste(MODEL_DIR, save_prefix, '-nonlinear.RData', sep=''))
  }
  if (save_plots) {png(paste(PLOT_DIR, save_prefix, '-linear.png', sep=''))}
  plot(dat$hour, dat$speed)
  points(dat$hour, suppressWarnings(predict(best_linear_fit, dat)), col='blue')
  if (save_plots) { dev.off()}
  
  if (save_plots) {png(paste(PLOT_DIR, save_prefix, '-nonlinear.png', sep=''))}
  plot(dat$hour, dat$speed)
  points(dat$hour, suppressWarnings(predict(best_nonlinear_fit, dat)), col='blue')
  if (save_plots) { dev.off()}
}

nonparametric_density_estimation <- function (dat, alpha = 0.05,
                                              save_plots=FALSE, save_prefix='') {
  # This function will perform a nonparametric estimate of the density using kernel density
  # estimation
  # Args:
  #    dat: The data set to be examined. Must have a 'speed' value.
  #    alpha: Size of confidence band.
  #    save_plots: A flag to indicate if plots should be saved or displayed.
  #    save_prefix: A prefix to be prepended to all plots saved by this function.
  
  # Create a density estimate
  density = density(dat$speed, bw='nrd')
  
  if (save_plots) {png(paste(PLOT_DIR, save_prefix, '-density.png', sep=''))}
  plot(density)
  xs = seq(0, 60, 0.1)
  lines(xs, dnorm(xs, mean(dat$speed),sd(dat$speed)), lty=2)
  if (save_plots) { dev.off()}
}

run_cross_validation <- function(model, train_data, alphas) {
  # Runs leave one out cross validation for a linear smoother.
  # Args:
  #   model: A formula that will be used as the local linear regression formula.
  #   data_set: The data set that the local linear regression should be trained on.
  #   alphas: A sequence of alphas to be run with
  best_cv = Inf
  best_alpha = min(alphas)
  for (alpha in alphas) {
    fit = do.call('locfit', list(formula(model), data=train_data, alpha=alpha))
    r = residuals(fit)
    infl = fitted(fit, what = "infl")
    cv_score = mean((r/(1-infl))^2)
    if (!is.nan(cv_score)) {
      if (cv_score < best_cv) {
        best_cv = cv_score
        best_alpha = alpha
      } else {
        return (best_alpha) # Bail early to speed up computation
      }
    }
  }
  return (best_alpha)
}

select_model_nonparametric <- function(models, dat, test_percent) {
  # This function will select the model that produces the lowest average risk. This 
  # uses AIC and takes the minimum value.
  # Args:
  #   models: All models that will be tested.
  #   dat: Data that the model should be trained/tested on.
  #   test_percent: What percent of dat should be held out as test data.
  min_aic = Inf
  best_model = NULL
  for (model in models) {
    all_mses = c()
    
    #best_alpha = run_cross_validation(model, dat, seq(0, 1, 0.1))
    best_alpha = 0.1 # Heuristic value of alpha
    aic_value = aic(formula(model), dat=dat, alpha=best_alpha)
    if (aic_value[4] < min_aic) {
      min_aic = aic_value[4]
      best_model = model
    }
  }
  return (best_model)
}

nonparametric_regression <- function (dat, test_percent = 0.25, seed = NULL,
                                      save_plots=FALSE, save_prefix='') {
  # This function will perform a nonparametric regression of the speed data and return
  # vital statistics
  # Args:
  #    dat: The data set to be examined. Must have a 'speed' value.
  #    save_plots: A flag to indicate if plots should be saved or displayed.
  #    save_prefix: A prefix to be prepended to all plots saved by this function.
  
  if (!is.null(seed)) { set.seed(seed) } 
  
  all_models = list(speed ~ hour,
                    speed ~ hour + minute * hour,
                    speed ~ hour + minute * hour + weekend * hour,
                    speed ~ hour + minute * hour + weekend * hour + day_label,
                    
                    # Add sports effects
                    speed ~ hour + weekend * hour + minute * hour + day_label + any_game * hour,
                    speed ~ hour + weekend * hour + minute * hour + day_label + bulls * hour,
                    speed ~ hour + weekend * hour + minute * hour + day_label + bears * hour,
                    speed ~ hour + weekend * hour + minute * hour + day_label + whitesox * hour,
                    speed ~ hour + weekend * hour + minute * hour + day_label + cubs * hour,
                    speed ~ hour + weekend * hour + minute * hour + day_label + blackhawks * hour,
                    
                    # Possibly simpler models
                    speed ~ hour + weekend * hour + any_game * hour,
                    speed ~ hour + weekend * hour + blackhawks * hour,
                    speed ~ hour + weekend * hour + bulls * hour,
                    speed ~ hour + weekend * hour + bears * hour,
                    speed ~ hour + weekend * hour + cubs * hour,
                    speed ~ hour + weekend * hour + whitesox * hour
                    )
  
  # Create test and train data
  test_rows = sample(1:nrow(dat), nrow(dat) * test_percent, replace=FALSE)
  test_data = dat[test_rows, ]
  train_data = dat[-test_rows,]
  
  # First we select a model
  model = select_model_nonparametric(all_models, train_data, test_percent)
  # Determine the bandwidth
  alpha = run_cross_validation(model, dat, seq(0, 1, 0.1))
  # Construct the actual fit
  fit = locfit(model, data=train_data, alpha=alpha)
  
  # Compute the errors
  training_error = mean((predict(fit, train_data) - train_data$speed)^2)
  testing_error = mean((predict(fit, test_data) - test_data$speed)^2)
  
  #print(sprintf("Nonparametric, Training error: %f, Testing error: %f", training_error, testing_error))
  #print("Best nonparametric model")
  #print(model)
  #print("Best bandwidth")
  #print(alpha)
  print(sprintf(", %.2f, %.2f", training_error, testing_error))
  
  # Save plots and models
  # To plot confidence bands use the following (generally this is hard to interpert for
  # higher dimensional data)
  # crit(fit) <- crit(fit,cov=0.99)
  # plot(fit, band="local")
  
  if (save_plots) {
    saveRDS(fit, paste(MODEL_DIR, save_prefix, '-nonparametric.RData', sep=''))
  }
  if (save_plots) {png(paste(PLOT_DIR, save_prefix, '-nonparametric.png', sep=''))}
  plot(dat$hour, dat$speed)
  points(dat$hour, suppressWarnings(predict(fit, dat)), col='blue')
  if (save_plots) { dev.off()}
}

run_analysis <- function(data_set, save_plots = FALSE, save_prefix = '') {
  # This function will essentially call all the other functions in this file.
  parametric_density_estimation(data_set, save_plots = save_plots, save_prefix = save_prefix)
  parametric_regression(data_set, seed = 42, save_plots = save_plots, save_prefix = save_prefix)
  nonparametric_density_estimation(data_set, save_plots = save_plots, save_prefix = save_prefix)
  nonparametric_regression(data_set, seed = 42, save_plots = save_plots, save_prefix = save_prefix)
}

for (region_id in 1:29) {
  print(sprintf("Running analysis for %d", region_id))
  run_analysis(region_data[region_data$id == region_id,], TRUE, toString(region_id))
}

#### FOR TESTING ####
current_data = region_data[region_data$id == 13,]
