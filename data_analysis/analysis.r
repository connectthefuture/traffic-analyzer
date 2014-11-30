library('locfit')

PLOT_DIR = '../plots/'
MODEL_DIR = '../models/'

# Load and prepare the data
region_data <- read.table('../dat/augmented_region_data.csv', header=TRUE, sep=',')
#segment_data <- read.table('../dat/augmented_segment_data.csv', header=TRUE, sep=',')

# Add a little noise to the data
region_data$speed = region_data$speed + rnorm(nrow(region_data), 0, 0.5)

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
  sigma = sd(dat$speed)
  mu_error = qnorm(1 - alpha / 2) * sigma / sqrt(n)
  
  # print the results
  print(sprintf("%f %f %f", mu, mu_error, sigma))
  
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
  
  
  select_model = function(models, num_runs = 10) {
    # This function will test several models and choose the best model. It runs num_runs
    # times and takes the model with the lowest average error.
    min_risk = Inf
    best_model = NULL
    for (model in models) {
      all_mses = c()
      for (run in 1:num_runs) {
        test_rows = sample(1:nrow(dat), nrow(dat) * test_percent, replace=FALSE)
        test_data = dat[test_rows, ]
        train_data = dat[-test_rows, ]
        
        # Fit the model
        fit = lm(model, dat=train_data)
        
        # Compute MSE on the test data
        all_mses = c(all_mses, mean((suppressWarnings(predict(fit, test_data)) - test_data$speed)^2))
      }
      estimated_risk = mean(all_mses)
      if (estimated_risk < min_risk) {
        min_risk = estimated_risk
        best_model = model
      }
    }
    return (best_model)
  }
  if (!is.null(seed)) { set.seed(seed) } 
  
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
  
  nonlinear_models = c(formula(speed ~ poly(hour, 3) + minute),
                       formula(speed ~ poly(hour, 3) + minute + day_label),
                       formula(speed ~ poly(hour, 3) + minute + day_label + weekend + weekend * hour),
                       # Add in sports games
                       formula(speed ~ poly(hour, 3) + minute + day_label + weekend + weekend * hour + any_game),
                       formula(speed ~ poly(hour, 3) + minute + day_label + weekend + weekend * hour + blackhawks),
                       formula(speed ~ poly(hour, 3) + minute + day_label + weekend + weekend * hour + bears),
                       formula(speed ~ poly(hour, 3) + minute + day_label + weekend + weekend * hour + bulls),
                       formula(speed ~ poly(hour, 3) + minute + day_label + weekend + weekend * hour + cubs),
                       formula(speed ~ poly(hour, 3) + minute + day_label + weekend + weekend * hour + whitesox)
  )

  # Find the best linear model
  best_linear_model = select_model(linear_models)
  best_nonlinear_model = select_model(nonlinear_models)
  
  # Create the actual fits
  best_linear_fit = lm(best_linear_model, dat)
  best_nonlinear_fit = lm(best_nonlinear_model, dat)
  
  # Calculate MSE for the fits
  linear_mse = mean(best_linear_fit$residuals ^ 2)
  nonlinear_mse = mean(best_nonlinear_fit$residuals ^ 2)
  
  # Print out everything of use
  print(sprintf("%f %f", linear_mse, nonlinear_mse))
  cat(deparse(best_linear_model))
  print('')
  cat(deparse(best_nonlinear_model))
  
  # Save any models/graphs
  if (save_plots) {
    saveRDS(best_linear_fit, paste(MODEL_DIR, save_prefix, '-linear.RData', sep=''))
    saveRDS(best_nonlinear_fit, paste(MODEL_DIR, save_prefix, '-nonlinear.RData', sep=''))
  }
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
  density = density(dat$speed)
  
  if (save_plots) {png(paste(PLOT_DIR, save_prefix, '-nonpar-density', sep=''))}
  plot(density)
  if (save_plots) { dev.off()}
}

run_cross_validation <- function(model, train_data, alphas) {
  # Runs leave one out cross validation for a linear smoother.
  # Args:
  #   model: A formula that will be used as the local linear regression formula.
  #   data_set: The data set that the local linear regression should be trained on.
  #   alphas: A sequence of alphas to be run with
  best_cv = Inf
  best_alpha = NULL
  for (alpha in alphas) {
    fit = do.call('locfit', list(formula(model), data=train_data, alpha=alpha))
    r = residuals(fit)
    infl = fitted(fit, what = "infl")
    cv_score = mean((r/(1-infl))^2)
    if (cv_score < best_cv) {
      best_cv = cv_score
      best_alpha = alpha
    }
  }
  return (best_alpha)
}

select_model_nonparametric <- function(models, dat, test_percent, num_runs = 10) {
  # This function will select the model that produces the lowest average risk.
  # Args:
  #   models: All models that will be tested.
  #   dat: Data that the model should be trained/tested on.
  #   test_percent: What percent of dat should be held out as test data.
  #   num_runs: How many runs should be used to estimate the risk of a specific model.
  min_aic = Inf
  best_model = NULL
  for (model in models) {
    all_mses = c()
    
    best_alpha = 0.1
    aic_value = aic(formula(model), dat=train_data, alpha=best_alpha)
    if (aic_value < min_aic) {
      min_aic = aic_value
      best_model = model
    }
    #for (run in 1:num_runs) {
    #  test_rows = sample(1:nrow(dat), nrow(dat) * test_percent, replace=FALSE)
    #  test_data <- dat[test_rows, ]
    #  train_data <- dat[-test_rows, ]
      
      # Fit the model
      #best_alpha = run_cross_validation(formula(model), train_data, seq(0.1, 1, 0.1))
    #  best_alpha = 0.1
      
    #  fit = 
      
      # Compute MSE on the test data
    #  all_mses = c(all_mses, mean((suppressWarnings(predict(fit, test_data)) - test_data$speed)^2))
    #}
    #estimated_risk = mean(all_mses)
    #if (estimated_risk < min_risk) {
    #  min_risk = estimated_risk
    #  best_model = model
    #}
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
                    speed ~ hour + weekend * hour,
                    
                    # Add sports effects
                    speed ~ hour + weekend * hour + any_game,
                    speed ~ hour + weekend * hour + bears,
                    speed ~ hour + weekend * hour + bulls,
                    speed ~ hour + weekend * hour + blackhawks,
                    speed ~ hour + weekend * hour + bulls,
                    speed ~ hour + weekend * hour + whitesox
                    )
  
  # First we select a model
  model = select_model_nonparametric(all_models, dat, test_percent)
  # Determine bandwidth
  #alpha = run_cross_validation(model, dat, seq(0.1, 1, 0.1))
  alpha = 0.1
  
  # Construct the actual fit
  fit = locfit(formula(model), data=dat, alpha=alpha)
  # Compute the mse
  mse = mean((predict(fit, dat) - dat$speed)^2)
  print(model)
  print(mse)
  
  # Save plots and models
  if (save_plots) {
    saveRDS(fit, paste(MODEL_DIR, save_prefix, '-nonparametric.RData', sep=''))
  }
}

run_analysis <- function(data_set, save_plots = FALSE, save_prefix = '') {
  # This function will essentially call all the other functions in this file.
  parametric_density_estimation(data_set, save_plots = save_plots, save_prefix = save_prefix)
  parametric_regression(data_set, save_plots = save_plots, save_prefix = save_prefix)
  nonparametric_density_estimation(data_set, save_plots = save_plots, save_prefix = save_prefix)
  nonparametric_regression(data_set, save_plots = save_plots, save_prefix = save_prefix)
}

for (region_id in 1:29) {
  print(sprintf("Running analysis for %d", region_id))
  run_analysis(region_data[region_data$id == region_id,], TRUE, toString(region_id))
}

#### FOR TESTING ####
current_data = region_data[region_data$id == 13,]