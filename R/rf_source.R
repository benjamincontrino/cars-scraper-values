
update_car_prediction_model <- function() {
  
  # pre process data to help distributions
  df <- read.csv("data/cars_data_db.csv") %>%
    mutate(miles = ifelse(is.na(miles), 0, miles)) %>%
    filter(!is.na(make) & !is.na(model_clean) & !is.na(year) & !is.na(miles) & !is.na(price) & !is.na(new_or_used)) %>%
    group_by(make, model_clean) %>%
    mutate(sample_count = n()) %>%
    ungroup() %>%
    filter(sample_count>=10) %>%
    dplyr::select(-sample_count)
  
  
  # ============================================
  # LOAD AND PREPARE DATA
  # ============================================
  
  # Assuming your data is already loaded as 'cars_data'
  # If not, load it:
  # cars_data <- read.csv("data/cars_data_charlotte_28271.csv")
  
  # Select and prepare features
  model_data <- df %>%
    select(price, make, model_clean, year, miles, new_or_used) %>%
    # Remove any rows with missing values
    drop_na()
  
  rm(df)
  gc()
  
  # Check the data
  message("Dataset prepared:")
  message("  Total observations: ", nrow(model_data))
  message("  Features: make, model_clean, year, miles, new_or_used")
  message("  Target: price")
  message("\nData summary:")
  print(summary(model_data))
  
  # ============================================
  # SET SEED FOR REPRODUCIBILITY
  # ============================================
  
  set.seed(123)
  
  # ============================================
  # CREATE CROSS-VALIDATION FOLDS
  # ============================================
  
  message("\n========================================")
  message("Creating 10-Fold Cross-Validation Splits")
  message("========================================\n")
  
  # Create 10-fold cross-validation
  cv_folds <- vfold_cv(model_data, v = 10, strata = price)
  
  message("Created ", nrow(cv_folds), " cross-validation folds")
  
  # ============================================
  # DEFINE RECIPE (DATA PREPROCESSING)
  # ============================================
  
  message("\n========================================")
  message("Defining Recipe")
  message("========================================\n")
  
  rf_recipe <- recipe(price ~ make + model_clean + year + miles + new_or_used, 
                      data = model_data) %>%
    # Handle new factor levels that weren't in training data
    step_novel(all_nominal_predictors(), new_level = "unknown") %>%
    # Convert character/factor variables to dummy variables for RF
    step_dummy(all_nominal_predictors()) %>%
    # Normalize numeric predictors
    step_normalize(all_numeric_predictors())
  
  message("Recipe steps:")
  message("  1. Create dummy variables for categorical predictors")
  message("  2. Normalize numeric predictors (year, miles)")
  
  # ============================================
  # DEFINE MODEL SPECIFICATION
  # ============================================
  
  message("\n========================================")
  message("Defining Random Forest Model")
  message("========================================\n")
  
  rf_spec <- rand_forest(
    mtry = tune(),           # Number of predictors at each split (will tune)
    trees = 500,             # Number of trees
    min_n = tune()           # Minimum observations in terminal nodes (will tune)
  ) %>%
    set_engine("ranger", importance = "impurity") %>%  # Use ranger for speed
    set_mode("regression")
  
  message("Model parameters:")
  message("  Trees: 500")
  message("  mtry: tuning (will find optimal)")
  message("  min_n: tuning (will find optimal)")
  
  # ============================================
  # CREATE WORKFLOW
  # ============================================
  
  message("\n========================================")
  message("Creating Workflow")
  message("========================================\n")
  
  rf_workflow <- workflow() %>%
    add_recipe(rf_recipe) %>%
    add_model(rf_spec)
  
  print(rf_workflow)
  
  # ============================================
  # DEFINE TUNING GRID
  # ============================================
  
  message("\n========================================")
  message("Setting Up Hyperparameter Grid")
  message("========================================\n")
  
  # Create a grid of hyperparameters to try
  rf_grid <- grid_regular(
    mtry(range = c(2, 5)),     # Try different numbers of predictors
    min_n(range = c(2, 20)),   # Try different minimum node sizes
    levels = 5                  # 5 levels for each parameter = 25 combinations
  )
  
  message("Hyperparameter grid:")
  message("  mtry values: ", paste(unique(rf_grid$mtry), collapse = ", "))
  message("  min_n values: ", paste(unique(rf_grid$min_n), collapse = ", "))
  message("  Total combinations: ", nrow(rf_grid))
  
  # ============================================
  # TUNE MODEL WITH CROSS-VALIDATION
  # ============================================
  
  message("\n========================================")
  message("Tuning Model with 10-Fold CV")
  message("This may take a few minutes...")
  message("========================================\n")
  
  # Parallel processing for speed
  doParallel::registerDoParallel()
  
  # Tune the model
  rf_tune_results <- rf_workflow %>%
    tune_grid(
      resamples = cv_folds,
      grid = rf_grid,
      metrics = metric_set(rmse, rsq, mae),
      control = control_grid(save_pred = TRUE, verbose = TRUE)
    )
  
  message("\nTuning complete!")
  
  # ============================================
  # EXAMINE TUNING RESULTS
  # ============================================
  
  message("\n========================================")
  message("CROSS-VALIDATION RESULTS")
  message("========================================\n")
  
  # Collect metrics
  cv_metrics <- collect_metrics(rf_tune_results)
  print(cv_metrics)
  
  # Show best models by RMSE
  message("\nTop 5 models by RMSE:")
  best_models <- cv_metrics %>%
    filter(.metric == "rmse") %>%
    arrange(mean) %>%
    head(5)
  print(best_models)
  
  # Get the best hyperparameters
  best_params <- select_best(rf_tune_results, metric = "rmse")
  message("\nBest hyperparameters:")
  message("  mtry: ", best_params$mtry)
  message("  min_n: ", best_params$min_n)
  
  # ============================================
  # VISUALIZE TUNING RESULTS
  # ============================================
  
  message("\n========================================")
  message("Creating Tuning Visualizations")
  message("========================================\n")
  
  # Plot tuning results
  tuning_plot <- autoplot(rf_tune_results, metric = "rmse") +
    labs(
      title = "Hyperparameter Tuning Results",
      subtitle = "10-Fold Cross-Validation Performance"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 14))
  
  print(tuning_plot)
  
  # ============================================
  # FINALIZE WORKFLOW WITH BEST PARAMETERS
  # ============================================
  
  message("\n========================================")
  message("Finalizing Model with Best Parameters")
  message("========================================\n")
  
  # Finalize workflow with best parameters
  final_workflow <- rf_workflow %>%
    finalize_workflow(best_params)
  
  # Fit on entire dataset
  final_fit <- final_workflow %>%
    fit(data = model_data)
  
  message("Final model fitted!")
  
  # ============================================
  # FINAL MODEL PERFORMANCE
  # ============================================
  
  message("\n========================================")
  message("FINAL MODEL PERFORMANCE")
  message("========================================\n")
  
  # Get CV metrics for best model
  best_metrics <- cv_metrics %>%
    filter(mtry == best_params$mtry, min_n == best_params$min_n)
  
  rmse_value <- best_metrics %>% filter(.metric == "rmse") %>% pull(mean)
  rsq_value <- best_metrics %>% filter(.metric == "rsq") %>% pull(mean)
  mae_value <- best_metrics %>% filter(.metric == "mae") %>% pull(mean)
  
  message("Cross-Validation Metrics (Best Model):")
  message("  RMSE:  $", format(round(rmse_value, 2), big.mark = ","))
  message("  R²:    ", round(rsq_value, 4))
  message("  MAE:   $", format(round(mae_value, 2), big.mark = ","))
  
  # ============================================
  # VARIABLE IMPORTANCE
  # ============================================
  
  message("\n========================================")
  message("VARIABLE IMPORTANCE")
  message("========================================\n")
  
  # Extract the fitted model from the workflow
  final_rf_fit <- extract_fit_parsnip(final_fit)
  
  # Get variable importance
  importance_df <- final_rf_fit$fit$variable.importance %>%
    enframe(name = "variable", value = "importance") %>%
    arrange(desc(importance)) %>%
    mutate(importance_pct = 100 * importance / sum(importance))
  
  message("Top 10 Most Important Variables:")
  print(head(importance_df, 10))
  
  # Plot variable importance
  importance_plot <- importance_df %>%
    head(20) %>%
    ggplot(aes(x = reorder(variable, importance), y = importance)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(
      title = "Top 20 Variables by Importance",
      subtitle = "Random Forest Feature Importance",
      x = "Variable",
      y = "Importance"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 14))
  
  print(importance_plot)
  
  # ============================================
  # MODEL PREDICTIONS AND DIAGNOSTICS
  # ============================================
  
  message("\n========================================")
  message("MODEL DIAGNOSTICS")
  message("========================================\n")
  
  # Get predictions on training data
  predictions <- predict(final_fit, model_data) %>%
    bind_cols(model_data)
  
  # Calculate residuals
  predictions <- predictions %>%
    mutate(residual = price - .pred)
  
  # Diagnostic statistics
  message("Prediction Statistics:")
  message("  Mean Residual:     $", format(round(mean(predictions$residual), 2), big.mark = ","))
  message("  Median Residual:   $", format(round(median(predictions$residual), 2), big.mark = ","))
  message("  Min Prediction:    $", format(round(min(predictions$.pred), 2), big.mark = ","))
  message("  Max Prediction:    $", format(round(max(predictions$.pred), 2), big.mark = ","))
  
  # ============================================
  # DIAGNOSTIC PLOTS
  # ============================================
  
  message("\nCreating diagnostic plots...")
  
  # 1. Actual vs Predicted
  plot1 <- predictions %>%
    ggplot(aes(x = price, y = .pred)) +
    geom_point(alpha = 0.5, color = "steelblue") +
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", size = 1) +
    scale_x_continuous(labels = scales::dollar_format()) +
    scale_y_continuous(labels = scales::dollar_format()) +
    labs(
      title = "Actual vs Predicted Prices",
      subtitle = "Random Forest with 10-Fold Cross-Validation (tidymodels)",
      x = "Actual Price",
      y = "Predicted Price"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 14))
  
  # 2. Residuals vs Predicted
  plot2 <- predictions %>%
    ggplot(aes(x = .pred, y = residual)) +
    geom_point(alpha = 0.5, color = "coral") +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 1) +
    scale_x_continuous(labels = scales::dollar_format()) +
    scale_y_continuous(labels = scales::dollar_format()) +
    labs(
      title = "Residual Plot",
      subtitle = "Checking for patterns in prediction errors",
      x = "Predicted Price",
      y = "Residuals"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 14))
  
  # 3. Distribution of Residuals
  plot3 <- predictions %>%
    ggplot(aes(x = residual)) +
    geom_histogram(bins = 50, fill = "steelblue", color = "white", alpha = 0.7) +
    geom_vline(xintercept = 0, color = "red", linetype = "dashed", size = 1) +
    scale_x_continuous(labels = scales::dollar_format()) +
    labs(
      title = "Distribution of Residuals",
      subtitle = "Should be centered around zero",
      x = "Residual",
      y = "Count"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 14))
  
  print(plot1)
  print(plot2)
  print(plot3)
  
  # ============================================
  # SAVE MODEL AND RESULTS
  # ============================================
  
  message("\n========================================")
  message("SAVING MODEL")
  message("========================================\n")
  
  # Save the final workflow
  saveRDS(list(final_fit, today()), "data/cars_rf_model.rds")
  message("Final model saved to: random_forest_tidymodels_fit.rds")
  
  # Save tuning results
  #saveRDS(rf_tune_results, "rf_tuning_results.rds")
  #message("Tuning results saved to: rf_tuning_results.rds")
  
  # # Save predictions
  # predictions_export <- predictions %>%
  #   select(
  #     actual_price = price,
  #     predicted_price = .pred,
  #     residual,
  #     make,
  #     model_clean,
  #     year,
  #     miles,
  #     new_or_used
  #   )
  # 
  # write_csv(predictions_export, "model_predictions_tidymodels.csv")
  # message("Predictions saved to: model_predictions_tidymodels.csv")
  # 
  # # ============================================
  # # EXAMPLE PREDICTIONS
  # # ============================================
  # 
  # message("\n========================================")
  # message("EXAMPLE PREDICTIONS")
  # message("========================================\n")
  # 
  # # Show some example predictions
  # examples <- predictions %>%
  #   sample_n(10) %>%
  #   select(make, model_clean, year, miles, new_or_used, actual = price, predicted = .pred, residual) %>%
  #   mutate(
  #     actual = scales::dollar(actual),
  #     predicted = scales::dollar(predicted),
  #     residual = scales::dollar(residual)
  #   )
  # 
  # print(examples)
  # 
  # # ============================================
  # # HOW TO USE THE MODEL
  # # ============================================
  # 
  # message("\n========================================")
  # message("HOW TO USE THIS MODEL")
  # message("========================================\n")
  # 
  # cat("
  # To make predictions on new data:
  # 
  # # Load the saved model
  # final_fit <- readRDS('random_forest_tidymodels_fit.rds')
  # 
  # # Prepare new data (must have same columns)
  # new_data <- tibble(
  #   make = c('Toyota', 'Honda'),
  #   model_clean = c('Camry', 'Accord'),
  #   year = c(2022, 2023),
  #   miles = c(25000, 15000),
  #   new_or_used = c('Used', 'Used')
  # )
  # 
  # # Make predictions
  # predictions <- predict(final_fit, new_data)
  # print(predictions)
  # 
  # # Get prediction intervals (if needed)
  # predictions_int <- predict(final_fit, new_data, type = 'conf_int')
  # predictions_combined <- bind_cols(new_data, predictions, predictions_int)
  # print(predictions_combined)
  # ")
  # 
  # message("\n========================================")
  # message("MODEL TRAINING COMPLETE!")
  # message("========================================\n")
  # 
  # # Return the final fit for further use
  # final_fit
  
  
  
}