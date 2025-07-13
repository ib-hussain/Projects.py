# Pakistan Inflation Analysis and Modeling
# Improved and streamlined code with enhanced visualizations

# Load required libraries
library(tidyverse)
library(forecast)
library(glmnet)
library(plotly)
library(tseries)
library(gridExtra)
library(knitr)
library(cowplot)
library(scales)
library(viridis)
library(DT)

# Set a theme for consistent visualizations
theme_set(theme_minimal(base_size = 12) + 
            theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                  plot.subtitle = element_text(hjust = 0.5),
                  panel.grid.minor = element_blank(),
                  legend.position = "bottom"))

# Custom color palette
my_palette <- viridis(7, option = "D")

#-------------------------------------------------------
# PART 1: DATA PREPARATION AND EXPLORATORY ANALYSIS
#-------------------------------------------------------

# Read the data
data <- read.csv("Inflation_Dataset_v2.csv")

# Rename variables for easier reference
names(data) <- c("Country", "Year", "Inflation", "MoneyGrowth", "FoodImports", 
                 "GDPgrowth", "ExchangeRate", "CrudeOil")

# Convert Year to numeric and arrange data chronologically
data <- data %>% 
  mutate(Year = as.numeric(as.character(Year))) %>%
  arrange(Year)

# Display basic information about the dataset
cat("Dataset Overview:\n")
print(str(data))
print(paste("Time Period:", min(data$Year), "to", max(data$Year)))
print(paste("Number of observations:", nrow(data)))

#-------------------------------------------------------
# PART 2: DESCRIPTIVE STATISTICS
#-------------------------------------------------------

# Detailed summary statistics
summary_stats <- data %>%
  select(-Country) %>%
  summary()

# Calculate additional statistics
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Create a comprehensive summary table
vars_to_analyze <- c("Inflation", "MoneyGrowth", "FoodImports", 
                     "GDPgrowth", "ExchangeRate", "CrudeOil")

summary_table <- data.frame(
  Variable = vars_to_analyze,
  Mean = sapply(data[, vars_to_analyze], mean),
  Median = sapply(data[, vars_to_analyze], median),
  Mode = sapply(data[, vars_to_analyze], get_mode),
  SD = sapply(data[, vars_to_analyze], sd),
  Min = sapply(data[, vars_to_analyze], min),
  Q1 = sapply(data[, vars_to_analyze], function(x) quantile(x, 0.25)),
  Q3 = sapply(data[, vars_to_analyze], function(x) quantile(x, 0.75)),
  Max = sapply(data[, vars_to_analyze], max),
  CV = sapply(data[, vars_to_analyze], function(x) sd(x) / mean(x) * 100) # Coefficient of variation
)

# Print formatted table
kable(summary_table, digits = 2, caption = "Summary Statistics for Key Variables")

#-------------------------------------------------------
# PART 3: DATA VISUALIZATION
#-------------------------------------------------------

# Time series plots for all variables
plot_list <- list()

for (i in 1:length(vars_to_analyze)) {
  var <- vars_to_analyze[i]
  p <- ggplot(data, aes(x = Year, y = .data[[var]])) +
    geom_line(color = my_palette[i], size = 1) +
    geom_point(color = my_palette[i], size = 2) +
    labs(title = paste("Time Series of", var),
         x = "Year",
         y = var) +
    scale_x_continuous(breaks = seq(min(data$Year), max(data$Year), by = 5))
  
  plot_list[[i]] <- p
}

# Arrange plots in a grid
time_series_grid <- plot_grid(plotlist = plot_list, ncol = 2)
print(time_series_grid)


# Calculate outliers manually using IQR
boxplot_data <- data %>%
  pivot_longer(cols = vars_to_analyze,
               names_to = "Variable", values_to = "Value")

# Calculate IQR, lower, and upper outliers for each variable
outliers <- boxplot_data %>%
  group_by(Variable) %>%
  mutate(Q1 = quantile(Value, 0.25),
         Q3 = quantile(Value, 0.75),
         IQR = Q3 - Q1,
         LowerOutlier = Value < (Q1 - 1.5 * IQR),
         UpperOutlier = Value > (Q3 + 1.5 * IQR)) %>%
  ungroup()

# Filter out the outliers (both lower and upper)
outlier_values <- outliers %>%
  filter(LowerOutlier | UpperOutlier) %>%
  select(Variable, Value)

# Print the outlier values for each variable
print(outlier_values)


# Enhanced boxplots with jittered points for outlier detection
boxplot_data <- data %>%
  pivot_longer(cols = vars_to_analyze,
               names_to = "Variable", values_to = "Value")

boxplot_enhanced <- ggplot(boxplot_data, aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.5, aes(color = Variable)) +
  scale_fill_viridis_d(option = "D") +
  scale_color_viridis_d(option = "D") +
  labs(title = "Distribution of Economic Variables",
       subtitle = "Box plots with data points to identify outliers",
       x = "", y = "Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

print(boxplot_enhanced)

#scatter PLot

# install.packages("GGally")   # if you haven't already
library(GGally)

vars <- c("Inflation", "MoneyGrowth", "FoodImports",
          "GDPgrowth", "ExchangeRate", "CrudeOil")

# ggplot-based scatterplot matrix
ggpairs(
  data[, vars],
  title = "Scatterplot Matrix of Economic Variables",
  aes(alpha = 0.6)
)

# 1. Specify the variables you want to include
vars <- c("Inflation", "MoneyGrowth", "FoodImports", 
          "GDPgrowth", "ExchangeRate", "CrudeOil")

# 2. Compute the correlation matrix
corr_matrix <- cor(data[, vars], use = "pairwise.complete.obs")

# 3. Round to two decimals and print
print(round(corr_matrix, 2))


# Create correlation matrix
corr_matrix <- cor(data[, vars_to_analyze])
corr_melted <- reshape2::melt(corr_matrix)

# Heatmap of correlation matrix
corr_heatmap <- ggplot(corr_melted, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), name = "Correlation") +
  geom_text(aes(label = sprintf("%.2f", value)), size = 3) +
  labs(title = "Correlation Matrix of Economic Variables",
       x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(corr_heatmap)

# Create scatter plot matrix for all variables against Inflation
scatter_list <- list()

for (i in 2:length(vars_to_analyze)) {
  var <- vars_to_analyze[i]
  p <- ggplot(data, aes_string(x = var, y = "Inflation")) +
    geom_point(aes(color = Year), size = 3, alpha = 0.7) +
    geom_smooth(method = "lm", se = TRUE, color = "darkred", fill = "pink", alpha = 0.3) +
    scale_color_viridis_c() +
    labs(title = paste("Inflation vs.", var),
         x = var,
         y = "Inflation Rate (%)") +
    theme(legend.position = "right")
  
  scatter_list[[i-1]] <- p
}

# Arrange scatter plots in a grid
scatter_grid <- plot_grid(plotlist = scatter_list, ncol = 2)
print(scatter_grid)

#-------------------------------------------------------
# MODEL 1: ARIMA – exhaustive AIC comparison, best-fit summary, and errors
#-------------------------------------------------------

# 1. Define candidate orders and whether to include a non-zero mean
orders     <- list(c(2,0,2), c(0,0,0), c(1,0,0), c(0,0,1),
                   c(0,0,0), c(2,0,0), c(1,0,1), c(2,0,1), c(1,0,0))
mean_flags <- c(TRUE,    TRUE,    TRUE,    TRUE,
                FALSE,   TRUE,    TRUE,    TRUE,    FALSE)
labels     <- c("ARIMA(2,0,2)", "ARIMA(0,0,0)", "ARIMA(1,0,0)",
                "ARIMA(0,0,1)", "ARIMA(0,0,0)", "ARIMA(2,0,0)",
                "ARIMA(1,0,1)", "ARIMA(2,0,1)", "ARIMA(1,0,0)")

# 2. Fit each and collect AIC
aic_table <- data.frame(
  Model       = character(),
  Mean        = character(),
  AIC         = numeric(),
  stringsAsFactors = FALSE
)

for(i in seq_along(orders)) {
  fit_i <- Arima(train_ts,
                 order        = orders[[i]],
                 include.mean = mean_flags[i],
                 seasonal     = FALSE)
  aic_table[i, ] <- list(
    Model = labels[i],
    Mean  = ifelse(mean_flags[i], "non-zero mean", "zero mean"),
    AIC   = round(fit_i$aic, 2)
  )
}

# 3. Print selection results
cat("ARIMA Model Selection Results:\n")
for(i in seq_len(nrow(aic_table))) {
  cat("•", aic_table$Model[i], "with", aic_table$Mean[i],
      ": AIC =", aic_table$AIC[i], "\n")
}

# 4. Identify best (minimum AIC) and refit it
best_idx   <- which.min(aic_table$AIC)
best_order <- orders[[best_idx]]
best_mean  <- mean_flags[best_idx]
best_label <- labels[best_idx]

best_fit <- Arima(train_ts,
                  order        = best_order,
                  include.mean = best_mean,
                  seasonal     = FALSE)

# 5. Print detailed summary
cat("\nModel Summary (", best_label, " with ",
    ifelse(best_mean, "Non-Zero Mean", "Zero Mean"), "):\n", sep = "")

coefs <- coef(best_fit)
se    <- sqrt(diag(best_fit$var.coef))

for(name in names(coefs)) {
  cat("•", name, "=", round(coefs[name], 3),
      "(s.e. =", round(se[name], 3), ")\n")
}

cat("\nModel Diagnostics:\n")
cat("• Sigma²       =", round(best_fit$sigma2, 2), "\n")
cat("• Log-Likelihood =", round(logLik(best_fit)[1], 2), "\n")
cat("• AIC          =", round(best_fit$aic, 2), "\n")
cat("• AICc         =", round(best_fit$aicc, 2), "\n")
cat("• BIC          =", round(best_fit$bic, 2), "\n")

# 6. Compute in-sample error metrics
acc <- accuracy(best_fit)   # first row: training set
cat("\nTraining Set Error Metrics:\n")
cat("Metric        Value\n")
cat("ME (Mean Error)      ", round(acc[1, "ME"], 4), "\n")
cat("RMSE                 ", round(acc[1, "RMSE"], 4), "\n")
cat("MAE                  ", round(acc[1, "MAE"], 4), "\n")
cat("MPE (%)              ", round(acc[1, "MPE"], 2), "%\n")
cat("MAPE (%)             ", round(acc[1, "MAPE"], 2), "%\n")
cat("MASE                 ", round(acc[1, "MASE"], 4), "\n")
cat("ACF1                 ", round(acc[1, "ACF1"], 4), "\n")


# Combine actual and predicted data
years_all <- c(train_data$Year, test_data$Year)
actual_all <- c(y_train, y_test)
predicted_all <- c(arima_pred_train, arima_pred_test)

plot_df <- data.frame(
  Year = years_all,
  Actual = actual_all,
  Predicted = predicted_all,
  Type = c(rep("Train", length(y_train)), rep("Test", length(y_test)))
)

# Plot
ggplot(plot_df, aes(x = Year)) +
  geom_line(aes(y = Actual, color = "Actual"), size = 1.2) +
  geom_line(aes(y = Predicted, color = "Predicted"), size = 1.2, linetype = "dashed") +
  facet_wrap(~Type, scales = "free_x") +
  scale_color_manual(values = c("Actual" = "black", "Predicted" = "red")) +
  labs(title = "Actual vs ARIMA Predicted Inflation Rates",
       y = "Inflation Rate (%)",
       x = "Year",
       color = "") +
  theme_minimal()


# Combine data for plotting
plot_df <- data.frame(
  Year = c(train_data$Year, test_data$Year),
  Actual = c(y_train, y_test),
  Predicted = c(arima_pred_train, arima_pred_test),
  Type = c(rep("Training", length(y_train)), rep("Testing", length(y_test)))
)

# Enhanced Plot
ggplot(plot_df, aes(x = Year)) +
  geom_line(aes(y = Actual, color = "Actual"), size = 1.3) +
  geom_line(aes(y = Predicted, color = "Predicted"), size = 1.3, linetype = "dashed") +
  geom_vline(xintercept = max(train_data$Year), linetype = "dotted", color = "gray40", size = 0.8) +
  annotate("text", x = max(train_data$Year) + 0.5, y = max(plot_df$Actual, na.rm = TRUE),
           label = "Train/Test Split", color = "gray40", angle = 90, hjust = 0) +
  scale_color_manual(values = c("Actual" = "#1f77b4", "Predicted" = "#ff7f0e")) +
  labs(title = "Actual vs ARIMA Predicted Inflation Rates",
       subtitle = "Dashed line shows ARIMA forecasts; vertical line is the train/test split",
       x = "Year",
       y = "Inflation Rate (%)",
       color = "") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top",
        plot.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold"))
#-------------------------------------------------------
# MODEL 2: RIDGE REGRESSION
#-------------------------------------------------------

# Fit Ridge model
cat("\nFitting Ridge regression model...\n")
cv_ridge <- cv.glmnet(X_train, y_train, alpha = 0)
ridge_model <- glmnet(X_train, y_train, alpha = 0, lambda = cv_ridge$lambda.min)

# Print Ridge model details
cat("Ridge model lambda:", cv_ridge$lambda.min, "\n")
cat("Ridge model coefficients:\n")
print(coef(ridge_model))

# Generate predictions
ridge_pred_train <- predict(ridge_model, X_train)
ridge_pred_test <- predict(ridge_model, X_test)

# Calculate MSE
mse_train_ridge <- mean((ridge_pred_train - y_train)^2)
mse_test_ridge <- mean((ridge_pred_test - y_test)^2)

#-------------------------------------------------------
# MODEL 3: LASSO REGRESSION
#-------------------------------------------------------

# Fit Lasso model
cat("\nFitting LASSO regression model...\n")
cv_lasso <- cv.glmnet(X_train, y_train, alpha = 1)
lasso_model <- glmnet(X_train, y_train, alpha = 1, lambda = cv_lasso$lambda.min)

# Print Lasso model details
cat("LASSO model lambda:", cv_lasso$lambda.min, "\n")
cat("LASSO model coefficients:\n")
print(coef(lasso_model))

# Generate predictions
lasso_pred_train <- predict(lasso_model, X_train)
lasso_pred_test <- predict(lasso_model, X_test)

# Calculate MSE
mse_train_lasso <- mean((lasso_pred_train - y_train)^2)
mse_test_lasso <- mean((lasso_pred_test - y_test)^2)

#-------------------------------------------------------
# MODEL 4: ELASTIC NET REGRESSION
#-------------------------------------------------------

# Fit Elastic Net model
cat("\nFitting Elastic Net regression model...\n")
cv_elastic <- cv.glmnet(X_train, y_train, alpha = 0.5)
elastic_model <- glmnet(X_train, y_train, alpha = 0.5, lambda = cv_elastic$lambda.min)

# Print Elastic Net model details
cat("Elastic Net model lambda:", cv_elastic$lambda.min, "\n")
cat("Elastic Net model coefficients:\n")
print(coef(elastic_model))

# Generate predictions
elastic_pred_train <- predict(elastic_model, X_train)
elastic_pred_test <- predict(elastic_model, X_test)

# Calculate MSE
mse_train_elastic <- mean((elastic_pred_train - y_train)^2)
mse_test_elastic <- mean((elastic_pred_test - y_test)^2)

#-------------------------------------------------------
# MODEL COMPARISON AND EVALUATION
#-------------------------------------------------------

# Create comparison table for MSE
mse_table <- data.frame(
  Model = c("ARIMA", "Ridge", "LASSO", "Elastic Net"),
  MSE_Train = c(mse_train_arima, mse_train_ridge, mse_train_lasso, mse_train_elastic),
  MSE_Test = c(mse_test_arima, mse_test_ridge, mse_test_lasso, mse_test_elastic)
)

# Format MSE table
mse_table$MSE_Train <- round(mse_table$MSE_Train, 4)
mse_table$MSE_Test <- round(mse_table$MSE_Test, 4)

# Print MSE table
cat("\nModel Performance Comparison:\n")

print(kable(mse_table))


# Create MSE data frame
mse_data <- data.frame(
  Model = c("ARIMA", "Ridge", "LASSO", "Elastic Net"),
  MSE_Train = c(10.5356, 10.5095, 10.5161, 10.5161),
  MSE_Test = c(31.3031, 32.0651, 32.0544, 32.0544)
)

# Reshape data for ggplot
mse_data_long <- mse_data %>%
  gather(key = "Dataset", value = "MSE", MSE_Train, MSE_Test)

# Plot MSE comparison
ggplot(mse_data_long, aes(x = Model, y = MSE, fill = Dataset)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c("MSE_Train" = "#1f77b4", "MSE_Test" = "#ff7f0e")) +
  labs(title = "MSE Comparison: ARIMA vs. Ridge vs. LASSO vs. Elastic Net",
       x = "Model",
       y = "Mean Squared Error (MSE)",
       fill = "Dataset") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top",
        plot.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold"))


# Find best model
best_model <- mse_table$Model[which.min(mse_table$MSE_Test)]
cat("\nBest performing model based on Test MSE:", best_model, "\n")

# Create prediction dataset for visualization
predictions_df <- data.frame(
  Year = test_data$Year,
  Actual = y_test,
  ARIMA = arima_pred_test,
  Ridge = as.numeric(ridge_pred_test),
  LASSO = as.numeric(lasso_pred_test),
  ElasticNet = as.numeric(elastic_pred_test)
)

# Reshape data for plotting
predictions_long <- predictions_df %>%
  pivot_longer(cols = c("Actual", "ARIMA", "Ridge", "LASSO", "ElasticNet"),
               names_to = "Model", values_to = "Value")

# Plot model predictions comparison
prediction_plot <- ggplot(predictions_long, aes(x = Year, y = Value, color = Model, group = Model)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(title = "Model Predictions vs Actual Inflation",
       subtitle = paste("Test period:", min(test_data$Year), "to", max(test_data$Year)),
       x = "Year", y = "Inflation Rate (%)") +
  scale_color_viridis_d() +
  theme(legend.position = "bottom")

print(prediction_plot)

# Create interactive plot with plotly
interactive_plot <- ggplotly(prediction_plot)
print(interactive_plot)

#-------------------------------------------------------
# VARIABLE IMPORTANCE ANALYSIS
#-------------------------------------------------------

# Function to extract and normalize variable importance
get_var_importance <- function(model, model_type) {
  if (model_type == "ARIMA") {
    return(NULL)  # ARIMA doesn't have variable importance
  } else {
    # Get absolute coefficients (excluding intercept)
    coeffs <- as.vector(coef(model))[-1]
    # Normalize to sum to 100%
    importance <- abs(coeffs) / sum(abs(coeffs)) * 100
    return(importance)
  }
}

# Extract variable importance from models
ridge_importance <- get_var_importance(ridge_model, "Ridge")
lasso_importance <- get_var_importance(lasso_model, "LASSO")
elastic_importance <- get_var_importance(elastic_model, "Elastic")

# Create variable importance dataframe
var_names <- colnames(X_train)
importance_df <- data.frame(
  Variable = var_names,
  Ridge = ridge_importance,
  LASSO = lasso_importance,
  ElasticNet = elastic_importance
)

# Reshape for plotting
importance_long <- importance_df %>%
  pivot_longer(cols = c("Ridge", "LASSO", "ElasticNet"),
               names_to = "Model", values_to = "Importance")

# Create variable importance plot
importance_plot <- ggplot(importance_long, aes(x = reorder(Variable, Importance), 
                                               y = Importance, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  scale_fill_viridis_d() +
  labs(title = "Variable Importance by Model",
       subtitle = "Based on standardized coefficient magnitudes",
       x = "", y = "Relative Importance (%)") +
  theme(legend.position = "bottom")

print(importance_plot)

#-------------------------------------------------------
# FORECASTING FUTURE INFLATION
#-------------------------------------------------------

# Use the best model to forecast one year ahead
cat("\nForecasting future inflation using the best model:", best_model, "\n")

last_year <- max(data$Year)
next_year <- last_year + 1

# Create a function for forecasting
forecast_next_year <- function(best_model_name) {
  if (best_model_name == "ARIMA") {
    # Fit ARIMA on full data
    full_ts <- ts(series_st, start = min(years_st), frequency = 1)
    final_arima <- auto.arima(full_ts, stationary = TRUE)
    next_forecast <- forecast(final_arima, h = 1)
    forecast_value <- as.numeric(next_forecast$mean)
    
    # If we used differencing, we need to convert back
    if (need_diff) {
      forecast_value <- tail(inflation_ts, 1) + forecast_value
    }
    
    return(list(value = forecast_value, model = final_arima))
  } else {
    # Prepare latest data for regression models
    latest_X <- as.matrix(tail(covars_st, 1))
    
    # Use the appropriate model
    if (best_model_name == "Ridge") {
      final_model <- glmnet(as.matrix(covars_st), y_values, alpha = 0, lambda = cv_ridge$lambda.min)
      forecast_value <- predict(final_model, latest_X)
    } else if (best_model_name == "LASSO") {
      final_model <- glmnet(as.matrix(covars_st), y_values, alpha = 1, lambda = cv_lasso$lambda.min)
      forecast_value <- predict(final_model, latest_X)
    } else {  # Elastic Net
      final_model <- glmnet(as.matrix(covars_st), y_values, alpha = 0.5, lambda = cv_elastic$lambda.min)
      forecast_value <- predict(final_model, latest_X)
    }
    
    return(list(value = as.numeric(forecast_value), model = final_model))
  }
}

# Generate forecast
forecast_result <- forecast_next_year(best_model)
cat("Forecasted inflation for year", next_year, ":", round(forecast_result$value, 2), "%\n")

# Plot historical and forecasted inflation
forecast_df <- data.frame(
  Year = c(data$Year, next_year),
  Inflation = c(data$Inflation, forecast_result$value),
  Type = c(rep("Historical", nrow(data)), "Forecast")
)

forecast_plot <- ggplot(forecast_df, aes(x = Year, y = Inflation, color = Type, group = 1)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_vline(xintercept = last_year, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c("Historical" = "blue", "Forecast" = "red")) +
  labs(title = "Pakistan Inflation: Historical and Forecast",
       subtitle = paste("Forecast for", next_year, "using", best_model),
       x = "Year", y = "Inflation Rate (%)") +
  theme(legend.position = "bottom")

print(forecast_plot)

#-------------------------------------------------------
# CONCLUSIONS AND RECOMMENDATIONS
#-------------------------------------------------------

cat("\n====== SUMMARY AND CONCLUSIONS ======\n")
cat("1. Analysis covered Pakistani inflation data from", min(data$Year), "to", max(data$Year), "\n")
cat("2. Best performing model:", best_model, "with Test MSE of", min(mse_table$MSE_Test), "\n")

# Give insights based on variable importance if applicable
if (best_model != "ARIMA") {
  # Get variable importance for the best model
  best_importance <- importance_df[, c("Variable", best_model)]
  best_importance <- best_importance[order(best_importance[, 2], decreasing = TRUE), ]
  
  cat("3. Key inflation drivers according to", best_model, "model:\n")
  for (i in 1:nrow(best_importance)) {
    if (!is.na(best_importance[i, 2])) {
      cat("   - ", best_importance$Variable[i], ": ", 
          round(best_importance[i, 2], 2), "%\n", sep = "")
    }
  }
}

cat("4. Inflation forecast for", next_year, ":", round(forecast_result$value, 2), "%\n")

# Save the results to a file
cat("\nSaving analysis results and plots...\n")

