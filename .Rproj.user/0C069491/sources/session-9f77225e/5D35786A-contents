# Load required libraries
library(readxl)
library(dplyr)
library(neuralnet)
library(Metrics)
library(ggplot2)

# Load the dataset
exchange_data <- read_excel("ExchangeUSD.xlsx")
colnames(exchange_data) <- c("Date", "DayOfWeek", "ExchangeRate")

# Normalize function
normalize <- function(x) {
  x_min <- min(x, na.rm = TRUE)
  x_max <- max(x, na.rm = TRUE)
  (x - x_min) / (x_max - x_min)
}

# Unnormalize function
unnormalize <- function(x, x_min, x_max) {
  x * (x_max - x_min) + x_min
}

original_data <- exchange_data$ExchangeRate
normalized_data <- normalize(exchange_data$ExchangeRate)

summary(original_data)
summary(normalized_data)

# Create a data frame for plotting
plot_data <- data.frame(
  Value = c(original_data, normalized_data),
  Status = rep(c("Original", "Normalized"), each = length(original_data))
)

# Generate box plots
ggplot(plot_data, aes(x = Status, y = Value, fill = Status)) +
  geom_boxplot() +
  labs(title = "Comparison of Exchange Rates Before and After Normalization",
       x = "Data Status", y = "Exchange Rate Values") +
  theme_minimal()

# Function to prepare time-delayed data
prepare_lagged_data <- function(data, max_lag, start_train, end_train) {
  lags <- setNames(lapply(1:max_lag, function(i) lag(data$ExchangeRate, i)), paste0("INPUT.", max_lag:1))
  data_lagged <- cbind(data, lags)
  data_lagged <- data_lagged[complete.cases(data_lagged),]
  data_lagged$OUTPUT <- data_lagged$ExchangeRate
  
  # Split data into training and testing
  train_data <- data_lagged[start_train:end_train, ]
  test_data <- data_lagged[(end_train+1):nrow(data_lagged), ]
  
  # Normalize data
  train_data_normalized <- as.data.frame(lapply(train_data[, -(1:3)], normalize))
  test_data_normalized <- as.data.frame(lapply(test_data[, -(1:3)], normalize))
  
  list(train=train_data_normalized, test=test_data_normalized, train_raw=train_data, test_raw=test_data)
}

# Train neural network model and generate plot
train_nn_and_plot <- function(train_data, hidden_layers, model_name) {
  formula <- as.formula(paste("OUTPUT ~", paste(names(train_data)[-ncol(train_data)], collapse = "+")))
  nn_model <- neuralnet(formula, data = train_data, hidden = hidden_layers, linear.output = FALSE)
  # Plot the neural network
  plot(nn_model, main = model_name)
  return(nn_model)
}

# Test neural network model
test_nn <- function(nn_model, test_data, test_raw) {
  test_predictions <- compute(nn_model, test_data[-ncol(test_data)])
  predictions <- unnormalize(test_predictions$net.result, min(test_raw$OUTPUT), max(test_raw$OUTPUT))
  
  results <- data.frame(ACTUAL = test_raw$OUTPUT, PREDICTED = round(predictions, digits = 1))
  return(results)
}

# Evaluate model
evaluate_model <- function(results) {
  RMSE <- rmse(results$ACTUAL, results$PREDICTED)
  MAE <- mae(results$ACTUAL, results$PREDICTED)
  MAPE <- mape(results$ACTUAL, results$PREDICTED) * 100
  sMAPE <- smape(results$ACTUAL, results$PREDICTED) * 100
  deviation <- (results$ACTUAL - results$PREDICTED) / results$ACTUAL
  accuracy <- 1 - abs(mean(deviation))
  
  data.frame(RMSE, MAE, MAPE, sMAPE, Accuracy = accuracy * 100)  # Converted to percentage
}

# Create models with different configurations
model_results <- list()
configurations <- list(
  list(lag=4, hidden=c(5)),
  list(lag=3, hidden=c(5)),
  list(lag=2, hidden=c(5)),
  list(lag=1, hidden=c(5)),
  list(lag=4, hidden=c(10,7)),
  list(lag=4, hidden=c(7)),
  list(lag=4, hidden=c(8,5)),
  list(lag=5, hidden=c(6)),
  list(lag=5, hidden=c(5,2)),
  list(lag=5, hidden=c(7,6)),
  list(lag=6, hidden=c(7)),
  list(lag=6, hidden=c(5,8)),
  list(lag=6, hidden=c(4,6)),
  list(lag=7, hidden=c(8)),
  list(lag=7, hidden=c(7, 5))
)

# Initialize a list to store plots
plots <- list()

for (config in configurations) {
  data_prepared <- prepare_lagged_data(exchange_data, config$lag, 1, 400)
  model_name <- paste("Lag", config$lag, "Hidden", paste(config$hidden, collapse="-"))
  nn_model <- train_nn_and_plot(data_prepared$train, config$hidden, model_name)
  results <- test_nn(nn_model, data_prepared$test, data_prepared$test_raw)
  performance <- evaluate_model(results)
  model_results[[model_name]] <- performance
  
  # Print formatted metrics
  cat(sprintf("\nModel: %s\n", model_name))
  cat(sprintf("RMSE: %.4f\n", performance$RMSE))
  cat(sprintf("MAE: %.4f\n", performance$MAE))
  cat(sprintf("MAPE: %.4f%%\n", performance$MAPE))
  cat(sprintf("sMAPE: %.4f%%\n", performance$sMAPE))
  cat(sprintf("Accuracy: %.4f%%\n", performance$Accuracy))
  
  # Plot actual vs predicted values
  plot_title <- paste("Actual vs Predicted: Lag", config$lag, "Hidden", paste(config$hidden, collapse="-"))
  actual_vs_predicted_plot <- ggplot(results, aes(x = ACTUAL, y = PREDICTED)) +
    geom_point() +
    geom_smooth(method = "lm", color = "red", se = FALSE) +
    labs(title = plot_title, x = "Actual Exchange Rate", y = "Predicted Exchange Rate")
  plots[[model_name]] <- actual_vs_predicted_plot
}

# Output all plots to a PDF file
pdf("neural_network_model_plots.pdf", width = 8, height = 6)
for (plot_name in names(plots)) {
  print(plots[[plot_name]])
}
dev.off()

# Collect results into a dataframe for comparison
comparison_table <- do.call(rbind, model_results)
comparison_table$model <- rownames(comparison_table)
rownames(comparison_table) <- NULL

# Print the comparison table
print(comparison_table)
