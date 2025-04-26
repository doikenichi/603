
# full EDA begin

```{r}
# Comprehensive EDA for Used Car Price Prediction Dataset
# ======================================================




# Check for missing values
missing_values <- colSums(is.na(used_cars_data))
cat("\nMissing values per column:\n")
print(missing_values[missing_values > 0])

# Display first few rows
head(used_cars_data)

# 2. Data Cleaning and Preparation
# -------------------------------

# Make column names consistent
names(used_cars_data) <- gsub("\\.", "_", tolower(names(used_cars_data)))

# Check for duplicates
cat("\nNumber of duplicate rows:", sum(duplicated(used_cars_data)), "\n")

# Convert categorical variables to factors where appropriate
categorical_vars <- c("brand", "model", "fuel_type", "transmission_type")
used_cars_data[categorical_vars] <- lapply(used_cars_data[categorical_vars], as.factor)

# Convert date to proper format if it exists (assuming there's a date column)
if("listing_date" %in% names(used_cars_data)) {
  used_cars_data$listing_date <- as.Date(used_cars_data$listing_date)
}

# 3. Univariate Analysis
# ---------------------

# 3.1 Price Distribution (Target Variable)
ggplot(used_cars_data, aes(x = price)) +
  geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Distribution of Car Prices",
       x = "Price ($)",
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Log-transformed price for better visualization
ggplot(used_cars_data, aes(x = log(price))) +
  geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
  labs(title = "Distribution of Log-Transformed Car Prices",
       x = "Log(Price)",
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# 3.2 Numerical Variables Analysis
numerical_vars <- c("year", "mileage", "engine_capacity", "price")

# Function to create histograms for numerical variables
plot_histogram <- function(data, var_name) {
  ggplot(data, aes_string(x = var_name)) +
    geom_histogram(bins = 30, fill = "cornflowerblue", alpha = 0.7) +
    labs(title = paste("Distribution of", var_name),
         x = var_name,
         y = "Count") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
}

# Apply the function to each numerical variable
num_plots <- lapply(numerical_vars[numerical_vars != "price"], function(var) {
  plot_histogram(used_cars_data, var)
})

# Display plots in a grid
grid.arrange(grobs = num_plots, ncol = 2)

# 3.3 Categorical Variables Analysis
# Create bar plots for categorical variables
cat_plots <- lapply(categorical_vars, function(var) {
  # Get top N categories if there are too many
  if(length(unique(used_cars_data[[var]])) > 15) {
    top_cats <- names(sort(table(used_cars_data[[var]]), decreasing = TRUE)[1:15])
    plot_data <- used_cars_data %>%
      mutate(temp_var = ifelse(used_cars_data[[var]] %in% top_cats, as.character(used_cars_data[[var]]), "Other")) %>%
      count(temp_var) %>%
      rename(!!var := temp_var)
  } else {
    plot_data <- used_cars_data %>% count(!!sym(var))
  }
  
  ggplot(plot_data, aes_string(x = var, y = "n")) +
    geom_bar(stat = "identity", fill = "seagreen", alpha = 0.7) +
    labs(title = paste("Count of Cars by", var),
         x = var,
         y = "Count") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1))
})

# Display plots in a grid
grid.arrange(grobs = cat_plots, ncol = 2)

# 4. Bivariate Analysis
# --------------------

# 4.1 Relationship between numerical variables and price
num_price_plots <- lapply(numerical_vars[numerical_vars != "price"], function(var) {
  ggplot(used_cars_data, aes_string(x = var, y = "price")) +
    geom_point(alpha = 0.3, color = "blue") +
    geom_smooth(method = "loess", se = TRUE, color = "red") +
    scale_y_continuous(labels = scales::comma) +
    labs(title = paste(var, "vs Price"),
         x = var,
         y = "Price ($)") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
})

# Display plots in a grid
grid.arrange(grobs = num_price_plots, ncol = 2)

# 4.2 Box plots of price by categorical variables
cat_price_plots <- lapply(categorical_vars, function(var) {
  # If too many categories, just show top ones
  if(length(unique(used_cars_data[[var]])) > 10) {
    top_cats <- names(sort(table(used_cars_data[[var]]), decreasing = TRUE)[1:10])
    plot_data <- used_cars_data %>% filter(used_cars_data[[var]] %in% top_cats)
  } else {
    plot_data <- used_cars_data
  }
  
  ggplot(plot_data, aes_string(x = var, y = "price")) +
    geom_boxplot(fill = "orange", alpha = 0.7) +
    scale_y_continuous(labels = scales::comma) +
    labs(title = paste("Price by", var),
         x = var,
         y = "Price ($)") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1))
})

# Display plots in a grid
grid.arrange(grobs = cat_price_plots, ncol = 2)

# 5. Multivariate Analysis
# ----------------------

# 5.1 Correlation Matrix of Numerical Variables
corr_vars <- used_cars_data %>% 
  select_if(is.numeric) %>%
  select(-any_of(c("id", "row_number")))  # Remove ID columns if they exist

correlation_matrix <- cor(corr_vars, use = "complete.obs")
corrplot(correlation_matrix, method = "circle", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black", 
         col = colorRampPalette(c("#6D9EC1", "white", "#E46726"))(200),
         title = "Correlation Matrix", mar = c(0, 0, 1, 0))

# 5.2 Scatter plot matrix
pairs(corr_vars, pch = 19, cex = 0.5,
      main = "Scatter Plot Matrix of Numerical Variables")

# 5.3 Price vs Year by Brand (Top Brands)
top_brands <- names(sort(table(used_cars_data$brand), decreasing = TRUE)[1:6])
brand_filtered_data <- used_cars_data %>% filter(brand %in% top_brands)

ggplot(brand_filtered_data, aes(x = year, y = price, color = brand)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Price vs Year by Brand",
       x = "Year",
       y = "Price ($)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# 5.4 Price vs Mileage by Transmission Type
ggplot(used_cars_data, aes(x = mileage, y = price, color = transmission_type)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Price vs Mileage by Transmission Type",
       x = "Mileage",
       y = "Price ($)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# 6. Advanced Analysis
# -------------------

# 6.1 Price per Year of Age
used_cars_data$age <- as.numeric(format(Sys.Date(), "%Y")) - used_cars_data$year
used_cars_data$price_per_year <- used_cars_data$price / used_cars_data$age

ggplot(used_cars_data, aes(x = age, y = price_per_year)) +
  geom_point(alpha = 0.3, aes(color = brand)) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Price per Year of Age vs Age",
       x = "Age (years)",
       y = "Price per Year of Age ($)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

# 6.2 Price vs Engine Capacity by Fuel Type
ggplot(used_cars_data, aes(x = engine_capacity, y = price, color = fuel_type)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Price vs Engine Capacity by Fuel Type",
       x = "Engine Capacity",
       y = "Price ($)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# 6.3 Price Distribution by Car Age Group
used_cars_data$age_group <- cut(used_cars_data$age, 
                          breaks = c(0, 3, 5, 10, 15, 100),
                          labels = c("0-3 years", "4-5 years", "6-10 years", "11-15 years", "16+ years"))

ggplot(used_cars_data, aes(x = age_group, y = price)) +
  geom_boxplot(fill = "skyblue", alpha = 0.7) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Price Distribution by Car Age Group",
       x = "Age Group",
       y = "Price ($)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# 7. Summary Statistics and Insights
# ---------------------------------

# 7.1 Summary statistics by brand (top brands)
brand_summary <- used_cars_data %>%
  filter(brand %in% top_brands) %>%
  group_by(brand) %>%
  summarize(
    Count = n(),
    Avg_Price = mean(price, na.rm = TRUE),
    Median_Price = median(price, na.rm = TRUE),
    Min_Price = min(price, na.rm = TRUE),
    Max_Price = max(price, na.rm = TRUE),
    Avg_Year = mean(year, na.rm = TRUE),
    Avg_Mileage = mean(mileage, na.rm = TRUE)
  ) %>%
  arrange(desc(Count))

kable(brand_summary, caption = "Summary Statistics by Brand",
      format.args = list(big.mark = ","))

# 7.2 Summary by fuel type
fuel_summary <- used_cars_data %>%
  group_by(fuel_type) %>%
  summarize(
    Count = n(),
    Avg_Price = mean(price, na.rm = TRUE),
    Median_Price = median(price, na.rm = TRUE),
    Avg_Engine_Size = mean(engine_capacity, na.rm = TRUE),
    Avg_Year = mean(year, na.rm = TRUE)
  ) %>%
  arrange(desc(Count))

kable(fuel_summary, caption = "Summary Statistics by Fuel Type",
      format.args = list(big.mark = ","))

# 7.3 Most expensive models (top 10)
top_models <- used_cars_data %>%
  group_by(brand, model) %>%
  summarize(
    Count = n(),
    Avg_Price = mean(price, na.rm = TRUE)
  ) %>%
  filter(Count >= 5) %>%  # Only consider models with at least 5 cars
  arrange(desc(Avg_Price)) %>%
  head(10)

kable(top_models, caption = "Top 10 Most Expensive Car Models",
      format.args = list(big.mark = ","))

# 8. Final Visualization - Price Trends
# ------------------------------------

# Price trend over the years
year_trend <- used_cars_data %>%
  group_by(year) %>%
  summarize(
    Avg_Price = mean(price, na.rm = TRUE),
    Count = n()
  ) %>%
  filter(Count > 5)  # Only consider years with enough data points

ggplot(year_trend, aes(x = year, y = Avg_Price)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Average Car Price Trend by Manufacturing Year",
       x = "Manufacturing Year",
       y = "Average Price ($)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Print session information for reproducibility
sessionInfo()
```

# full EDA end







# model building start
```{r}
# ================================================================
# Comprehensive Multiple Linear Regression Model for Used Car Prices
# ================================================================

# Load necessary libraries
library(tidyverse)      # Data manipulation and visualization
library(car)            # For VIF and regression diagnostics
library(olsrr)          # For stepwise regression and model selection
library(leaps)          # For regsubsets
library(MASS)           # For stepAIC
library(lmtest)         # For hypothesis tests
library(nortest)        # For normality tests
library(ggplot2)        # For visualizations
library(gridExtra)      # For arranging multiple plots
library(corrplot)       # For correlation plots

# 1. Load and Prepare the Data
# ---------------------------
# Set your working directory to where the file is located or provide the full path
used_cars_data <- read.csv("used_car_price_prediction_dataset.csv")

# Data preprocessing
# Make column names consistent
names(used_cars_data) <- gsub("\\.", "_", tolower(names(used_cars_data)))

# Convert categorical variables to factors
categorical_vars <- c("brand", "model", "fuel_type", "transmission_type")
used_cars_data[categorical_vars] <- lapply(used_cars_data[categorical_vars], as.factor)

# Check for missing values and handle them
missing_values <- colSums(is.na(used_cars_data))
print(missing_values)

# If there are missing values, impute or remove them
if(sum(missing_values) > 0) {
  used_cars_data <- na.omit(used_cars_data)  # Remove rows with any missing values
  # Alternatively, you could impute missing values with mean/median for numerical variables
}

# Create new features that might be useful
used_cars_data$age <- max(used_cars_data$year) - used_cars_data$year + 1
used_cars_data$miles_per_year <- used_cars_data$mileage / used_cars_data$age

# Create log-transformed price (often more normally distributed)
used_cars_data$log_price <- log(used_cars_data$price)

# 2. Explore Relationships and Check Multicollinearity
# --------------------------------------------------

# 2.1 Correlation Analysis for Numerical Variables
numeric_vars <- c("year", "mileage", "engine_capacity", "price", "age", "miles_per_year")
cor_matrix <- cor(used_cars_data[numeric_vars], use = "complete.obs")
print(cor_matrix)

# Visualize the correlation matrix
corrplot(cor_matrix, method = "circle", type = "upper", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black", 
         col = colorRampPalette(c("#6D9EC1", "white", "#E46726"))(200),
         title = "Correlation Matrix of Numerical Variables")

# 2.2 Explore relationships with scatterplots
pairs(used_cars_data[c("price", "year", "mileage", "engine_capacity", "age")], 
      main = "Scatterplot Matrix of Key Variables")

# 3. Initial Model Building
# -----------------------

# Build a full model with all main effects
# For categorical variables with many levels, we might need to be selective
# Limit categorical variables to top levels

# Get top brands (e.g., top 10)
brand_counts <- table(used_cars_data$brand)
top_brands <- names(sort(brand_counts, decreasing = TRUE)[1:10])
used_cars_data$brand_simplified <- ifelse(used_cars_data$brand %in% top_brands, 
                                   as.character(used_cars_data$brand), "Other")
used_cars_data$brand_simplified <- as.factor(used_cars_data$brand_simplified)

# Get top models (e.g., top 20)
model_counts <- table(used_cars_data$model)
top_models <- names(sort(model_counts, decreasing = TRUE)[1:20])
used_cars_data$model_simplified <- ifelse(used_cars_data$model %in% top_models, 
                                   as.character(used_cars_data$model), "Other")
used_cars_data$model_simplified <- as.factor(used_cars_data$model_simplified)

# 3.1 Build initial model
initial_model <- lm(price ~ year + mileage + engine_capacity + 
                   brand_simplified + fuel_type + transmission_type, 
                 data = used_cars_data)

summary(initial_model)

# 3.2 Check for multicollinearity
vif_values <- vif(initial_model)
print(vif_values)

# 4. Model Selection Using Different Approaches
# -------------------------------------------

# 4.1 Subset Selection using regsubsets
# This identifies the best subset of predictors for different model sizes

# Setting a smaller limit for the number of variables to avoid computational issues
max_vars <- min(25, ncol(used_cars_data) - 1)

# Run regsubsets on numerical and simplified categorical variables
reg_subset_model <- regsubsets(price ~ year + mileage + engine_capacity + 
                              brand_simplified + fuel_type + transmission_type,
                            data = used_cars_data, 
                            nvmax = max_vars, 
                            method = "exhaustive")

# Display results of regsubsets
reg_subset_summary <- summary(reg_subset_model)
print(reg_subset_summary)

# Visualize the results
par(mfrow = c(2, 2))
plot(reg_subset_summary$rss, xlab = "Number of Variables", ylab = "RSS", 
     main = "RSS vs. Number of Variables")
plot(reg_subset_summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted R^2", 
     main = "Adjusted R^2 vs. Number of Variables")
plot(reg_subset_summary$cp, xlab = "Number of Variables", ylab = "Cp", 
     main = "Cp vs. Number of Variables")
plot(reg_subset_summary$bic, xlab = "Number of Variables", ylab = "BIC", 
     main = "BIC vs. Number of Variables")
par(mfrow = c(1, 1))

# Find the model with the best adjusted R-squared
best_adjr2_idx <- which.max(reg_subset_summary$adjr2)
best_vars_adjr2 <- names(coef(reg_subset_model, best_adjr2_idx))
cat("Best model based on adjusted R-squared includes variables:", 
    paste(best_vars_adjr2, collapse = ", "), "\n")

# Find the model with the lowest BIC
best_bic_idx <- which.min(reg_subset_summary$bic)
best_vars_bic <- names(coef(reg_subset_model, best_bic_idx))
cat("Best model based on BIC includes variables:", 
    paste(best_vars_bic, collapse = ", "), "\n")

# 4.2 Using olsrr package for comprehensive model selection

# First, create a model with manageable number of predictors
select_model <- lm(price ~ year + mileage + engine_capacity + 
                  brand_simplified + fuel_type + transmission_type, 
                data = used_cars_data)

# Use ols_step_best_subset for comprehensive model metrics
best_subset <- ols_step_best_subset(select_model)
print(best_subset)
plot(best_subset)

# 5. Building Models with Higher-Order Terms and Interactions
# ---------------------------------------------------------

# 5.1 Adding polynomial terms
poly_model <- lm(price ~ poly(year, 2) + poly(mileage, 2) + poly(engine_capacity, 2) + 
                brand_simplified + fuel_type + transmission_type, 
               data = used_cars_data)
summary(poly_model)

# 5.2 Adding interaction terms
# Focus on potentially meaningful interactions (limit to avoid overfitting)
interaction_model <- lm(price ~ year + mileage + engine_capacity + 
                       brand_simplified + fuel_type + transmission_type +
                       year:mileage + year:engine_capacity + 
                       transmission_type:engine_capacity + 
                       fuel_type:engine_capacity,
                     data = used_cars_data)
summary(interaction_model)

# 5.3 Build a comprehensive model with selected higher-order terms and interactions
comprehensive_model <- lm(price ~ poly(year, 2) + poly(mileage, 2) + 
                         poly(engine_capacity, 2) + brand_simplified + 
                         fuel_type + transmission_type +
                         year:mileage + year:engine_capacity + 
                         transmission_type:engine_capacity,
                       data = used_cars_data)
summary(comprehensive_model)

# 6. Model Comparison and Selection Using Statistical Tests
# -------------------------------------------------------

# 6.1 Compare models using ANOVA
anova_comparison <- anova(initial_model, poly_model, interaction_model, comprehensive_model)
print(anova_comparison)

# 6.2 Compare using AIC and BIC
models_list <- list(
  "Initial" = initial_model,
  "Polynomial" = poly_model,
  "Interaction" = interaction_model,
  "Comprehensive" = comprehensive_model
)

models_comparison <- data.frame(
  Model = names(models_list),
  AIC = sapply(models_list, AIC),
  BIC = sapply(models_list, BIC),
  Adj_R_squared = sapply(models_list, function(x) summary(x)$adj.r.squared)
)
print(models_comparison)

# Select the best model based on criteria
best_model_idx <- which.min(models_comparison$AIC)
best_model_name <- models_comparison$Model[best_model_idx]
cat("The best model based on AIC is:", best_model_name, "\n")

# Assign the best model for further analysis
best_model <- models_list[[best_model_name]]

# 7. Diagnostic Tests and Assumption Validation
# -------------------------------------------
# We'll validate all linear regression assumptions on the best model

# 7.1 Linearity Assumption
# Create residual vs. fitted values plot
residual_plot <- ggplot(data.frame(fitted = fitted(best_model), 
                                  residuals = residuals(best_model)),
                       aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(title = "Residuals vs Fitted Values",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()

# Component + Residual Plots (partial residual plots)
crPlots(best_model, terms = ~ year + mileage + engine_capacity)

# 7.2 Independence Assumption
# Durbin-Watson test for autocorrelation
dw_test <- dwtest(best_model)
print(dw_test)

# 7.3 Equal Variance Assumption (Homoscedasticity)
# Breusch-Pagan test
bp_test <- bptest(best_model)
print(bp_test)

# Scale-Location plot
scale_location_plot <- ggplot(data.frame(fitted = fitted(best_model), 
                                        std_residuals = sqrt(abs(rstandard(best_model)))),
                             aes(x = fitted, y = std_residuals)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Scale-Location Plot",
       x = "Fitted Values",
       y = "√|Standardized Residuals|") +
  theme_minimal()

# 7.4 Normality Assumption
# Shapiro-Wilk test for normality
shapiro_test <- shapiro.test(residuals(best_model))
print(shapiro_test)

# Alternative tests for larger datasets if Shapiro-Wilk fails
ad_test <- ad.test(residuals(best_model))
print(ad_test)

# QQ Plot
qq_plot <- ggplot(data.frame(std_resid = rstandard(best_model)),
                 aes(sample = std_resid)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Normal Q-Q Plot",
       x = "Theoretical Quantiles",
       y = "Standardized Residuals") +
  theme_minimal()

# Histogram of residuals
hist_plot <- ggplot(data.frame(residuals = residuals(best_model)),
                   aes(x = residuals)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Residuals",
       x = "Residuals",
       y = "Frequency") +
  theme_minimal()

# Display diagnostic plots
grid.arrange(residual_plot, scale_location_plot, qq_plot, hist_plot, ncol = 2)

# 7.5 Multicollinearity Check
vif_best <- vif(best_model)
print(vif_best)

# 7.6 Outlier Detection and Influence Analysis
# Cook's distance
cooksd <- cooks.distance(best_model)
plot(cooksd, pch = 20, main = "Cook's Distance Plot")
abline(h = 4/(nrow(used_cars_data)-length(coef(best_model))), col = "red", lty = 2)

# Influential observations
influential_obs <- which(cooksd > 4/(nrow(used_cars_data)-length(coef(best_model))))
cat("Number of influential observations:", length(influential_obs), "\n")

# DFBETAS
dfbetas_plot <- dfbetas(best_model)
# Plot DFBETAS for coefficients of interest (e.g., first few)
par(mfrow = c(2, 2))
for(i in 2:min(5, ncol(dfbetas_plot))) {
  plot(dfbetas_plot[, i], main = paste("DFBETAS for", names(coef(best_model))[i]))
  abline(h = 0, col = "red", lty = 2)
  abline(h = 2/sqrt(nrow(used_cars_data)), col = "blue", lty = 2)
  abline(h = -2/sqrt(nrow(used_cars_data)), col = "blue", lty = 2)
}
par(mfrow = c(1, 1))

# 8. Final Model After Addressing Issues
# ------------------------------------

# If there are issues with the best model, address them here
# For example, if outliers are a problem:
if(length(influential_obs) > 0 && length(influential_obs) < nrow(used_cars_data) * 0.05) {
  # Remove influential observations if they're less than 5% of the data
  used_cars_data_cleaned <- used_cars_data[-influential_obs, ]
  
  # Refit the model
  final_model <- update(best_model, data = used_cars_data_cleaned)
  
  # Compare original and cleaned models
  models_comparison_final <- data.frame(
    Model = c("Best Model", "Final Model (Outliers Removed)"),
    AIC = c(AIC(best_model), AIC(final_model)),
    Adj_R_squared = c(summary(best_model)$adj.r.squared, summary(final_model)$adj.r.squared)
  )
  print(models_comparison_final)
  
  # If the cleaned model is better, use it as final
  if(models_comparison_final$AIC[2] < models_comparison_final$AIC[1]) {
    best_model <- final_model
    cat("Final model has been updated after removing outliers\n")
  }
}

# 9. Model Performance and Validation
# ---------------------------------

# 9.1 Final model summary
final_summary <- summary(best_model)
print(final_summary)

# 9.2 Print significant coefficients
significant_coefs <- coef(summary(best_model))[coef(summary(best_model))[,4] < 0.05, ]
cat("\nSignificant coefficients (p < 0.05):\n")
print(significant_coefs)

# 9.3 Validate model on a test set (Cross-validation)
# Create a train/test split
set.seed(123)
train_indices <- sample(1:nrow(used_cars_data), 0.7 * nrow(used_cars_data))
train_data <- used_cars_data[train_indices, ]
test_data <- used_cars_data[-train_indices, ]

# Fit the model on training data
train_model <- update(best_model, data = train_data)

# Predict on test data
test_predictions <- predict(train_model, newdata = test_data)

# Calculate performance metrics
test_rmse <- sqrt(mean((test_data$price - test_predictions)^2))
test_mae <- mean(abs(test_data$price - test_predictions))
test_mape <- mean(abs((test_data$price - test_predictions) / test_data$price)) * 100
test_r2 <- 1 - sum((test_data$price - test_predictions)^2) / 
           sum((test_data$price - mean(test_data$price))^2)

validation_metrics <- data.frame(
  Metric = c("RMSE", "MAE", "MAPE (%)", "R-squared"),
  Value = c(test_rmse, test_mae, test_mape, test_r2)
)
print(validation_metrics)

# 9.4. Visualize actual vs. predicted
ggplot(data.frame(actual = test_data$price, predicted = test_predictions),
      aes(x = actual, y = predicted)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Actual vs. Predicted Prices",
       x = "Actual Price",
       y = "Predicted Price") +
  theme_minimal()

# 10. Final Model Equation and Interpretation
# -----------------------------------------

# Extract the equation of the final model
model_terms <- attr(terms(best_model), "term.labels")
model_coefs <- coef(best_model)

cat("\nFinal Model Equation:\n")
cat("price =", round(model_coefs[1], 2))
for(i in 2:length(model_coefs)) {
  if(!is.na(model_coefs[i])) {
    cat(" +", round(model_coefs[i], 2), "*", names(model_coefs)[i])
  }
}
cat("\n")

# Interpretation of key variables
cat("\nInterpretation of Key Variables:\n")
for(var in c("year", "mileage", "engine_capacity")) {
  var_idx <- grep(var, names(model_coefs))
  if(length(var_idx) > 0) {
    cat("- A one unit increase in", var, "is associated with a change of", 
        round(model_coefs[var_idx[1]], 2), "in the predicted price, holding other variables constant.\n")
  }
}

# 11. Save the final model
# ----------------------
saveRDS(best_model, "best_car_price_prediction_model.rds")

# That's it! You now have a comprehensive, well-validated linear regression model for car price prediction.
```

# model building end

















```{r}
# Preview the data
head(data)
```


```{r}
# Describe the data
summary(data)
```

```{r}
#structure
str(data)
```

```{r}
# Count NA values
na_counts <- colSums(is.na(data))
na_counts
```

```{r}
# Count NA values
sapply(data, function(x) sum(!is.finite(x)))  # counts Inf/NaN
```

```{r}
# Remove rows with any NA values
clean_data <- na.omit(data)
cat("\nRemoved rows with NA values. New dimensions:\n")
print(dim(clean_data))
```

```{r}
# Detecting Outliers using IQR for numeric columns
numeric_cols <- sapply(clean_data, is.numeric)

outliers <- list()
for (col in names(clean_data)[numeric_cols]) {
  Q1 <- quantile(clean_data[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(clean_data[[col]], 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR_val
  upper <- Q3 + 1.5 * IQR_val
  outlier_rows <- which(clean_data[[col]] < lower | clean_data[[col]] > upper)
  outliers[[col]] <- outlier_rows
  cat(sprintf("\nOutliers in %s: %d found\n", col, length(outlier_rows)))
}
```

```{r}
# Optional: Remove rows with outliers (union of all outlier indices)
all_outlier_indices <- unique(unlist(outliers))
data_no_outliers <- clean_data[-all_outlier_indices, ]
```

```{r}
# Final Cleaned Dataset (No NAs, No Outliers) 
dim(data_no_outliers)
```


```{r}
full <- lm(price~factor(brand)+factor(model)+factor(model_year)+milage+factor(fuel_type)+factor(engine)+factor(transmission)+factor(ext_col)+factor(int_col)+factor(accident)+factor(clean_title), data = data_no_outliers)
summary(full)
```

