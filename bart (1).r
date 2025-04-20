# Install necessary packages if not already installed
required_packages <- c(
  "data.table", "dplyr", "ggplot2", "mgcv", "dbarts", "xgboost", "stringr", "pdp", 
  "tidyr", "viridis"
)

# Install missing packages
missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(missing_packages) > 0) {
  install.packages(missing_packages, dependencies = TRUE)
}

# Load the required libraries
lapply(required_packages, library, character.only = TRUE)

# Special handling for the `pdp` package
if (!require(pdp)) {
  install.packages("pdp", dependencies = TRUE)
  library(pdp)
}
# Load necessary libraries
library(dplyr)
library(stringr)

# Load the dataset
file_path <- "/cloud/project/cleaned_for_bart.csv" # Update the path to your CSV file
car_data <- read.csv(file_path, stringsAsFactors = FALSE)

# Convert string columns to factors
factor_columns <- c("Brand", "Model", "Fuel_Type", "Transmission", "Region", 
                    "doors_category", "transmission_fuel_combo", "brand_x_region")
car_data[factor_columns] <- lapply(car_data[factor_columns], as.factor)

# Cast numeric columns and identify NAs
numeric_columns <- c("Year", "Engine_Size", "Mileage", "Doors", "Owner_Count", "Price",
                     "car_age", "mileage_per_year", "high_mileage_indicator", 
                     "age_mileage_interaction", "mileage_per_engine_size", 
                     "owner_impact_score", "luxury_brand_indicator", 
                     "engine_size_per_door", "fuel_type_efficiency_score", 
                     "brand_mean_price_ratio", "model_popularity_score", 
                     "rare_model_indicator", "log_price", "log_mileage", 
                     "age_x_mileage", "owner_stability_score", 
                     "transmission_popularity", "fuel_efficiency_per_mile", 
                     "estimated_original_price", "depreciation_rate", 
                     "region_price_deviation", "mileage_variance", 
                     "Fuel_Type_encoded", "Transmission_encoded", 
                     "Region_encoded", "doors_category_encoded", 
                     "transmission_fuel_combo_encoded", "brand_x_region_encoded", 
                     "cpi_year", "inflation_adjustment", 
                     "price_inflation_adjusted", "original_price_inflation_adjusted")
car_data[numeric_columns] <- lapply(car_data[numeric_columns], as.numeric)

# Add derived columns
current_year <- 2024
car_data <- car_data %>%
  mutate(
    car_age = current_year - Year,  # Calculate car age
    mileage_per_year = ifelse(car_age > 0, Mileage / car_age, NA),  # Avoid division by zero
    high_mileage_indicator = ifelse(Mileage > quantile(Mileage, 0.75, na.rm = TRUE), 1, 0),
    age_mileage_interaction = car_age * Mileage,
    mileage_per_engine_size = Mileage / Engine_Size,
    engine_size_per_door = Engine_Size / Doors,
    log_price = log(Price),
    log_mileage = log(Mileage),
    luxury_brand_indicator = ifelse(Brand %in% c("Mercedes", "Audi", "BMW"), 1, 0),
    brand_x_region = paste(Brand, Region, sep = "-"),
    owner_impact_score = Owner_Count / max(Owner_Count, na.rm = TRUE),
    price_inflation_adjusted = Price / inflation_adjustment
  )

# Outlier detection and handling
# Flag extreme outliers in Mileage, Engine_Size, and Price
car_data <- car_data %>%
  mutate(
    outlier_flag = ifelse(Mileage > 500000 | Engine_Size < 0.6 | Engine_Size > 7 |
                            Price < 200 | Price > 200000, 1, 0)
  )

# Filter out outliers or keep track of them
car_data_cleaned <- car_data %>% filter(outlier_flag == 0)

# Save the cleaned and enriched dataset

# Ensure directory exists
if (!dir.exists("data/processed")) {
  dir.create("data/processed", recursive = TRUE)
}

# Save the cleaned and enriched dataset
write.csv(car_data_cleaned, "data/processed/enriched_car_data.csv", row.names = FALSE)

print("✅ Preprocessing complete. Cleaned dataset saved to: data/processed/enriched_car_data.csv")

summary(car_data_cleaned)
# Core modeling and plotting libraries
# Load Required Libraries
# Install required packages
install.packages(c("data.table", "dplyr", "ggplot2", "mgcv", "dbarts", "xgboost", "stringr", "pdp"), dependencies = TRUE)

# Load the required libraries
library(data.table)
library(dplyr)
library(ggplot2)
library(mgcv)
library(dbarts)
library(xgboost)
library(pdp)

# Check and install `pdp` package if it’s not installed
if (!require(pdp)) install.packages("pdp", dependencies = TRUE)
library(pdp)

# Load Data
file_path <- "/cloud/project/data/processed/enriched_car_data.csv"
if (!file.exists(file_path)) stop("Data file not found at the specified path!")
df <- fread(file_path)

# 1️⃣ CAST & SANITIZE COLUMNS ---------------------------------

# Corrected CAST & SANITIZE COLUMNS ---------------------------------

# Define factor (categorical) variables
factor_vars <- c("Brand", "Model", "Fuel_Type", "Transmission", "Region", 
                 "doors_category", "transmission_fuel_combo", "brand_x_region")
df[, (factor_vars) := lapply(.SD, as.factor), .SDcols = factor_vars]

# Convert remaining character columns to numeric (auto-validation)
df <- df %>% mutate(across(setdiff(names(df), factor_vars), ~ as.numeric(.), .names = "converted_{.col}"))
# 2️⃣ DERIVED FIELDS CHECK ------------------------------------

# Recreate/verify key derived variables
df <- df %>%
  mutate(
    car_age = pmax(2024 - Year, 0),  # Ensure no negative values
    mileage_per_year = ifelse(car_age > 0, Mileage / car_age, NA),
    high_mileage_indicator = as.integer(Mileage > quantile(Mileage, 0.75, na.rm = TRUE)),
    age_mileage_interaction = Mileage * car_age,
    mileage_per_engine_size = ifelse(Engine_Size > 0, Mileage / Engine_Size, NA),
    engine_size_per_door = ifelse(Doors > 0, Engine_Size / Doors, NA),
    log_price = ifelse(price_inflation_adjusted > 0, log(price_inflation_adjusted), NA),
    log_mileage = ifelse(Mileage > 0, log(Mileage), NA),
    owner_impact_score = Owner_Count / max(Owner_Count, na.rm = TRUE),
    luxury_brand_indicator = as.integer(Brand %in% c("BMW", "Mercedes", "Audi")),
    transmission_fuel_combo = paste(Transmission, Fuel_Type, sep = "-"),
    brand_x_region = paste(Brand, Region, sep = "-")
  )

# Validation checks for derived fields
stopifnot(all(df$Engine_Size > 0, na.rm = TRUE))
stopifnot(all(df$car_age >= 0, na.rm = TRUE))
stopifnot(all(!is.na(df$log_price)))

# 3️⃣ HEDONIC MODEL: GAM ---------------------------------------

# GAM formula
gam_model <- gam(
  log_price ~ 
    s(car_age, bs = "cs") + 
    s(log_mileage, bs = "cs") + 
    s(mileage_per_year, bs = "cs") + 
    s(brand_mean_price_ratio, bs = "cs") + 
    s(depreciation_rate, bs = "cs") +
    luxury_brand_indicator + 
    high_mileage_indicator + 
    owner_impact_score + 
    engine_size_per_door + 
    Fuel_Type + 
    Transmission + 
    Region + 
    owner_stability_score + 
    transmission_popularity,
  data = df,
  method = "REML"
)

# Display GAM summary
cat("📊 GAM Summary:\n")
print(summary(gam_model))

# Residuals for ML layer
df$residual_hedonic <- residuals(gam_model)



# 4️⃣ RESIDUAL MODELING: BART + XGBOOST ------------------------

# Define ML input variables (verify these exist in the dataset)
ml_vars <- c("mileage_variance", "region_price_deviation", "owner_stability_score",
             "fuel_efficiency_per_mile", grep("_encoded$", names(df), value = TRUE))
ml_vars <- ml_vars[ml_vars %in% names(df)]  # ensure all exist

# Extract and normalize features for ML models
X <- df[, ..ml_vars]
X <- as.data.frame(scale(X))

# ✅ BART model (Bayesian Additive Regression Trees)
set.seed(42)
bart_model <- bart(x.train = X, y.train = df$residual_hedonic, keeptrees = TRUE)

# ⚠️ BART returns multiple posterior draws — take posterior mean per observation
bart_posterior_samples <- predict(bart_model, newdata = X)  # matrix: n_samples x n_obs
df$bart_resid <- colMeans(bart_posterior_samples)  # mean across posterior for each row

# ✅ XGBoost model
xgb_data <- xgb.DMatrix(data = as.matrix(X), label = df$residual_hedonic)
xgb_model <- xgboost(data = xgb_data, nrounds = 100, max_depth = 4, eta = 0.1, verbose = 0)
df$xgb_resid <- predict(xgb_model, newdata = as.matrix(X))

# 🔄 Combine: Bayesian model averaging of predictions
df$final_log_price_pred <- gam_model$fitted.values + 0.5 * df$bart_resid + 0.5 * df$xgb_resid
df$final_price_pred <- exp(df$final_log_price_pred)

# ✅ Display sample results
cat("✅ Final predictions added to the dataset.\n")
print(head(df[, .(final_log_price_pred, final_price_pred)]))

# ------------- Compute scaling parameters for ml_vars -------------
# (Place this section before your sensitivity analysis block)
X_original <- df[, ..ml_vars]                # select the ML input features from df
scaled_X <- scale(X_original)                 # scale them
center_vec <- attr(scaled_X, "scaled:center")   # save center parameters
scale_vec <- attr(scaled_X, "scaled:scale")     # save scale parameters

# ------------- 5️⃣ SENSITIVITY ANALYSIS --------------------------------------

# 1. Choose a representative row (median values of key variables + Year)
baseline <- df %>%
  select(Mileage, Engine_Size, Owner_Count, Year) %>%
  summarise(across(everything(), ~ median(.x, na.rm = TRUE))) %>%
  as_tibble()  # Ensure tibble so operations work correctly

# 2. Define percentage change range (±10% in 5% steps)
perturbation_levels <- seq(-0.1, 0.1, by = 0.05)

# 3. Generate perturbed scenarios for each variable with proper labeling
sensitivity_df <- bind_rows(lapply(c("Mileage", "Engine_Size", "Owner_Count"), function(var) {
  bind_rows(lapply(perturbation_levels, function(pct) {
    modified <- baseline                      # start with the baseline
    modified[[var]] <- modified[[var]] * (1 + pct)  # apply perturbation
    modified$var_perturbed <- var             # label the variable being perturbed
    modified$change <- pct                    # record the percentage change
    modified
  }))
}))

# 4. Summarise the remaining static columns (for both numeric and factor variables)
static_cols <- df %>%
  summarise(across(-c(Mileage, Engine_Size, Owner_Count, Year), 
                   ~ if (is.numeric(.x)) median(.x, na.rm = TRUE) 
                   else as.character(names(sort(table(.x), decreasing = TRUE))[1])
  )) %>%
  as_tibble() %>%
  head(1)  # Use head() instead of slice()

# 5. Combine static and perturbed data into one data frame
sensitivity_df_full <- bind_cols(
  sensitivity_df,
  static_cols[rep(1, nrow(sensitivity_df)), ]
)

# 6. Ensure factor levels in sensitivity_df_full match those in the original df
for (col in names(df)) {
  if (is.factor(df[[col]])) {
    sensitivity_df_full[[col]] <- factor(sensitivity_df_full[[col]], levels = levels(df[[col]]))
  }
}

# 7. Recalculate derived features using Year
sensitivity_df_full <- sensitivity_df_full %>%
  mutate(
    car_age = pmax(2024 - Year, 0),
    mileage_per_year = ifelse(car_age > 0, Mileage / car_age, NA),
    high_mileage_indicator = as.integer(Mileage > quantile(df$Mileage, 0.75, na.rm = TRUE)),
    age_mileage_interaction = Mileage * car_age,
    mileage_per_engine_size = Mileage / Engine_Size,
    engine_size_per_door = ifelse(Doors > 0, Engine_Size / Doors, NA),
    log_mileage = log(Mileage),
    owner_impact_score = Owner_Count / max(df$Owner_Count, na.rm = TRUE)
  )

# 8. Scale the ML input features for sensitivity analysis using the training scaler parameters.
X_sens <- sensitivity_df_full[, ml_vars, drop = FALSE]
X_sens_scaled <- scale(X_sens, center = center_vec, scale = scale_vec)
X_sens_scaled <- as.matrix(X_sens_scaled)  # Convert to matrix for prediction

# 9. Make predictions with the fitted models
gam_pred <- predict(gam_model, newdata = sensitivity_df_full)
bart_pred <- colMeans(predict(bart_model, newdata = X_sens_scaled))  # Averaging across posterior draws
xgb_pred <- predict(xgb_model, newdata = X_sens_scaled)

# 10. Combine predictions for final log price and compute final price
sensitivity_df_full <- sensitivity_df_full %>%
  mutate(
    final_log_price = gam_pred + 0.5 * bart_pred + 0.5 * xgb_pred,
    final_price = exp(final_log_price)
  )

# 11. Filter for complete cases before plotting
sensitivity_plot_df <- sensitivity_df_full %>% filter(complete.cases(.))

# 12. Plot the sensitivity analysis results using 'linewidth' for lines
print(
  ggplot(sensitivity_plot_df, aes(x = change * 100, y = final_price, color = var_perturbed)) +
    geom_line(linewidth = 1.2) +
    geom_point() +
    labs(
      title = "Sensitivity Analysis of Key Predictors",
      x = "% Change in Variable",
      y = "Predicted Price",
      color = "Variable"
    ) +
    theme_minimal()
)

xgb_params <- list(
  objective = "reg:squarederror",
  max_depth = 4,
  eta = 0.1
)

xgb_model <- xgboost(
  params = xgb_params,
  data = xgb_data,
  nrounds = 100,
  verbose = 0
)


# --- Compute Friedman's H statistic for all numeric feature pairs ---
library(tidyr)
library(ggplot2)

# Define numeric features to include in the H statistic computation
numeric_features <- c("Mileage", "Engine_Size", "Owner_Count", "car_age", 
                      "mileage_per_year", "age_mileage_interaction", 
                      "mileage_per_engine_size", "engine_size_per_door", 
                      "log_mileage", "owner_impact_score", "brand_mean_price_ratio", 
                      "fuel_efficiency_per_mile", "depreciation_rate", 
                      "region_price_deviation", "mileage_variance", 
                      "owner_stability_score")

# Get all unique feature pairs (combinations)
feature_pairs <- combn(numeric_features, 2, simplify = FALSE)

# Compute H statistic for each pair
h_results <- lapply(feature_pairs, function(pair) {
  h <- tryCatch({
    friedman_h(model = xgb_model, data = as.data.frame(df), var1 = pair[1], var2 = pair[2])
  }, error = function(e) NA)  # If there's an error, return NA
  data.frame(var1 = pair[1], var2 = pair[2], H = h)
})

# Combine into a single data frame
h_df <- do.call(rbind, h_results)

# Sort by H descending
h_df <- h_df %>% arrange(desc(H))
print(head(h_df, 10))  # View top 10 interactions

# --- Heatmap Visualization ---
# Prepare data for a symmetric matrix heatmap
heatmap_df <- h_df %>%
  bind_rows(h_df %>% rename(var1 = var2, var2 = var1)) %>%  # Mirror entries
  complete(var1 = numeric_features, var2 = numeric_features, fill = list(H = NA))  # Fill missing pairs

ggplot(heatmap_df, aes(x = var1, y = var2, fill = H)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "C", na.value = "grey90") +
  labs(title = "Friedman's H Statistic Heatmap",
       subtitle = "Interaction Strength Between Feature Pairs (XGBoost)",
       x = "Feature 1", y = "Feature 2", fill = "H Value") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Top N interaction pairs
top_n <- 15
print ( ggplot(h_df %>% top_n(top_n, H), aes(x = reorder(paste(var1, var2, sep = " x "), H), y = H)) +
          geom_col(fill = "steelblue") +
          coord_flip() +
          labs(title = paste("Top", top_n, "Friedman's H Interactions"),
               x = "Feature Pair", y = "H Statistic") +
          theme_minimal()
)