library(tidymodels)
library(glmnet)

# PS9_Banerjee.R
# Econ 5253 - Spring 2026

# Load housing data
housing <- read.table(
  "https://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data",
  header = FALSE
)

colnames(housing) <- c("crim","zn","indus","chas","nox","rm","age",
                       "dis","rad","tax","ptratio","b","lstat","medv")

cat("Original housing dimensions:", dim(housing), "\n")

# Set seed
set.seed(123456)

# Train/test split
housing_split <- initial_split(housing, prop = 0.75)
housing_train <- training(housing_split)
housing_test  <- testing(housing_split)

# Build recipe
housing_recipe <- recipe(medv ~ ., data = housing) %>%
  step_log(all_outcomes()) %>%
  step_bin2factor(chas) %>%
  step_interact(terms = ~ crim:zn:indus:rm:age:rad:tax:
                  ptratio:b:lstat:dis:nox) %>%
  step_poly(crim, zn, indus, rm, age, rad, tax, ptratio, b,
            lstat, dis, nox, degree = 6)

# Prep and juice
housing_prep          <- housing_recipe %>% prep(housing_train, retain = TRUE)
housing_train_prepped <- housing_prep %>% juice()
housing_test_prepped  <- housing_prep %>% bake(new_data = housing_test)

# Separate X and Y
housing_train_x <- housing_train_prepped %>% select(-medv)
housing_test_x  <- housing_test_prepped  %>% select(-medv)
housing_train_y <- housing_train_prepped %>% select(medv)
housing_test_y  <- housing_test_prepped  %>% select(medv)

# Print dimensions
cat("\n--- Step 7: Dimensions ---\n")
cat("Rows in training data:", nrow(housing_train_prepped), "\n")
cat("Cols in training data (including medv):", ncol(housing_train_prepped), "\n")
cat("Number of X columns after recipe:", ncol(housing_train_x), "\n")
cat("Original X columns:", ncol(housing) - 1, "\n")
cat("Extra X columns added:", ncol(housing_train_x) - (ncol(housing) - 1), "\n")

# Convert to matrices
train_x_mat <- as.matrix(housing_train_x)
test_x_mat  <- as.matrix(housing_test_x)
train_y_vec <- as.matrix(housing_train_y)
test_y_vec  <- as.matrix(housing_test_y)

# LASSO (alpha = 1)
set.seed(123456)
lasso_cv <- cv.glmnet(
  x      = train_x_mat,
  y      = train_y_vec,
  alpha  = 1,
  nfolds = 6
)

lasso_lambda_opt <- lasso_cv$lambda.min
cat("\n--- Step 8: LASSO Results ---\n")
cat("Optimal lambda:", lasso_lambda_opt, "\n")

lasso_train_pred    <- predict(lasso_cv, s = lasso_lambda_opt, newx = train_x_mat)
lasso_insample_rmse <- sqrt(mean((lasso_train_pred - train_y_vec)^2))
cat("In-sample RMSE:", lasso_insample_rmse, "\n")

lasso_test_pred <- predict(lasso_cv, s = lasso_lambda_opt, newx = test_x_mat)
lasso_oos_rmse  <- sqrt(mean((lasso_test_pred - test_y_vec)^2))
cat("Out-of-sample RMSE:", lasso_oos_rmse, "\n")

# Ridge (alpha = 0)
set.seed(123456)
ridge_cv <- cv.glmnet(
  x      = train_x_mat,
  y      = train_y_vec,
  alpha  = 0,
  nfolds = 6
)

ridge_lambda_opt <- ridge_cv$lambda.min
cat("\n--- Step 9: Ridge Results ---\n")
cat("Optimal lambda:", ridge_lambda_opt, "\n")

ridge_test_pred <- predict(ridge_cv, s = ridge_lambda_opt, newx = test_x_mat)
ridge_oos_rmse  <- sqrt(mean((ridge_test_pred - test_y_vec)^2))
cat("Out-of-sample RMSE:", ridge_oos_rmse, "\n")

cat("\nDone!\n")