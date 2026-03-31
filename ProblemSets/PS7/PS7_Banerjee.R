library(tidyverse)
library(mice)
library(modelsummary)

# Load data
df <- read.csv("wages.csv")

# Drop missing hgc and tenure
df <- df %>% filter(!is.na(hgc) & !is.na(tenure))

# Missing rate
mean(is.na(df$logwage))

# Summary table
datasummary_skim(df, output = "latex", histogram = FALSE)

# Model 1: Complete cases
model_cc <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married,
               data = df %>% filter(!is.na(logwage)))

# Model 2: Mean imputation
df_mean <- df
df_mean$logwage[is.na(df_mean$logwage)] <- mean(df$logwage, na.rm=TRUE)
model_mean <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married,
                 data = df_mean)

# Model 3: Predicted value imputation
df_pred <- df
df_pred$logwage[is.na(df_pred$logwage)] <- predict(model_cc, newdata = df_pred)[is.na(df_pred$logwage)]
model_pred <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married,
                 data = df_pred)

# Model 4: MICE
set.seed(12345)
mice_imp <- mice(df, m=5, method="pmm", printFlag=FALSE)
mice_fit <- with(mice_imp, lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married))
model_mice <- pool(mice_fit)

# Regression table
modelsummary(
  list("Complete Cases"    = model_cc,
       "Mean Imputation"   = model_mean,
       "Pred. Imputation"  = model_pred,
       "MICE"              = model_mice),
  output = "latex",
  stars  = TRUE,
  title  = "Returns to Schooling Under Different Imputation Methods"
)
