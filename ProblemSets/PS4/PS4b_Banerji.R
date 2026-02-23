# PS4b_Banerji.R
# sparklyr exercise

library(sparklyr)
library(tidyverse)

# Connect to Spark
sc <- spark_connect(master = "local")

# Load iris dataset into Spark
df <- copy_to(sc, iris, "iris", overwrite = TRUE)

# Part (b): What is df?
print(class(df))

# Part (c): Filter where Sepal_Length > 5.5
df_filtered <- df %>% filter(Sepal_Length > 5.5)
print(df_filtered)

# Part (d): How many rows?
cat("Number of rows:", sdf_nrow(df_filtered), "\n")

# Disconnect
spark_disconnect(sc)
