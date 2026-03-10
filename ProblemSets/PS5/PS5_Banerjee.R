# PS5 - Jhinuk Banerji
# Task 3: Web Scraping - Perfume Houses from Wikipedia
# Task 4: World Bank API - Gender Indicators

library(rvest)
library(tidyverse)
library(wbstats)
library(httr)

# ── TASK 3: Web Scraping ────────────────────────────────────────────────────

url <- "https://en.wikipedia.org/wiki/Perfume"

response <- GET(url, user_agent("Mozilla/5.0"))
page <- read_html(content(response, "text"))

tables <- page %>% html_nodes("table.wikitable") %>% html_table(fill = TRUE)
cat("Number of tables found:", length(tables), "\n")

if (length(tables) > 0) {
  perfume_data <- tables[[1]]
} else {
  stop("No tables found on page")
}

head(perfume_data)
cat("Rows scraped:", nrow(perfume_data), "\n")
write.csv(perfume_data, "perfume_houses.csv", row.names = FALSE)

# ── TASK 4: World Bank API ──────────────────────────────────────────────────

wb_data <- wb_data(
  indicator  = c("SL.TLF.TOTL.FE.ZS", "SG.GEN.PARL.ZS"),
  country    = "countries_only",
  start_date = 2000,
  end_date   = 2023
)

wb_data <- wb_data %>%
  rename(
    female_labor_force_pct  = SL.TLF.TOTL.FE.ZS,
    women_in_parliament_pct = SG.GEN.PARL.ZS
  )

head(wb_data)
cat("Total observations:", nrow(wb_data), "\n")
summary(wb_data %>% select(female_labor_force_pct, women_in_parliament_pct))
write.csv(wb_data, "worldbank_gender.csv", row.names = FALSE)
