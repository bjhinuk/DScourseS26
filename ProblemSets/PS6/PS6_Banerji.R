options(bitmapType = "cairo")
# PS6_Banerji.R
# Econ 5253 - Spring 2026
# Data Cleaning and Visualization

# Load libraries
library(tidyverse)
library(ggplot2)
library(maps)
library(viridis)

# Load data
df <- read.csv("../PS5/worldbank_gender.csv")

# Clean data
df_clean <- df %>%
  filter(!is.na(women_in_parliament_pct) &
         !is.na(female_labor_force_pct)) %>%
  rename(
    year         = date,
    women_parl   = women_in_parliament_pct,
    female_labor = female_labor_force_pct
  ) %>%
  filter(year >= 2000 & year <= 2023)

cat("Rows after cleaning:", nrow(df_clean), "\n")
# PLOT 1: World Map
world_map <- map_data("world")

df_2022 <- df_clean %>%
  filter(year == 2022) %>%
  mutate(country = recode(country,
    "United States"       = "USA",
    "United Kingdom"      = "UK",
    "Korea, Rep."         = "South Korea",
    "Congo, Dem. Rep."    = "Democratic Republic of the Congo",
    "Congo, Rep."         = "Republic of Congo",
    "Iran, Islamic Rep."  = "Iran",
    "Russian Federation"  = "Russia",
    "Egypt, Arab Rep."    = "Egypt",
    "Venezuela, RB"       = "Venezuela",
    "Syria Arab Republic" = "Syria"
  ))

map_data_joined <- world_map %>%
  left_join(df_2022, by = c("region" = "country"))

plot1 <- ggplot(map_data_joined,
                aes(x = long, y = lat, group = group,
                    fill = women_parl)) +
  geom_polygon(color = "white", linewidth = 0.1) +
  scale_fill_viridis(
    option   = "plasma",
    na.value = "grey85",
    name     = "Women in\nParliament (%)"
  ) +
  labs(
    title    = "Women in Parliament (%) Across the World",
    subtitle = "2022  |  Grey = No data available",
    caption  = "Source: World Bank"
  ) +
  theme_void() +
  theme(
    plot.title       = element_text(size = 16, face = "bold",
                                    hjust = 0.5),
    plot.subtitle    = element_text(size = 11, hjust = 0.5,
                                    color = "grey40"),
    plot.caption     = element_text(size = 8, color = "grey60"),
    legend.position  = "bottom",
    legend.key.width = unit(2, "cm")
  )

ggsave("PS6a_Banerji.png", plot = plot1,
       width = 12, height = 7, dpi = 300)
cat("Plot 1 (World Map) saved!\n")
# PLOT 2: Heatmap
top30 <- df_clean %>%
  group_by(country) %>%
  summarise(avg = mean(women_parl, na.rm = TRUE)) %>%
  arrange(desc(avg)) %>%
  top_n(30, avg) %>%
  pull(country)

df_heatmap <- df_clean %>%
  filter(country %in% top30)

plot2 <- ggplot(df_heatmap,
                aes(x = year,
                    y = reorder(country, women_parl),
                    fill = women_parl)) +
  geom_tile(color = "white") +
  scale_fill_viridis(option = "magma",
                     name = "Women in\nParliament (%)") +
  labs(
    title    = "Women in Parliament (%) - Top 30 Countries Over Time",
    subtitle = "2000-2023",
    x        = "Year",
    y        = NULL,
    caption  = "Source: World Bank"
  ) +
  theme_minimal() +
  theme(
    plot.title      = element_text(size = 14, face = "bold"),
    plot.subtitle   = element_text(size = 10, color = "grey40"),
    axis.text.y     = element_text(size = 8),
    axis.text.x     = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

ggsave("PS6b_Banerji.png", plot = plot2,
       width = 12, height = 8, dpi = 300)
cat("Plot 2 (Heatmap) saved!\n")

# PLOT 3: Dumbbell
df_dumbbell <- df_clean %>%
  filter(year %in% c(2000, 2023)) %>%
  select(country, year, women_parl) %>%
  pivot_wider(names_from   = year,
              values_from  = women_parl,
              names_prefix = "yr_") %>%
  filter(!is.na(yr_2000) & !is.na(yr_2023)) %>%
  arrange(desc(yr_2023)) %>%
 top_n(25, yr_2023)

plot3 <- ggplot(df_dumbbell) +
  geom_segment(
    aes(x    = yr_2000, xend = yr_2023,
        y    = reorder(country, yr_2023),
        yend = reorder(country, yr_2023)),
    color = "grey70", size = 1.2
  ) +
  geom_point(
    aes(x = yr_2000,
        y = reorder(country, yr_2023)),
    color = "#E69F00", size = 3.5
  ) +
  geom_point(
    aes(x = yr_2023,
        y = reorder(country, yr_2023)),
    color = "#0072B2", size = 3.5
  ) +
  labs(
    title    = "Change in Women in Parliament (%): 2000 vs. 2023",
    subtitle = "Orange = 2000  |  Blue = 2023  |  Top 25 countries by 2023 value",
    x        = "Women in Parliament (%)",
    y        = NULL,
    caption  = "Source: World Bank"
  ) +
  theme_minimal() +
  theme(
    plot.title       = element_text(size = 14, face = "bold"),
    plot.subtitle    = element_text(size = 10, color = "grey40"),
    axis.text.y      = element_text(size = 9),
    panel.grid.minor = element_blank()
  )

ggsave("PS6c_Banerji.png", plot = plot3,
       width = 10, height = 8, dpi = 300)
cat("Plot 3 (Dumbbell) saved!\n")

# PLOT 4: Line Chart
selected_countries <- c("United States", "Sweden", "India",
                        "Brazil", "South Africa", "Rwanda")

df_line <- df_clean %>%
  filter(country %in% selected_countries)

plot4 <- ggplot(df_line,
                aes(x = year, y = women_parl,
                    color = country)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  scale_color_viridis_d(option = "turbo") +
  labs(
    title    = "Women in Parliament (%) Over Time",
    subtitle = "Selected Countries, 2000-2023",
    x        = "Year",
    y        = "Women in Parliament (%)",
    color    = "Country",
    caption  = "Source: World Bank"
  ) +
  theme_minimal() +
  theme(
    plot.title      = element_text(size = 14, face = "bold"),
    legend.position = "bottom"
  )

ggsave("PS6d_Banerji.png", plot = plot4,
       width = 9, height = 6, dpi = 300)
cat("Plot 4 (Line Chart) saved!\n")

# PLOT 5: Scatter Plot
plot5 <- ggplot(df_clean %>% filter(year == 2022),
                aes(x = female_labor, y = women_parl)) +
  geom_point(alpha = 0.6, color = "steelblue", size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  labs(
    title    = "Women in Parliament vs. Female Labor Force Participation",
    subtitle = "Country-level data, 2022",
    x        = "Female Labor Force Participation (%)",
    y        = "Women in Parliament (%)",
    caption  = "Source: World Bank"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))

ggsave("PS6e_Banerji.png", plot = plot5,
       width = 8, height = 5, dpi = 300)
cat("Plot 5 (Scatter) saved!\n")

# PLOT 6: Bar Chart
df_top20 <- df_clean %>%
  group_by(country) %>%
  summarise(avg_women_parl = mean(women_parl, na.rm = TRUE)) %>%
  arrange(desc(avg_women_parl)) %>%
 top_n(20, avg_women_parl)

plot6 <- ggplot(df_top20,
                aes(x = avg_women_parl,
                    y = reorder(country, avg_women_parl),
                    fill = avg_women_parl)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis(option = "cividis", name = NULL) +
  labs(
    title    = "Top 20 Countries: Average Women in Parliament (%)",
    subtitle = "Average across all available years, 2000-2023",
    x        = "Average Women in Parliament (%)",
    y        = NULL,
    caption  = "Source: World Bank"
  ) +
  theme_minimal() +
  theme(
    plot.title      = element_text(size = 14, face = "bold"),
    legend.position = "none"
  )

ggsave("PS6f_Banerji.png", plot = plot6,
       width = 9, height = 7, dpi = 300)
cat("Plot 6 (Bar Chart) saved!\n")

cat("\nAll 6 plots saved!\n")
  
  

 
