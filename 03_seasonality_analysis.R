# ============================================================
# UPI ANALYSIS — SEASONALITY ANALYSIS
# ============================================================
# Requires: df_monthly (run 00_setup_and_clean.R first)
#
# install.packages(c("ggplot2", "dplyr", "scales", "tidyr"))

library(ggplot2)
library(dplyr)
library(scales)
library(tidyr)


# ============================================================
# PART A — MONTHLY SEASONALITY
# ============================================================

monthly_avg <- df_monthly %>%
  group_by(Month_Num, Month_Name) %>%
  summarise(
    Avg_Volume = round(mean(Volume_Mn), 2),
    Avg_Value  = round(mean(Value_Cr), 2),
    Median_Vol = round(median(Volume_Mn), 2),
    SD_Vol     = round(sd(Volume_Mn), 2),
    Count      = n(),
    .groups = "drop"
  ) %>%
  arrange(Month_Num)

cat("=== AVERAGE VOLUME & VALUE BY MONTH ===\n")
print(monthly_avg, n = Inf)

cat("\n=== TOP 3 MONTHS BY AVG VOLUME ===\n")
print(monthly_avg %>% arrange(desc(Avg_Volume)) %>% head(3))

cat("\n=== BOTTOM 3 MONTHS BY AVG VOLUME ===\n")
print(monthly_avg %>% arrange(Avg_Volume) %>% head(3))

# ── Deseasonalised analysis ─────────────────────────────────
# Controls for the growth trend by computing each month's volume
# as % of that year's annual average
seasonal_index <- df_monthly %>%
  group_by(Year) %>%
  mutate(Year_Avg = mean(Volume_Mn)) %>%
  ungroup() %>%
  mutate(Seasonal_Index = round((Volume_Mn / Year_Avg) * 100, 2)) %>%
  group_by(Month_Num, Month_Name) %>%
  summarise(
    Avg_Index = round(mean(Seasonal_Index), 2),
    SD_Index  = round(sd(Seasonal_Index), 2),
    .groups = "drop"
  ) %>%
  arrange(Month_Num)

cat("\n=== SEASONAL INDEX BY MONTH (100 = annual average) ===\n")
print(seasonal_index, n = Inf)
# Index > 100 = above average month, < 100 = below average month

festive_months <- df_monthly %>%
  filter(Month_Num %in% c(10, 11)) %>%
  group_by(Month_Name, Year) %>%
  summarise(Volume_Mn = sum(Volume_Mn), .groups = "drop")

cat("\n=== OCTOBER & NOVEMBER VOLUMES BY YEAR ===\n")
print(festive_months %>% arrange(Year, Month_Name), n = Inf)

covid_monthly <- df_monthly %>%
  filter(Is_Covid == TRUE) %>%
  group_by(Month_Name, Month_Num) %>%
  summarise(Avg_Vol_Covid = round(mean(Volume_Mn), 2), .groups = "drop") %>%
  arrange(Month_Num)

cat("\n=== AVERAGE VOLUME BY MONTH (COVID PERIOD ONLY) ===\n")
print(covid_monthly)


# ============================================================
# PART B — SEASONALITY VISUALIZATIONS
# ============================================================

ggplot(monthly_avg, aes(x = Month_Name, y = Avg_Volume, fill = Avg_Volume)) +
  geom_col(width = 0.75) +
  geom_text(aes(label = comma(round(Avg_Volume))), vjust = -0.4, size = 2.8, fontface = "bold") +
  scale_fill_gradient(low = "#AED6F1", high = "#1B4F72") +
  scale_x_discrete(limits = month.name) +
  labs(title = "Average Monthly UPI Transaction Volume by Month",
       subtitle = "Averaged across all years (2016–2025) | Growth trend not removed",
       x = "Month", y = "Avg Volume (Millions)", fill = "Volume") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
ggsave("plot12_monthly_avg_bar.png", width = 10, height = 5)

ggplot(seasonal_index, aes(x = reorder(Month_Name, Month_Num), y = Avg_Index)) +
  geom_col(aes(fill = Avg_Index > 100), width = 0.75) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_text(aes(label = paste0(Avg_Index, "%")), vjust = -0.4, size = 2.8, fontface = "bold") +
  scale_fill_manual(values = c("TRUE" = "#1B4F72", "FALSE" = "#AED6F1"),
                     labels = c("TRUE" = "Above avg", "FALSE" = "Below avg")) +
  labs(title = "Seasonal Index by Month (Trend-Adjusted)",
       subtitle = "100 = annual average | Above 100 = high-activity month",
       x = "Month", y = "Seasonal Index (%)", fill = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("plot13_seasonal_index.png", width = 10, height = 5)

heatmap_data <- df_monthly %>%
  select(Year, Month_Num, Month_Name, Volume_Mn) %>%
  mutate(Month_Name = factor(Month_Name, levels = month.name))

ggplot(heatmap_data, aes(x = factor(Year), y = Month_Name, fill = Volume_Mn)) +
  geom_tile(color = "white", linewidth = 0.4) +
  scale_fill_gradient(low = "#EBF5FB", high = "#1B4F72", labels = comma, name = "Volume (Mn)") +
  scale_y_discrete(limits = rev(month.name)) +
  labs(title = "UPI Transaction Volume Heatmap — Month × Year",
       subtitle = "Darker = higher volume | Clearly shows year-on-year growth",
       x = "Year", y = "Month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("plot14_heatmap.png", width = 10, height = 6)

compare_monthly <- df_monthly %>%
  mutate(Period = ifelse(Is_Covid, "Covid Period", "Non-Covid Period")) %>%
  group_by(Month_Name, Month_Num, Period) %>%
  summarise(Avg_Vol = round(mean(Volume_Mn), 2), .groups = "drop")

ggplot(compare_monthly, aes(x = reorder(Month_Name, Month_Num), y = Avg_Vol, fill = Period)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("Covid Period" = "#E74C3C", "Non-Covid Period" = "#4E79A7")) +
  scale_y_continuous(labels = comma) +
  labs(title = "Average Monthly Volume: Covid vs Non-Covid Period",
       subtitle = "Side-by-side comparison by month",
       x = "Month", y = "Avg Volume (Millions)", fill = "Period") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("plot15_covid_monthly.png", width = 11, height = 5)

ggplot(festive_months, aes(x = Year, y = Volume_Mn, color = Month_Name, group = Month_Name)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("October" = "#F28E2B", "November" = "#4E79A7")) +
  scale_y_continuous(labels = comma) +
  labs(title = "October & November UPI Volumes by Year",
       subtitle = "Tracking festive season performance over time",
       x = "Year", y = "Volume (Millions)", color = "Month") +
  theme_minimal()
ggsave("plot16_festive_trend.png", width = 10, height = 5)

cat("\nSeasonality analysis complete — 5 plots saved (plot12 to plot16).\n")
