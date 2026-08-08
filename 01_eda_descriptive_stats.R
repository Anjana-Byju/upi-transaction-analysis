# ============================================================
# UPI ANALYSIS — EDA & DESCRIPTIVE STATISTICS
# ============================================================
# Requires: df_monthly (run 00_setup_and_clean.R first)
#
# install.packages(c("ggplot2", "dplyr", "scales", "corrplot", "e1071"))

library(dplyr)
library(ggplot2)
library(scales)
library(corrplot)
library(e1071)   # for skewness & kurtosis


# ============================================================
# PART A — DESCRIPTIVE STATISTICS
# ============================================================

cat("=== DESCRIPTIVE STATISTICS: Volume (Millions) ===\n")
cat("Mean     :", round(mean(df_monthly$Volume_Mn), 2), "\n")
cat("Median   :", round(median(df_monthly$Volume_Mn), 2), "\n")
cat("Std Dev  :", round(sd(df_monthly$Volume_Mn), 2), "\n")
cat("Min      :", round(min(df_monthly$Volume_Mn), 2), "\n")
cat("Max      :", round(max(df_monthly$Volume_Mn), 2), "\n")
cat("Skewness :", round(skewness(df_monthly$Volume_Mn), 2), "\n")
cat("Kurtosis :", round(kurtosis(df_monthly$Volume_Mn), 2), "\n")

cat("\n=== DESCRIPTIVE STATISTICS: Value (Crores) ===\n")
cat("Mean     :", round(mean(df_monthly$Value_Cr), 2), "\n")
cat("Median   :", round(median(df_monthly$Value_Cr), 2), "\n")
cat("Std Dev  :", round(sd(df_monthly$Value_Cr), 2), "\n")
cat("Min      :", round(min(df_monthly$Value_Cr), 2), "\n")
cat("Max      :", round(max(df_monthly$Value_Cr), 2), "\n")
cat("Skewness :", round(skewness(df_monthly$Value_Cr), 2), "\n")
cat("Kurtosis :", round(kurtosis(df_monthly$Value_Cr), 2), "\n")

# ── Year-wise summary table ────────────────────────────────
yearly_summary <- df_monthly %>%
  group_by(Year) %>%
  summarise(
    Total_Volume_Mn = round(sum(Volume_Mn), 2),
    Total_Value_Cr  = round(sum(Value_Cr), 2),
    Avg_Volume_Mn   = round(mean(Volume_Mn), 2),
    Avg_Value_Cr    = round(mean(Value_Cr), 2),
    Months_Recorded = n()
  )

cat("\n=== YEAR-WISE SUMMARY ===\n")
print(yearly_summary, n = Inf)

# ── Covid vs Non-Covid comparison ──────────────────────────
covid_compare <- df_monthly %>%
  group_by(Is_Covid) %>%
  summarise(
    Months        = n(),
    Avg_Volume_Mn = round(mean(Volume_Mn), 2),
    Avg_Value_Cr  = round(mean(Value_Cr), 2),
    Median_Volume = round(median(Volume_Mn), 2)
  )

cat("\n=== COVID vs NON-COVID PERIOD ===\n")
print(covid_compare)

# ── Festive vs Non-Festive comparison ──────────────────────
festive_compare <- df_monthly %>%
  group_by(Is_Festive) %>%
  summarise(
    Months        = n(),
    Avg_Volume_Mn = round(mean(Volume_Mn), 2),
    Avg_Value_Cr  = round(mean(Value_Cr), 2)
  )

cat("\n=== FESTIVE vs NON-FESTIVE SEASON ===\n")
print(festive_compare)

# ── Correlation between Volume and Value ───────────────────
cor_val <- cor(df_monthly$Volume_Mn, df_monthly$Value_Cr)
cat("\nCorrelation (Volume vs Value):", round(cor_val, 4), "\n")


# ============================================================
# PART B — VISUALIZATIONS
# ============================================================

# ── Distribution of Transaction Volume ─────────────────────
ggplot(df_monthly, aes(x = Volume_Mn)) +
  geom_histogram(bins = 20, fill = "#4E79A7", color = "white") +
  labs(
    title    = "Distribution of Monthly UPI Transaction Volume",
    subtitle = "Aug 2016 – Dec 2025",
    x = "Volume (Millions)", y = "Count"
  ) +
  theme_minimal()
ggsave("plot1_volume_distribution.png", width = 8, height = 5)

# ── Distribution of Transaction Value ──────────────────────
ggplot(df_monthly, aes(x = Value_Cr)) +
  geom_histogram(bins = 20, fill = "#F28E2B", color = "white") +
  scale_x_continuous(labels = comma) +
  labs(
    title    = "Distribution of Monthly UPI Transaction Value",
    subtitle = "Aug 2016 – Dec 2025",
    x = "Value (Crores ₹)", y = "Count"
  ) +
  theme_minimal()
ggsave("plot2_value_distribution.png", width = 8, height = 5)

# ── Volume over Time ────────────────────────────────────────
ggplot(df_monthly, aes(x = Date, y = Volume_Mn)) +
  geom_line(color = "#4E79A7", linewidth = 0.8) +
  geom_smooth(method = "loess", se = FALSE, color = "red", linetype = "dashed") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "UPI Transaction Volume Over Time",
    subtitle = "Monthly data | Dashed line = trend",
    x = "Date", y = "Volume (Millions)"
  ) +
  theme_minimal()
ggsave("plot3_volume_trend.png", width = 10, height = 5)

# ── Value over Time ──────────────────────────────────────────
ggplot(df_monthly, aes(x = Date, y = Value_Cr)) +
  geom_line(color = "#F28E2B", linewidth = 0.8) +
  geom_smooth(method = "loess", se = FALSE, color = "red", linetype = "dashed") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = comma) +
  labs(
    title = "UPI Transaction Value Over Time",
    subtitle = "Monthly data | Dashed line = trend",
    x = "Date", y = "Value (Crores ₹)"
  ) +
  theme_minimal()
ggsave("plot4_value_trend.png", width = 10, height = 5)

# ── Boxplot by Month (Seasonality) ─────────────────────────
ggplot(df_monthly, aes(x = Month_Name, y = Volume_Mn)) +
  geom_boxplot(fill = "#76B7B2") +
  labs(title = "Monthly Seasonality in UPI Transaction Volume",
       x = "Month", y = "Volume (Millions)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("plot5_monthly_boxplot.png", width = 10, height = 5)

# ── Correlation Matrix ───────────────────────────────────────
cor_data <- df_monthly %>%
  select(Volume_Mn, Value_Cr, Volume_Roll3M, Value_Roll3M) %>%
  cor(use = "complete.obs")

png("plot6_correlation_matrix.png", width = 600, height = 500)
corrplot(cor_data, method = "color", type = "upper",
         addCoef.col = "black", tl.col = "black",
         title = "Correlation Matrix", mar = c(0, 0, 1, 0))
dev.off()

cat("\nEDA complete — 6 plots saved to your working directory.\n")
