# ============================================================
# UPI ANALYSIS — GROWTH TREND ANALYSIS
# ============================================================
# Requires: df_monthly (run 00_setup_and_clean.R first)
#
# install.packages(c("ggplot2", "dplyr", "scales", "ggrepel", "patchwork"))

library(ggplot2)
library(dplyr)
library(scales)
library(ggrepel)
library(patchwork)


# ============================================================
# PART A — YoY & MoM GROWTH RATE ANALYSIS
# ============================================================

yoy_summary <- df_monthly %>%
  filter(!is.na(Volume_YoY)) %>%
  summarise(
    Avg_YoY_Volume = round(mean(Volume_YoY), 2),
    Med_YoY_Volume = round(median(Volume_YoY), 2),
    Max_YoY_Volume = round(max(Volume_YoY), 2),
    Min_YoY_Volume = round(min(Volume_YoY), 2),
    Avg_YoY_Value  = round(mean(Value_YoY, na.rm = TRUE), 2),
    Med_YoY_Value  = round(median(Value_YoY, na.rm = TRUE), 2)
  )

cat("=== YoY GROWTH RATE SUMMARY ===\n")
print(yoy_summary)

yearly_yoy <- df_monthly %>%
  filter(!is.na(Volume_YoY)) %>%
  group_by(Year) %>%
  summarise(
    Avg_Vol_YoY = round(mean(Volume_YoY), 2),
    Avg_Val_YoY = round(mean(Value_YoY, na.rm = TRUE), 2),
    Months = n()
  )

cat("\n=== YEAR-WISE AVERAGE YoY GROWTH (%) ===\n")
print(yearly_yoy, n = Inf)

# ── Key inflection points ──────────────────────────────────
top_growth <- df_monthly %>%
  filter(!is.na(Volume_YoY)) %>%
  arrange(desc(Volume_YoY)) %>%
  select(Date, Year, Month_Name, Volume_Mn, Volume_YoY, Value_YoY) %>%
  head(5)

cat("\n=== TOP 5 MONTHS BY YoY VOLUME GROWTH ===\n")
print(top_growth)

low_growth <- df_monthly %>%
  filter(!is.na(Volume_YoY)) %>%
  arrange(Volume_YoY) %>%
  select(Date, Year, Month_Name, Volume_Mn, Volume_YoY, Value_YoY) %>%
  head(5)

cat("\n=== BOTTOM 5 MONTHS BY YoY VOLUME GROWTH ===\n")
print(low_growth)


# ============================================================
# PART B — GROWTH VISUALIZATIONS
# ============================================================

df_yoy <- df_monthly %>%
  filter(!is.na(Volume_YoY) & Volume_YoY < 500)  # remove early "inf" outliers

ggplot(df_yoy, aes(x = Date, y = Volume_YoY)) +
  geom_line(color = "#4E79A7", linewidth = 0.8) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", linewidth = 0.5) +
  geom_hline(yintercept = mean(df_yoy$Volume_YoY),
             color = "red", linetype = "dotted", linewidth = 0.7) +
  annotate("text", x = max(df_yoy$Date), y = mean(df_yoy$Volume_YoY) + 5,
           label = paste0("Mean YoY: ", round(mean(df_yoy$Volume_YoY), 1), "%"),
           hjust = 1, size = 3, color = "red") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "UPI Transaction Volume — Year-on-Year Growth Rate (%)",
       subtitle = "Early 'infinite' growth values excluded | Red dotted = mean YoY",
       x = "Date", y = "YoY Growth (%)") +
  theme_minimal()
ggsave("plot7_yoy_growth.png", width = 10, height = 5)

ggplot(yearly_yoy, aes(x = factor(Year), y = Avg_Vol_YoY, fill = Avg_Vol_YoY)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(Avg_Vol_YoY, "%")), vjust = -0.5, size = 3, fontface = "bold") +
  scale_fill_gradient(low = "#AED6F1", high = "#1B4F72") +
  labs(title = "Average Annual YoY Volume Growth Rate by Year",
       subtitle = "Based on months with valid YoY data",
       x = "Year", y = "Avg YoY Growth (%)", fill = "Growth %") +
  theme_minimal() + theme(legend.position = "none")
ggsave("plot8_yearly_yoy_bar.png", width = 10, height = 5)

ggplot(df_monthly, aes(x = Date)) +
  geom_line(aes(y = Volume_Mn), color = "#AED6F1", linewidth = 0.7, alpha = 0.8) +
  geom_line(aes(y = Volume_Roll3M), color = "#1B4F72", linewidth = 1.2) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "UPI Volume: Actual vs 3-Month Rolling Average",
       subtitle = "Light blue = monthly actual | Dark blue = 3M rolling mean",
       x = "Date", y = "Volume (Millions)") +
  theme_minimal()
ggsave("plot9_rolling_mean.png", width = 10, height = 5)

# ── Annotated timeline with key events ─────────────────────
events <- data.frame(
  Date  = as.Date(c("2016-11-08", "2020-03-01", "2021-01-01", "2022-01-01")),
  Label = c("Demonetisation\n(Nov 2016)", "Covid Lockdown\n(Mar 2020)",
            "Post-Covid\nRecovery", "UPI 2Bn\nTransactions/Month"),
  y     = c(500, 1200, 2800, 5500)
)

ggplot(df_monthly, aes(x = Date, y = Volume_Mn)) +
  geom_line(color = "#4E79A7", linewidth = 0.9) +
  geom_vline(data = events, aes(xintercept = Date),
             linetype = "dashed", color = "#E74C3C", linewidth = 0.5) +
  geom_label(data = events, aes(x = Date, y = y, label = Label),
             size = 2.5, color = "#E74C3C", fill = "white",
             label.size = 0.3, hjust = 0.5) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "UPI Transaction Volume with Key Event Annotations",
       subtitle = "Red dashed lines mark major economic/policy events",
       x = "Date", y = "Volume (Millions)") +
  theme_minimal()
ggsave("plot10_annotated_timeline.png", width = 11, height = 5)

ggplot(df_monthly %>% filter(Volume_Mn > 0), aes(x = Date, y = Volume_Mn)) +
  geom_line(color = "#4E79A7", linewidth = 0.9) +
  scale_y_log10(labels = comma) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "UPI Transaction Volume — Log Scale",
       subtitle = "Log scale reveals consistent growth rate across all years",
       x = "Date", y = "Volume (Millions) — Log Scale") +
  theme_minimal()
ggsave("plot11_log_scale.png", width = 10, height = 5)

cat("\nGrowth trend analysis complete — 5 plots saved (plot7 to plot11).\n")
