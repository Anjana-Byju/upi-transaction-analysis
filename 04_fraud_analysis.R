# ============================================================
# UPI ANALYSIS — FRAUD ANALYSIS
# ============================================================
# Requires: df_monthly, df_fraud (run 00_setup_and_clean.R first)
#
# install.packages(c("ggplot2", "dplyr", "scales", "tidyr"))

library(ggplot2)
library(dplyr)
library(scales)
library(tidyr)


# ============================================================
# PART A — FRAUD STATISTICS
# ============================================================

cat("=== FRAUD DATA OVERVIEW ===\n")
print(df_fraud)

# ── Fraud as % of total UPI value ──────────────────────────
annual_upi <- df_monthly %>%
  mutate(FY = case_when(
    Month_Num >= 4 ~ paste0("FY", substr(Year, 3, 4), "-",
                             sprintf("%02d", as.integer(substr(Year, 3, 4)) + 1)),
    TRUE ~ paste0("FY", sprintf("%02d", as.integer(substr(Year, 3, 4)) - 1),
                  "-", substr(Year, 3, 4))
  )) %>%
  group_by(FY) %>%
  summarise(
    Total_Value_Cr = round(sum(Value_Cr), 2),
    Total_Vol_Mn   = round(sum(Volume_Mn), 2),
    .groups = "drop"
  )

fraud_analysis <- df_fraud %>%
  rename(FY = Financial_Year) %>%
  mutate(FY = as.character(FY)) %>%
  left_join(annual_upi, by = "FY") %>%
  mutate(
    Fraud_Pct_Value = round((Fraud_Value_Cr / Total_Value_Cr) * 100, 4),
    Fraud_Per_1000  = round((Fraud_Cases / (Total_Vol_Mn * 1e6)) * 1000, 6)
  )

cat("\n=== FRAUD AS % OF TOTAL UPI VALUE ===\n")
print(fraud_analysis %>%
        select(FY, Fraud_Cases, Fraud_Value_Cr, Total_Value_Cr,
               Avg_Loss_Rs, Fraud_Pct_Value, Fraud_Per_1000),
      n = Inf)

# ── YoY fraud growth ────────────────────────────────────────
fraud_yoy <- df_fraud %>%
  mutate(
    Case_YoY  = round((Fraud_Cases / lag(Fraud_Cases) - 1) * 100, 1),
    Value_YoY = round((Fraud_Value_Cr / lag(Fraud_Value_Cr) - 1) * 100, 1)
  )

cat("\n=== YoY FRAUD GROWTH ===\n")
print(fraud_yoy %>%
        select(Financial_Year, Fraud_Cases, Fraud_Value_Cr,
               Avg_Loss_Rs, Case_YoY, Value_YoY))

cat("\n=== FRAUD SUMMARY STATISTICS ===\n")
cat("Total fraud cases (FY21-FY25)   :",
    format(sum(df_fraud$Fraud_Cases, na.rm = TRUE), big.mark = ","), "\n")
cat("Total fraud value Cr (FY21-FY25):",
    format(sum(df_fraud$Fraud_Value_Cr, na.rm = TRUE), big.mark = ","), "\n")
cat("Avg loss per case range         : Rs",
    min(df_fraud$Avg_Loss_Rs, na.rm = TRUE), "to Rs",
    max(df_fraud$Avg_Loss_Rs, na.rm = TRUE), "\n")


# ============================================================
# PART B — FRAUD VISUALIZATIONS
# ============================================================

p1 <- ggplot(df_fraud %>% filter(!is.na(Fraud_Cases)),
             aes(x = as.character(Financial_Year), y = Fraud_Cases / 1e6)) +
  geom_col(fill = "#C0392B", width = 0.65) +
  geom_text(aes(label = paste0(round(Fraud_Cases / 1e6, 2), "M")),
            vjust = -0.4, size = 3, fontface = "bold") +
  labs(title = "UPI Fraud Cases by Financial Year",
       subtitle = "In millions | FY21-22 data unavailable",
       x = "Financial Year", y = "Fraud Cases (Millions)") +
  theme_minimal()
ggsave("plot17_fraud_cases.png", plot = p1, width = 9, height = 5)

p2 <- ggplot(df_fraud, aes(x = as.character(Financial_Year), y = Fraud_Value_Cr)) +
  geom_col(fill = "#922B21", width = 0.65) +
  geom_text(aes(label = paste0("₹", Fraud_Value_Cr, " Cr")),
            vjust = -0.4, size = 2.8, fontface = "bold") +
  scale_y_continuous(labels = comma) +
  labs(title = "UPI Fraud Value by Financial Year (₹ Crores)",
       subtitle = "Includes FY21-22 (value only — case count unavailable)",
       x = "Financial Year", y = "Fraud Value (Crores ₹)") +
  theme_minimal()
ggsave("plot18_fraud_value.png", plot = p2, width = 9, height = 5)

p3 <- ggplot(df_fraud %>% filter(!is.na(Avg_Loss_Rs)),
             aes(x = as.character(Financial_Year), y = Avg_Loss_Rs, group = 1)) +
  geom_line(color = "#E67E22", linewidth = 1.2) +
  geom_point(color = "#E67E22", size = 4) +
  geom_text(aes(label = paste0("₹", format(round(Avg_Loss_Rs), big.mark = ","))),
            vjust = -1, size = 3, fontface = "bold") +
  scale_y_continuous(labels = comma) +
  labs(title = "Average Loss Per Fraud Case (₹)",
       subtitle = "FY21-22 excluded — case count unavailable",
       x = "Financial Year", y = "Avg Loss Per Case (₹)") +
  theme_minimal()
ggsave("plot19_avg_loss.png", plot = p3, width = 9, height = 5)

p4 <- ggplot(fraud_analysis %>% filter(!is.na(Fraud_Pct_Value)),
             aes(x = FY, y = Fraud_Pct_Value, group = 1)) +
  geom_line(color = "#8E44AD", linewidth = 1.2) +
  geom_point(color = "#8E44AD", size = 4) +
  geom_text(aes(label = paste0(Fraud_Pct_Value, "%")), vjust = -1, size = 3, fontface = "bold") +
  labs(title = "Fraud Value as % of Total UPI Transaction Value",
       subtitle = "Lower % = fraud growing slower than UPI overall",
       x = "Financial Year", y = "Fraud Value (% of Total)") +
  theme_minimal()
ggsave("plot20_fraud_pct.png", plot = p4, width = 9, height = 5)

fraud_yoy_plot <- fraud_yoy %>%
  filter(!is.na(Case_YoY) | !is.na(Value_YoY)) %>%
  select(Financial_Year, Case_YoY, Value_YoY) %>%
  pivot_longer(cols = c(Case_YoY, Value_YoY), names_to = "Metric", values_to = "Growth_Pct") %>%
  filter(!is.na(Growth_Pct))

p5 <- ggplot(fraud_yoy_plot, aes(x = as.character(Financial_Year), y = Growth_Pct, fill = Metric)) +
  geom_col(position = "dodge", width = 0.65) +
  geom_text(aes(label = paste0(Growth_Pct, "%")),
            position = position_dodge(width = 0.65), vjust = -0.4, size = 2.8, fontface = "bold") +
  scale_fill_manual(values = c("Case_YoY" = "#C0392B", "Value_YoY" = "#922B21"),
                     labels = c("Case_YoY" = "Cases YoY %", "Value_YoY" = "Value YoY %")) +
  labs(title = "YoY Growth: Fraud Cases vs Fraud Value",
       subtitle = "Value growing faster than cases = rising avg loss per case",
       x = "Financial Year", y = "YoY Growth (%)", fill = "Metric") +
  theme_minimal()
ggsave("plot21_fraud_yoy.png", plot = p5, width = 9, height = 5)

cat("\nFraud analysis complete — 5 plots saved (plot17 to plot21).\n")
