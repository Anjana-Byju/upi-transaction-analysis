# ============================================================
# UPI ANALYSIS — TIME SERIES FORECASTING (ARIMA vs ETS)
# ============================================================
# Requires: df_monthly (run 00_setup_and_clean.R first)
#
# install.packages(c("forecast", "tseries", "ggplot2", "dplyr", "scales"))

library(forecast)
library(tseries)
library(ggplot2)
library(dplyr)
library(scales)


# ============================================================
# PART A — PREPARE TIME SERIES OBJECT
# ============================================================

upi_ts <- ts(df_monthly$Volume_Mn, start = c(2016, 8), frequency = 12)

cat("=== TIME SERIES OBJECT ===\n")
print(upi_ts)
cat("\nLength:", length(upi_ts), "months\n")
cat("Start :", start(upi_ts), "\n")
cat("End   :", end(upi_ts), "\n")


# ============================================================
# PART B — STATIONARITY TEST (Augmented Dickey-Fuller)
# ============================================================
# H0: series has a unit root (non-stationary) | H1: stationary

cat("\n=== AUGMENTED DICKEY-FULLER TEST ===\n")
adf_result <- adf.test(upi_ts)
cat("ADF statistic :", round(adf_result$statistic, 4), "\n")
cat("p-value       :", round(adf_result$p.value, 4), "\n")
cat("Stationary?   :", ifelse(adf_result$p.value < 0.05, "YES", "NO"), "\n")

cat("\n=== ADF TEST ON FIRST-DIFFERENCED SERIES ===\n")
adf_diff <- adf.test(diff(upi_ts))
cat("ADF statistic :", round(adf_diff$statistic, 4), "\n")
cat("p-value       :", round(adf_diff$p.value, 4), "\n")
cat("Stationary?   :", ifelse(adf_diff$p.value < 0.05, "YES", "NO"), "\n")


# ============================================================
# PART C — STL DECOMPOSITION
# ============================================================

cat("\n=== STL DECOMPOSITION ===\n")
stl_decomp <- stl(upi_ts, s.window = "periodic")
print(summary(stl_decomp))

png("plot25_stl_decomposition.png", width = 900, height = 700)
plot(stl_decomp, main = "STL Decomposition of UPI Transaction Volume")
dev.off()
cat("STL plot saved.\n")


# ============================================================
# PART D — ARIMA MODEL
# ============================================================

cat("\n=== AUTO ARIMA MODEL ===\n")
arima_model <- auto.arima(upi_ts, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
print(summary(arima_model))
cat("\nARIMA order selected:", arimaorder(arima_model), "\n")

arima_forecast <- forecast(arima_model, h = 12)
cat("\n=== ARIMA FORECAST: Next 12 Months ===\n")
print(arima_forecast)


# ============================================================
# PART E — ETS MODEL (Exponential Smoothing)
# ============================================================

cat("\n=== ETS MODEL ===\n")
ets_model <- ets(upi_ts)
print(summary(ets_model))

ets_forecast <- forecast(ets_model, h = 12)
cat("\n=== ETS FORECAST: Next 12 Months ===\n")
print(ets_forecast)


# ============================================================
# PART F — MODEL ACCURACY COMPARISON
# ============================================================

cat("\n=== MODEL ACCURACY COMPARISON ===\n")
cat("\nARIMA accuracy:\n")
print(accuracy(arima_model))
cat("\nETS accuracy:\n")
print(accuracy(ets_model))


# ============================================================
# PART G — FORECAST VISUALIZATIONS
# ============================================================

autoplot(arima_forecast) +
  scale_y_continuous(labels = comma) +
  labs(title = "UPI Transaction Volume — ARIMA Forecast (12 Months)",
       subtitle = "Blue = forecast | Dark shading = 80% CI | Light = 95% CI",
       x = "Year", y = "Volume (Millions)") +
  theme_minimal()
ggsave("plot26_arima_forecast.png", width = 10, height = 5)

autoplot(ets_forecast) +
  scale_y_continuous(labels = comma) +
  labs(title = "UPI Transaction Volume — ETS Forecast (12 Months)",
       subtitle = "Blue = forecast | Dark shading = 80% CI | Light = 95% CI",
       x = "Year", y = "Volume (Millions)") +
  theme_minimal()
ggsave("plot27_ets_forecast.png", width = 10, height = 5)

png("plot28_arima_residuals.png", width = 900, height = 600)
checkresiduals(arima_model)
dev.off()
cat("Residual diagnostics plot saved.\n")

# ── Combined ARIMA vs ETS comparison chart ─────────────────
arima_df <- data.frame(
  Date     = seq(as.Date("2026-01-01"), by = "month", length.out = 12),
  Forecast = as.numeric(arima_forecast$mean),
  Lo80     = as.numeric(arima_forecast$lower[, 1]),
  Hi80     = as.numeric(arima_forecast$upper[, 1]),
  Model    = "ARIMA"
)

ets_df <- data.frame(
  Date     = seq(as.Date("2026-01-01"), by = "month", length.out = 12),
  Forecast = as.numeric(ets_forecast$mean),
  Lo80     = as.numeric(ets_forecast$lower[, 1]),
  Hi80     = as.numeric(ets_forecast$upper[, 1]),
  Model    = "ETS"
)

historical_df <- data.frame(Date = df_monthly$Date, Volume = df_monthly$Volume_Mn)
combined_df   <- rbind(arima_df, ets_df)
recent_hist   <- historical_df %>% filter(Date >= as.Date("2023-01-01"))

ggplot() +
  geom_line(data = recent_hist, aes(x = Date, y = Volume), color = "black", linewidth = 0.9) +
  geom_line(data = combined_df, aes(x = Date, y = Forecast, color = Model), linewidth = 1.1) +
  geom_ribbon(data = combined_df, aes(x = Date, ymin = Lo80, ymax = Hi80, fill = Model), alpha = 0.15) +
  scale_color_manual(values = c("ARIMA" = "#4E79A7", "ETS" = "#F28E2B")) +
  scale_fill_manual(values  = c("ARIMA" = "#4E79A7", "ETS" = "#F28E2B")) +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  labs(title = "UPI Volume Forecast 2026 — ARIMA vs ETS Comparison",
       subtitle = "Black = historical (2023–2025) | Coloured = 12-month forecast",
       x = "Date", y = "Volume (Millions)", color = "Model", fill = "Model") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("plot29_forecast_comparison.png", width = 11, height = 5)

cat("\nForecasting complete — 5 plots saved (plot25 to plot29).\n")
