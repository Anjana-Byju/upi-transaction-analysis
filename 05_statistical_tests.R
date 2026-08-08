# ============================================================
# UPI ANALYSIS — STATISTICAL HYPOTHESIS TESTS
# ============================================================
# Requires: df_monthly (run 00_setup_and_clean.R first)
#
# install.packages(c("ggplot2", "dplyr", "scales", "car", "lmtest"))

library(ggplot2)
library(dplyr)
library(scales)
library(car)      # for Levene's test
library(lmtest)   # for Breusch-Pagan test


# ============================================================
# PART A — NORMALITY TESTS
# ============================================================

cat("=== NORMALITY TESTS (Shapiro-Wilk) ===\n")

sw_vol <- shapiro.test(df_monthly$Volume_Mn)
sw_val <- shapiro.test(df_monthly$Value_Cr)

cat("\nVolume_Mn:\n")
cat("  W =", round(sw_vol$statistic, 4),
    "| p-value =", format(sw_vol$p.value, scientific = TRUE), "\n")

cat("\nValue_Cr:\n")
cat("  W =", round(sw_val$statistic, 4),
    "| p-value =", format(sw_val$p.value, scientific = TRUE), "\n")

cat("\nInterpretation: p < 0.05 = NOT normally distributed\n")

# ── Log-transform for parametric tests ─────────────────────
df_monthly <- df_monthly %>%
  mutate(
    Log_Volume = log(Volume_Mn),
    Log_Value  = log(Value_Cr)
  )

sw_logvol <- shapiro.test(df_monthly$Log_Volume)
cat("\nLog(Volume_Mn) Shapiro-Wilk:\n")
cat("  W =", round(sw_logvol$statistic, 4),
    "| p-value =", format(sw_logvol$p.value, scientific = TRUE), "\n")


# ============================================================
# PART B — TEST 1: Covid vs Non-Covid (Wilcoxon)
# ============================================================

cat("\n=== TEST 1: Covid vs Non-Covid Volume (Wilcoxon) ===\n")

covid_vol    <- df_monthly %>% filter(Is_Covid == TRUE)  %>% pull(Volume_Mn)
noncovid_vol <- df_monthly %>% filter(Is_Covid == FALSE) %>% pull(Volume_Mn)

# Non-parametric test since data is not normally distributed
wilcox_covid <- wilcox.test(covid_vol, noncovid_vol, alternative = "two.sided")
cat("Wilcoxon W =", wilcox_covid$statistic, "\n")
cat("p-value    =", format(wilcox_covid$p.value, scientific = TRUE), "\n")
cat("Significant at 0.05?", ifelse(wilcox_covid$p.value < 0.05, "YES", "NO"), "\n")

# Effect size (rank-biserial correlation)
n1 <- length(covid_vol)
n2 <- length(noncovid_vol)
r_covid <- 1 - (2 * wilcox_covid$statistic) / (n1 * n2)
cat("Effect size (r) =", round(r_covid, 3), "\n")


# ============================================================
# PART C — TEST 2: Festive vs Non-Festive (Wilcoxon)
# ============================================================

cat("\n=== TEST 2: Festive vs Non-Festive Volume (Wilcoxon) ===\n")

festive_vol    <- df_monthly %>% filter(Is_Festive == TRUE)  %>% pull(Volume_Mn)
nonfestive_vol <- df_monthly %>% filter(Is_Festive == FALSE) %>% pull(Volume_Mn)

wilcox_fest <- wilcox.test(festive_vol, nonfestive_vol, alternative = "two.sided")
cat("Wilcoxon W =", wilcox_fest$statistic, "\n")
cat("p-value    =", format(wilcox_fest$p.value, scientific = TRUE), "\n")
cat("Significant at 0.05?", ifelse(wilcox_fest$p.value < 0.05, "YES", "NO"), "\n")

r_fest <- 1 - (2 * wilcox_fest$statistic) / (length(festive_vol) * length(nonfestive_vol))
cat("Effect size (r) =", round(r_fest, 3), "\n")


# ============================================================
# PART D — CORRELATION TESTS
# ============================================================

cat("\n=== CORRELATION TESTS ===\n")

cor_pearson <- cor.test(df_monthly$Log_Volume, df_monthly$Log_Value, method = "pearson")
cat("Pearson r (log-log)  =", round(cor_pearson$estimate, 4), "\n")
cat("p-value              =", format(cor_pearson$p.value, scientific = TRUE), "\n")
cat("95% CI               : [", round(cor_pearson$conf.int[1], 4),
    ",", round(cor_pearson$conf.int[2], 4), "]\n")

cor_spearman <- cor.test(df_monthly$Volume_Mn, df_monthly$Value_Cr, method = "spearman")
cat("\nSpearman rho (original) =", round(cor_spearman$estimate, 4), "\n")
cat("p-value                 =", format(cor_spearman$p.value, scientific = TRUE), "\n")


# ============================================================
# PART E — LINEAR REGRESSION
# ============================================================

cat("\n=== LINEAR REGRESSION: Log(Value) ~ Log(Volume) ===\n")
model1 <- lm(Log_Value ~ Log_Volume, data = df_monthly)
summary(model1)

cat("\n=== MULTIPLE REGRESSION: Log(Value) ~ Log(Volume) + Covid + Festive ===\n")
model2 <- lm(Log_Value ~ Log_Volume + Is_Covid + Is_Festive, data = df_monthly)
summary(model2)

cat("\n=== MODEL DIAGNOSTICS ===\n")
cat("Model 2 R-squared  :", round(summary(model2)$r.squared, 4), "\n")
cat("Model 2 Adj R-sq   :", round(summary(model2)$adj.r.squared, 4), "\n")
cat("Model 2 F-statistic:", round(summary(model2)$fstatistic[1], 2), "\n")

bp_test <- bptest(model2)
cat("\nBreusch-Pagan (heteroscedasticity):\n")
cat("  BP =", round(bp_test$statistic, 3), "| p-value =", round(bp_test$p.value, 4), "\n")


# ============================================================
# PART F — DIAGNOSTIC VISUALIZATIONS
# ============================================================

df_monthly %>%
  mutate(Period = ifelse(Is_Covid, "Covid Period", "Non-Covid Period")) %>%
  ggplot(aes(x = Period, y = Volume_Mn, fill = Period)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2) +
  scale_fill_manual(values = c("Covid Period" = "#E74C3C", "Non-Covid Period" = "#4E79A7")) +
  scale_y_continuous(labels = comma) +
  labs(title = "Volume Distribution: Covid vs Non-Covid Period",
       subtitle = "Wilcoxon test | p < 0.05 = statistically significant difference",
       x = "", y = "Volume (Millions)") +
  theme_minimal() + theme(legend.position = "none")
ggsave("plot22_covid_boxplot.png", width = 8, height = 5)

ggplot(df_monthly, aes(x = Log_Volume, y = Log_Value)) +
  geom_point(color = "#4E79A7", alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", color = "red", se = TRUE, linewidth = 1) +
  labs(title = "Log(Volume) vs Log(Value) — Linear Regression",
       subtitle = "Red line = fitted regression | Shaded area = 95% confidence interval",
       x = "Log(Volume Mn)", y = "Log(Value Cr)") +
  theme_minimal()
ggsave("plot23_regression.png", width = 8, height = 5)

df_monthly$Residuals <- residuals(model2)
df_monthly$Fitted    <- fitted(model2)

ggplot(df_monthly, aes(x = Fitted, y = Residuals)) +
  geom_point(color = "#4E79A7", alpha = 0.6) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs Fitted Values (Model 2)",
       subtitle = "Random scatter around 0 = good model fit",
       x = "Fitted Values", y = "Residuals") +
  theme_minimal()
ggsave("plot24_residuals.png", width = 8, height = 5)

cat("\nHypothesis testing complete — 3 plots saved (plot22 to plot24).\n")
