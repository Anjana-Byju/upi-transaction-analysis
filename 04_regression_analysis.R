# ============================================================
#  MSc Project: Digital Payment Fraud and Awareness
#  Step 4: Regression Analysis
#  Objective 3 — Multiple Linear Regression
#               (Predict Overall Awareness + Security Score)
#  Objective 4 — Binary Logistic Regression
#               (Predict Fraud Victimisation)
# ============================================================

# ── 0. Load Step 1 (encoding) ────────────────────────────────
source("01_load_and_encode.R")

# install.packages("ggplot2")
# install.packages("car")        # for VIF multicollinearity check
library(ggplot2)
library(car)


# ============================================================
#  SECTION A — Multiple Linear Regression
#  Dependent Variable : Overall_Score (0–6)
#  Independent Variables: Demographics + Security Behaviour
# ============================================================

cat("\n========== MULTIPLE LINEAR REGRESSION ==========\n")
cat("Dependent Variable: Overall Score (Awareness + Security)\n")

# ── A1. Build the model ──────────────────────────────────────
lm_model <- lm(Overall_Score ~ Gender_Code +
                                Age_Code +
                                Edu_Code +
                                Usage_Code +
                                AppLock_Code +
                                PIN_Code +
                                WiFi_Code,
               data = df)

# ── A2. Model Summary ────────────────────────────────────────
cat("\n--- Linear Regression Model Summary ---\n")
print(summary(lm_model))

# ── A3. Extract key metrics ──────────────────────────────────
lm_sum <- summary(lm_model)
cat("\nR-squared     :", round(lm_sum$r.squared, 4))
cat("\nAdj R-squared :", round(lm_sum$adj.r.squared, 4))
cat("\nF-statistic p :", round(pf(lm_sum$fstatistic[1],
                                  lm_sum$fstatistic[2],
                                  lm_sum$fstatistic[3],
                                  lower.tail = FALSE), 4), "\n")

# ── A4. Check multicollinearity (VIF) ────────────────────────
cat("\n--- Variance Inflation Factor (VIF) ---\n")
cat("VIF > 5 indicates multicollinearity concern\n")
print(vif(lm_model))

# ── A5. Check model assumptions ──────────────────────────────
cat("\n--- Checking Model Assumptions ---\n")

# Normality of residuals
shapiro_test <- shapiro.test(lm_model$residuals)
cat("Shapiro-Wilk test for normality of residuals:\n")
print(shapiro_test)
cat("Interpretation:", ifelse(shapiro_test$p.value > 0.05,
    "Residuals are normally distributed (assumption met).",
    "Residuals deviate from normality — interpret results with caution."), "\n")

# ── A6. Diagnostic Plots ─────────────────────────────────────
par(mfrow = c(2, 2))  # 2x2 grid of plots
plot(lm_model)
par(mfrow = c(1, 1))  # Reset

# ── A7. Coefficients table (clean) ───────────────────────────
cat("\n--- Coefficients Interpretation ---\n")
coef_df <- as.data.frame(summary(lm_model)$coefficients)
coef_df$Significant <- ifelse(coef_df$`Pr(>|t|)` < 0.05, "Yes *", "No")
print(round(coef_df, 4))

# ── A8. Visualise significant predictors ─────────────────────
# Plot: Actual vs Predicted Overall Score
df$Predicted_Score <- predict(lm_model)

p10 <- ggplot(df, aes(x = Predicted_Score, y = Overall_Score)) +
  geom_point(alpha = 0.4, colour = "steelblue") +
  geom_smooth(method = "lm", colour = "red", se = TRUE) +
  labs(title = "Actual vs Predicted Overall Score",
       subtitle = "Multiple Linear Regression",
       x = "Predicted Overall Score",
       y = "Actual Overall Score") +
  theme_minimal()
print(p10)
ggsave("plot10_actual_vs_predicted.png", plot = p10, width = 7, height = 5)


# ============================================================
#  SECTION B — Binary Logistic Regression
#  Dependent Variable : Victim_Code (0 = No, 1 = Yes)
#  Independent Variables: Demographics + Awareness + Security
# ============================================================

cat("\n\n========== BINARY LOGISTIC REGRESSION ==========\n")
cat("Dependent Variable: Victimisation (0 = No, 1 = Yes)\n")

# ── B1. Build the model ──────────────────────────────────────
log_model <- glm(Victim_Code ~ Gender_Code +
                                Age_Code +
                                Edu_Code +
                                Usage_Code +
                                Awareness_Score +
                                Security_Score +
                                Confidence,
                 data = df,
                 family = binomial(link = "logit"))

# ── B2. Model Summary ────────────────────────────────────────
cat("\n--- Logistic Regression Model Summary ---\n")
print(summary(log_model))

# ── B3. Odds Ratios ──────────────────────────────────────────
cat("\n--- Odds Ratios (Exponentiated Coefficients) ---\n")
cat("OR > 1 = increases odds of victimisation\n")
cat("OR < 1 = decreases odds of victimisation\n\n")
odds_ratios <- exp(cbind(OR = coef(log_model),
                          confint(log_model)))
print(round(odds_ratios, 4))

# ── B4. Model fit — Pseudo R-squared ─────────────────────────
cat("\n--- Model Fit ---\n")
null_model <- glm(Victim_Code ~ 1, data = df,
                  family = binomial(link = "logit"))

mcfadden_r2 <- 1 - (logLik(log_model) / logLik(null_model))
cat("McFadden Pseudo R-squared:", round(mcfadden_r2, 4), "\n")
cat("Interpretation:",
    ifelse(mcfadden_r2 > 0.2,
           "Good model fit (> 0.2)",
           ifelse(mcfadden_r2 > 0.1,
                  "Acceptable model fit (0.1–0.2)",
                  "Weak model fit (< 0.1)")), "\n")

# ── B5. Classification accuracy ──────────────────────────────
cat("\n--- Classification Accuracy ---\n")
predicted_prob  <- predict(log_model, type = "response")
predicted_class <- ifelse(predicted_prob > 0.5, 1, 0)
conf_matrix     <- table(Actual = df$Victim_Code,
                          Predicted = predicted_class)
print(conf_matrix)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Overall Accuracy:", round(accuracy * 100, 1), "%\n")

# ── B6. Coefficients table (clean) ───────────────────────────
cat("\n--- Coefficients Interpretation ---\n")
log_coef <- as.data.frame(summary(log_model)$coefficients)
log_coef$Significant <- ifelse(log_coef$`Pr(>|z|)` < 0.05, "Yes *", "No")
print(round(log_coef, 4))

# ── B7. Visualise predicted probability of victimisation ─────

# By Awareness Score
p11 <- ggplot(df, aes(x = Awareness_Score,
                       y = predict(log_model, type = "response"))) +
  geom_jitter(alpha = 0.3, width = 0.1, colour = "steelblue") +
  geom_smooth(method = "loess", colour = "red", se = TRUE) +
  labs(title = "Predicted Probability of Victimisation",
       subtitle = "By Awareness Score",
       x = "Awareness Score (0–3)",
       y = "Predicted Probability of Being a Victim") +
  theme_minimal()
print(p11)
ggsave("plot11_victimisation_by_awareness.png", plot = p11, width = 7, height = 5)

# By Security Score
p12 <- ggplot(df, aes(x = Security_Score,
                       y = predict(log_model, type = "response"))) +
  geom_jitter(alpha = 0.3, width = 0.1, colour = "darkorange") +
  geom_smooth(method = "loess", colour = "red", se = TRUE) +
  labs(title = "Predicted Probability of Victimisation",
       subtitle = "By Security Score",
       x = "Security Score (0–3)",
       y = "Predicted Probability of Being a Victim") +
  theme_minimal()
print(p12)
ggsave("plot12_victimisation_by_security.png", plot = p12, width = 7, height = 5)

cat("\nStep 4 complete. Both regression models done and 3 plots saved.\n")
cat("Check coefficients marked 'Yes *' — these are your significant predictors.\n")
