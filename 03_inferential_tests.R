# ============================================================
#  MSc Project: Digital Payment Fraud and Awareness
#  Step 3: Inferential Tests
#  Objective 2 — Examine whether demographic factors
#               influence fraud awareness and security behaviour
# ============================================================

# ── 0. Load Step 1 (encoding) ────────────────────────────────
source("01_load_and_encode.R")

library(ggplot2)


# ============================================================
#  SECTION A — Chi-Square Tests
#  (Categorical variable vs Categorical variable)
# ============================================================

cat("\n========== CHI-SQUARE TESTS ==========\n")


# ── A1. Gender vs Knowledge Check ───────────────────────────
cat("\n--- A1: Gender vs Knowledge Check ---\n")
tbl_a1 <- table(df$Gender, df$KC_Score)
colnames(tbl_a1) <- c("Incorrect", "Correct")
print(tbl_a1)
chi_a1 <- chisq.test(tbl_a1)
print(chi_a1)
cat("Interpretation:", ifelse(chi_a1$p.value < 0.05,
    "Significant — Gender influences Knowledge Check response.",
    "Not significant — No gender difference in Knowledge Check."), "\n")


# ── A2. Gender vs Scenario 1 ─────────────────────────────────
cat("\n--- A2: Gender vs Scenario 1 (Phishing SMS) ---\n")
tbl_a2 <- table(df$Gender, df$S1_Score)
colnames(tbl_a2) <- c("Incorrect", "Correct")
print(tbl_a2)
chi_a2 <- chisq.test(tbl_a2)
print(chi_a2)
cat("Interpretation:", ifelse(chi_a2$p.value < 0.05,
    "Significant — Gender influences Scenario 1 response.",
    "Not significant — No gender difference in Scenario 1."), "\n")


# ── A3. Gender vs Scenario 2 ─────────────────────────────────
cat("\n--- A3: Gender vs Scenario 2 (Remote Access) ---\n")
tbl_a3 <- table(df$Gender, df$S2_Score)
colnames(tbl_a3) <- c("Incorrect", "Correct")
print(tbl_a3)
chi_a3 <- chisq.test(tbl_a3)
print(chi_a3)
cat("Interpretation:", ifelse(chi_a3$p.value < 0.05,
    "Significant — Gender influences Scenario 2 response.",
    "Not significant — No gender difference in Scenario 2."), "\n")


# ── A4. Education vs Knowledge Check ─────────────────────────
cat("\n--- A4: Education vs Knowledge Check ---\n")
tbl_a4 <- table(df$Education, df$KC_Score)
colnames(tbl_a4) <- c("Incorrect", "Correct")
print(tbl_a4)
chi_a4 <- chisq.test(tbl_a4)
print(chi_a4)
cat("Interpretation:", ifelse(chi_a4$p.value < 0.05,
    "Significant — Education level influences Knowledge Check.",
    "Not significant — Education has no effect on Knowledge Check."), "\n")


# ── A5. Age Group vs Victimisation ───────────────────────────
cat("\n--- A5: Age Group vs Victimisation ---\n")
tbl_a5 <- table(df$Age_Group, df$Victim)
print(tbl_a5)
chi_a5 <- chisq.test(tbl_a5)
print(chi_a5)
cat("Interpretation:", ifelse(chi_a5$p.value < 0.05,
    "Significant — Age group is associated with victimisation.",
    "Not significant — Age group has no association with victimisation."), "\n")


# ── A6. Employment vs App Lock ────────────────────────────────
cat("\n--- A6: Employment Status vs App Lock ---\n")
tbl_a6 <- table(df$Employment, df$App_Lock)
print(tbl_a6)
chi_a6 <- chisq.test(tbl_a6)
print(chi_a6)
cat("Interpretation:", ifelse(chi_a6$p.value < 0.05,
    "Significant — Employment status influences app lock behaviour.",
    "Not significant — Employment has no effect on app lock."), "\n")


# ── A7. Usage Frequency vs Public Wi-Fi ──────────────────────
cat("\n--- A7: Usage Frequency vs Public Wi-Fi Usage ---\n")
tbl_a7 <- table(df$Usage_Freq, df$Public_WiFi)
print(tbl_a7)
chi_a7 <- chisq.test(tbl_a7)
print(chi_a7)
cat("Interpretation:", ifelse(chi_a7$p.value < 0.05,
    "Significant — Usage frequency is associated with public Wi-Fi behaviour.",
    "Not significant — No association between usage frequency and Wi-Fi."), "\n")


# ============================================================
#  SECTION B — Kruskal-Wallis Test
#  (Ordinal/continuous DV vs 3+ group categorical IV)
# ============================================================

cat("\n========== KRUSKAL-WALLIS TESTS ==========\n")


# ── B1. Age Group vs Confidence Score ────────────────────────
cat("\n--- B1: Age Group vs Confidence Score ---\n")
kw_b1 <- kruskal.test(Confidence ~ Age_Group, data = df)
print(kw_b1)
cat("Interpretation:", ifelse(kw_b1$p.value < 0.05,
    "Significant — Confidence score differs across age groups.",
    "Not significant — Confidence score is similar across age groups."), "\n")

# Group medians
cat("Median Confidence by Age Group:\n")
print(tapply(df$Confidence, df$Age_Group, median))


# ── B2. Education vs Confidence Score ────────────────────────
cat("\n--- B2: Education vs Confidence Score ---\n")
kw_b2 <- kruskal.test(Confidence ~ Education, data = df)
print(kw_b2)
cat("Interpretation:", ifelse(kw_b2$p.value < 0.05,
    "Significant — Confidence score differs across education levels.",
    "Not significant — Education has no effect on confidence score."), "\n")

cat("Median Confidence by Education:\n")
print(tapply(df$Confidence, df$Education, median))


# ── B3. Age Group vs Awareness Score ─────────────────────────
cat("\n--- B3: Age Group vs Awareness Score ---\n")
kw_b3 <- kruskal.test(Awareness_Score ~ Age_Group, data = df)
print(kw_b3)
cat("Interpretation:", ifelse(kw_b3$p.value < 0.05,
    "Significant — Awareness score differs across age groups.",
    "Not significant — Awareness score is similar across age groups."), "\n")

cat("Median Awareness Score by Age Group:\n")
print(tapply(df$Awareness_Score, df$Age_Group, median))


# ── B4. Employment vs Overall Score ──────────────────────────
cat("\n--- B4: Employment Status vs Overall Score ---\n")
kw_b4 <- kruskal.test(Overall_Score ~ Employment, data = df)
print(kw_b4)
cat("Interpretation:", ifelse(kw_b4$p.value < 0.05,
    "Significant — Overall score differs across employment groups.",
    "Not significant — Employment has no effect on overall score."), "\n")

cat("Median Overall Score by Employment:\n")
print(tapply(df$Overall_Score, df$Employment, median))


# ============================================================
#  SECTION C — Mann-Whitney U Test
#  (Ordinal/continuous DV vs 2-group categorical IV)
# ============================================================

cat("\n========== MANN-WHITNEY U TESTS ==========\n")


# ── C1. Gender vs Confidence Score ───────────────────────────
cat("\n--- C1: Gender vs Confidence Score ---\n")
mw_c1 <- wilcox.test(Confidence ~ Gender, data = df)
print(mw_c1)
cat("Interpretation:", ifelse(mw_c1$p.value < 0.05,
    "Significant — Confidence score differs between males and females.",
    "Not significant — No gender difference in confidence score."), "\n")

cat("Median Confidence by Gender:\n")
print(tapply(df$Confidence, df$Gender, median))


# ── C2. Gender vs Awareness Score ────────────────────────────
cat("\n--- C2: Gender vs Awareness Score ---\n")
mw_c2 <- wilcox.test(Awareness_Score ~ Gender, data = df)
print(mw_c2)
cat("Interpretation:", ifelse(mw_c2$p.value < 0.05,
    "Significant — Awareness score differs between males and females.",
    "Not significant — No gender difference in awareness score."), "\n")

cat("Median Awareness Score by Gender:\n")
print(tapply(df$Awareness_Score, df$Gender, median))


# ── C3. App Lock vs Confidence Score ─────────────────────────
cat("\n--- C3: App Lock vs Confidence Score ---\n")
mw_c3 <- wilcox.test(Confidence ~ App_Lock, data = df)
print(mw_c3)
cat("Interpretation:", ifelse(mw_c3$p.value < 0.05,
    "Significant — Confidence differs between those with/without app lock.",
    "Not significant — App lock has no association with confidence."), "\n")


# ============================================================
#  SECTION D — Spearman Correlation
#  (Ordinal vs Ordinal)
# ============================================================

cat("\n========== SPEARMAN CORRELATIONS ==========\n")


# ── D1. Usage Frequency vs Confidence Score ──────────────────
cat("\n--- D1: Usage Frequency vs Confidence Score ---\n")
sp_d1 <- cor.test(df$Usage_Code, df$Confidence, method = "spearman")
print(sp_d1)
cat("Interpretation:", ifelse(sp_d1$p.value < 0.05,
    paste("Significant — rho =", round(sp_d1$estimate, 3),
          "— Usage frequency correlates with confidence."),
    "Not significant — No correlation between usage frequency and confidence."), "\n")


# ── D2. Usage Frequency vs Awareness Score ───────────────────
cat("\n--- D2: Usage Frequency vs Awareness Score ---\n")
sp_d2 <- cor.test(df$Usage_Code, df$Awareness_Score, method = "spearman")
print(sp_d2)
cat("Interpretation:", ifelse(sp_d2$p.value < 0.05,
    paste("Significant — rho =", round(sp_d2$estimate, 3)),
    "Not significant — No correlation between usage frequency and awareness."), "\n")


# ── D3. Education vs Awareness Score ─────────────────────────
cat("\n--- D3: Education vs Awareness Score ---\n")
sp_d3 <- cor.test(df$Edu_Code, df$Awareness_Score, method = "spearman")
print(sp_d3)
cat("Interpretation:", ifelse(sp_d3$p.value < 0.05,
    paste("Significant — rho =", round(sp_d3$estimate, 3),
          "— Higher education correlates with awareness."),
    "Not significant — Education does not correlate with awareness."), "\n")


# ============================================================
#  SECTION E — Visualisations
# ============================================================

# ── Plot 6: Confidence Score by Gender ───────────────────────
p6 <- ggplot(df, aes(x = Gender, y = Confidence, fill = Gender)) +
  geom_boxplot(width = 0.5, outlier.colour = "red") +
  labs(title = "Confidence Score by Gender",
       x = "Gender", y = "Confidence Score (1-5)") +
  theme_minimal() +
  theme(legend.position = "none")
print(p6)
ggsave("plot6_confidence_by_gender.png", plot = p6, width = 6, height = 5)


# ── Plot 7: Awareness Score by Age Group ─────────────────────
p7 <- ggplot(df, aes(x = Age_Group, y = Awareness_Score, fill = Age_Group)) +
  geom_boxplot(width = 0.5, outlier.colour = "red") +
  labs(title = "Awareness Score by Age Group",
       x = "Age Group", y = "Awareness Score (0-3)") +
  theme_minimal() +
  theme(legend.position = "none")
print(p7)
ggsave("plot7_awareness_by_age.png", plot = p7, width = 7, height = 5)


# ── Plot 8: Confidence Score by Education ────────────────────
p8 <- ggplot(df, aes(x = Education, y = Confidence, fill = Education)) +
  geom_boxplot(width = 0.5, outlier.colour = "red") +
  labs(title = "Confidence Score by Education Level",
       x = "Education", y = "Confidence Score (1-5)") +
  theme_minimal() +
  theme(legend.position = "none")
print(p8)
ggsave("plot8_confidence_by_education.png", plot = p8, width = 7, height = 5)


# ── Plot 9: Overall Score by Employment ──────────────────────
p9 <- ggplot(df, aes(x = Employment, y = Overall_Score, fill = Employment)) +
  geom_boxplot(width = 0.5, outlier.colour = "red") +
  labs(title = "Overall Awareness + Security Score by Employment Status",
       x = "Employment Status", y = "Overall Score (0-6)") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 15, hjust = 1))
print(p9)
ggsave("plot9_overall_by_employment.png", plot = p9, width = 7, height = 5)

cat("\nStep 3 complete. All inferential tests done and 4 plots saved.\n")
