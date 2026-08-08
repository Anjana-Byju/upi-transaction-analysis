# ============================================================
#  MSc Project: Digital Payment Fraud and Awareness
#  Step 5: Final Visualisations
#  Dissertation-ready charts summarising all key findings
# ============================================================

# ── 0. Load Step 1 (encoding) ────────────────────────────────
source("01_load_and_encode.R")

# install.packages("ggplot2")
# install.packages("tidyr")
# install.packages("dplyr")
library(ggplot2)
library(tidyr)
library(dplyr)


# ============================================================
#  PLOT 13 — Types of Scams Experienced by Victims
# ============================================================

# Extract only victim rows and clean scam types
victims <- df %>% filter(Victim == "Yes")

# Split multi-select scam types into individual entries
scam_list <- strsplit(victims$Scam_Type, ", ")
scam_vector <- unlist(scam_list)

# Remove "No, I have never..." and "Unspecified" entries
scam_vector <- scam_vector[!grepl("No, I have never|Unspecified", scam_vector)]
scam_vector <- trimws(scam_vector)

scam_df <- as.data.frame(table(scam_vector))
colnames(scam_df) <- c("Scam_Type", "Count")
scam_df <- scam_df[order(-scam_df$Count), ]

p13 <- ggplot(scam_df, aes(x = reorder(Scam_Type, Count), y = Count, fill = Scam_Type)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = Count), hjust = -0.3, size = 4) +
  coord_flip() +
  labs(title = "Types of Digital Payment Scams Experienced",
       subtitle = "Among respondents who reported being a victim (n=34)",
       x = "", y = "Number of Respondents") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 9))
print(p13)
ggsave("plot13_scam_types.png", plot = p13, width = 9, height = 5)


# ============================================================
#  PLOT 14 — UPI App Usage Distribution
# ============================================================

# Clean super money capitalisation
df$UPI_App_Clean <- gsub("super money", "Super Money", df$UPI_App, ignore.case = TRUE)

upi_df <- as.data.frame(table(df$UPI_App_Clean))
colnames(upi_df) <- c("App", "Count")
upi_df$Percent <- round(upi_df$Count / 220 * 100, 1)
upi_df <- upi_df[order(-upi_df$Count), ]

p14 <- ggplot(upi_df, aes(x = reorder(App, Count), y = Count, fill = App)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(Count, " (", Percent, "%)")), hjust = -0.2, size = 3.5) +
  coord_flip() +
  labs(title = "UPI App Usage Among Respondents",
       x = "", y = "Number of Respondents") +
  theme_minimal() +
  theme(legend.position = "none")
print(p14)
ggsave("plot14_upi_app_usage.png", plot = p14, width = 7, height = 5)


# ============================================================
#  PLOT 15 — Awareness Score by Gender (Side by Side)
# ============================================================

aware_gender <- df %>%
  group_by(Gender, Awareness_Score) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Gender) %>%
  mutate(Percent = round(Count / sum(Count) * 100, 1))

p15 <- ggplot(aware_gender, aes(x = factor(Awareness_Score),
                                 y = Percent, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  geom_text(aes(label = paste0(Percent, "%")),
            position = position_dodge(width = 0.6),
            vjust = -0.5, size = 3.5) +
  labs(title = "Awareness Score Distribution by Gender",
       x = "Awareness Score (0 = Low, 3 = High)",
       y = "Percentage of Respondents",
       fill = "Gender") +
  theme_minimal()
print(p15)
ggsave("plot15_awareness_by_gender.png", plot = p15, width = 7, height = 5)


# ============================================================
#  PLOT 16 — Victimisation Rate by Age Group
# ============================================================

victim_age <- df %>%
  group_by(Age_Group) %>%
  summarise(
    Total = n(),
    Victims = sum(Victim_Code),
    Rate = round(Victims / Total * 100, 1)
  )

p16 <- ggplot(victim_age, aes(x = Age_Group, y = Rate, fill = Age_Group)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(Rate, "%\n(", Victims, "/", Total, ")")),
            vjust = -0.4, size = 3.5) +
  labs(title = "Fraud Victimisation Rate by Age Group",
       x = "Age Group", y = "Victimisation Rate (%)") +
  theme_minimal() +
  theme(legend.position = "none") +
  ylim(0, 35)
print(p16)
ggsave("plot16_victimisation_by_age.png", plot = p16, width = 7, height = 5)


# ============================================================
#  PLOT 17 — Confidence vs Awareness Score (Scatter)
# ============================================================

p17 <- ggplot(df, aes(x = Awareness_Score, y = Confidence, colour = Gender)) +
  geom_jitter(alpha = 0.5, width = 0.15, height = 0.15, size = 2) +
  geom_smooth(method = "lm", se = TRUE, aes(group = 1),
              colour = "black", linetype = "dashed") +
  labs(title = "Relationship Between Awareness Score and Confidence",
       x = "Awareness Score (0–3)",
       y = "Self-Rated Confidence (1–5)",
       colour = "Gender") +
  theme_minimal()
print(p17)
ggsave("plot17_confidence_vs_awareness.png", plot = p17, width = 7, height = 5)


# ============================================================
#  PLOT 18 — Summary of Significant Findings (Bar Chart)
# ============================================================

findings_df <- data.frame(
  Test = c("Gender → Confidence\n(Mann-Whitney)",
           "Employment → Overall Score\n(Kruskal-Wallis)",
           "Usage Freq → Public Wi-Fi\n(Chi-Square)",
           "App Lock → Overall Score\n(Linear Regression)",
           "Wi-Fi Safety → Overall Score\n(Linear Regression)",
           "PIN Change → Overall Score\n(Linear Regression)",
           "Awareness → Victimisation\n(Logistic Regression)",
           "Confidence → Victimisation\n(Logistic Regression)"),
  P_Value = c(0.001, 0.038, 0.046, 0.000, 0.000, 0.000, 0.000, 0.002),
  Section = c("Inferential", "Inferential", "Inferential",
              "Linear Reg", "Linear Reg", "Linear Reg",
              "Logistic Reg", "Logistic Reg")
)

p18 <- ggplot(findings_df, aes(x = reorder(Test, -P_Value),
                                y = P_Value, fill = Section)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_hline(yintercept = 0.05, linetype = "dashed",
             colour = "red", linewidth = 0.8) +
  annotate("text", x = 1, y = 0.055,
           label = "p = 0.05 threshold", colour = "red", size = 3.5) +
  coord_flip() +
  labs(title = "Summary of All Significant Findings",
       subtitle = "All bars below the red line are statistically significant",
       x = "", y = "p-value", fill = "Analysis Type") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))
print(p18)
ggsave("plot18_significant_findings_summary.png", plot = p18, width = 10, height = 6)


# ============================================================
#  PLOT 19 — Security Practice Adoption Rate
# ============================================================

security_df <- data.frame(
  Practice = c("App Lock", "Regular PIN Change", "Avoids Public Wi-Fi"),
  Adopted  = c(sum(df$AppLock_Code),
               sum(df$PIN_Safe),
               sum(df$WiFi_Safe)),
  Not_Adopted = c(220 - sum(df$AppLock_Code),
                  220 - sum(df$PIN_Safe),
                  220 - sum(df$WiFi_Safe))
)

security_long <- pivot_longer(security_df,
                               cols = c("Adopted", "Not_Adopted"),
                               names_to = "Status",
                               values_to = "Count")
security_long$Status  <- ifelse(security_long$Status == "Adopted",
                                 "Adopted", "Not Adopted")
security_long$Percent <- round(security_long$Count / 220 * 100, 1)

p19 <- ggplot(security_long, aes(x = Practice, y = Percent, fill = Status)) +
  geom_bar(stat = "identity", position = "stack", width = 0.6) +
  geom_text(aes(label = paste0(Percent, "%")),
            position = position_stack(vjust = 0.5), size = 4) +
  scale_fill_manual(values = c("Adopted" = "#2ecc71", "Not Adopted" = "#e74c3c")) +
  labs(title = "Security Practice Adoption Among Respondents",
       x = "", y = "Percentage (%)", fill = "") +
  theme_minimal()
print(p19)
ggsave("plot19_security_adoption.png", plot = p19, width = 7, height = 5)


cat("\nStep 5 complete. 7 final dissertation plots saved.\n")
cat("Files saved: plot13 to plot19\n")
