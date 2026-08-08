# ============================================================
#  MSc Project: Digital Payment Fraud and Awareness
#  Step 2: Descriptive Statistics
#  Objective 1 — Assess the level of digital payment fraud
#               awareness among UPI users
# ============================================================

# ── 0. Load Step 1 (encoding) ────────────────────────────────
source("01_load_and_encode.R")

# install.packages("ggplot2")
# install.packages("scales")
library(ggplot2)
library(scales)


# ============================================================
#  SECTION A — Respondent Profile
# ============================================================

cat("\n========== RESPONDENT PROFILE ==========\n")

# Age Group
cat("\n--- Age Group ---\n")
print(table(df$Age_Group))
print(round(prop.table(table(df$Age_Group)) * 100, 1))

# Gender
cat("\n--- Gender ---\n")
print(table(df$Gender))
print(round(prop.table(table(df$Gender)) * 100, 1))

# Education
cat("\n--- Education ---\n")
print(table(df$Education))
print(round(prop.table(table(df$Education)) * 100, 1))

# Employment
cat("\n--- Employment Status ---\n")
print(table(df$Employment))
print(round(prop.table(table(df$Employment)) * 100, 1))

# Usage Frequency
cat("\n--- UPI Usage Frequency ---\n")
print(table(df$Usage_Freq))
print(round(prop.table(table(df$Usage_Freq)) * 100, 1))


# ============================================================
#  SECTION B — Fraud Awareness
# ============================================================

cat("\n========== FRAUD AWARENESS ==========\n")

# Knowledge Check
cat("\n--- Knowledge Check (UPI PIN myth) ---\n")
cat("Correct (False): ", sum(df$KC_Score), "/220 =",
    round(mean(df$KC_Score) * 100, 1), "%\n")
cat("Incorrect (True):", sum(1 - df$KC_Score), "/220 =",
    round((1 - mean(df$KC_Score)) * 100, 1), "%\n")

# Scenario 1
cat("\n--- Scenario 1 (Phishing SMS) ---\n")
print(table(df$Scenario1))
print(round(prop.table(table(df$Scenario1)) * 100, 1))

# Scenario 2
cat("\n--- Scenario 2 (Remote Access App) ---\n")
print(table(df$Scenario2))
print(round(prop.table(table(df$Scenario2)) * 100, 1))

# Confidence Score
cat("\n--- Self-Rated Confidence (1-5) ---\n")
print(table(df$Confidence))
cat("Mean:", round(mean(df$Confidence), 2), "\n")
cat("SD  :", round(sd(df$Confidence), 2), "\n")
cat("Median:", median(df$Confidence), "\n")


# ============================================================
#  SECTION C — Security Practices
# ============================================================

cat("\n========== SECURITY PRACTICES ==========\n")

# App Lock
cat("\n--- App Lock ---\n")
print(table(df$App_Lock))
print(round(prop.table(table(df$App_Lock)) * 100, 1))

# PIN Change
cat("\n--- PIN Change Frequency ---\n")
print(table(df$PIN_Change))
print(round(prop.table(table(df$PIN_Change)) * 100, 1))

# Public Wi-Fi
cat("\n--- Public Wi-Fi Usage ---\n")
print(table(df$Public_WiFi))
print(round(prop.table(table(df$Public_WiFi)) * 100, 1))

# Victimisation
cat("\n--- Fraud Victimisation ---\n")
print(table(df$Victim))
print(round(prop.table(table(df$Victim)) * 100, 1))


# ============================================================
#  SECTION D — Composite Scores Summary
# ============================================================

cat("\n========== COMPOSITE SCORES ==========\n")

cat("\n--- Awareness Score (0-3) ---\n")
print(table(df$Awareness_Score))
cat("Mean:", round(mean(df$Awareness_Score), 2), "\n")
cat("SD  :", round(sd(df$Awareness_Score), 2), "\n")

cat("\n--- Security Score (0-3) ---\n")
print(table(df$Security_Score))
cat("Mean:", round(mean(df$Security_Score), 2), "\n")
cat("SD  :", round(sd(df$Security_Score), 2), "\n")

cat("\n--- Overall Score (0-6) ---\n")
print(table(df$Overall_Score))
cat("Mean:", round(mean(df$Overall_Score), 2), "\n")
cat("SD  :", round(sd(df$Overall_Score), 2), "\n")


# ============================================================
#  SECTION E — Visualisations
# ============================================================

# ── Plot 1: Age Group Distribution ──────────────────────────
age_df <- as.data.frame(table(df$Age_Group))
colnames(age_df) <- c("Age_Group", "Count")
age_df$Percent <- round(age_df$Count / 220 * 100, 1)

ggplot(age_df, aes(x = Age_Group, y = Count, fill = Age_Group)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(Percent, "%")), vjust = -0.5, size = 4) +
  labs(title = "Distribution of Respondents by Age Group",
       x = "Age Group", y = "Number of Respondents") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("plot1_age_distribution.png", width = 7, height = 5)


# ── Plot 2: Gender Distribution (Pie Chart) ──────────────────
gender_df <- as.data.frame(table(df$Gender))
colnames(gender_df) <- c("Gender", "Count")
gender_df$Percent <- round(gender_df$Count / 220 * 100, 1)
gender_df$Label <- paste0(gender_df$Gender, "\n", gender_df$Percent, "%")

ggplot(gender_df, aes(x = "", y = Count, fill = Gender)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5), size = 4) +
  labs(title = "Gender Distribution of Respondents") +
  theme_void() +
  theme(legend.position = "none")

ggsave("plot2_gender_distribution.png", width = 6, height = 6)


# ── Plot 3: Confidence Score Distribution ───────────────────
conf_df <- as.data.frame(table(df$Confidence))
colnames(conf_df) <- c("Score", "Count")
conf_df$Percent <- round(conf_df$Count / 220 * 100, 1)

ggplot(conf_df, aes(x = Score, y = Count, fill = Score)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(Percent, "%")), vjust = -0.5, size = 4) +
  labs(title = "Self-Rated Confidence in Identifying Digital Scams",
       x = "Confidence Score (1 = Low, 5 = High)",
       y = "Number of Respondents") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("plot3_confidence_distribution.png", width = 7, height = 5)


# ── Plot 4: Awareness Score Distribution ────────────────────
aware_df <- as.data.frame(table(df$Awareness_Score))
colnames(aware_df) <- c("Score", "Count")
aware_df$Percent <- round(aware_df$Count / 220 * 100, 1)

ggplot(aware_df, aes(x = Score, y = Count, fill = Score)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(Count, "\n(", Percent, "%)")), vjust = -0.4, size = 4) +
  labs(title = "Composite Awareness Score Distribution",
       subtitle = "Score based on Knowledge Check + Scenario 1 + Scenario 2",
       x = "Awareness Score (0 = Low, 3 = High)",
       y = "Number of Respondents") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("plot4_awareness_score.png", width = 7, height = 5)


# ── Plot 5: Security Practices Overview ─────────────────────
security_items <- data.frame(
  Practice  = c("Has App Lock", "Changes PIN Regularly", "Avoids Public Wi-Fi"),
  Yes_Count = c(sum(df$AppLock_Code), sum(df$PIN_Safe), sum(df$WiFi_Safe)),
  No_Count  = c(220 - sum(df$AppLock_Code), 220 - sum(df$PIN_Safe), 220 - sum(df$WiFi_Safe))
)

security_long <- tidyr::pivot_longer(security_items,
                                      cols = c("Yes_Count", "No_Count"),
                                      names_to = "Response",
                                      values_to = "Count")
security_long$Response <- ifelse(security_long$Response == "Yes_Count", "Yes", "No")
security_long$Percent  <- round(security_long$Count / 220 * 100, 1)

# install.packages("tidyr") if pivot_longer not found
library(tidyr)

ggplot(security_long, aes(x = Practice, y = Count, fill = Response)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  geom_text(aes(label = paste0(Percent, "%")),
            position = position_dodge(width = 0.6), vjust = -0.5, size = 3.5) +
  labs(title = "Security Practices Among Respondents",
       x = "", y = "Number of Respondents", fill = "Response") +
  theme_minimal()

ggsave("plot5_security_practices.png", width = 8, height = 5)

cat("\nStep 2 complete. All descriptive stats printed and 5 plots saved.\n")
