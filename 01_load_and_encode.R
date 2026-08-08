# ============================================================
#  MSc Project: Digital Payment Fraud and Awareness
#  Step 1: Load, Clean & Encode Dataset
# ============================================================

# ── 1. Install & load required packages ─────────────────────
# Run install lines only once; comment them out after first run
# install.packages("readxl")
# install.packages("dplyr")

library(readxl)
library(dplyr)


# ── 2. Load the dataset ──────────────────────────────────────
df <- read_excel("_Survey_on_Digital_Payment_Behaviour___Fraud_Awareness___4_.xlsx")

# Quick check
dim(df)        # Check row & column count
str(df)        # Overview of column types
head(df, 3)    # First 3 rows


# ── 3. Rename columns to short, clean names ─────────────────
df <- df %>%
  rename(
    Timestamp        = `Timestamp`,
    Age_Group        = `Age Group`,
    Gender           = `Gender`,
    Education        = `Educational Qualification`,
    Employment       = `Employment Status`,
    UPI_App          = `Which UPI app do you use most frequently?`,
    Usage_Freq       = `How often do you use UPI/Mobile Banking for transactions?`,
    Payment_Purpose  = `What is your primary purpose of your digital payments?`,
    Knowledge_Check  = `Knowledge Check: "To receive money via UPI, I need to enter my UPI PIN."  `,
    Scenario1        = `Scenario 1: You receive an SMS saying your bank account is blocked and you must click a link bit.ly/bank-update-2026 to verify your KYC. What do you do?  `,
    Scenario2        = `Scenario 2: A "Customer Care" agent asks you to download an app like AnyDesk or TeamViewer to help you with a failed transaction. Would you do it?   `,
    Confidence       = `On a scale 1 to 5, how confident are you in identifying a digital scam?`,
    App_Lock         = `Do you have a seperate lock(Biometric/Pattern) for your payment apps in addition to your phone lock?`,
    PIN_Change       = `How often do you change your UPI pin?`,
    Public_WiFi      = `Do you use public Wi-Fi (e.g.,at airports or cafes or shops) to make financial transactions?`,
    Victim           = `Have you ever been a victim of a digital payment scam?`,
    Scam_Type        = `If yes, type of scam`
  )


# ── 4. Encode variables ──────────────────────────────────────

# 4a. Ordinal encoding
df <- df %>%
  mutate(
    # Age: ordinal 1–4
    Age_Code = case_when(
      Age_Group == "18-25" ~ 1,
      Age_Group == "25-36" ~ 2,
      Age_Group == "36-50" ~ 3,
      Age_Group == "50+"   ~ 4
    ),

    # Education: ordinal 1–3
    Edu_Code = case_when(
      Education == "Secondary School/High School" ~ 1,
      Education == "UG"                           ~ 2,
      Education == "PG"                           ~ 3
    ),

    # Usage frequency: ordinal 1–4
    Usage_Code = case_when(
      Usage_Freq == "Rarely(Once a month)"  ~ 1,
      Usage_Freq == "A few times a week"    ~ 2,
      Usage_Freq == "Once a day"            ~ 3,
      Usage_Freq == "Multiple times a day"  ~ 4
    ),

    # PIN change: ordinal 1–3
    PIN_Code = case_when(
      PIN_Change == "Never/Only when I forget it" ~ 1,
      PIN_Change == "Every 6 months"              ~ 2,
      PIN_Change == "Every month"                 ~ 3
    ),

    # Public Wi-Fi: ordinal 1–3 (higher = safer)
    WiFi_Code = case_when(
      Public_WiFi == "Yes, frequently" ~ 1,
      Public_WiFi == "Sometimes"       ~ 2,
      Public_WiFi == "Never"           ~ 3
    )
  )

# 4b. Binary encoding
df <- df %>%
  mutate(
    # Gender: Male=0, Female=1
    Gender_Code = ifelse(Gender == "Female", 1, 0),

    # App Lock: No=0, Yes=1
    AppLock_Code = ifelse(App_Lock == "Yes", 1, 0),

    # Victimisation: No=0, Yes=1
    Victim_Code = ifelse(Victim == "Yes", 1, 0),

    # Knowledge Check: correct answer is FALSE (you do NOT need PIN to receive money)
    KC_Score = ifelse(Knowledge_Check == "FALSE" | Knowledge_Check == FALSE, 1, 0),

    # Scenario 1: correct = Ignore the message
    S1_Score = ifelse(grepl("Ignore", Scenario1), 1, 0),

    # Scenario 2: correct = No, these apps can be used to see my screen
    S2_Score = ifelse(grepl("No,", Scenario2), 1, 0)
  )

# 4c. Employment: dummy encoding (Student as reference category)
df <- df %>%
  mutate(
    Emp_Salaried     = ifelse(Employment == "Salaried", 1, 0),
    Emp_SelfEmployed = ifelse(Employment == "Self Employed", 1, 0),
    Emp_Retired      = ifelse(Employment == "Retired", 1, 0)
    # Student = reference (all three above = 0)
  )


# ── 5. Create composite scores ───────────────────────────────

df <- df %>%
  mutate(
    # Awareness Score (0–3): knowledge + 2 scenario responses
    Awareness_Score = KC_Score + S1_Score + S2_Score,

    # Security Score (0–3): app lock + PIN habit + Wi-Fi safety
    # Wi-Fi: Never(3)=safe → recode to binary for score
    WiFi_Safe    = ifelse(Public_WiFi == "Never", 1, 0),
    PIN_Safe     = ifelse(PIN_Change != "Never/Only when I forget it", 1, 0),
    Security_Score = AppLock_Code + PIN_Safe + WiFi_Safe,

    # Overall Score (0–6): combined awareness + security
    Overall_Score = Awareness_Score + Security_Score
  )


# ── 6. Verify encoding ───────────────────────────────────────
cat("\n--- Awareness Score Distribution ---\n")
table(df$Awareness_Score)

cat("\n--- Security Score Distribution ---\n")
table(df$Security_Score)

cat("\n--- Overall Score Distribution ---\n")
table(df$Overall_Score)

cat("\n--- Victim Code ---\n")
table(df$Victim_Code)

cat("\n--- Encoding complete. df is ready for analysis. ---\n")


# ── 7. Save encoded dataset (optional) ──────────────────────
# Saves a CSV you can reload in later scripts without re-running encoding
write.csv(df, "encoded_dataset.csv", row.names = FALSE)
cat("Encoded dataset saved as encoded_dataset.csv\n")
