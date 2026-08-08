# ============================================================
# UPI ANALYSIS — SETUP: LOAD & CLEAN DATA
# ============================================================
# Run this script first. Every other script in this repo
# sources this file to get df_monthly and df_fraud ready to use.
#
# Required packages (install once, if not already installed):
# install.packages(c("readxl", "dplyr", "lubridate", "tidyr"))

library(readxl)
library(dplyr)
library(lubridate)
library(tidyr)

# ── 1. Load the raw dataset ────────────────────────────────
# Set your working directory to the folder containing the data file
# setwd("path/to/your/data")
df_raw <- read_xlsx("combined_upi_dataset.xlsx")

# ── 2. Select & rename columns ─────────────────────────────
df <- df_raw %>%
  select(
    Financial_Year,
    Date,
    Year,
    Month_Num,
    Month_Name,
    Volume_Mn,
    Value_Cr,
    Volume_MoM     = `Volume_MoM_%`,
    Value_MoM      = `Value_MoM_%`,
    Volume_YoY     = `Volume_YoY_%`,
    Value_YoY      = `Value_YoY_%`,
    Volume_Roll3M  = Volume_RollMean_3M,
    Value_Roll3M   = Value_RollMean_3M,
    Is_Covid       = Is_Covid_Period,
    Is_Festive     = Is_Festive_Season,
    Fraud_Cases    = UPI_Fraud_Cases,
    Fraud_Value_Cr = UPI_Fraud_Value_Cr,
    Avg_Loss_Rs    = Avg_Loss_Per_Case_Rs
  )

# ── 3. Fix data types ───────────────────────────────────────
# Date is rebuilt from Year + Month_Num, which are clean numeric
# columns with no parsing ambiguity (avoids DD-MM-YYYY vs
# MM-DD-YYYY issues in the original Date column).
df <- df %>%
  mutate(
    Date           = as.Date(paste(Year, Month_Num, "01", sep = "-")),
    Month_Name     = factor(Month_Name, levels = month.name, ordered = TRUE),
    Is_Covid       = as.logical(Is_Covid),
    Is_Festive     = as.logical(Is_Festive),
    Financial_Year = as.factor(Financial_Year),
    Volume_MoM     = suppressWarnings(as.numeric(Volume_MoM)),
    Value_MoM      = suppressWarnings(as.numeric(Value_MoM)),
    Volume_YoY     = suppressWarnings(as.numeric(Volume_YoY)),
    Value_YoY      = suppressWarnings(as.numeric(Value_YoY))
  )

# ── 4. Remove pre-UPI rows (Jan–Jul 2016, volume = 0) ──────
df_clean <- df %>%
  filter(!(Year == 2016 & Month_Num <= 7))

# ── 5. Split into two analysis-ready tables ─────────────────

# (a) Monthly transaction data — all rows, no fraud columns
df_monthly <- df_clean %>%
  select(Financial_Year, Date, Year, Month_Num, Month_Name,
         Volume_Mn, Value_Cr,
         Volume_MoM, Value_MoM, Volume_YoY, Value_YoY,
         Volume_Roll3M, Value_Roll3M,
         Is_Covid, Is_Festive)

# (b) Annual fraud data — one row per financial year
#     (fraud figures repeat across months, so we take distinct values)
df_fraud <- df_clean %>%
  filter(!is.na(Fraud_Cases) | !is.na(Fraud_Value_Cr)) %>%
  distinct(Financial_Year, Fraud_Cases, Fraud_Value_Cr, Avg_Loss_Rs)

# ── 6. Sanity checks ─────────────────────────────────────────
cat("df_monthly:", nrow(df_monthly), "rows\n")
cat("df_fraud  :", nrow(df_fraud), "rows\n")
cat("Date range:", format(min(df_monthly$Date)), "to",
    format(max(df_monthly$Date)), "\n")
cat("Missing Volume_Mn:", sum(is.na(df_monthly$Volume_Mn)), "\n")
cat("Missing Value_Cr :", sum(is.na(df_monthly$Value_Cr)), "\n")

cat("\nSetup complete — df_monthly and df_fraud are ready to use.\n")
