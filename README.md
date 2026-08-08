# UPI in India: A Dual-Data Statistical Analysis of National Transaction Trends and User Fraud Awareness

MSc Statistics dissertation (University of Kerala) analyzing the growth of UPI (Unified Payments Interface) transactions in India and user fraud awareness, using a combination of national secondary data and a primary field survey.

This repository contains the R code for both halves of the dissertation:
- **Secondary data analysis** — national UPI transaction trends, seasonality, fraud statistics, hypothesis testing, and time-series forecasting
- **Primary survey analysis** — a 275-respondent survey on digital payment fraud awareness and security behaviour

## Overview

### Secondary Analysis
Uses monthly national UPI transaction data (August 2016 – December 2025) to:
- Characterize growth trends and seasonality
- Quantify the relationship between transaction volume and value
- Test for statistically significant differences across Covid / non-Covid and festive / non-festive periods
- Analyze the growth of UPI fraud cases and value over time
- Forecast future transaction volume using ARIMA and ETS models

### Primary Survey Analysis
Uses a structured survey of digital payment users to:
- Profile respondent demographics and UPI usage habits
- Measure fraud awareness through a knowledge check and two scam-scenario questions
- Test whether demographic factors (age, gender, education, employment) are associated with awareness and security behaviour
- Model predictors of overall awareness/security score (multiple linear regression) and fraud victimisation (binary logistic regression)

## Key Findings

**Secondary data:**
- Identified a three-phase UPI growth pattern over 2016–2025
- Built an ARIMA(0,2,5) forecasting model achieving a **3.80% MAPE** (Mean Absolute Percentage Error)
- Found a statistically significant difference in transaction volume between Covid and non-Covid periods (Wilcoxon test)
- Documented a sharp rise in reported UPI fraud case volume and value alongside overall transaction growth

**Primary survey:**
- Identified significant associations between demographic factors (gender, employment, usage frequency) and fraud awareness/security behaviour
- Awareness score and self-rated confidence were significant predictors of fraud victimisation in the logistic regression model
- App lock adoption, PIN-change habits, and public Wi-Fi avoidance were all significant predictors of the overall awareness + security score

## Repository Structure

```
├── data-cleaning/
│   ├── 00_setup_and_clean.R          # Load & clean the raw secondary dataset — run this first
│   └── 01_eda_descriptive_stats.R    # Descriptive statistics & exploratory visualizations
├── growth-and-seasonality/
│   ├── 02_growth_trend_analysis.R    # YoY/MoM growth rates, rolling averages, key events
│   └── 03_seasonality_analysis.R     # Monthly seasonality, seasonal index, heatmaps
├── fraud-analysis/
│   └── 04_fraud_analysis.R           # Fraud case/value trends, fraud as % of total UPI value
├── hypothesis-testing/
│   └── 05_statistical_tests.R        # Normality, Wilcoxon tests, correlation, regression
├── forecasting/
│   └── 06_arima_ets_forecasting.R    # Stationarity tests, STL decomposition, ARIMA & ETS forecasts
├── primary-survey-analysis/
│   ├── 01_load_and_encode.R          # Load, clean & encode the survey dataset — run this first
│   ├── 02_descriptive_statistics.R   # Respondent profile, awareness & security practice summaries
│   ├── 03_inferential_tests.R        # Chi-square, Kruskal-Wallis, Mann-Whitney, Spearman tests
│   ├── 04_regression_analysis.R      # Multiple linear regression & binary logistic regression
│   └── 05_final_visualisations.R     # Dissertation-ready summary charts
└── README.md
```

## How to Run

**Secondary analysis:**
1. Open `data-cleaning/00_setup_and_clean.R`, set your working directory to the folder containing `combined_upi_dataset.xlsx`, and run it. This creates `df_monthly` and `df_fraud`, which every other secondary-analysis script depends on.
2. Run the remaining scripts in numeric order (01 → 06).

**Primary survey analysis:**
1. Open `primary-survey-analysis/01_load_and_encode.R`, set your working directory to the folder containing the survey Excel file, and run it. This creates the encoded `df` used by every other script in this folder.
2. Run the remaining scripts in numeric order (02 → 05). Each one sources `01_load_and_encode.R` automatically.

> Note: the underlying datasets are not included in this repository. These scripts are shared for code reference; get in touch if you'd like access to the underlying data.

## Tools

- **R** — data cleaning, statistical modeling, and visualization
- Key packages: `dplyr`, `ggplot2`, `forecast`, `tseries`, `lmtest`, `car`, `corrplot`, `tidyr`
- Key techniques: time-series forecasting (ARIMA, ETS, STL decomposition), non-parametric hypothesis testing (Wilcoxon, Kruskal-Wallis, Mann-Whitney, Chi-square), correlation & regression analysis (linear & logistic), heteroscedasticity diagnostics (Breusch-Pagan)

## Author

**Anjana Byju**
MSc Statistics, University of Kerala
anjanabyju723@gmail.com
