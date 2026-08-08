# UPI in India: A Dual-Data Statistical Analysis of National Transaction Trends and User Fraud Awareness

MSc Statistics dissertation (University of Kerala) analyzing the growth of UPI (Unified Payments Interface) transactions in India and user fraud awareness, using a combination of national secondary data and a primary field survey.

This repository contains the R code for the **secondary data analysis** — national UPI transaction trends, seasonality, fraud statistics, hypothesis testing, and time-series forecasting.

## Overview

The secondary analysis uses monthly national UPI transaction data (August 2016 – December 2025) to:
- Characterize growth trends and seasonality
- Quantify the relationship between transaction volume and value
- Test for statistically significant differences across Covid / non-Covid and festive / non-festive periods
- Analyze the growth of UPI fraud cases and value over time
- Forecast future transaction volume using ARIMA and ETS models

## Key Findings

- Identified a three-phase UPI growth pattern over 2016–2025
- Built an ARIMA(0,2,5) forecasting model achieving a **3.80% MAPE** (Mean Absolute Percentage Error)
- Found a statistically significant difference in transaction volume between Covid and non-Covid periods (Wilcoxon test)
- Documented a sharp rise in reported UPI fraud case volume and value alongside overall transaction growth

## Repository Structure

```
├── data-cleaning/
│   ├── 00_setup_and_clean.R          # Load & clean the raw dataset — run this first
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
└── README.md
```

## How to Run

1. Open `data-cleaning/00_setup_and_clean.R`, set your working directory to the folder containing `combined_upi_dataset.xlsx`, and run it. This creates `df_monthly` and `df_fraud`, which every other script depends on.
2. Run the remaining scripts in numeric order (01 → 06). Each one assumes `df_monthly` / `df_fraud` are already in your R session.

> Note: the raw dataset (`combined_upi_dataset.xlsx`) is not included in this repository. These scripts are shared for code reference; get in touch if you'd like access to the underlying data.

## Tools

- **R** — data cleaning, statistical modeling, and visualization
- Key packages: `dplyr`, `ggplot2`, `forecast`, `tseries`, `lmtest`, `car`, `corrplot`
- Key techniques: time-series forecasting (ARIMA, ETS, STL decomposition), non-parametric hypothesis testing (Wilcoxon), correlation & regression analysis, heteroscedasticity diagnostics (Breusch-Pagan)

## Author

**Anjana Byju**
MSc Statistics, University of Kerala
anjanabyju723@gmail.com
