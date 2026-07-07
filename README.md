# Macro-Financial Asset Selection Model for Turkey (2015–2026)

**SCI-level empirical study** using **12 Multi-Criteria Decision Making (MCDM)** methods for selecting optimal macro-financial assets in Turkey: USD, EUR, Gold, and BIST 100.

## Key Features
- Real-time capable data pipeline (Yahoo Finance + FRED + TCMB EVDS)
- Inflation-adjusted real returns (Fama & Schwert, 1977)
- Comprehensive liquidity proxies (Amihud for BIST, volatility-based for FX)
- 5 investor weight scenarios + normalization sensitivity analysis
- Full literature-verified fixes and positive-domain handling for Turkish high-inflation environment
- Publication-ready heatmaps and tables

## Weight Scenarios
1. **W1_Risk_Dominant** (Conservative)
2. **W2_Return_Oriented** (Growth)
3. **W3_Sharpe_Centric** (Efficiency)
4. **W4_Inflation_Protection** (Priority)
5. **W5_Equal_Weights** (Benchmark)

## Reproducibility
All data was fixed on **May 18, 2026** to ensure full reproducibility independent of live market updates.

## Technologies
- R (tidyverse, quantmod, ggplot2)
- 12 MCDM Methods: SAW, TOPSIS, VIKOR, MOORA, COPRAS, ARAS, WASPAS, EDAS, MAIRCA, MARCOS, SPOTIS, REGIME

## Repository Structure
