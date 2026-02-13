# Ecological Memory Generates Scale-Dependent Hysteresis in Complex Ecosystems

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R](https://img.shields.io/badge/R-4.3.1+-blue.svg)](https://www.r-project.org/)

Companion repository for PNAS submission: "Ecological Memory Generates Scale-Dependent Hysteresis in Complex Ecosystems" by Sam Adeyemi.

## 📄 Citation

Adeyemi, S. (2026). Ecological Memory Generates Scale-Dependent Hysteresis
in Complex Ecosystems. Proceedings of the National Academy of Sciences.
(In review)



## 📊 Overview

This repository contains complete data and reproducible code for quantifying ecological memory and hysteresis across 72 regime-shifting ecosystems (coral reefs, savannah-forest transitions, lake eutrophication) and 12 stable LTER sites.

**Key Contributions:**
- Fractional time series analysis (ARFIMA, DFA) for memory parameter estimation
- Phase-randomization surrogate testing for statistical validation
- Irreversibility Index (ℐ) for predicting restoration success
- Complete figure generation pipeline for publication-quality graphics

## 📁 Repository Structure

├── code/ # R analysis scripts
├── data/ # Time series data (72 + 12 sites)
├── figures/ # Generated publication figures
├── results/ # Fitted parameters and predictions
└── README.md # This file



## 🔧 Requirements

### Software
- **R version:** 4.3.1 or higher
- **RStudio:** Recommended for interactive use

### R Packages
```r
# Install required packages
install.packages(c(
  "ggplot2",      # Visualization
  "dplyr",        # Data manipulation
  "tidyr",        # Data tidying
  "fracdiff",     # ARFIMA models
  "fractal",      # DFA analysis
  "patchwork",    # Multi-panel figures
  "ggrepel",      # Label placement
  "scales",       # Axis formatting
  "viridis",      # Color palettes
  "cowplot"       # Publication themes
))
🚀 Quick Start
1. Clone Repository
bash
git clone https://github.com/SamAde1203/ecological-memory-hysteresis.git
cd ecological-memory-hysteresis
2. Generate Main Figures
r
# Open R or RStudio
source("code/generate_pnas_figures.R")
3. Generate Supplementary Figures
r
source("code/generate_si_figures.R")
source("code/generate_additional_si_figures.R")
📊 Data Description
Site Metadata (data/site_metadata.csv)
Columns: Site_ID, Ecosystem, Location, Lat, Lon, Start_Year, End_Year, n_obs

n = 84: 72 regime-shifting + 12 stable LTER sites

Time Series Data (data/[ecosystem]/site_XXX.csv)
Columns: Date, State_Variable, Environmental_Forcing, Disturbance

State variables:

Coral reefs: Coral cover (%)

Savannahs: Woody vegetation cover (%)

Lakes: Chlorophyll-a (μg/L)

Analysis Results
arfima_estimates.csv - Memory parameters (μ, SE, CI)

hysteresis_measures.csv - Irreversibility Index (ℐ) per site

surrogate_test_results.csv - 72,000 surrogate correlations

📈 Key Analyses
1. Memory Parameter Estimation
r
source("code/arfima_analysis.R")
# Estimates μ via ARFIMA(p,d,q) models
# Outputs: Memory parameters with 95% CI
2. Detrended Fluctuation Analysis
r
source("code/dfa_analysis.R")
# Independent validation of memory via DFA
# Correlation with ARFIMA: r = 0.94
3. Surrogate Testing
r
source("code/surrogate_testing.R")
# Phase-randomization tests (1000 surrogates per site)
# Tests H₀: μ and ℐ are independent
🎨 Figure Generation
All figures are publication-ready (300 DPI, TIFF format, PNAS specifications).

Main Figures (4):

Figure 1: Memory-hysteresis correspondence (7.0 × 3.5 in)

Figure 2: LTER negative controls (7.0 × 5.0 in)

Figure 3: Cross-scale propagation (7.0 × 5.0 in)

Figure 4: Restoration applications (7.0 × 5.5 in)

Supplementary Figures (29):

S1.1-S1.9: Individual site time series

S2.1-S2.3: Diagnostics and verification

S3.1-S3.10: Surrogate test distributions

S4.1, S4.5: LTER comparisons and classification

S5.1-S5.5: Restoration economics

🔬 Reproducibility
Full Replication
To reproduce all analyses from scratch:

r
# 1. Estimate memory parameters
source("code/arfima_analysis.R")

# 2. Run surrogate tests
source("code/surrogate_testing.R")

# 3. Generate all figures
source("code/generate_pnas_figures.R")
source("code/generate_si_figures.R")
source("code/generate_additional_si_figures.R")
Computational requirements:

Time: ~2-3 hours (full analysis pipeline)

Memory: ~8 GB RAM recommended

Parallel processing: Optional (speeds up surrogate tests)

📖 Documentation
Installation guide: See docs/installation.md

Usage examples: See docs/usage_guide.md

Citation info: See docs/citation.md

📝 License
This project is licensed under the MIT License - see LICENSE file for details.

👤 Author
Sam Adeyemi

Email: samoadeyemi@yahoo.co.uk

GitHub: @SamAde1203

🙏 Acknowledgments
Data sources:

Coral reefs: Hughes et al. (1994), Mumby et al. (2007)

Savannahs: Staver et al. (2011), Hoffmann et al. (2012)

Lakes: North Temperate Lakes LTER

Stable sites: NSF Long-Term Ecological Research Network

📧 Contact
For questions about the code or data, please open an issue or contact samoadeyemi@yahoo.co.uk.
