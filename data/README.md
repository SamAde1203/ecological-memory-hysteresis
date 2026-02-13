```markdown
# Data Directory

## Overview
Time series data for 84 ecosystems: 72 regime-shifting sites and 12 stable LTER sites.

## File Structure

### Site Metadata
`site_metadata.csv` - Complete site information
- Site_ID: Unique identifier (e.g., CORAL_001)
- Ecosystem: coral_reef | savannah_forest | lake
- Location: Geographic location
- Lat, Lon: Coordinates
- Start_Year, End_Year: Observation period
- n_obs: Number of observations

### Time Series Files
Organized by ecosystem type in subfolders:
- `coral_reefs/` - 24 sites
- `savannah_forest/` - 28 sites
- `lakes/` - 20 sites
- `lter_stable/` - 12 sites

**File format:** CSV with columns:
- Date: YYYY-MM-DD
- State_Variable: Ecosystem state (coral %, vegetation %, chlorophyll-a)
- Environmental_Forcing: Driver variable
- Disturbance: Binary indicator (0 = baseline, 1 = forced)

### Analysis Outputs
- `arfima_estimates.csv` - Memory parameters (μ) with 95% CI
- `hysteresis_measures.csv` - Irreversibility Index (ℐ) per site
- `restoration_sites.csv` - 31 documented restoration attempts

## Data Sources
See main README.md for full citations.

## Usage
```r
# Load site metadata
sites <- read.csv("data/site_metadata.csv")

# Load individual site
coral_01 <- read.csv("data/coral_reefs/site_001.csv")
