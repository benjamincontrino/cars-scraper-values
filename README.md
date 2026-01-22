# carsscrapervalues

> An R package for scraping, analyzing, and predicting car prices from Cars.com

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

App viewable at:
https://benjamincontrino-car-market-value-finder.hf.space

## Overview

`carsscrapervalues` helps you find the best deals on used and new cars by:
- 🔍 Scraping car listings from Cars.com
- 📊 Visualizing price distributions across makes and models
- 🤖 Predicting fair market values using Random Forest models
- 💎 Identifying overpriced and underpriced vehicles

## Features

### 🚗 **Car Market Value Finder App**
Launch an interactive Shiny app to search for cars and get instant price predictions:
```r
library(carsscrapervalues)
upload_shiny_app()
```

The app provides:
- **About**: Overview of the model and recommendations
- **Graph Distributions**: Compare average prices across makes and models
- **Find Car Values**: Paste a Cars.com search URL and get predictions for all listings

### 📈 **Core Functions**

#### 1. Scrape Car Listings
```r
# Scrape up to 25 pages from a Cars.com search
scrape_cars(
  base_url = "https://www.cars.com/shopping/results/?...",
  max_pages = 10,
  write_new_csv = "YES"
)
```

#### 2. Predict Car Values
```r
# Get price predictions for cars from a search URL
predictions <- predict_cars(
  base_url = "https://www.cars.com/shopping/results/?...",
  max_pages = 5,
  write_new_csv = "NO"
)
```
Returns a data table with:
- Car details (make, model, year, mileage)
- Actual price
- **Predicted price** (what it should cost)
- **Over/Under value** (negative = good deal, positive = overpriced)

#### 3. Visualize Price Distributions
```r
# Compare average prices by make and model
plots <- plot_make_prices(
  new_or_used_input = "Used",
  make_input = "Toyota"
)

# Returns a list of two ggplot objects
plots[[1]]  # Prices across all makes
plots[[2]]  # Prices for specific make's models
```

#### 4. Update Prediction Model
```r
# Retrain the Random Forest model with new data
update_car_prediction_model()
```

## Installation

Install from GitHub:
```r
# Install devtools if needed
install.packages("devtools")

# Install carsscrapervalues
devtools::install_github("yourusername/carsscrapervalues")
```

## Model Details

### Training Data
The Random Forest model was trained on Cars.com listings with these criteria:
- **Body Types**: SUVs and Sedans
- **Years**: 2016+
- **Price**: < $75,000
- **Mileage**: 0 - 80,000 miles
- **Location**: Charlotte, NC area (can work for other regions)

### Features Used
- Make (e.g., Toyota, Honda, BMW)
- Model (cleaned/binned - e.g., "RAV4" for RAV4LE, RAV4XLE, RAV4Hybrid)
- Year
- Mileage
- New vs. Used

### Model Performance
Random Forest was selected after comparing:
- ✅ **Random Forest** (best performance)
- Mixed Effects Models
- XGBoost
- Neural Networks
- Linear Regression

**Note**: Models and trims are binned together to improve sample size. For example, RAV4LE, RAV4XLE, and RAV4Hybrid are all grouped as "RAV4".

## Quick Start Example

```r
library(carsscrapervalues)

# 1. Launch the interactive app (easiest way to start)
upload_shiny_app()

# 2. Or use functions directly
# Search for used Toyotas in your area on Cars.com, copy the URL, then:
predictions <- predict_cars(
  base_url = "YOUR_CARS_COM_URL_HERE",
  max_pages = 5
)

# View the results
View(predictions)

# Find the best deals (most underpriced)
library(dplyr)
predictions %>% 
  arrange(price_over_expectation) %>% 
  head(10)
```

## Workflow

```
┌─────────────────┐
│  Cars.com       │
│  Search URL     │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  scrape_cars()  │  ← Collect training data
│  or              │
│  predict_cars() │  ← Get predictions
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  Random Forest  │  ← Trained model predicts fair prices
│  Model          │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  Interactive    │  ← Visualize results with color-coded deals
│  DT Table       │    Green = underpriced, Red = overpriced
└─────────────────┘
```

## Tips for Best Results

### 🎯 Maximize Prediction Accuracy
For best results, filter your Cars.com search to match the training data:
1. Go to Cars.com
2. Filter by:
   - Body style: SUV or Sedan
   - Year: 2016 or newer
   - Price: Up to $75,000
   - Mileage: 0-80,000
3. Copy the URL
4. Use in `predict_cars()` or the Shiny app

### 💡 Interpreting Results
- **Negative values** (Green): Car is priced BELOW expected value → Good deal! 💰
- **Near zero** (White): Fair price
- **Positive values** (Red): Car is priced ABOVE expected value → Overpriced 🚫

Example:
```
Toyota RAV4 2023
Actual Price: $28,500
Predicted Price: $32,000
Value: -$3,500 (Great deal!)
```

## Package Structure

```
carsscrapervalues/
├── R/
│   ├── scrape_cars.R              # Scrape Cars.com listings
│   ├── predict_cars.R             # Scrape + predict prices
│   ├── plot_make_prices.R         # Visualize distributions
│   ├── update_car_prediction_model.R  # Retrain model
│   └── upload_shiny_app.R         # Launch Shiny app
├── data/
│   ├── cars_data_db.csv           # Training data
│   └── cars_rf_model.rds          # Trained model
├── DESCRIPTION                     # Package metadata
├── NAMESPACE                       # Exported functions
└── README.md                       # This file
```

## Dependencies

The package requires:
- `shiny` - Interactive web app
- `DT` - Interactive tables
- `ggplot2` - Data visualization
- `dplyr`, `tidyr` - Data manipulation
- `tidymodels` - Machine learning framework
- `ranger` - Fast Random Forest implementation
- `httr`, `rvest` - Web scraping
- `stringr` - String manipulation
- `bslib` - Modern UI themes



## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

MIT License - see LICENSE file for details

## Acknowledgments

- Data sourced from Cars.com
- Built with the Tidymodels framework
- UI powered by Shiny and bslib

## Contact

For questions or issues, please open an issue on GitHub.

---

**Disclaimer**: This package is for educational and personal use. Please respect Cars.com's terms of service and robots.txt when scraping. The predictions are estimates and should not be the sole factor in purchasing decisions.
And yes, I used AI to help do everything in this process. It's 2026, might as well leverage this stuff to improve my own efficiency instead of wasting time formatting a graph ;)