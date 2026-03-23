# World Bank Development Indicators Analysis

This project processes and analyzes various World Bank Development Indicators to create a consolidated dataset for statistical modeling in R.

## Project Structure

- **Data/**: Contains the processed datasets.
  - `consolidated_data.csv`: The final cleaned dataset (83 countries, 20 indicators).
  - `indicator_descriptions.csv`: Data dictionary mapping indicator codes to full names.
- **Data/raw/**: Contains the original World Bank CSV files and metadata.
- **src/**:
  - `dataProcessing.py`: Python script that cleans raw data, identifies the latest available values per country, and merges them with regional metadata.
- **ProjectDetails/**: Project guidelines, checklists, and report templates.

## Data Processing Logic

The `dataProcessing.py` script performs the following steps:
1. Loads 20 distinct indicator datasets from `Data/raw/`.
2. For each country, it identifies the **most recent available data point** (forward-filling across years from 1960 to present).
3. Merges these values with **Region** metadata.
4. **Filtering**: Any country missing a specific region or missing data for *any* of the 20 indicators is removed to ensure a complete dataset for analysis.

## Usage in R

To load the data in your R scripts (from the project root):

```R
library(readr)

# Load the main dataset
data <- read_csv("Data/consolidated_data.csv")

# Load the descriptions for plotting labels
descriptions <- read_csv("Data/indicator_descriptions.csv")
```

## Indicators Included
The dataset includes 20 indicators across Economic, Labor, Education, and Environmental categories, such as:
- GNI per capita (constant 2015 US$)
- Manufacturing value added (% of GDP)
- Access to electricity (% of population)
- Labor force participation rates
- Urban/Rural population percentages
- And more.
