import pandas as pd
from pathlib import Path

def get_latest_values(df):
    """Finds the most recent non-null value for each country."""
    year_cols = [c for c in df.columns if c.isdigit()]
    
    # Forward fill across years to get the most recent data point for each country
    latest = df.set_index('Country Code')[year_cols].ffill(axis=1).iloc[:, -1]
    
    # Use Indicator Code as the column name
    indicator_code = df['Indicator Code'].iloc[0]
    latest.name = indicator_code
    return latest

def main():
    base_dir = Path(__file__).resolve().parent.parent / "Data"
    raw_dir = base_dir / "raw"
    data_dict = {}
    metadata_dict = {}
    latest_series_list = []

    # Load all files and extract latest values from Data/raw
    for path in raw_dir.glob("*.csv"):
        name = path.stem
        if name.startswith("Metadata_"):
            metadata_dict[name] = pd.read_csv(path)
        elif name.startswith("API_"):
            df = pd.read_csv(path, skiprows=4)
            data_dict[name] = df
            latest_series_list.append(get_latest_values(df))

    print(f"Loaded {len(data_dict)} data files from {raw_dir}")
    print(f"Loaded {len(metadata_dict)} metadata files.")

    # Combine all indicators into one DataFrame
    combined_indicators = pd.concat(latest_series_list, axis=1).reset_index()

    # Merge with Region/Income metadata
    country_meta_key = next((k for k in metadata_dict if "Country" in k), None)
    if country_meta_key:
        # Select Country Code and Region
        meta_df = metadata_dict[country_meta_key][['Country Code', 'Region']]
        final_df = pd.merge(meta_df, combined_indicators, on='Country Code', how='inner')

        # Remove rows missing Region or ANY indicator data
        final_df = final_df.dropna()

        # Save Consolidated Data to CSV for R
        output_path = base_dir / "consolidated_data.csv"
        final_df.to_csv(output_path, index=False)
        print(f"\nConsolidated DataFrame saved to: {output_path}")

        indicator_info = []
        for key, meta_df in metadata_dict.items():
            if "Indicator" in key:
                # Get the Code and Name (Description)
                row = meta_df[['INDICATOR_CODE', 'INDICATOR_NAME']].iloc[0]
                indicator_info.append(row)

        desc_df = pd.DataFrame(indicator_info).drop_duplicates()
        # Only keep descriptions for columns that survived the dropna() filter
        desc_df = desc_df[desc_df['INDICATOR_CODE'].isin(final_df.columns)]

        desc_path = base_dir / "indicator_descriptions.csv"
        desc_df.to_csv(desc_path, index=False)
        print(f"Indicator descriptions saved to: {desc_path}")

        print(f"Final Shape: {final_df.shape}")
        return final_df
    else:
        print("\nWarning: No country metadata found for region merging.")
        return combined_indicators

if __name__ == "__main__":
    df_consolidated = main()
