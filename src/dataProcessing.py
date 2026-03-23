from __future__ import annotations

import logging
from pathlib import Path
from typing import Dict, List, Optional, Tuple

import pandas as pd


logging.basicConfig(
    level=logging.INFO,
    format="%(levelname)s: %(message)s"
)


REQUIRED_DATA_COLUMNS = {
    "Country Name",
    "Country Code",
    "Indicator Name",
    "Indicator Code",
}

REQUIRED_COUNTRY_META_COLUMNS = {
    "Country Code",
    "Region",
}

REQUIRED_INDICATOR_META_COLUMNS = {
    "INDICATOR_CODE",
    "INDICATOR_NAME",
}


def get_base_dir() -> Path:
    """
    Returns the project Data directory.
    Assumes this script lives in src/ and Data/ is one level above src/.
    """
    return Path(__file__).resolve().parent.parent / "Data"


def get_year_columns(df: pd.DataFrame) -> List[str]:
    """
    Extract all year columns from the World Bank format.
    """
    return [col for col in df.columns if str(col).isdigit()]


def validate_data_file(df: pd.DataFrame, file_name: str) -> None:
    """
    Ensures the indicator data file has the expected columns.
    """
    missing = REQUIRED_DATA_COLUMNS - set(df.columns)
    if missing:
        raise ValueError(
            f"{file_name} is missing required columns: {sorted(missing)}"
        )

    year_cols = get_year_columns(df)
    if not year_cols:
        raise ValueError(f"{file_name} contains no year columns.")


def validate_metadata_file(
    df: pd.DataFrame,
    file_name: str,
    required_cols: set[str]
) -> bool:
    """
    Returns True if required columns exist, else False.
    """
    missing = required_cols - set(df.columns)
    if missing:
        logging.warning(
            "%s is missing expected columns: %s",
            file_name,
            sorted(missing)
        )
        return False
    return True


def get_latest_values(df: pd.DataFrame) -> pd.Series:
    """
    For a single indicator file, returns the most recent non-null value
    for each country as a Series indexed by Country Code.

    Example output:
        index = Country Code
        name  = Indicator Code
        values = latest available value per country
    """
    validate_data_file(df, "indicator dataframe")

    year_cols = get_year_columns(df)

    # Keep only relevant columns and drop duplicate country rows if present
    subset = df[["Country Code", "Indicator Code", *year_cols]].copy()
    subset = subset.drop_duplicates(subset=["Country Code"], keep="first")

    # Forward fill across the years so the last column becomes the latest available value
    latest = subset.set_index("Country Code")[year_cols].ffill(axis=1).iloc[:, -1]

    indicator_code = subset["Indicator Code"].iloc[0]
    latest.name = indicator_code
    return latest


def load_raw_files(raw_dir: Path) -> Tuple[Dict[str, pd.DataFrame], Dict[str, pd.DataFrame]]:
    """
    Loads all raw CSV files from Data/raw into:
      - indicator data files (API_*)
      - metadata files (Metadata_*)
    """
    if not raw_dir.exists():
        raise FileNotFoundError(f"Raw data folder not found: {raw_dir}")

    data_files: Dict[str, pd.DataFrame] = {}
    metadata_files: Dict[str, pd.DataFrame] = {}

    csv_files = sorted(raw_dir.glob("*.csv"))
    if not csv_files:
        raise FileNotFoundError(f"No CSV files found in: {raw_dir}")

    for path in csv_files:
        name = path.stem

        try:
            if name.startswith("Metadata_"):
                metadata_files[name] = pd.read_csv(path)
            elif name.startswith("API_"):
                # World Bank API files usually have 4 header rows
                df = pd.read_csv(path, skiprows=4)
                validate_data_file(df, path.name)
                data_files[name] = df
        except Exception as exc:
            logging.warning("Skipping %s due to error: %s", path.name, exc)

    return data_files, metadata_files


def build_combined_indicator_frame(data_files: Dict[str, pd.DataFrame]) -> pd.DataFrame:
    """
    Extracts latest values from each indicator file and combines them into
    one dataframe keyed by Country Code.
    """
    latest_series_list: List[pd.Series] = []

    for name, df in data_files.items():
        try:
            latest_series_list.append(get_latest_values(df))
        except Exception as exc:
            logging.warning("Failed to process %s: %s", name, exc)

    if not latest_series_list:
        raise ValueError("No valid indicator series were extracted.")

    combined = pd.concat(latest_series_list, axis=1).reset_index()
    combined = combined.rename(columns={"index": "Country Code"})
    return combined


def find_country_metadata(metadata_files: Dict[str, pd.DataFrame]) -> Optional[pd.DataFrame]:
    """
    Finds the metadata file containing country/region information.
    """
    for name, df in metadata_files.items():
        if "Country" in name and validate_metadata_file(df, name, REQUIRED_COUNTRY_META_COLUMNS):
            return df[["Country Code", "Region"]].copy()
    return None


def build_indicator_descriptions(
    metadata_files: Dict[str, pd.DataFrame],
    final_columns: List[str]
) -> pd.DataFrame:
    """
    Builds a lookup table mapping indicator codes to indicator names/descriptions.
    """
    rows = []

    for name, df in metadata_files.items():
        if "Indicator" not in name:
            continue

        if not validate_metadata_file(df, name, REQUIRED_INDICATOR_META_COLUMNS):
            continue

        for _, row in df[["INDICATOR_CODE", "INDICATOR_NAME"]].dropna().drop_duplicates().iterrows():
            rows.append(row)

    desc_df = pd.DataFrame(rows).drop_duplicates()

    if desc_df.empty:
        return desc_df

    desc_df = desc_df[desc_df["INDICATOR_CODE"].isin(final_columns)]
    desc_df = desc_df.sort_values("INDICATOR_CODE").reset_index(drop=True)
    return desc_df


def save_outputs(base_dir: Path, final_df: pd.DataFrame, desc_df: pd.DataFrame) -> None:
    """
    Saves the consolidated dataset and indicator descriptions CSVs.
    """
    output_path = base_dir / "consolidated_data.csv"
    desc_path = base_dir / "indicator_descriptions.csv"

    final_df.to_csv(output_path, index=False)
    logging.info("Saved consolidated data to: %s", output_path)

    if not desc_df.empty:
        desc_df.to_csv(desc_path, index=False)
        logging.info("Saved indicator descriptions to: %s", desc_path)
    else:
        logging.warning("Indicator descriptions dataframe was empty; no file written.")


def process_world_bank_data(drop_missing: bool = True) -> pd.DataFrame:
    """
    Main processing pipeline.

    Parameters
    ----------
    drop_missing : bool
        If True, drop rows missing Region or any indicator values.
        This matches the current project requirement for a complete dataset.
    """
    base_dir = get_base_dir()
    raw_dir = base_dir / "raw"

    data_files, metadata_files = load_raw_files(raw_dir)

    logging.info("Loaded %d indicator files from %s", len(data_files), raw_dir)
    logging.info("Loaded %d metadata files", len(metadata_files))

    combined_indicators = build_combined_indicator_frame(data_files)
    country_meta = find_country_metadata(metadata_files)

    if country_meta is None:
        logging.warning("No valid country metadata found. Returning indicators only.")
        return combined_indicators

    final_df = pd.merge(country_meta, combined_indicators, on="Country Code", how="inner")

    if drop_missing:
        before = len(final_df)
        final_df = final_df.dropna()
        after = len(final_df)
        logging.info("Dropped %d rows with missing values", before - after)

    desc_df = build_indicator_descriptions(metadata_files, final_df.columns.tolist())
    save_outputs(base_dir, final_df, desc_df)

    logging.info("Final shape: %s", final_df.shape)
    return final_df


def main() -> None:
    df_consolidated = process_world_bank_data(drop_missing=True)
    print("\nPreview of consolidated data:")
    print(df_consolidated.head())


if __name__ == "__main__":
    main()
