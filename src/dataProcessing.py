from __future__ import annotations
from pathlib import Path
from typing import Dict, List, Optional, Tuple
import pandas as pd

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
    return Path(__file__).resolve().parent.parent / "Data"

def get_year_columns(df: pd.DataFrame) -> List[str]:
    return [col for col in df.columns if str(col).isdigit()]

def get_latest_values(df: pd.DataFrame) -> pd.Series:
    year_cols = get_year_columns(df)
    subset = df[["Country Code", "Indicator Code", *year_cols]].copy()
    subset = subset.drop_duplicates(subset=["Country Code"], keep="first")
    latest = subset.set_index("Country Code")[year_cols].ffill(axis=1).iloc[:, -1]
    indicator_code = subset["Indicator Code"].iloc[0]
    latest.name = indicator_code
    return latest


def load_raw_files(raw_dir: Path) -> Tuple[Dict[str, pd.DataFrame], Dict[str, pd.DataFrame]]:
    data_files: Dict[str, pd.DataFrame] = {}
    metadata_files: Dict[str, pd.DataFrame] = {}

    csv_files = sorted(raw_dir.glob("*.csv"))

    for path in csv_files:
        name = path.stem

        if name.startswith("Metadata_"):
            metadata_files[name] = pd.read_csv(path)
        elif name.startswith("API_"):
            df = pd.read_csv(path, skiprows=4)
            data_files[name] = df
    return data_files, metadata_files


def build_combined_indicator_frame(data_files: Dict[str, pd.DataFrame]) -> pd.DataFrame:
    latest_series_list: List[pd.Series] = []

    for name, df in data_files.items():
        latest_series_list.append(get_latest_values(df))

    combined = pd.concat(latest_series_list, axis=1).reset_index()
    combined = combined.rename(columns={"index": "Country Code"})
    return combined


def find_country_metadata(metadata_files: Dict[str, pd.DataFrame]) -> Optional[pd.DataFrame]:
    for name, df in metadata_files.items():
        if "Country" in name:
            return df[["Country Code", "Region"]].copy()
    return None


def build_indicator_descriptions(metadata_files: Dict[str, pd.DataFrame], final_columns: List[str]) -> pd.DataFrame:
    all_indicator_rows = []

    for name, df in metadata_files.items():
        if "INDICATOR_CODE" in df.columns and "INDICATOR_NAME" in df.columns:
            subset = df[["INDICATOR_CODE", "INDICATOR_NAME"]].dropna().drop_duplicates()
            all_indicator_rows.append(subset)

    if not all_indicator_rows:
        return pd.DataFrame(columns=["INDICATOR_CODE", "INDICATOR_NAME"])

    desc_df = pd.concat(all_indicator_rows).drop_duplicates()

    # Filter to only include the indicators actually present in your final data
    desc_df = desc_df[desc_df["INDICATOR_CODE"].isin(final_columns)]
    desc_df = desc_df.sort_values("INDICATOR_CODE").reset_index(drop=True)
    
    return desc_df


def save_outputs(base_dir: Path, final_df: pd.DataFrame, desc_df: pd.DataFrame) -> None:
    output_path = base_dir / "consolidated_data.csv"
    desc_path = base_dir / "indicator_descriptions.csv"

    final_df.to_csv(output_path, index=False)
    desc_df.to_csv(desc_path, index=False)


def process_world_bank_data(drop_missing: bool = True) -> pd.DataFrame:
    base_dir = get_base_dir()
    raw_dir = base_dir / "raw"

    data_files, metadata_files = load_raw_files(raw_dir)

    combined_indicators = build_combined_indicator_frame(data_files)
    country_meta = find_country_metadata(metadata_files)

    final_df = pd.merge(country_meta, combined_indicators, on="Country Code", how="inner")

    if drop_missing:
        before = len(final_df)
        final_df = final_df.dropna()
        after = len(final_df)

    desc_df = build_indicator_descriptions(metadata_files, final_df.columns.tolist())
    save_outputs(base_dir, final_df, desc_df)

    return final_df


def main() -> None:
    df_consolidated = process_world_bank_data(drop_missing=True)
    print(df_consolidated.head())


if __name__ == "__main__":
    main()