# -*- coding: utf-8 -*-
"""

Project: TLD NFWF
 
Script name: 02-GDELT-v1-combine-outputs.py

Purpose of script: this script combines all CSV outputs from LML_GDELT_v1_final.py 
                   into one master table for all URLs found and then a separate
                   master table for all article text accessed.

Author: Mae Lacey - Data Scientist

Email contact: mae[at]csp-inc.org

Date created: 11/10/2025

Date last updated: 2/25/2026

"""

import pandas as pd
import glob
import csv
import os
import re

# -------------------------------------------------------------------------
# Directory where all daily GKG outputs are stored
# -------------------------------------------------------------------------
save_dir = "./gdelt_v1_final"

# -------------------------------------------------------------------------
# Find all .csv files in our file directory
# -------------------------------------------------------------------------
all_csvs = glob.glob(os.path.join(save_dir, "*.csv"))

# Separate into "text" and "urls" groups
url_csvs = [f for f in all_csvs if not f.endswith("_text.csv")]
text_csvs = [f for f in all_csvs if f.endswith("_text.csv")]

print(f"Found {len(url_csvs)} URL CSVs and {len(text_csvs)} text CSVs.")

# -------------------------------------------------------------------------
# Helper functions
# -------------------------------------------------------------------------
# extract date from filename
def extract_date_from_filename(fname):
    """
    Extracts date from GDELT filename
    e.g., 'gdelt_gkg_20130414.csv' becomes '2013-04-14'
    Returns (date_str, year)
    """
    match = re.search(r"(\d{8})", fname)
    if match:
        date_raw = match.group(1)
        date_fmt = f"{date_raw[:4]}-{date_raw[4:6]}-{date_raw[6:]}"
        return date_fmt, date_raw[:4]
    else:
        return None, None

# safely read a CSV
def read_csv(fpath):
    try:
        df = pd.read_csv(fpath, low_memory=False)
        base = os.path.basename(fpath)
        date_fmt, year = extract_date_from_filename(base)

        # Keep both fields
        df["date"] = date_fmt # string YYYY-MM-DD
        df["year"] = year # string YYYY

        # other option - could also store as a real datetime column (handy for filtering/sorting)
        # df["date"] = pd.to_datetime(df["date"], errors="coerce")

        return df
    except Exception as e:
        print(f"Failed to read {fpath}: {e}")
        return pd.DataFrame()
    
    
# -------------------------------------------------------------------------
# Combine URL tables into single master table
# -------------------------------------------------------------------------
if url_csvs:
    url_dfs = [read_csv(f) for f in url_csvs]
    url_master = pd.concat(url_dfs, ignore_index=True)
    
    # Drop duplicates by SOURCEURL if present
    if "SOURCEURL" in url_master.columns:
        url_master = url_master.drop_duplicates(subset=["SOURCEURL"])

    out_url = os.path.join(save_dir, "gdelt_v1_gkg_master_urls.csv")
    url_master.to_csv(out_url,
                      index=False,
                      quoting=csv.QUOTE_ALL,
                      doublequote=True,
                      encoding="utf-8-sig",
                      lineterminator="\n")
    print(f"Saved master URL table: {out_url} ({len(url_master)} rows)")
    

# -------------------------------------------------------------------------
# Combine text tables into single master table
# -------------------------------------------------------------------------
if text_csvs:
    text_dfs = [read_csv(f) for f in text_csvs]
    text_master = pd.concat(text_dfs, ignore_index=True)
    
    # Drop duplicates by URL if present
    if "url" in text_master.columns:
        text_master = text_master.drop_duplicates(subset=["url"])
    
    out_text = os.path.join(save_dir, "gdelt_v1_gkg_master_text.csv")
    text_master.to_csv(out_text, 
                       index=False,
                       quoting=csv.QUOTE_ALL,
                       doublequote=True,
                       encoding="utf-8-sig",
                       lineterminator="\n")
    print(f"Saved master TEXT table: {out_text} ({len(text_master)} rows)")