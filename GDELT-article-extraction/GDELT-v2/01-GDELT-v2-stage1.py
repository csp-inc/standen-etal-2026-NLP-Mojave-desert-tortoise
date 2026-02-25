# -*- coding: utf-8 -*-
"""

Project: TLD NFWF
 
Script name: 01-GDELT-v2-stage1.py

Purpose of script: this script accesses URLs from 2015-2024 using GDELT v2.0

Author: Mae Lacey - Data Scientist

Email contact: mae[at]csp-inc.org

Date created: 11/21/2025

Date last updated: 2/25/2026

"""

import os
import io
import re
import time
import random
import zipfile
import requests
import pandas as pd
from datetime import datetime, timedelta
from concurrent.futures import ThreadPoolExecutor, as_completed

# -----------------------------------------------------------------------------
# Configuring directories
# -----------------------------------------------------------------------------
SAVE_DIR = "./gdelt_v2_stage1"
os.makedirs(SAVE_DIR, exist_ok=True)

URL_FILE = os.path.join(SAVE_DIR, "urls_all.csv")

GDELT_V2_BASE = "http://data.gdeltproject.org/gdeltv2/"
MAX_DOWNLOAD_THREADS = 20


# -----------------------------------------------------------------------------
# URL cleaner
# -----------------------------------------------------------------------------
def clean_url(u):
    if not isinstance(u, str):
        return None
    u = u.strip()
    if u.startswith("www."):
        u = "http://" + u
    elif re.match(r"^[A-Za-z0-9.-]+\.[A-Za-z]{2,}", u) and not u.startswith("http"):
        u = "http://" + u
    return u if u.startswith("http") else None


# -----------------------------------------------------------------------------
# Generate GDELT v2 minute file URLs
# -----------------------------------------------------------------------------
def generate_gkg_v2_urls_for_day(date):
    urls = []
    for hh in range(24):
        for mm in [0, 15, 30, 45]:
            ts = date.strftime("%Y%m%d") + f"{hh:02d}{mm:02d}00"
            urls.append(f"{GDELT_V2_BASE}{ts}.gkg.csv.zip")
    return urls


# -----------------------------------------------------------------------------
# Download and filter each minute file
# -----------------------------------------------------------------------------
def fetch_and_filter_single_file(url, keyword_pattern, theme_pattern):
    try:
        r = requests.get(url, timeout=60)
        if r.status_code == 404:
            return None
        r.raise_for_status()

        with zipfile.ZipFile(io.BytesIO(r.content)) as z:
            name = z.namelist()[0]
            with z.open(name) as f:
                df = pd.read_csv(f, sep="\t", header=None, low_memory=False)
                df.columns = [f"col_{i}" for i in range(df.shape[1])]
    except:
        return None

    if "col_4" not in df.columns:
        return None

    # Apply keyword/theme filtering
    mask = (
        df["col_3"].astype(str).str.lower().str.contains(keyword_pattern, na=False) |
        df["col_4"].astype(str).str.lower().str.contains(keyword_pattern, na=False) |
        df["col_5"].astype(str).str.lower().str.contains(keyword_pattern, na=False) |
        df["col_6"].astype(str).str.lower().str.contains(keyword_pattern, na=False)
    )

    if theme_pattern:
        mask |= df["col_3"].astype(str).str.lower().str.contains(theme_pattern, na=False)

    df = df[mask]
    if df.empty:
        return None

    return df.rename(columns={"col_4": "SOURCEURL"})


# -----------------------------------------------------------------------------
# Process a single day
# -----------------------------------------------------------------------------
def query_gkg_v2_day(date, keywords, theme_codes=None):
    urls = generate_gkg_v2_urls_for_day(date)

    keyword_pattern = r"\b(?:" + "|".join([re.escape(k.lower()) for k in keywords]) + r")\b"
    theme_pattern = "|".join([t.lower() for t in theme_codes]) if theme_codes else None

    results = []
    print(f"Processing {date.strftime('%Y-%m-%d')}...")

    with ThreadPoolExecutor(max_workers=MAX_DOWNLOAD_THREADS) as exe:
        futures = {
            exe.submit(fetch_and_filter_single_file, u, keyword_pattern, theme_pattern): u
            for u in urls
        }

        for fut in as_completed(futures):
            df = fut.result()
            if df is not None:
                results.append(df)

    if not results:
        return None

    return pd.concat(results, ignore_index=True)


# -----------------------------------------------------------------------------
# Process a single year
# -----------------------------------------------------------------------------
def query_gkg_v2_year(year, keywords, theme_codes=None):
    start = datetime(year, 1, 1)
    end = datetime(year, 12, 31)
    urls_accum = []

    while start <= end:
        date_str = start.strftime("%Y%m%d")

        df = query_gkg_v2_day(start, keywords, theme_codes)

        if df is not None and not df.empty:
            # Save daily GKG CSV (exactly like original pipeline)
            out = os.path.join(SAVE_DIR, f"gdelt_gkg_{year}_{date_str}.csv")
            df.to_csv(out, index=False)

            # Collect URLs for Stage 2
            urls_accum.extend(df["SOURCEURL"].dropna().tolist())

        start += timedelta(days=1)

    # Save accumulated URLs for this year
    return urls_accum


# -----------------------------------------------------------------------------
# Parse date from file name
# -----------------------------------------------------------------------------
def parse_date_from_filename(filename):
    """
    Expects filenames like: gdelt_gkg_YYYYMMDD.csv, but can also accommodate
    filenames in older format (see fallback)
    """
    # Try simple pattern first: gdelt_gkg_YYYYMMDD.csv
    m = re.match(r"gdelt_gkg_(\d{8})\.csv$", filename)
    if not m:
        # Fallback: gdelt_gkg_YYYY_YYYYMMDD.csv
        m = re.match(r"gdelt_gkg_\d{4}_(\d{8})\.csv$", filename)
    if not m:
        return None, None

    date_str = m.group(1)
    try:
        date_obj = datetime.strptime(date_str, "%Y%m%d").date()
        year = date_obj.year
        return date_obj.isoformat(), year  # date as 'YYYY-MM-DD'
    except ValueError:
        return None, None


# -----------------------------------------------------------------------------
# Main
# -----------------------------------------------------------------------------
if __name__ == "__main__":
    keywords = [
        "Mojave Desert Tortoise", "Gopherus agassizii", "Desert Tortoise",
        "tortoise", "mojave", "desert"
    ]

    theme_codes = [
        "ANIMALS_TURTLES", "ENV_DESERTIFICATION", "ENV_WILDLIFE",
        "ENV_ENDANGERED_SPECIES", "ENV_CONSERVATION"
    ]
    
    # use this code chunk when running over entire study period ------ v ------
    #all_urls = []
    #for year in range(2016, 2025):
    #    print(f"\n===== YEAR {year} =====")
    #    urls = query_gkg_v2_year(year, keywords, theme_codes)
    #    all_urls.extend(urls)
    
    # Deduplicate before saving
    #all_urls = list(set(all_urls))
    #pd.DataFrame({"url": all_urls}).to_csv(URL_FILE, index=False)

    #print("\nStage 1 complete!")
    #print(f"Saved {len(all_urls)} unique URLs to {URL_FILE}")
    # use this code chunk when running over entire study period ------ ^ ------
    
    # use this code chunk for running on shorter periods ------------- v ------
    start_date = datetime(2024, 12, 30)
    end_date   = datetime(2024, 12, 31)
    
    print(f"\n===== RUNNING FROM {start_date.date()} TO {end_date.date()} =====")
    
    current = start_date
    
    while current <= end_date:
        date_str = current.strftime("%Y%m%d")
        out = os.path.join(SAVE_DIR, f"gdelt_gkg_{date_str}.csv")
        
        # skip if CSV for a given day already exists
        if os.path.exists(out):
            print(f"Skipping {date_str} — already processed.")
            current += timedelta(days=1)
            continue
    
        # Run extraction for this day
        df = query_gkg_v2_day(current, keywords, theme_codes)
    
        if df is not None and not df.empty:
            df.to_csv(out, index=False)
        else:
            print(f"No results for {date_str}")
    
        current += timedelta(days=1)
    
    # Deduplicate & save
    #all_urls = list(set(all_urls))
    #pd.DataFrame({"url": all_urls}).to_csv(URL_FILE, index=False)
    
    # -----------------------------------------------------------
    # Rebuild URL list with `url`, `date`, `year`
    # -----------------------------------------------------------
    print("\nRebuilding URL list from saved CSVs...")

    master_rows = []

    for file in os.listdir(SAVE_DIR):
        if file.endswith(".csv") and file.startswith("gdelt_gkg_"):
            date_str, year = parse_date_from_filename(file)
            if date_str is None or year is None:
                print(f"Skipping {file} — could not parse date from filename.")
                continue

            path = os.path.join(SAVE_DIR, file)
            try:
                df = pd.read_csv(path, usecols=["SOURCEURL"])
            except Exception as e:
                print(f"Could not read URLs from {file}: {e}")
                continue

            df = df.rename(columns={"SOURCEURL": "url"})
            df = df.dropna(subset=["url"])
            if df.empty:
                continue

            # Add date and year columns
            df["date"] = date_str # 'YYYY-MM-DD'
            df["year"] = int(year)

            master_rows.append(df)

    if master_rows:
        urls_df = pd.concat(master_rows, ignore_index=True)

        # Deduplicate on URL -- keep first occurrence of date/year
        urls_df = urls_df.drop_duplicates(subset=["url"])

        # Save master URL list with date & year
        urls_df.to_csv(URL_FILE, index=False)

        print(f"\nStage 1 complete — saved {len(urls_df)} unique URLs with date/year to {URL_FILE}.")
    else:
        print("\nNo gdelt_gkg_*.csv files found or no URLs to write.")