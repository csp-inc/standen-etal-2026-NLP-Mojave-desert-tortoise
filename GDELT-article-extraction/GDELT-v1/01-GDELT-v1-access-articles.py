# -*- coding: utf-8 -*-
"""

Project: TLD NFWF
 
Script name: 01-GDELT-v1-access-articles.py

Purpose of script: this script accesses URLs from 2013-2015 using GDELT v1.0

Author: Mae Lacey - Data Scientist

Email contact: mae[at]csp-inc.org

Date created: 11/10/2025

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
from newspaper import Article


# ------------------------------------------------------------------------------
# Configuring directories
# ------------------------------------------------------------------------------
GDELT_V1_GKG_URL = "http://data.gdeltproject.org/gkg/"
SAVE_DIR = "./gdelt_v1_final"
os.makedirs(SAVE_DIR, exist_ok=True)


# ------------------------------------------------------------------------------
# Archived URL function 
# ------------------------------------------------------------------------------
# pings Wayback Machine if URL not available
def get_archived_url(url):
    """
    Try to find an archived version of the URL using the Internet Archive (Wayback
    Machine). Returns a working archived URL if found, otherwise None.
    """
    api_url = f"http://archive.org/wayback/available?url={url}"
    try:
        r = requests.get(api_url, timeout=10)
        if r.status_code == 200:
            data = r.json()
            if "archived_snapshots" in data and "closest" in data["archived_snapshots"]:
                return data["archived_snapshots"]["closest"]["url"]
    except Exception as e:
        print(f"Archive lookup failed for {url}: {e}")
    return None


# ------------------------------------------------------------------------------
# Text extraction function 
# ------------------------------------------------------------------------------
# extracts text from existing URLs and prepares it for eventual export to CSV with '_text' appended
def extract_text_from_urls(url_list, max_articles=50):
    """
    Extract text from URLs, attempting direct access first and falling back to the
    Wayback Machine if the original link is unavailable.
    Returns a DataFrame including a flag for whether the archived version was used.
    """  
    articles = []
    archived_count = 0
    total_attempted = 0

    for url in url_list[:max_articles]:
        total_attempted += 1
        archived_used = False
        try:
            a = Article(url)
            a.download()
            a.parse()
        except Exception as e:
            #print(f"Failed to parse {url}: {e}")
            # Try the Wayback Machine fallback
            archived = get_archived_url(url)
            if archived:
                archived_used = True
                #print(f"Trying archived version: {archived}")
                try:
                    a = Article(archived)
                    a.download()
                    a.parse()
                except Exception as e2:
                    continue
            else:
                continue

        articles.append({
            "url": url,
            "title": a.title,
            "text": a.text,
            "archived_used": archived_used
        })

        if archived_used:
            archived_count += 1

    df_articles = pd.DataFrame(articles)
    print(f"4. Extracted {len(df_articles)} articles ({archived_count} from archive, "
          f"{len(df_articles) - archived_count} direct).")
    return df_articles


# ------------------------------------------------------------------------------
# GKG URL generation function
# ------------------------------------------------------------------------------
def generate_gkg_urls(year):
    """
    Generate daily URLs for GDELT v1 GKG (available from 2013-04-01 onward). 
    (GDELT v1 GKG only goes back to this time point -- if we want to go back further
     we would need to use GDELT Events, but this does not have full article text)
    """
    start = datetime(year, 1, 1)
    end = datetime(year, 12, 31)
    delta = timedelta(days=1)
    urls = []
    while start <= end:
        fname = start.strftime("%Y%m%d.gkg.csv.zip")
        urls.append(GDELT_V1_GKG_URL + fname)
        start += delta
    return urls


# ------------------------------------------------------------------------------
# Fetch and filter function
# ------------------------------------------------------------------------------
def fetch_and_filter_gkg(url, keywords, theme_codes=None, retries=3):
    """
    Fetch a GDELT v1 GKG daily file and filter it by:
      - multiple text columns (themes, persons, orgs, locations, URL)
      - optional theme codes
    """
    # Download with retry logic
    for attempt in range(retries):
        try:
            r = requests.get(url, stream=True, timeout=60)
            if r.status_code == 404:
                return None
            if r.status_code == 429:
                wait = (attempt + 1) * 60 + random.randint(5, 15)
                print(f"429 Too Many Requests. Retrying in {wait}s...")
                time.sleep(wait)
                continue
            r.raise_for_status()
            break
        except Exception as e:
            print(f"Request failed for {url}: {e}")
            time.sleep(10)
    else:
        return None

    # Read zip contents
    try:
        with zipfile.ZipFile(io.BytesIO(r.content)) as z:
            name = z.namelist()[0]
            with z.open(name) as f:
                df = pd.read_csv(f, sep="\t", header=None, low_memory=False)
                df.columns = [f"col_{i}" for i in range(df.shape[1])]
    except Exception as e:
        print(f"Failed to read {url}: {e}")
        return None

    # Identify columns of interest
    source_col = f"col_{df.shape[1]-1}"  # last column = URL
    search_cols = ["col_3", "col_4", "col_5", "col_6", source_col]

    # Build regex patterns
    keyword_pattern = r"\b(?:" + "|".join([re.escape(kw.lower()) for kw in keywords]) + r")\b"
    theme_pattern = re.compile("|".join([t.lower() for t in theme_codes])) if theme_codes else None

    # Apply filters
    mask = pd.Series(False, index=df.index)
    for c in search_cols:
        if c in df.columns:
            mask |= df[c].astype(str).str.lower().str.contains(keyword_pattern, na=False)

    if theme_pattern and "col_3" in df.columns:
        mask |= df["col_3"].astype(str).str.lower().str.contains(theme_pattern, na=False)

    df_filtered = df[mask]
    if df_filtered.empty:
        return None

    df_filtered = df_filtered.rename(columns={source_col: "SOURCEURL"})
    print(f"1. {len(df_filtered)} matches found in {url.split('/')[-1]}")
    return df_filtered


# ------------------------------------------------------------------------------
# Query year function
# ------------------------------------------------------------------------------
def query_gkg_year(keywords, year, save_dir=SAVE_DIR, theme_codes=None):
    urls = generate_gkg_urls(year)
    yearly_data = pd.DataFrame()

    for url in urls:
        date_str = url.split("/")[-1].split(".")[0]
        outfile = os.path.join(save_dir, f"gdelt_gkg_{year}_{date_str}.csv")
        text_outfile = os.path.join(save_dir, f"gdelt_gkg_{year}_{date_str}_text.csv")

        if os.path.exists(outfile):
            print(f"File exists for {date_str}. Skipping.")
            df = pd.read_csv(outfile)
        else:
            print(f"Fetching {date_str} ...")
            df = fetch_and_filter_gkg(url, keywords, theme_codes)
            if df is not None and not df.empty:
                df.to_csv(outfile, index=False)
                print(f"2. Saved {outfile} ({len(df)} records)")
            else:
                print(f"No matches for {date_str}")
                continue
            time.sleep(random.randint(5, 15))

        # Extract article text
        if df is not None and not df.empty:
            urls = [u for u in df["SOURCEURL"].dropna().unique().tolist() if u.startswith("http")]
            print(f"3. Extracting text from {min(len(urls), 50)} URLs for {date_str}...")
            article_texts = extract_text_from_urls(urls, max_articles=50)

            if not article_texts.empty:
                article_texts.to_csv(text_outfile, index=False)
                print(f"5. Saved article text: {text_outfile}")
            else:
                #print(f"No article text extracted for {date_str}")
                pass

        yearly_data = pd.concat([yearly_data, df], ignore_index=True)

    return yearly_data


# ------------------------------------------------------------------------------
# Multi-year wrapper
# ------------------------------------------------------------------------------
# querying multiple years across our study period
def query_gkg_multiple_years(keywords, start_year=2013, end_year=2015, save_dir=SAVE_DIR, theme_codes=None):
    combined = pd.DataFrame()
    for year in range(start_year, end_year + 1):
        print(f"\n===== YEAR {year} =====")
        data = query_gkg_year(keywords, year, save_dir=save_dir, theme_codes=theme_codes)
        if data is not None and not data.empty:
            combined = pd.concat([combined, data], ignore_index=True)
    return combined


# ------------------------------------------------------------------------------
# Main
# ------------------------------------------------------------------------------
if __name__ == "__main__":

    # Broadened keyword coverage for initial URL pull
    # updated keywords to match Twitter scrape:
    keywords = [
        "Mojave Desert Tortoise",
        "Gopherus agassizii", 
        "Gopherusagassizii", 
        "#Gopherusagassizii",
        "Desert Tortoise", 
        "DesertTortoise",
        "MojaveDesertTortoise",
        "#MojaveDesertTortoise", 
        "#DesertTortoise",
        #"(tortoise (mojave OR desert OR California OR Nevada OR Utah OR Arizona))" # from Twitter scrape, GDELT can't handle this logic
        "tortoise",
        "mojave",
        "desert"
        "mojave desert"
    ]

    # GDELT theme codes (semantic tags)
    theme_codes = [
        "ANIMALS_TURTLES",
        "ENV_DESERTIFICATION",
        "ENV_WILDLIFE",
        "ENV_ENDANGERED_SPECIES",
        "ENV_CONSERVATION"
    ]

    df = query_gkg_multiple_years(
        keywords=keywords,
        start_year=2013,
        end_year=2015,
        save_dir=SAVE_DIR,
        theme_codes=theme_codes
    )

    print(f"\nComplete. {len(df)} total records collected.")