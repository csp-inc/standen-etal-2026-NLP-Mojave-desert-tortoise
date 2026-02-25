# -*- coding: utf-8 -*-
"""
Created on Wed Dec  3 15:23:47 2025

@author: Mae Lacey
"""

import os
import re
import json
import csv
import pandas as pd
import multiprocessing as mp
import requests
from trafilatura import extract, fetch_url

# ===============================================================
# CONFIG
# ===============================================================
STAGE1_URL_FILE = "./gdelt_v2_stage1/urls_all.csv"
CACHE_DIR = "./article_cache"
OUTFILE = "./gdelt_v2_stage1/GDELT-v2-final/article_text_master.csv"
os.makedirs(CACHE_DIR, exist_ok=True)

MAX_PROCESSES = 4   # keep this small so your machine stays usable


# ===============================================================
# UTILS
# ===============================================================
def clean_url(u):
    """Basic URL sanitizer."""
    if not isinstance(u, str):
        return None
    u = u.strip()

    if u.startswith("www."):
        u = "http://" + u
    elif re.match(r"^[A-Za-z0-9.-]+\.[A-Za-z]{2,}", u) and not u.startswith("http"):
        u = "http://" + u

    return u if u.startswith("http") else None


def url_to_cachefile(url):
    safe = re.sub(r"[^A-Za-z0-9]+", "_", url)
    return os.path.join(CACHE_DIR, safe + ".json")


# ===============================================================
# ARTICLE EXTRACTION WORKER
# ===============================================================
def extract_article(url):
    """Worker process for extracting article text with caching."""
    url = clean_url(url)
    if url is None:
        return None

    cachefile = url_to_cachefile(url)

    # ---------- Return cached version if available ----------
    if os.path.exists(cachefile):
        try:
            with open(cachefile, "r", encoding="utf-8") as f:
                return json.load(f)
        except Exception:
            pass  # fall through to re-download if cache corrupted

    # ---------- Try direct URL ----------
    try:
        html = fetch_url(url)
        if html:
            text = extract(html)
            if text:
                data = {"url": url, "text": text, "archived_used": False}
                with open(cachefile, "w", encoding="utf-8") as f:
                    json.dump(data, f)
                return data
    except Exception:
        pass

    # ---------- Archive fallback ----------
    try:
        api = f"http://archive.org/wayback/available?url={url}"
        r = requests.get(api, timeout=10).json()
        if "archived_snapshots" in r and "closest" in r["archived_snapshots"]:
            arch = r["archived_snapshots"]["closest"]["url"]
            html = fetch_url(arch)
            if html:
                text = extract(html)
                if text:
                    data = {"url": url, "text": text, "archived_used": True}
                    with open(cachefile, "w", encoding="utf-8") as f:
                        json.dump(data, f)
                    return data
    except Exception:
        return None

    return None


# ===============================================================
# MAIN
# ===============================================================
if __name__ == "__main__":
    print("Loading URL list...")
    df = pd.read_csv(STAGE1_URL_FILE)
    urls = df["url"].dropna().unique().tolist()
    urls = [u for u in urls if isinstance(u, str)]
    print(f"Total URLs loaded (raw): {len(urls)}")

    # ---- Load already-processed URLs from OUTFILE (if exists) ----
    already_done = set()
    if os.path.exists(OUTFILE):
        print(f"Found existing outfile: {OUTFILE}, loading processed URLs...")
        try:
            out_df = pd.read_csv(OUTFILE, usecols=["url"])
            already_done = set(out_df["url"].dropna().tolist())
        except Exception as e:
            print(f"Warning: could not read existing OUTFILE ({e}), starting fresh.")

    # ---- Filter URLs to process: not in OUTFILE AND (optionally) no cache ----
    urls_to_process = []
    for u in urls:
        cu = clean_url(u)
        if cu is None:
            continue
        if cu in already_done:
            continue
        cachefile = url_to_cachefile(cu)
        # If you want to skip re-downloading cached URLs, keep this condition:
        if os.path.exists(cachefile):
            # It's already cached; we'll use it later when we rebuild the CSV
            continue
        urls_to_process.append(cu)

    print(f"URLs still needing processing: {len(urls_to_process)}")

    fieldnames = ["url", "text", "archived_used"]

    # ---- Open OUTFILE in append or write mode depending on existence ----
    file_exists = os.path.exists(OUTFILE)
    mode = "a" if file_exists else "w"

    with open(OUTFILE, mode, newline="", encoding="utf-8") as f_out:
        writer = csv.DictWriter(f_out, fieldnames=fieldnames)
        if not file_exists:
            writer.writeheader()

        processed = 0
        non_null = len(already_done)

        if urls_to_process:
            with mp.Pool(processes=MAX_PROCESSES, maxtasksperchild=1000) as pool:
                for result in pool.imap_unordered(extract_article, urls_to_process, chunksize=10):
                    processed += 1
                    if result is not None:
                        writer.writerow(result)
                        non_null += 1

                    if processed % 100 == 0:
                        print(
                            f"Newly processed {processed}/{len(urls_to_process)} URLs "
                            f"(total articles: {non_null})",
                            flush=True
                        )

    print("\nStage 2 complete (or current chunk finished)!")
    print(f"Total articles recorded in {OUTFILE}: {non_null}")
