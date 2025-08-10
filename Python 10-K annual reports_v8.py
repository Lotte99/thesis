# Disclaimer: ChatGPT-o4-mini-high helped me debug and improve the script.
# It also helped restructuring it and making it visibly more pleasing.

# This Python script performs the following steps:
# 1. Downloads SEC 10-K filings for S&P 1500 companies from 2000 to 2023.
# 2. Extracts the "Item 1. Business" section from each filing.
# 3. Saves the extracted sections to a CSV file.
# 4. Handles network errors with retries and exponential backoff. 

import os
import re
import time
import json
import shutil
import ssl
import tempfile
import threading
import pandas as pd
import requests

from bs4 import BeautifulSoup
from urllib3.exceptions import NameResolutionError
from requests.adapters import HTTPAdapter
from urllib3.util.retry import Retry
from sec_edgar_downloader import Downloader
import sec_edgar_downloader._orchestrator as orch
import concurrent.futures

# ------------------------
# CONFIGURATION
# ------------------------
# Note: Change files for years 2020-2023 after running the script for 2000-2020.
OUTPUT_CSV    = "/Users/lottegeerts/Documents/extracted_business_sections_2000-2020.csv"
PROGRESS_FILE = "/Users/lottegeerts/Documents/processed_tickers_2000-2020.csv"
START_YEAR    = 2000 
END_YEAR      = 2020
NUM_WORKERS   = 8

# Where to cache the SEC mapping JSON
CACHE_PATH = os.path.join(os.path.dirname(PROGRESS_FILE), "sec_ticker_mapping.json")

# ------------------------
# 1) BUILD A RETRY-ENABLED SESSION & FAIL FAST
# ------------------------
UA = "Lotte Geerts (Erasmus University; 498897lg@student.eur.nl)"
EMAIL = "498897lg@student.eur.nl"
session = requests.Session()
retry_strategy = Retry(
    total=5,
    backoff_factor=1,
    status_forcelist=[429, 500, 502, 503, 504],
    allowed_methods=["GET"],
)
adapter = HTTPAdapter(max_retries=retry_strategy)
session.mount("https://", adapter)
session.mount("http://", adapter)
session.headers.update({"User-Agent": UA})

# Fail fast if SEC is unreachable
try:
    resp = session.get("https://www.sec.gov/files/company_tickers_exchange.json", timeout=10)
    resp.raise_for_status()
except Exception as e:
    raise RuntimeError(f"🚨 Cannot reach SEC mapping endpoint: {e}")

# ------------------------
# 2) CACHE THE MAPPING JSON LOCALLY
# ------------------------
if not os.path.exists(CACHE_PATH):
    data = resp.json()
    with open(CACHE_PATH, "w") as fp:
        json.dump(data, fp)

# 3) OVERRIDE sec_edgar_downloader TO USE THE LOCAL CACHE
def _get_ticker_to_cik_mapping_cached(user_agent=None):
    with open(CACHE_PATH, "r") as fp:
        raw = json.load(fp)
    mapping = {}
    for entry in raw.values():
        t = entry.get("ticker", "").strip().upper()
        c = str(entry.get("cik_str", "")).zfill(10)
        if t and c:
            mapping[t] = c
    return mapping

orch.get_ticker_to_cik_mapping = _get_ticker_to_cik_mapping_cached


# ------------------------
# Ticker normalization
# ------------------------
def normalize_ticker(t):
    t = str(t).strip()
    if not t:
        return ""
    # strip trailing dot or .1, .2, etc.
    t = t.rstrip(".")
    t = re.sub(r"\.\d+$", "", t)
    m = re.search(r"\.([A-Za-z]+)$", t)
    if m:
        s = m.group(1)
        if s in ("A", "B"):
            t = t[:-(len(s)+1)] + "-" + s
        else:
            t = t[:-(len(s)+1)]
    if t.upper() == "NVRI":
        t = "NVR"
    return t.upper()


# ------------------------
# Extract “Item 1. Business”
# (with internal guards on parser errors)
# ------------------------
def extract_item_1(html_text):
    try:
        soup = BeautifulSoup(html_text, "lxml")
    except Exception:
        try:
            soup = BeautifulSoup(html_text, "html.parser")
        except Exception:
            return None

    try:
        txt = soup.get_text("\n")
        txt = re.sub(r"\n+", "\n", txt).replace("\xa0", " ")
        lower = txt.lower()

        # find “item 1. business”
        matches = list(re.finditer(r"\bitem\s+1[^a-z0-9]*business\b", lower))
        if len(matches) < 2:
            matches = list(re.finditer(r"\bitem\s+1\.*\b", lower))
        if not matches:
            return None

        start = matches[1].start() if len(matches) > 1 else matches[0].start()
        end = start + 10000
        for pat in (r"\bitem\s+1a\.*\b", r"\bitem\s+2\.*\b", r"\bpart\s+ii\b", r"\bitem\s+7\.*\b"):
            m = re.search(pat, lower[start+50:], re.IGNORECASE)
            if m:
                end = start + 50 + m.start()
                break

        section = txt[start:end].strip()
        if "table of contents" in section.lower():
            return None
        if len(section) < 800 or section.count(".") < 5:
            return None
        return section

    except Exception:
        return None


# ------------------------
# Read tickers from Excel
# ------------------------
def read_tickers_from_excel(path):
    xls = pd.read_excel(path, sheet_name=None, header=1)
    tasks = []
    for sheet, df in xls.items():
        if not sheet.isdigit():
            continue
        yr = int(sheet)
        if yr < START_YEAR or yr > END_YEAR:
            continue
        seen = set()
        for _, row in df.iterrows():
            raw = row.iloc[1]
            if pd.isna(raw):
                continue
            t0 = normalize_ticker(raw)
            if not t0 or t0 in seen:
                continue
            seen.add(t0)
            tasks.append((t0, yr))
    return tasks


# ------------------------
# 4) RETRIES + BACKOFF for dl.get
# ------------------------
RETRYABLE = (
    requests.exceptions.ConnectionError,
    requests.exceptions.Timeout,
    ssl.SSLError,
    ConnectionResetError,
    NameResolutionError,
)

def download_with_retries(dl, form_type, ticker, after, before, retries=3, delay=1):
    backoff = delay
    for i in range(1, retries+1):
        try:
            dl.get(form_type, ticker, after=after, before=before)
            time.sleep(0.5)
            return True
        except RETRYABLE as e:
            print(f"⚠️ [{ticker}] attempt {i}/{retries} failed: {e}")
            if i < retries:
                time.sleep(backoff)
                backoff *= 2
            else:
                print(f"❌ [{ticker}] giving up after {retries} network errors.")
                return False
        except Exception as e:
            print(f"❌ [{ticker}] non-retryable error: {e}")
            return False


# ------------------------
# 5) PROCESS ONE TICKER-YEAR (safe against any exception)
# ------------------------
def process_ticker_year(ticker, year):
    tempdir = tempfile.mkdtemp()
    try:
        dl = Downloader(UA, EMAIL, download_folder=tempdir)
    except Exception as e:
        print(f"❌ [{ticker}] Downloader init failed: {e}")
        return None

    try:
        if not download_with_retries(dl, "10-K", ticker, f"{year}-01-01", f"{year}-12-31"):
            return None

        root = os.path.join(tempdir, "sec-edgar-filings", ticker, "10-K")
        if not os.path.isdir(root):
            return None
        versions = sorted(os.listdir(root), reverse=True)
        if not versions:
            return None
        sub = os.path.join(root, versions[0], "full-submission.txt")
        if not os.path.isfile(sub):
            return None

        content = open(sub, "r", encoding="utf-8", errors="ignore").read()
        docs = re.findall(r"<DOCUMENT>(.*?)</DOCUMENT>", content, re.DOTALL)
        for doc in docs:
            try:
                tmatch = re.search(r"<TYPE>(.*?)\n", doc)
                if not (tmatch and "10-K" in tmatch.group(1)):
                    continue
                html = re.search(r"<TEXT>(.*)", doc, re.DOTALL)
                if not html:
                    continue
                section = extract_item_1(html.group(1))
                if section:
                    print(f"✅ [{ticker}-{year}] extracted")
                    return {"ticker": ticker, "year": year, "business_section": section}
            except Exception as e:
                print(f"⚠️ [{ticker}-{year}] parse error: {e}")
                continue
        return None

    except Exception as e:
        print(f"❌ [{ticker}-{year}] unexpected error: {e}")
        return None

    finally:
        shutil.rmtree(tempdir)


# ------------------------
# Progress saving/loading
# ------------------------
def save_progress(processed, path=PROGRESS_FILE):
    pd.DataFrame(list(processed), columns=["ticker", "year"]).to_csv(path, index=False)

def load_progress(path=PROGRESS_FILE):
    if os.path.exists(path):
        df = pd.read_csv(path)
        return set(zip(df.ticker, df.year))
    return set()


# ------------------------
# MAIN
# ------------------------
if __name__ == "__main__":
    excel_file = "/Users/lottegeerts/Library/CloudStorage/OneDrive-ErasmusUniversityRotterdam/Scriptie/S&P1500 yearly history overview NEW.xlsx"
    tasks = read_tickers_from_excel(excel_file)
    done  = load_progress()
    to_do = [t for t in tasks if t not in done]

    results = []
    cnt_done, cnt_ok = 0, 0

    lock_proc = threading.Lock()
    lock_res  = threading.Lock()

    with concurrent.futures.ThreadPoolExecutor(max_workers=NUM_WORKERS) as exe:
        futures = {exe.submit(process_ticker_year, t, y): (t, y) for (t, y) in to_do}
        for fut in concurrent.futures.as_completed(futures):
            tkr, yr = futures[fut]
            res = fut.result()

            with lock_proc:
                cnt_done += 1
                done.add((tkr, yr))

            if res:
                with lock_res:
                    results.append(res)
                    cnt_ok += 1

            rate = cnt_ok / cnt_done * 100
            print(f"📊 {cnt_done}/{len(to_do)} done, {cnt_ok} succeeded ({rate:.2f}%)")

            # checkpoint every 500
            if cnt_done % 500 == 0:
                save_progress(done)
                pd.DataFrame(results).to_csv(OUTPUT_CSV, mode="a",
                                             index=False,
                                             header=not os.path.exists(OUTPUT_CSV))
                results.clear()

    # final save
    save_progress(done)
    pd.DataFrame(results).to_csv(OUTPUT_CSV, mode="a",
                                 index=False,
                                 header=not os.path.exists(OUTPUT_CSV))

    print(f"✅ Finished: {cnt_done} processed, {cnt_ok} succeeded.")

