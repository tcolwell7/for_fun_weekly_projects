import os
import zipfile
import requests
import pandas as pd
from bs4 import BeautifulSoup
from io import BytesIO

ARCHIVE_URL = "https://www.uktradeinfo.com/trade-data/latest-bulk-data-sets/bulk-data-sets-archive/"

# Save output relative to where the .py script is stored
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
OUTPUT_DIR = os.path.join(SCRIPT_DIR, "output")
os.makedirs(OUTPUT_DIR, exist_ok=True)

OUTPUT_FILE = os.path.join(OUTPUT_DIR, "pur_2022_test.csv")

# Get archive page
page = requests.get(ARCHIVE_URL)
page.raise_for_status()
soup = BeautifulSoup(page.text, "html.parser")

# Find the correct section heading
heading = soup.find(
    lambda tag: tag.name in ["h2", "h3"]
    and "Import data by preference 2022 onwards" in tag.get_text(" ", strip=True)
)

if heading is None:
    raise ValueError("Could not find the 'Import data by preference 2022 onwards' section.")

# Find the 2022 ZIP link
zip_url = None
for a in heading.find_all_next("a", href=True):
    text = a.get_text(" ", strip=True)

    if text.startswith("Preference: 2022"):
        zip_url = a["href"]
        break

    if a.name in ["h2", "h3"] and "2021" in a.get_text(" ", strip=True):
        break

if zip_url is None:
    raise ValueError("Could not find the 2022 preference ZIP link.")

# Convert relative link to full URL if needed
if zip_url.startswith("/"):
    zip_url = "https://www.uktradeinfo.com" + zip_url

print("Downloading 2022 ZIP...")
r = requests.get(zip_url)
r.raise_for_status()

# Read all CSV files inside the ZIP
dfs = []
with zipfile.ZipFile(BytesIO(r.content)) as z:
    csv_files = [f for f in z.namelist() if f.lower().endswith(".csv")]
    print(f"CSV files found: {len(csv_files)}")

    for file in csv_files:
        print(f"Reading: {file}")
        with z.open(file) as f:
            df = pd.read_csv(f, low_memory=False)
            df["source_file"] = os.path.basename(file)
            dfs.append(df)

# Combine all 2022 files
full_2022_df = pd.concat(dfs, ignore_index=True)
print(f"Total 2022 rows before sample: {len(full_2022_df):,}")

# Keep first 1,000 rows only
test_df = full_2022_df.head(1000)

# Save sample
test_df.to_csv(OUTPUT_FILE, index=False)

print(f"Test file saved to: {OUTPUT_FILE}")
print(f"Rows saved: {len(test_df):,}")