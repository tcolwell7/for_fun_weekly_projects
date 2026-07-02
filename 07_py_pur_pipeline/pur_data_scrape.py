import os
import zipfile
import requests
import pandas as pd
from bs4 import BeautifulSoup
from io import BytesIO

# Archive page that contains the yearly preference ZIP links
ARCHIVE_URL = "https://www.uktradeinfo.com/trade-data/latest-bulk-data-sets/bulk-data-sets-archive/"

# Final output file location
OUTPUT_FILE = "output/pur_2022_2025_combined.csv"


# Save output relative to where the .py script is stored
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
OUTPUT_DIR = os.path.join(SCRIPT_DIR, "output")
os.makedirs(OUTPUT_DIR, exist_ok=True)

OUTPUT_FILE = os.path.join(OUTPUT_DIR, "pur_data_full.csv")

# -----------------------------
# 1. Scrape the archive page
# -----------------------------
page = requests.get(ARCHIVE_URL)
page.raise_for_status()  # stop immediately if the page request fails
soup = BeautifulSoup(page.text, "html.parser")

# Find the heading for the section we care about:
# "Import data by preference 2022 onwards"
heading = soup.find(
    lambda tag: tag.name in ["h2", "h3"]
    and "Import data by preference 2022 onwards" in tag.get_text(strip=True)
)

# Collect yearly ZIP links from that section only
year_links = []

for a in heading.find_all_next("a", href=True):
    text = a.get_text(" ", strip=True)

    # Keep only the preference links for 2022 to 2026
    if text.startswith("Preference:"):
        if any(y in text for y in ["2022", "2023", "2024", "2025", "2026"]):
            year = [y for y in ["2022", "2023", "2024", "2025", "2026" ] if y in text][0]
            url = a["href"]

            # Some links may be relative rather than full URLs
            if url.startswith("/"):
                url = "https://www.uktradeinfo.com" + url

            year_links.append((year, url))

    # Stop once we move into the older 2021 section
    elif a.name in ["h2", "h3"] and "Imports by Preference data 2021" in a.get_text(" ", strip=True):
        break


# -----------------------------
# 2. Load each year
# -----------------------------
all_dfs = []
base_columns = None  # this will store the 2022 schema for comparison

for year, url in sorted(year_links):
    print(f"\nLoading year: {year}")

    # Download the ZIP file for that year
    r = requests.get(url)
    r.raise_for_status()

    # Open the ZIP file in memory without saving it locally first
    with zipfile.ZipFile(BytesIO(r.content)) as z:

        # Keep only CSV files inside the ZIP
        csv_files = [f for f in z.namelist() if f.lower().endswith(".csv")]
        print(f"Monthly files found: {len(csv_files)}")

        year_dfs = []

        for file in csv_files:
            # Read each monthly CSV into a dataframe
            with z.open(file) as f:
                df = pd.read_csv(f, low_memory=False)
                # Keep first 1,000 rows only - only creating test data
                df = df.head(1000)
                year_dfs.append(df)

        # Combine all monthly files for the current year
        year_df = pd.concat(year_dfs, ignore_index=True)

        # Use 2022 as the baseline schema
        if year == "2022":
            base_columns = list(year_df.columns)
            print(f"2022 set as baseline schema ({len(base_columns)} columns)")

        else:
            # Compare later years against the 2022 column structure
            current_columns = list(year_df.columns)
            missing = sorted(set(base_columns) - set(current_columns))
            extra = sorted(set(current_columns) - set(base_columns))

            if missing or extra:
                print(f"Schema issue in {year}")
                if missing:
                    print(f"  Missing columns: {missing}")
                if extra:
                    print(f"  Extra columns: {extra}")
            else:
                print(f"Schema matches 2022")

        print(f"Rows loaded for {year}: {len(year_df):,}")
        all_dfs.append(year_df)


# -----------------------------
# 3. Combine all years and save
# -----------------------------
final_df = pd.concat(all_dfs, ignore_index=True)
final_df.to_csv(OUTPUT_FILE, index=False)

print(f"\nFinal combined rows: {len(final_df):,}")
print(f"Saved to: {OUTPUT_FILE}")