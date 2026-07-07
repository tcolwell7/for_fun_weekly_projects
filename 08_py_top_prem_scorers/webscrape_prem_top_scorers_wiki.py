import pandas as pd
import requests
from bs4 import BeautifulSoup
from io import StringIO
import re

url = "https://en.wikipedia.org/wiki/List_of_top_Premier_League_goal_scorers_by_season"
headers = {"User-Agent": "Mozilla/5.0"}

# Step 1: download the page once and save the HTML locally
r = requests.get(url, headers=headers, timeout=30)
r.raise_for_status()
html = r.text
with open("premier_league_scorers_page.html", "w", encoding="utf-8") as f:
    f.write(html)

# Step 2: parse the HTML we just downloaded
soup = BeautifulSoup(html, "html.parser")
dfs = []

# Helper: convert season ids like 1992–93 to 1992/93
# If the id is already a nice season label, it stays as-is.
def season_label(raw):
    raw = str(raw).strip()
    if "–" in raw:
        a, b = raw.split("–", 1)
        return f"{a}/{b}"
    return raw

# Helper: try to extract nationality from the same table row
# by looking for a flag/country-style link in the row HTML.
def extract_nationality_from_row(tr):
    # Common case: a country/flag link in the same row
    # We inspect linked text inside the row and prefer country-like labels.
    country_candidates = []
    for a in tr.find_all("a", href=True):
        txt = a.get_text(" ", strip=True)
        href = a.get("href", "")
        if not txt:
            continue
        # Ignore player links and general wiki links that are not nationality
        if "/wiki/" not in href:
            continue
        # Heuristic: keep short country-like linked text and flag-related labels
        if len(txt) <= 25 and txt.lower() not in {"edit", "[edit]"}:
            country_candidates.append(txt)

    # Prefer obvious country names, otherwise return the first likely country-ish link
    preferred = [
        "England", "Wales", "Scotland", "Northern Ireland", "Ireland",
        "France", "Netherlands", "Germany", "Spain", "Italy", "Brazil",
        "Argentina", "Portugal", "Belgium", "Denmark", "Norway", "Sweden",
        "Ivory Coast", "Nigeria", "Ghana", "Cameroon", "Senegal", "Iceland"
    ]
    for p in preferred:
        if p in country_candidates:
            return p

    # If the row contains a visible flag alt/title/country label, try that too
    for img in tr.find_all("img"):
        alt = img.get("alt", "").strip()
        title = img.get("title", "").strip()
        for val in (alt, title):
            if val and len(val) <= 25:
                return val

    return ""

# Step 3: walk through heading + table pairs
for heading in soup.select("div.mw-heading.mw-heading2 h2"):
    season = season_label(heading.get("id", ""))
    if not season:
        continue

    block = heading.find_parent("div")
    table = block.find_next_sibling("table") if block else None
    if table is None:
        continue

    # Read the scorer table into pandas
    df = pd.read_html(StringIO(str(table)))[0]
    df.columns = [str(c).split("[")[0].strip() for c in df.columns]

    # Keep only the expected scorer-table columns
    if not {"Rank", "Player", "Goals"}.issubset(df.columns):
        continue

    club_col = None
    if "Club" in df.columns:
        club_col = "Club"
    elif "Team" in df.columns:
        club_col = "Team"

    if club_col is None:
        continue

    # Add season and nationality row-by-row using the original HTML row
    rows = table.find_all("tr")
    out_rows = []

    for i, (_, row) in enumerate(df.iterrows(), start=1):
        # Skip header row when matching against HTML rows
        if i >= len(rows):
            break
        tr = rows[i]
        nat = extract_nationality_from_row(tr)
        out_rows.append({
            "Rank": row["Rank"],
            "Player": row["Player"],
            "Club": row[club_col],
            "Goals": row["Goals"],
            "Nationality": nat,
            "Season": season
        })

    if out_rows:
        dfs.append(pd.DataFrame(out_rows))

# Step 4: combine and save
if not dfs:
    raise RuntimeError("No season tables were extracted.")

final_df = pd.concat(dfs, ignore_index=True)
final_df["Goals"] = pd.to_numeric(final_df["Goals"], errors="coerce").astype("Int64")
final_df["Rank"] = pd.to_numeric(final_df["Rank"], errors="coerce").astype("Int64")
final_df.to_csv("premier_league_top_scorers_full.csv", index=False, encoding="utf-8-sig")

print(final_df.head(20).to_string(index=False))
print("Saved premier_league_top_scorers_full.csv")