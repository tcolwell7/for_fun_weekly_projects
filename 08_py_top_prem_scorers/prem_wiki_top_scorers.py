# Import the libraries we need
import pandas as pd
import requests
from io import StringIO
from bs4 import BeautifulSoup

# The Wikipedia page we want to scrape
url = "https://en.wikipedia.org/wiki/List_of_top_Premier_League_goal_scorers_by_season"

# Add a basic browser-style user agent so Wikipedia is less likely to block the request
headers = {
    "User-Agent": "Mozilla/5.0"
}

# Download the page HTML
response = requests.get(url, headers=headers, timeout=30)

# Stop the script immediately if the request failed
response.raise_for_status()

# Parse the HTML with BeautifulSoup so we can inspect headings and tables
soup = BeautifulSoup(response.text, "html.parser")

# Create an empty list to store each season's DataFrame
dfs = []

# Find every season heading on the page
# On this Wikipedia page, each season heading sits inside:
# <div class="mw-heading mw-heading2"> ... <h2 id="1992–93"> ... </h2>
for heading in soup.select("div.mw-heading.mw-heading2 h2"):

    # Get the season from the h2 id attribute
    # Example: "1992–93"
    season = heading.get("id")

    # If for some reason there is no id, skip this heading
    if not season:
        continue

    # Move up to the parent div, then find the next table after that heading block
    table = heading.find_parent("div").find_next_sibling("table")

    # If no table is found, skip and move to the next heading
    if table is None:
        continue

    # Read that single HTML table into a pandas DataFrame
    df = pd.read_html(StringIO(str(table)))[0]

    # Clean the column names
    # This removes citation markers like Goals[3] -> Goals
    df.columns = [str(c).split("[")[0].strip() for c in df.columns]

    # Some seasons use "Club", one uses "Team"
    # Work out which column name is present
    if "Club" in df.columns:
        club_col = "Club"
    elif "Team" in df.columns:
        club_col = "Team"
    else:
        club_col = None

    # Only keep tables that actually match the scorer-table structure
    if {"Rank", "Player", "Goals"}.issubset(df.columns) and club_col is not None:

        # Keep only the columns we care about
        temp = df[["Rank", "Player", club_col, "Goals"]].copy()

        # Rename Team -> Club so all seasons use the same column name
        temp = temp.rename(columns={club_col: "Club"})

        # Add the season from the heading id
        temp["Season"] = season

        # Add this cleaned season table to our list
        dfs.append(temp)

# Combine all season tables into one big DataFrame
final_df = pd.concat(dfs, ignore_index=True)

# Optional cleanup:
# Reorder the columns into the exact order we want
final_df = final_df[["Rank", "Player", "Club", "Goals", "Season"]]

# Optional cleanup:
# Convert season format from 1992–93 to 1992/93
final_df["Season"] = final_df["Season"].str.replace("–", "/", regex=False)

# Show the first 20 rows so we can check the result
print(final_df.head(20))

# Save to CSV
final_df.to_csv("premier_league_top_scorers.csv", index=False, encoding="utf-8-sig")