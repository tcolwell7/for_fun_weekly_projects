import pandas as pd
import numpy as np

# --------------------------------------------------
# 1. LOAD RAW DATA
# --------------------------------------------------
# Read in the raw file created by pur_data.py
data = pd.read_csv("output/pur_data_full.csv")

# Stop pandas showing scientific notation when checking outputs
pd.set_option("display.float_format", lambda x: "%.5f" % x)


# --------------------------------------------------
# 2. CREATE NEW PUR COLUMNS
# --------------------------------------------------
# These columns copy your notebook logic.
# Each one is the Python version of a SQL CASE WHEN statement.

data["imports_ex_special"] = np.where(
    data["statreg"] == 1,
    data["statvalue"],
    0
)

data["eligibility_mfn"] = np.where(
    (data["eligibility"] == "e1") & (data["statreg"] == 1),
    data["statvalue"],
    0
)

data["eligibility_gsp"] = np.where(
    (data["eligibility"] == "e2") &
    (data["statreg"] == 1) &
    (~data["use"].isin(["u10", "uzz"])),
    data["statvalue"],
    0
)

data["eligibility_fta"] = np.where(
    (data["eligibility"] == "e3") &
    (data["statreg"] == 1) &
    (~data["use"].isin(["u10", "uzz"])),
    data["statvalue"],
    0
)

data["eligibility_combined_pref"] = np.where(
    (data["eligibility"] == "e5") &
    (data["statreg"] == 1) &
    (~data["use"].isin(["u10", "uzz"])),
    data["statvalue"],
    0
)

data["eligibility_unknown"] = np.where(
    (data["eligibility"] == "ez") & (data["statreg"] == 1),
    data["statvalue"],
    0
)

data["eligibility_pref_mfn_0"] = np.where(
    (data["eligibility"].isin(["e2", "e3", "e5"])) &
    (data["statreg"] == 1) &
    (data["use"] == "u10"),
    data["statvalue"],
    0
)

data["eligibility_pref_unknown"] = np.where(
    (data["eligibility"].isin(["e2", "e3", "e5"])) &
    (data["statreg"] == 1) &
    (data["use"] == "uzz"),
    data["statvalue"],
    0
)

data["use_mfn_0"] = np.where(
    (data["statreg"] == 1) & (data["use"] == "u10"),
    data["statvalue"],
    0
)

data["use_mfn_non_0"] = np.where(
    (data["statreg"] == 1) & (data["use"] == "u11"),
    data["statvalue"],
    0
)

data["use_gsp_0"] = np.where(
    (data["statreg"] == 1) & (data["use"] == "u20"),
    data["statvalue"],
    0
)

data["use_gsp_non_0"] = np.where(
    (data["statreg"] == 1) & (data["use"] == "u21"),
    data["statvalue"],
    0
)

data["use_fta_0"] = np.where(
    (data["statreg"] == 1) & (data["use"] == "u30"),
    data["statvalue"],
    0
)

data["use_fta_non_0"] = np.where(
    (data["statreg"] == 1) & (data["use"] == "u31"),
    data["statvalue"],
    0
)

data["use_unknown"] = np.where(
    (data["statreg"] == 1) & (data["use"] == "uzz"),
    data["statvalue"],
    0
)

data["eligibility_pref"] = np.where(
    (data["eligibility"].isin(["e2", "e3", "e5"])) &
    (data["statreg"] == 1) &
    (~data["use"].isin(["u10", "uzz"])),
    data["statvalue"],
    0
)

data["use_pref"] = np.where(
    (data["statreg"] == 1) &
    (data["use"].isin(["u20", "u21", "u30", "u31"])),
    data["statvalue"],
    0
)


# --------------------------------------------------
# 3. CREATE YEAR AND MONTH
# --------------------------------------------------
# perref is in YYYYMM format, so split it into separate fields
data["perref"] = data["perref"].astype(str)
data["year"] = data["perref"].str[:4]
data["month"] = data["perref"].str[-2:]


# --------------------------------------------------
# 4. AGGREGATE TO FINAL LEVEL
# --------------------------------------------------
# Group to the same level shown in your notebook:
# country of origin + country of dispatch + commodity code + year
# If you want month included in the final output, keep "month" in group_cols.

group_cols = ["cooalpha", "codalpha", "comcode", "year"]

sum_cols = [
    "statvalue",
    "imports_ex_special",
    "eligibility_mfn",
    "eligibility_gsp",
    "eligibility_fta",
    "eligibility_combined_pref",
    "eligibility_unknown",
    "eligibility_pref_mfn_0",
    "eligibility_pref_unknown",
    "use_mfn_0",
    "use_mfn_non_0",
    "use_gsp_0",
    "use_gsp_non_0",
    "use_fta_0",
    "use_fta_non_0",
    "use_unknown",
    "eligibility_pref",
    "use_pref"
]

final_data = (
    data.groupby(group_cols, dropna=False)[sum_cols]
    .sum()
    .reset_index()
)


# --------------------------------------------------
# 5. CALCULATE PUR %
# --------------------------------------------------
# If eligible preference imports are greater than 0,
# PUR % = use_pref / eligibility_pref
# Otherwise return missing value

final_data["pur_pct"] = np.where(
    final_data["eligibility_pref"] > 0,
    final_data["use_pref"] / final_data["eligibility_pref"],
    np.nan
)


# --------------------------------------------------
# 6. OPTIONAL RENAME
# --------------------------------------------------
# Rename statvalue so the final table is easier to read
final_data = final_data.rename(columns={"statvalue": "imports_total"})


# --------------------------------------------------
# 7. CHECK RESULT
# --------------------------------------------------
print(final_data.head())
print(f"\nFinal row count: {len(final_data):,}")


# --------------------------------------------------
# 8. SAVE OUTPUT
# --------------------------------------------------
final_data.to_csv("output/pur_ref_dataset.csv", index=False)