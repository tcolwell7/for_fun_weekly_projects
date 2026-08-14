# python test script for hmrc data pipeline

"""
parse_bds_to_csv.py

This script reads a UK Trade Info bulk data file (e.g. BDSexp2602.txt)
which uses fixed-width columns, and writes a CSV file with named columns.

The column positions come from the official spec under:
"Exports and Imports: BDSExpYYMM and BDSImpYYMM"
https://www.uktradeinfo.com/trade-data/latest-bulk-data-sets/bulk-data-set-export-and-import-technical-specifications/

A timer is added to show how long the parsing takes.
"""

import csv
import time  # Used to measure elapsed time


# ----------------------------------------------------------------------
# Field layout definition
# ----------------------------------------------------------------------
# Each tuple is: (column_name, start_position, end_position)
# start_position and end_position are 1-based and inclusive, exactly as
# in the UK Trade Info table (From / To columns).
#
# Example: ("PERREF", 1, 6) means characters 1 to 6 in the line.
#
# We will convert these to Python's 0-based slicing when parsing.
# ----------------------------------------------------------------------
FIELDS = [
    ("PERREF",            1,   6),  # Period Reference (YYYYMM)
    ("TYPE",              7,   7),  # Record type (1=Declared, 2=BTTA, etc.)
    ("MONTHAC",           8,  13),  # Month of Account (CCYYMM)
    ("COMCODE",          14,  21),  # 8-digit commodity code
    ("SITC",             22,  26),  # 5-digit SITC code
    ("COD_SEQ",          27,  29),  # Country code (numeric sequence)
    ("COD_ALPHA",        30,  31),  # Country code (alpha)
    ("PORT_SEQ",         32,  34),  # Port code (numeric sequence)
    ("PORT_ALPHA",       35,  37),  # Port code (alpha)
    ("COO_SEQ",          38,  40),  # Country of Origin (numeric)
    ("COO_ALPHA",        41,  42),  # Country of Origin (alpha)
    ("MODE_OF_TRANSPORT",43,  44),  # Mode of transport code
    ("STAT_VALUE",       45,  56),  # Statistical value (price + costs)
    ("NET_MASS",         57,  68),  # Net mass in kg
    ("SUPP_UNIT",        69,  80),  # Supplementary unit (e.g. number of items)
    ("SUPPRESSION",      81,  81),  # Suppression indicator (0–3)
    ("FLOW",             82,  84),  # "IMP" or "EXP" etc.
    ("REC_TYPE",         85,  85),  # Record type flag for aggregation
]


def parse_line(line: str) -> dict:
    """
    Parse a single fixed-width line into a dictionary of field values.

    Parameters
    ----------
    line : str
        One line from the BDS text file (without the trailing newline).

    Returns
    -------
    dict
        A dictionary mapping field names (e.g. "PERREF", "TYPE") to their
        string values extracted from the line.
    """
    row = {}

    for name, start, end in FIELDS:
        # Convert from 1-based inclusive positions (as in the spec)
        # to 0-based Python slice indices.
        #
        # Spec: characters start..end (inclusive, 1-based)
        # Python: line[start0:end0] where start0 = start - 1, end0 = end
        #
        # Example: start=1, end=6  ->  line[0:6]  (characters 1–6)
        start0 = start - 1
        end0 = end  # Python's slice end is exclusive, which matches spec end+1

        # Safely extract the substring.
        # If the line is shorter than expected, just take what's available.
        if len(line) >= end0:
            value = line[start0:end0]
        else:
            value = line[start0:]

        row[name] = value

    return row


def parse_bds_file(input_path: str, output_path: str) -> None:
    """
    Read a fixed-width BDS file and write a CSV file with headers.

    This version also prints how long the parsing took.

    Parameters
    ----------
    input_path : str
        Path to the input text file (e.g. "BDSexp2602.txt").
    output_path : str
        Path to the output CSV file (e.g. "BDSexp2602_parsed.csv").
    """
    # Record the start time
    start_time = time.time()

    # Open the input text file for reading
    with open(input_path, "r", encoding="utf-8") as f_in:
        # Open the output CSV file for writing
        # newline="" is recommended when using csv module in Python
        with open(output_path, "w", encoding="utf-8", newline="") as f_out:

            # Build the list of column names in order from FIELDS
            fieldnames = [name for name, _, _ in FIELDS]

            # Create a CSV writer that writes dictionaries to rows
            writer = csv.DictWriter(f_out, fieldnames=fieldnames)

            # Write the header row (column names) to the CSV
            writer.writeheader()

            # Process the input file line by line
            for line_num, line in enumerate(f_in, start=1):
                # Remove trailing newline characters (\n, \r)
                line = line.rstrip("\n\r")

                # Skip completely empty lines (if any)
                if not line.strip():
                    continue

                # Optional: basic length check.
                # According to the spec, each record should be 85 characters long.
                # If it's shorter, something might be wrong, but we still try to parse.
                if len(line) < 85:
                    # You could log a warning here if you want, e.g.:
                    # print(f"Warning: line {line_num} is shorter than 85 chars")
                    pass

                # Parse the fixed-width line into a dictionary of fields
                row = parse_line(line)

                # Write the dictionary as a CSV row
                writer.writerow(row)

    # Record the end time and compute elapsed time in seconds
    end_time = time.time()
    elapsed_seconds = end_time - start_time

    # Print how long the parsing took
    print(f"Done. Wrote {output_path}")
    print(f"Time taken: {elapsed_seconds:.2f} seconds")


# ----------------------------------------------------------------------
# Main entry point
# ----------------------------------------------------------------------
if __name__ == "__main__":
    # ------------------------------------------------------------------
    # Configure input and output file paths here
    # ------------------------------------------------------------------
    # Change these to match your actual file names / paths.
    input_file = "BDSexp2602.txt"  # Input: fixed-width text file (e.g. first 10k lines)
    output_file = "BDSexp2602_parsed.csv"  # Output: CSV file

    # Call the parser
    parse_bds_file(input_file, output_file)