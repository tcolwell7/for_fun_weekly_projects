### Overview

This project recreates a Python data pipeline based on an earlier workflow that, in a slightly different form, was used for automated live data updates through Airflow. The pipeline scrapes source data from the web, performs a raw extraction with basic validation checks, and then applies a transformation step before saving the cleaned output as a CSV for further use and analysis.

### What this project covers

- Web scraping in Python using BeautifulSoup
- Building a simple end-to-end data pipeline
- Separating extraction and transformation steps
- Applying basic validation and error checks during ingestion
- Producing a structured output file for downstream analysis
- Demonstrating the foundations of a real-world automated data workflow

