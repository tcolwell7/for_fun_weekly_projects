Overview
This project automates the process of downloading annual Preference Utilisation of UK Trade in Goods database files from GOV.UK.
Each year’s dataset is published on its own page, and historically the process of retrieving these files has required manual navigation, clicking, and downloading.

The goal of this script is to replace that manual workflow with a fully reproducible, automated, and scalable solution using R and the rvest package. 

By running a single function, the script:

- identifies the correct yearly page

- extracts the relevant database file links

- downloads the .ods files into a local folder

- prepares the data for further cleaning and analysis

This ensures that future updates (e.g., when new years are added, data schema changes) require minimal manual intervention.

Comment:

I first learnt web‑scraping in R long before COVID — back when I was scraping Wikipedia pages for football stats (Spurs) and UK public spending figures just for fun (my sad life). Having access to LLMs now makes it much quicker to generate code, which is great, but I’m glad I learnt the fundamentals the slow way. Understanding how HTML works, how to inspect a webpage’s “under the hood” structure, and how to debug scraping errors so I'm confident to challenge AI‑generated code rather than blindly trusting it. 

For this project, even though the dataset itself is straightforward trade data, I wanted to approach the code more holistically as if I were setting up a small pipeline that would be reused and updated over time. The web‑scrape itself is simple, so it doesn’t need heavy engineering or unit testing, but I still wanted to include basic structural checks and patterns that make the workflow more robust and reproducible.

That reflects how I like to work, more of a technical data analyst who can build pipelines, not just run one‑off scripts. Even small projects are an opportunity to practise good habits — automation, reproducibility, and thinking ahead to future updates.

Anyway, this is for fun and always enjoy when a web-scrape works! 😎
