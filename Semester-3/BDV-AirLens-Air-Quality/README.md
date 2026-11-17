# AirLens — Station-Level Air Quality Pipeline, Visualization and Dashboard

**Course:** Big Data and Data Visualization (BDV), IIM Amritsar. **Group 3** project, instructor Dr Sunil Reddy Kunduru.

## Problem

Under India's National Clean Air Programme (NCAP), cities that improve get more funding. But air data sits across many websites, has hidden gaps and is hard to compare. Officials struggle to answer three questions: where to act first, what to target, and whether it is working.

## Solution

AirLens merges **10+ years of daily city AQI data into one clean table**: **530,955 rows, 326 cities, 33 states and UTs, January 2016 to September 2026**. Four linked Tableau dashboards let an official answer those questions in a few clicks: Overview, Geographic Explorer, Time & Season, and Pollutants.

| Source | Period | Rows | Cities |
|--------|--------|------|--------|
| UrbanEmissions.info (CPCB daily city AQI) | Jan 2016 – Dec 2025 | 468,105 | 297 |
| JanVayu | Jan 2026 – 17 Sep 2026 | 62,850 | 303 |
| **AirLens (combined)** | **Jan 2016 – Sep 2026** | **530,955** | **326** |

Data issues handled: inconsistent city spellings, missing monitor days kept as gaps rather than counted as clean days, and an all-India average that can mislead as cleaner cities join the data.

## Key findings

- **68,209 bad-air days** (AQI above 200) across all cities, with average AQI of 113.4.
- The problem is concentrated. The worst 10 cities hold 22% of bad-air days, the worst 30 hold 46%, the worst 50 hold 61%.
- Delhi had the most bad-air days (1,913), on 49% of all days and 88% of winter days. Seven Delhi-area cities alone account for 16%.
- **76% of bad-air days fall between October and February.** Average AQI is 153 in winter and 142 post-monsoon, against 113 in summer.
- PM10 and PM2.5 are the most prominent pollutants, higher in winter and post-monsoon.

AirLens helps officials set priorities. It does not predict pollution or trace its source; both are listed as future work in the report.

## Files

| File | Description |
|------|-------------|
| `AirLens_BDV_Report.pdf` | Full project report |
| `AirLens_Data_Pipeline.ipynb` | Data loading, cleaning, merging and validation code |
| `AirLens_Tableau_Workbook.twb` | Tableau workbook with the four dashboards |
| `airlens_city_coverage.csv`, `airlens_source_metadata.csv` | Coverage and source metadata |

The full daily dataset (`airlens_historical_daily.csv`, about 110 MB) is not included because it exceeds GitHub's file size limit. It can be rebuilt from the two sources above using the pipeline notebook.
