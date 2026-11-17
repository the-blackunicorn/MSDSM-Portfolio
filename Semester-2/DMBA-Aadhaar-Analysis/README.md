# Aadhaar Biometric Activity Analysis

Semester 2, DMBA. Cleaning and analysing public Aadhaar biometric-update data published as multiple CSV files.

## What the code does

- Loads all `api_data_aadhar_biometric_*` CSV files from a folder automatically.
- **Cleans** dates, state names (with a mapping for spelling variants, ampersands and case), city-to-state mapping for misfiled rows, and invalid rows with numeric state or district fields.
- **Validates** pincodes (keeps only valid 6-digit codes) and keeps only recognised states and UTs.
- **Aggregates** biometric activity by pincode and date, by pincode, and by state, and flags high-activity states with more than 1,000,000 total biometric updates.

## File

`Aadhaar_Analysis_Code.docx` contains the full code. The raw data is public and not included. Update the folder path at the top of the code to run it.
