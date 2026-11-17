# Food Wastage Analysis

Semester 2, FDS. Understanding and predicting the **percentage of cooked food that gets wasted** in a college hostel mess from daily meal records.

## Data

360 daily records and 10 columns (attendance, food cooked in kg, food rating, menu category, meal, day, academic pressure and others), with no missing values. Target: `Wastage %`, ranging from about 8.8% to 24% with a mean near 16.3%.

## Workflow

1. Load and explore the data.
2. Preprocess: fix the date format, cap outliers with the IQR method instead of dropping rows, label-encode categories.
3. EDA by meal, weekday, holidays and correlations.
4. Model **Linear Regression** and **Random Forest Regressor**, with the scaler fit on training data only to avoid leakage.
5. Compare on R², MAE and RMSE, and read feature importance.

## Findings

- Average wastage is around **16.3%** of food cooked.
- **Dinner** has the highest wastage, and **weekends and holidays** are higher too, since food is cooked in fixed quantities while fewer students are present.
- Attendance and food rating are negatively correlated with wastage.
- **Random Forest beats Linear Regression** on R², MAE and RMSE, because wastage depends on several non-linear factors.

## Files

`Food_Wastage_Analysis.ipynb`. The notebook expects the dataset as `final_data.csv`, which is not included.
