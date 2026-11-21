# Campaign Analytics: ANOVA, Regression and Targeting in R

**Group project**, Predictive Analytics with R (PAR), Semester 3.

## What it does

One R script that works through a retail campaign dataset of 6,000 customers, randomised into Control, Voucher and Free Shipping groups, with a separate Expedited or Standard delivery pilot.

**Part 1**
- One-way ANOVA on 90-day contribution. Shapiro-Wilk and Levene checks decide between classic and Welch ANOVA, followed by Tukey or Games-Howell post-hoc tests.
- A two-factor extension with interaction (campaign group × delivery group).
- Multiple linear regression for 90-day net sales, with VIF checks, residual diagnostics, influence checks, and a log-model candidate. The test set is scored once.

**Part 2**
- Randomisation balance and data audit.
- Chi-square test of repeat-purchase rate by campaign group.
- Welch t-test on delivery days, Expedited vs Standard.
- Classification of repeat buyers: decision tree, random forest and gradient boosting, with ROC and precision-recall evaluation.
- A budgeted target list (INR 15 lakh, up to 1,200 customers) with a randomised no-treatment holdout.

## Files

| File | Description |
|------|-------------|
| `project_group_par.R` | Full analysis script |
| `*.png` | Diagnostic and model plots: ANOVA Q-Q and interaction, regression fit and Cook's distance, CART tree, ROC curve |

The dataset belongs to the course and is not included. To rerun, point `DATA_FILE` at the Excel file in the CONFIG block at the top of the script.
