# Predicting Customer Churn at QWE Inc.

Individual case analysis in **R** (logistic regression), Darden case UV6694, from the PAR course.

**Question:** which customers will leave in the next two months, and why?

## Data

6,347 customers, with 323 (5.1%) leaving within two months, and 11 characteristics each: customer age in months, a happiness score (CHI), support cases and priority, logins and their month-on-month changes.

## Findings

- The sales head's hunch was only half right. Customers aged **6 to 14 months are the riskiest** (7.2% churn, against 4.1% for the rest), but loyal customers over 14 months are not safe either.
- **The real risk is a date.** 20.7% of customers leave at exactly month 12, against 4.4% at every other age. Annual contracts expire then. Adding a month-12 flag to the model matters.
- Age group and the happiness score do most of the work in the final model. Support priority, logins, blog articles and days since last login were dropped by stepwise selection. All VIFs stay below 2.4.
- The model is good at ranking, not at certainty: **AUC 0.72**, 3.0× lift in the riskiest 10%, and 63% of leavers caught in the riskiest 30%. Of the top-100 list, 24% actually left, about 5× the base rate.
- The cut-off is a business decision. A low cut-off catches most leavers but floods the team with false alarms, so if the team can make 100 calls, rank customers and take the top 100.

## Files

- `Predicting Customer Churn_Abhishek Dewangan(PPT).pptx`: 13-slide presentation
- `QWE Churn Analysis(Abhishek Dewangan).R`: full analysis code

The case dataset belongs to the case publisher and is not included.
