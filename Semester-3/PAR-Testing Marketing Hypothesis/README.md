# Testing Marketing Hypotheses at WSES

Individual case analysis in **R** (hypothesis testing), IIMB case IMB 693, from the PAR course.

**Question:** six beliefs held by the sales and marketing team decide where sales effort goes. Which of them does the data support?

## Data and method

1,000 past sales opportunities (2014–2018), 48.1% won, with a mean deal size of USD 8.04 Mn. One test per belief at a 5% significance level.

## Scorecard: two beliefs hold, two lack evidence, two are backwards

| Belief | Verdict | Evidence |
|--------|---------|----------|
| Win chances differ by product | Supported | Win rates range from 29% to 69% (χ² = 29.03, df = 6) |
| We win more often in Africa than in the UK | Supported | 59.1% against 48.3% (z = 1.94, p = 0.026), though Africa has only 93 deals |
| The average deal is at least USD 8 Mn | Not rejected | Mean is 8.04 (p = 0.76) |
| Average deal size differs by region | Not supported | ANOVA p = 0.26, confirmed by Welch, Kruskal-Wallis and Bartlett checks |
| Higher relative product strength means more wins | Reverse is true | Won deals had weaker products (46.1) than lost ones (52.9) |

The slides cover all six beliefs in full.

## Files

- `Testing Markeing Hypothesis_Abhishek Dewangan PPT.pptx`: 13-slide presentation
- `WSES_hypothesis_tests Abhishek.R`: full analysis code

The case dataset belongs to the case publisher and is not included.
