# Marketing Head's Conundrum

Individual case analysis in **R** (discriminant analysis and optimisation), IIMB case IMB 541, from the PAR course.

**Question:** can WSES halve its marketing spend on the Procsys sales pipeline while keeping 90% of expected sales and profit?

## Approach

1. Learn from 2,965 past sales opportunities (48.9% won) and test on 1,000 held-out ones.
2. Predict each deal's win probability with logistic regression and LDA. Both perform almost identically.
3. Use those probabilities in an **integer optimisation (lpSolve)** that decides, deal by deal, whether to fund each of the 142 live Procsys deals so that the case's four targets are still met.

## Findings

- A flat 6% marketing rate treats every deal the same, even though some customers would have bought anyway and others were never going to buy.
- Four numbers predict almost every win or loss. The model fits unusually tightly (McFadden pseudo-R² of 0.92) and is right on **93% of 1,000 unseen deals**.
- **The budget halves**, from $76.3 Mn to **$38.2 Mn**, with 70 of 142 deals still funded and every target met. Of the 57 deals with over a 90% chance of winning, 53 stay funded. 62 of the 76 deals under a 10% chance lose their funding.
- The case's 50% pipeline rule is what stops more savings. It forces funding of 14 deals with under a 10% chance. Dropping the floor to 40% raises the saving to 60% ($45.8 Mn).

## Files

- `Marketing Head’s Conundrum_Abhishek Dewangan(PPT).pptx`: 12-slide presentation
- `Marketing Conundrum(Abhishek Dewangan).R`: full analysis code

The case dataset belongs to the case publisher and is not included.
