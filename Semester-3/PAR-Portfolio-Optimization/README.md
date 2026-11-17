# Portfolio Optimization (PAR)

Optimization of a five-stock portfolio in Excel, before and after, across four historical windows.

## Stocks

NTPC, Bharti Airtel, HAL, M&M, ICICI Bank

## Method

For each window the workbook computes daily returns, expected returns and annual returns, starts from equal weights (0.2 each), and then uses **Excel Solver** to find optimal weights.

| Window | Sheets |
|--------|--------|
| 5 years | Before / After Optimization |
| 3 years | Before / After Optimization |
| 1 year | Before / After Optimization |
| 6 months | Before / After Optimization |

As an example, on the 1-year window Solver moved the equal 20% weights into about 17.8% Bharti Airtel and 82.2% HAL.

## File

`PAR_Portfolio_Optimization.xlsx` has 8 sheets, a before and an after for each window.
