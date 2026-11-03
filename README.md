# S&P 500 Bootstrap

Is the average daily return of the S&P 500 really above zero? This project uses simulation to explain bootstrap confidence intervals and hypothesis testing.

## Folder structure

```
sp500-bootstrap/
├── README.md
├── sp500-bootstrap.Rproj
├── scripts/
│   ├── 01_download.R      # download S&P 500 prices from FRED, compute daily returns
│   ├── 02_bootstrap.R     # bootstrap distribution and 100 confidence intervals
│   └── 03_test.R          # null distribution, rejection region and p-value
├── data/
│   ├── sp500_raw.csv
│   └── sp500_returns.csv
├── output/                # simulation results and coverage tables
└── figures/               # interactive Plotly figures (html)
```

## Data
- S&P 500 daily closing index from FRED (series SP500): https://fred.stlouisfed.org/series/SP500. FRED provides about the last 10 years.

## How to reproduce
1. Open `sp500-bootstrap.Rproj` in RStudio.
2. Install packages once: `install.packages(c("dplyr", "plotly", "htmlwidgets"))`
3. Run the scripts in order: `01_download.R`, `02_bootstrap.R`, `03_test.R`. The random seed is fixed, so results are the same each time.
