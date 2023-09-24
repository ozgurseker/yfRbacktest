# yfRbacktest

## Repo Content

- **data/tickerlistall.xlsx** : Contains list of all symbols in the Istanbul Stock Exchange, raw data is scraped from isyatirim.com and cleaned with cleanrawtickerlist.R script.
- **savedata.ipynb** : Jupyter notebook to save data from yfinance. Takes the tickerlist in data folder. Then download and save daily historical data for all tickers. You could change saving location and startdate.
- **technicalanalysis.R** : Allows you to use daily, weekly, and monthly indicators at the same time. Comments in the script could guide you. 

## Usage

- The goal of this repo is to backtest different filtering methods.
- Additionally to allow to use daily/weekly/monthly filters at the same time.
- After downloading the repo, run the **savedata.R** file to get data from yfinance.
- Then by using **technicalanalysis.R**, you could add any indicator in daily/weekly/monthly time frame and save the combined dataset.
- The rest is up to you. 
