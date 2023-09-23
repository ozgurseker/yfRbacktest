# yfRbacktest

## Repo Content

- data/tickerlistall.xlsx : Contains list of all symbols in the Istanbul Stock Exchange, raw data is scraped from isyatirim.com and cleaned with cleanrawtickerlist.R script.
- savedata.R : Takes the tickerlist in data folder. Then download and save daily historical data for all tickers. You could change saving method and startdate.
- technicalanalysis.R : Allows you to use daily, weekly, and monthly indicators at the same time. Comments in the script could guide you. 
