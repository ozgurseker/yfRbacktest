library(yahoofinancer)
library(tidyverse)

get_ticker_list <- function(piyasa = c("Ana Pazar", "Yıldız Pazar")){
  df <- read_xlsx("data/tickerlistall.xlsx")
  return(df %>% filter(Piyasa %in% piyasa))
}

tlist <- get_ticker_list()$Symbols
get_data <- function(tickerlist, startdate = '2016-01-01', interval = '1d'){
  dat <- data.frame()
  
  for(tick in tickerlist){
    symb <- paste0(tick, ".IS")
    print(symb)
    binder <- Ticker$new(symb)
    try(
    dat <- rbind(dat, binder$get_history(start = startdate, interval = interval) %>% 
                   mutate(symbol = tick))
    )
  }
  dat$date <- strftime(dat$date, format="%Y-%m-%d")
  return(dat)
}

write_csv(get_data(tlist), file = "data/historicaldataAll.csv")
