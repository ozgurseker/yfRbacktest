library(pander)
library(tidyverse)
library(quantmod)


## Change the file location below
df <- read_csv("data/historicaldataAll.csv") %>% group_by(symbol) %>% 
  filter(! (duplicated(date))) %>% ungroup()

## Dont touch this part
add_month_indicator <- function(df){
  df$month <- month(df$date)
  df <- df %>% group_by(symbol) %>%
    mutate(monthclose = ifelse(month != lead(month), 1, 0)) %>% 
    mutate(months = ifelse(month != lag(month), 1, 0)) %>% 
    mutate(months = ifelse(is.na(months), 0, months)) %>%
    mutate(months = cumsum(months)) %>% 
    select(-month)
  longestdatesymbol <- df %>% group_by(symbol) %>% summarise(n = length(date))
  longestdatesymbol <- longestdatesymbol$symbol[which.max(longestdatesymbol$n)]
  dateweek <- df %>% ungroup() %>% filter(symbol == longestdatesymbol) %>% select(date, months)
  df <- df %>% select(-months) %>% left_join(dateweek)
  return(df)
}

add_week_indicator <- function(df){
  df$day <- weekdays(df$date, abbreviate = T)
  df$day[df$day == "Mon"] <- 1
  df$day[df$day == "Tue"] <- 2
  df$day[df$day == "Wed"] <- 3
  df$day[df$day == "Thu"] <- 4
  df$day[df$day == "Fri"] <- 5
  df$day[df$day == "Sat"] <- 6
  df$day[df$day == "Sun"] <- 7
  df$day <- as.numeric(df$day)
  df <- df %>% group_by(symbol) %>%
    mutate(weekclose = ifelse(day > lead(day), 1, 0)) %>%
    mutate(weeks = ifelse(day <= lag(day), 1, 0)) %>%
    mutate(weeks = ifelse(is.na(weeks), 0, weeks)) %>%
    mutate(weeks = cumsum(weeks)) %>%
    select(-day)
  longestdatesymbol <- df %>% group_by(symbol) %>% summarise(n = length(date))
  longestdatesymbol <- longestdatesymbol$symbol[which.max(longestdatesymbol$n)]
  dateweek <- df %>% ungroup() %>% filter(symbol == longestdatesymbol) %>% select(date, weeks)
  df <- df %>% select(-weeks) %>% left_join(dateweek)
  return(df)
}

weekly_sum <- function(df){
  return(
  df %>% group_by(symbol, weeks) %>% summarise(
    date = date[1],
    adj_open = open[1]*adj_close[1]/close[1],
    adj_high = max(high)*adj_close[which.max(high)]/close[which.max(high)],
    adj_low = min(low)*adj_close[which.min(low)]/close[which.min(low)],
    adj_close = adj_close[length(weeks)],
    volume = sum(volume),
    high = max(high),
    low = min(low),
    open = open[1],
    close = close[length(weeks)]
  ) 
  )
}

monthly_sum <- function(df){
  return(
    df %>% group_by(symbol, months) %>% summarise(
      date = date[1],
      adj_open = open[1]*adj_close[1]/close[1],
      adj_high = max(high)*adj_close[which.max(high)]/close[which.max(high)],
      adj_low = min(low)*adj_close[which.min(low)]/close[which.min(low)],
      adj_close = adj_close[length(months)],
      volume = sum(volume),
      high = max(high),
      low = min(low),
      open = open[1],
      close = close[length(months)]
    )
  )
}

df <- add_week_indicator(df)
df_weekly <- weekly_sum(df)

df <- add_month_indicator(df)
df_monthly <- monthly_sum(df)

## Add your daily indicators here
df <- df %>% group_by(symbol) %>% filter(length(adj_close) > 14) %>% 
  mutate(
    ema14 = EMA(adj_close, 14),
    rsi = RSI(adj_close)
  )

## Weekly indicators here 
## (filter if existing number of weeks greater than what I need in indicator)
df_weekly <- df_weekly %>% group_by(symbol) %>%
  filter(max(weeks) > 14) %>%
  mutate(
    w_sma14 = SMA(adj_close, 14),
    w_rsi5 = RSI(adj_close, 5)
  )

## Monthly indicators here
## (filter if existing number of months greater than what I need in indicator)
df_monthly <- df_monthly %>% group_by(symbol) %>%
  filter(max(months) > 8) %>%
  mutate(
    m_sma8 = SMA(adj_close, 8),
    m_rsi5 = RSI(adj_close, 5)
  )

remcolnames <- c("date", "adj_open", "adj_high", "adj_low",
                 "adj_close", "high", "close", "open", "low", "volume")


# Combines weekly indicators on daily data, no need to change
df <- left_join(df, df_weekly %>% select(-all_of(remcolnames)))

# Combine monthly indicators on daily data, no need to change
df <- left_join(df, df_monthly %>% select(-all_of(remcolnames)))

# Change save location
write_csv(df, "data/save.csv")


