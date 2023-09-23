library(pander)
library(tidyverse)
library(quantmod)


## Change the file location below
df <- read_csv("data/historicaldataAll.csv")

## Dont touch this part
add_month_indicator <- function(df){
  df$month <- month(df$date)
  df <- df %>% group_by(symbol) %>%
    mutate(monthclose = ifelse(month != lead(month), 1, 0)) %>% 
    mutate(months = ifelse(month != lag(month), 1, 0)) %>% 
    mutate(months = ifelse(is.na(months), 0, months)) %>%
    mutate(months = cumsum(months)) %>% 
    select(-month)
  return(df)
}

add_week_indicator <- function(df){
  df$day <- weekdays(df$date, abbreviate = T)
  df$day[df$day == "Mon"] <- 1
  df$day[df$day == "Tue"] <- 2
  df$day[df$day == "Wed"] <- 3
  df$day[df$day == "Thu"] <- 4
  df$day[df$day == "Fri"] <- 5
  df$day <- as.numeric(df$day)
  df <- df %>% group_by(symbol) %>%
    mutate(weekclose = ifelse(day > lead(day), 1, 0)) %>%
    mutate(weeks = ifelse(day <= lag(day), 1, 0)) %>%
    mutate(weeks = ifelse(is.na(weeks), 0, weeks)) %>%
    mutate(weeks = cumsum(weeks)) %>%
    select(-day)
  return(df)
}

weekly_sum <- function(df){
  return(
  df %>% group_by(symbol, weeks) %>% summarise(
    date = date[1],
    volume = sum(volume),
    high = max(high),
    low = min(low),
    open = open[1],
    close = close[length(weeks)],
    adj_close = adj_close[length(weeks)]
  ) 
  )
}

monthly_sum <- function(df){
  return(
    df %>% group_by(symbol, months) %>% summarise(
      date = date[1],
      volume = sum(volume),
      high = max(high),
      low = min(low),
      open = open[1],
      close = close[length(months)],
      adj_close = adj_close[length(months)]
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

remcolnames <- c("date", "adj_close", "high", "close", "open", "low", "volume")


# Combines weekly indicators on daily data, no need to change
df <- left_join(df, df_weekly %>% select(-all_of(remcolnames)))

# Combine monthly indicators on daily data, no need to change
df <- left_join(df, df_monthly %>% select(-all_of(remcolnames)))

# Change save location
write_csv(df, "data/save.csv")


