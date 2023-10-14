

# Working weekly filters 

#### Filter 1 ####
dff <- df %>% group_by(symbol) %>% 
  mutate(momentum10 = adj_close > lag(adj_close,10)*1.1) %>% # Momentum(10) > 1
  filter(! symbol %in% c("XU100", "USDTRY")) %>%
  filter(weekclose == 1) %>% # Take only weekly closes
  mutate(nextweekPerformance = lead(inweekperformance)) %>% # Add next weeks performances to the dataframe to calculate returns later
  filter(relativeVolume > 1.5,
         rsi > 60,
         adj_close > sma200*1.13,
         adj_close > sma5,
         adj_close > sma10,
         #w_rsi5 > 30,
         #m_rsi5 > 30,
         weeklyperformance < 0.20,
         monthlyperformance < 3.43,
         momentum10
  ) %>% arrange(weeks)

#### Filter 2 ####


#### Filter 3 ####


#### Filter 4 ####



#### Filter 5 ####