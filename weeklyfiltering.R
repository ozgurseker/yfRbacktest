


# Get df data.frame from the file technicalanalysis.R

# Filter after weekly close

dff <- df %>% group_by(symbol) %>% 
  filter(adj_close > lag(adj_close,10)) %>% # Momentum(10) > 1
  filter(! symbol %in% c("XU100", "USDTRY")) %>%
  filter(weekclose == 1) %>% # Take only weekly closes
  mutate(nextweekPerformance = lead(inweekperformance)) %>% # Add next weeks performances to the dataframe to calculate returns later
  filter(relativeVolume > 1.5,
          rsi > 30,
          adj_close > sma200,
          adj_close > sma5,
          adj_close > sma10,
          w_rsi5 > 30,
          m_rsi5 > 30,
          weeklyperformance < 0.10,
          monthlyperformance < 3.43
        ) %>% arrange(weeks)

portfolioreturns <- dff %>% ungroup() %>% group_by(weeks) %>% arrange(weeks) %>% 
  summarise(filterWeeklyPerformance = mean(nextweekPerformance, na.rm = TRUE))

xu100returns <- df %>% filter(symbol == "XU100") %>% filter(weekclose == 1) %>% mutate(
  adj_close = ifelse(date < "2020-07-29", adj_close/100, adj_close),
  weeklyperformance = lead((adj_close - lag(adj_close) )/lag(adj_close))
)

weeks <- min(dff$weeks):max(dff$weeks)
filter <- 10000
xu100 <- 10000

xu100money <- c(xu100)
filtermoney <- c(filter)
for(week in weeks){
  if(week %in% portfolioreturns$weeks){
    filter <- filter*(1+portfolioreturns$filterWeeklyPerformance[portfolioreturns$weeks == week])
  }
  
  filtermoney <- c(filtermoney, filter)
  if(week %in% xu100returns$weeks){
    xu100 <- xu100*(1+xu100returns$weeklyperformance[xu100returns$weeks == week])
  }
  xu100money <- c(xu100money, xu100)
  
}


weeks <- weeks - min(weeks)
dfreturns <- data.frame(weeks = weeks, filter = filtermoney[!is.na(filtermoney)], 
                        xu100 = xu100money[!is.na(xu100money)])
manual_color_scale <- c("red", "blue")
names(manual_color_scale) <- c("filter", "xu100")

ggplot(dfreturns) + geom_line(aes(x = weeks, y = filter, color = "filter")) +
  geom_line(aes(x = weeks, y = xu100, color="xu100")) + theme_classic() + 
  theme(legend.position=c(0.2,0.8)) +
  labs(x = "Weeks",
      y = "Total Money") + 
  scale_y_continuous(breaks = scales::pretty_breaks()) + 
  scale_color_manual(values = manual_color_scale)

ggplot(dfreturns) + geom_line(aes(x = weeks, y = log(filter),color = "filter")) +
  geom_line(aes(x = weeks, y = log(xu100), color = "xu100")) + theme_classic() + 
  theme(legend.position=c(0.2,0.8)) +
  labs(x = "Weeks",
       y = "Total Money Log") + 
  scale_y_continuous(breaks = scales::pretty_breaks()) + 
  scale_color_manual(values = manual_color_scale)

ggplot(dfreturns %>% filter(weeks < 300)) + geom_line(aes(x = weeks, y = filter, color = "filter")) +
  geom_line(aes(x = weeks, y = xu100, color="xu100")) + theme_classic() + 
  theme(legend.position=c(0.2,0.8)) +
  labs(x = "Weeks",
       y = "Total Money") + 
  scale_y_continuous(breaks = scales::pretty_breaks()) + 
  scale_color_manual(values = manual_color_scale)

plot(density(portfolioreturns$filterWeeklyPerformance, na.rm = TRUE), 
     main = "Return Distribution", xlab = "Weekly Return")
abline(v= 0, col = "red")

plot(hist(portfolioreturns$filterWeeklyPerformance), 
     main = "Return Distribution", xlab = "Weekly Return")

