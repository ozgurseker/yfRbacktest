
# Adjustments
penalize <- FALSE
penalize_rate <- 0.005

trimming <- FALSE
trimrate <- 0.10

riskfreerate <- 0.30
# Add buy only low < prev close daily
# Add buy only if low < prev close weekly
# Add buy only if low < open
# Buy only n_firms is less than 10
# Buy in the mid of low - high in monday
# Erase best and worst %10 to see new performance
# 

# Get df data.frame from the file technicalanalysis.R

# Filter after weekly close

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
          w_rsi5 > 30,
          m_rsi5 > 50,
          weeklyperformance < 0.20,
          monthlyperformance < 3.43,
          momentum10
        ) %>% arrange(weeks)

# See last selections
#View(dff %>% ungroup() %>% filter(weeks > max(weeks)-2) %>% 
#       arrange(weeks) %>% select(date, symbol, nextweekPerformance) )

portfolioreturns <- dff %>% ungroup() %>% group_by(weeks) %>% arrange(weeks) %>% 
  summarise(filterWeeklyPerformance = mean(nextweekPerformance, na.rm = TRUE),
            numberfirms = n())

if(penalize){
  portfolioreturns <- portfolioreturns %>% 
    mutate(filterWeeklyPerformance = filterWeeklyPerformance - penalize_rate)
}

if(trimming){
  portfolioreturns <- portfolioreturns %>% 
    filter(between(filterWeeklyPerformance, quantile(filterWeeklyPerformance, trimrate, na.rm = TRUE), 
                                     quantile(filterWeeklyPerformance, 1-trimrate, na.rm = TRUE)))
}

xu100returns <- df %>% filter(symbol == "XU100") %>% filter(weekclose == 1) %>% mutate(
  adj_close = ifelse(date < "2020-07-29", adj_close/100, adj_close),
  weeklyperformance = lead((adj_close - lag(adj_close) )/lag(adj_close))
)

usdreturns <- df %>% filter(symbol == "USDTRY") %>% filter(weekclose == 1) %>% mutate(
  weeklyperformance = lead((adj_close - lag(adj_close) )/lag(adj_close))
)

weeks <- min(dff$weeks):max(dff$weeks)
filter <- 10000
xu100 <- 10000
usdm <- 10000

usdmoney <- c(usdm)
xu100money <- c(xu100)
filtermoney <- c(filter)
for(week in weeks){
  if(week %in% portfolioreturns$weeks){
    r <- portfolioreturns$filterWeeklyPerformance[portfolioreturns$weeks == week]
    filter <- filter*(1+ifelse(is.na(r),0,r))
  }
  
  filtermoney <- c(filtermoney, filter)
  if(week %in% xu100returns$weeks){
    r <- xu100returns$weeklyperformance[xu100returns$weeks == week]
    xu100 <- xu100*(1+ifelse(is.na(r),0,r))
  }
  xu100money <- c(xu100money, xu100)
  
  if(week %in% usdreturns$weeks){
    r <- usdreturns$weeklyperformance[usdreturns$weeks == week]
    usdm <- usdm*(1+ifelse(is.na(r),0,r))
  }
  usdmoney <- c(usdmoney, usdm)
  
}


weeks <- 1:length(filtermoney[!is.na(filtermoney)])
dfreturns <- data.frame(weeks = weeks, filter = filtermoney[!is.na(filtermoney)], 
                        xu100 = xu100money[!is.na(xu100money)],
                        usd = usdmoney[!is.na(usdmoney)])
manual_color_scale <- c("red", "blue")
names(manual_color_scale) <- c("filter", "xu100")


plot(density(portfolioreturns$filterWeeklyPerformance, na.rm = TRUE), 
     main = "Return Distribution", xlab = "Weekly Return")
abline(v= 0, col = "red")

plot(hist(portfolioreturns$filterWeeklyPerformance), 
     main = "Return Distribution", xlab = "Weekly Return")



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


dfreturns %>% filter(weeks == max(weeks))

# Find Sharpe Ratios 
if(TRUE){
  
  annualizeweekly <- function(rs){ (1+rs)^52 - 1 }
  x <- xu100returns %>% ungroup() %>% select(weeks, weeklyperformance)
  u <- usdreturns %>% ungroup() %>% select(weeks, weeklyperformance)
  colnames(x)[2] <- "xu100perf"
  colnames(u)[2] <- "usdperf"
  pr <- portfolioreturns
  for(wk in min(portfolioreturns$weeks):max(min(portfolioreturns$weeks))){
    pr <- pr %>% add_row(weeks = wk,
                         filterWeeklyPerformance = 0,
                         numberfirms = 0)
  }
  alldf <- left_join(left_join(pr, x),u)
  excessxu100 <- alldf$filterWeeklyPerformance - alldf$xu100perf
  sharpe_xu100 <- 52^(1/2)*mean(excessxu100, na.rm = TRUE)/sd(excessxu100, na.rm = TRUE)
  excessUSD <- alldf$filterWeeklyPerformance - alldf$usdperf
  sharpe_usd <- 52^(1/2)*mean(excessUSD, na.rm = TRUE)/sd(excessUSD, na.rm = TRUE)
  sharpe_free <- 52^(1/2)*mean(alldf$filterWeeklyPerformance - riskfreerate/50, na.rm = TRUE)/sd(alldf$filterWeeklyPerformance- riskfreerate/50, na.rm = TRUE)
  dfsharpe <- data_frame(sharpe_xu100 = sharpe_xu100, sharpe_usd = sharpe_usd, sharpe_free = sharpe_free)
  dfsharpe2 <- alldf %>% ungroup() %>% group_by(numberfirms) %>% 
    filter(!is.na(filterWeeklyPerformance)) %>% 
    summarise(sharpe_bist100 = mean(filterWeeklyPerformance-xu100perf) / sd(filterWeeklyPerformance-xu100perf),
              sharpe_usdtry = mean(filterWeeklyPerformance-usdperf) / sd(filterWeeklyPerformance-usdperf),
              sharpe_0 = mean(filterWeeklyPerformance) / sd(filterWeeklyPerformance))
  
  #annualized
  excessxu100 <- annualizeweekly(alldf$filterWeeklyPerformance) - annualizeweekly(alldf$xu100perf)
  sharpe_xu100 <- mean(excessxu100, na.rm = TRUE)/sd(excessxu100, na.rm = TRUE)
  excessUSD <- annualizeweekly(alldf$filterWeeklyPerformance) - annualizeweekly(alldf$usdperf)
  sharpe_usd <- mean(excessUSD, na.rm = TRUE)/sd(excessUSD, na.rm = TRUE)
  sharpe_free <- mean(annualizeweekly(alldf$filterWeeklyPerformance), na.rm = TRUE)/sd(annualizeweekly(alldf$filterWeeklyPerformance), na.rm = TRUE)
  dfsharpe3 <- data_frame(sharpe_xu100 = sharpe_xu100, sharpe_usd = sharpe_usd, sharpe_free = sharpe_free)
  
}

dfreturns %>% filter(weeks == max(weeks))
dfsharpe
dfsharpe3




