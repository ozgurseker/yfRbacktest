library(tidyverse)
library(readxl)
library(writexl)

df <- read_xlsx("data/rawsheet.xlsx") %>% filter(!str_detect(`Pay Senedi / Senet Grubu(*)`, "Pay Sened"))
df$Symbols <- str_split(df$`Pay Senedi / Senet Grubu(*)`, "-", simplify = T)[,1]
df$Groups <- str_split(df$`Pay Senedi / Senet Grubu(*)`, "-", simplify = T)[,2]
df <- df %>% select(Symbols, Piyasa, Sektör, Groups)

write_xlsx(df, "data/tickerlistall.xlsx")
