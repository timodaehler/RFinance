# install.packages("tidyquant")
# install.packages("fredr")

library(tidyquant)
library(tidyverse)
library(ggplot2)
library(fredr)
# 
# options("getSymbols.warning4.0"=FALSE)
# options("getSymbols.yahoo.warning"=FALSE)

today = Sys.Date()

getSymbols("^W5000", from = '1971-01-01',
           to = today,warnings = FALSE,
           auto.assign = TRUE)


W5000_df <- as.data.frame(W5000)

W5000_df <- cbind(date = rownames(W5000_df), W5000_df)

# rownames(W5000_df) <- 1:nrow(W5000_df)

as.tibble(W5000_df)

W5000_df$date <- parse_date(W5000_df$date)

W5000_df <- select(W5000_df, date, W5000.Close)
rownames(W5000_df) = 1:nrow(W5000_df)

W5000_df <- arrange(W5000_df, desc(date))

ggplot(data = W5000_df) + geom_line(mapping = aes( x = date, y = W5000.Close ) )

GDP <- fredr(
  series_id = "GDP",
  observation_start = as.Date("1971-01-01"),
  observation_end = today ) 


merged <- left_join(x = GDP, y = W5000_df, by=c("date" = "date"))

merged <- select(merged, date, value, W5000.Close)
merged <- fill(merged, W5000.Close, .direction = "down")
merged <- merged %>% mutate(ratio = W5000.Close/value)

ggplot(data = merged, mapping = aes(x=date, y = ratio)) + geom_line() + geom_smooth(method = "gam", se = FALSE) + ylim(0, 2)
ggplot(data = merged) + geom_line(mapping = aes(x=date, y = c(ratio))) +  geom_line(mapping = aes(x=date, y = c(value))) + geom_line(aes(x = date, y = W5000.Close), color = "blue") 




