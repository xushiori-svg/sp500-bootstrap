library(dplyr)

dir.create("data", showWarnings = FALSE)

url <- "https://fred.stlouisfed.org/graph/fredgraph.csv?id=SP500"
raw <- read.csv(url, na.strings = c(".", ""))
write.csv(raw, "data/sp500_raw.csv", row.names = FALSE)

returns <- raw |>
  rename(date = 1, price = 2) |>
  mutate(date = as.Date(date)) |>
  filter(!is.na(price)) |>
  arrange(date) |>
  mutate(ret = (price / lag(price) - 1) * 100) |>
  filter(!is.na(ret))

write.csv(returns, "data/sp500_returns.csv", row.names = FALSE)

nrow(returns)
range(returns$date)
mean(returns$ret)
sd(returns$ret)
