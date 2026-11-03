library(dplyr)
library(plotly)
source("scripts/00_theme.R")

returns <- read.csv("data/sp500_returns.csv")
returns$year <- substr(returns$date, 1, 4)
returns <- filter(returns, year >= "2017", year <= "2025")

yearly <- returns |>
  group_by(year) |>
  summarise(mean = mean(ret), sd = sd(ret), .groups = "drop")
write.csv(yearly, "output/yearly_summary.csv", row.names = FALSE)
yearly

fig0 <- plot_ly() |>
  add_boxplot(data = returns, x = ~year, y = ~ret, name = "Daily returns",
              marker = list(color = blue, size = 3, opacity = 0.5),
              line = list(color = navy), fillcolor = "rgba(52,152,219,0.25)",
              boxpoints = "outliers", hoverinfo = "y") |>
  add_markers(data = yearly, x = ~year, y = ~mean, name = "Average daily return",
              marker = list(color = red, size = 11, symbol = "diamond"),
              hovertemplate = "%{x}<br>Average: %{y:.3f}%<extra></extra>") |>
  style_fig("S&P 500 daily returns by year",
            "", "Daily return (%)",
            yaxis = list(title = "Daily return (%)", gridcolor = light, zeroline = TRUE,
                         zerolinecolor = navy, range = c(-6, 6)))

htmlwidgets::saveWidget(fig0, "figures/fig0_boxplot.html")
