library(dplyr)
library(plotly)

returns <- read.csv("data/sp500_returns.csv")
returns$date <- as.Date(returns$date)

last_year <- returns |> filter(date > max(date) - 365)
obs <- mean(last_year$ret)
shifted <- last_year$ret - obs

set.seed(6400)
null <- replicate(10000, mean(sample(shifted, replace = TRUE)))
p_value <- mean(abs(null) >= abs(obs))

results <- data.frame(days = nrow(last_year), observed_mean = obs, p_value = p_value)
write.csv(results, "output/test_last_year.csv", row.names = FALSE)
results

alphas <- c(0.01, 0.05, 0.10)
breaks <- seq(min(null), max(null), length.out = 61)
mids <- (head(breaks, -1) + tail(breaks, -1)) / 2
counts <- as.vector(table(cut(null, breaks, include.lowest = TRUE)))

fig3 <- plot_ly()
for (a in alphas) {
  cut_off <- quantile(abs(null), 1 - a)
  reject <- abs(mids) >= cut_off
  fig3 <- fig3 |>
    add_bars(x = mids[!reject], y = counts[!reject], marker = list(color = "#9ecae1"),
             name = "Do not reject", visible = a == 0.05) |>
    add_bars(x = mids[reject], y = counts[reject], marker = list(color = "#d7301f"),
             name = "Rejection region", visible = a == 0.05)
}

steps <- lapply(seq_along(alphas), function(j) {
  list(method = "restyle",
       args = list("visible", as.list(rep(seq_along(alphas) == j, each = 2))),
       label = paste0("alpha = ", alphas[j]))
})

fig3 <- fig3 |>
  layout(title = paste0("Average daily return if the true mean were zero, p-value = ",
                        round(p_value, 3)),
         xaxis = list(title = "Average daily return (%)"),
         yaxis = list(title = "Count"),
         bargap = 0,
         shapes = list(list(type = "line", x0 = obs, x1 = obs, y0 = 0, y1 = 1, yref = "paper",
                            line = list(color = "black", width = 2))),
         annotations = list(list(x = obs, y = 1, yref = "paper", text = "Last year's average",
                                 showarrow = FALSE, yanchor = "bottom")),
         sliders = list(list(active = 1, currentvalue = list(prefix = "Significance level: "),
                             steps = steps)))

htmlwidgets::saveWidget(fig3, "figures/fig3_null_test.html")
