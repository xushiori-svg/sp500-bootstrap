library(dplyr)
library(plotly)
source("scripts/00_theme.R")

returns <- read.csv("data/sp500_returns.csv")
returns$date <- as.Date(returns$date)

null_test <- function(x, reps = 10000) {
  obs <- mean(x)
  null <- replicate(reps, mean(sample(x - obs, replace = TRUE)))
  list(obs = obs, null = null, p = mean(abs(null) >= abs(obs)))
}

set.seed(6400)
years <- c(1, 2, 5, 10)
tests <- lapply(years, function(y) null_test(returns$ret[returns$date > max(returns$date) - 365 * y]))
names(tests) <- paste(years, ifelse(years == 1, "year", "years"))

results <- data.frame(window = names(tests),
                      days = sapply(years, function(y) sum(returns$date > max(returns$date) - 365 * y)),
                      observed_mean = sapply(tests, `[[`, "obs"),
                      p_value = sapply(tests, `[[`, "p"))
write.csv(results, "output/test_results.csv", row.names = FALSE)
results

hist_bars <- function(null, cut_off) {
  breaks <- seq(-max(abs(null)), max(abs(null)), length.out = 61)
  mids <- (head(breaks, -1) + tail(breaks, -1)) / 2
  counts <- as.vector(table(cut(null, breaks, include.lowest = TRUE)))
  data.frame(mid = mids, count = counts, reject = abs(mids) >= cut_off)
}

add_null <- function(p, h, visible) {
  p |>
    add_bars(data = filter(h, !reject), x = ~mid, y = ~count, name = "Do not reject",
             marker = list(color = blue), visible = visible,
             hovertemplate = "Average return: %{x:.3f}%<br>Count: %{y}<extra></extra>") |>
    add_bars(data = filter(h, reject), x = ~mid, y = ~count, name = "Rejection region",
             marker = list(color = red), visible = visible,
             hovertemplate = "Average return: %{x:.3f}%<br>Count: %{y}<extra></extra>")
}

t1 <- tests[["1 year"]]
alphas <- c(0.01, 0.05, 0.10)
fig3 <- plot_ly()
for (a in alphas) {
  fig3 <- add_null(fig3, hist_bars(t1$null, quantile(abs(t1$null), 1 - a)), a == 0.05)
}

fig3 <- fig3 |>
  style_fig(paste0("If the true mean were zero: last year's average and the rejection region (p = ",
                   round(t1$p, 2), ")"),
            "Average daily return (%)", "Number of simulations",
            bargap = 0.05,
            shapes = list(vline(t1$obs, width = 3)),
            annotations = list(list(x = t1$obs, y = 1, yref = "paper",
                                    text = paste0("Last year: ", round(t1$obs, 3), "%"),
                                    showarrow = FALSE, xanchor = "left", yanchor = "bottom")),
            sliders = list(slider(paste0("α = ", alphas), 2, 2, "Significance level: ")))

htmlwidgets::saveWidget(fig3, "figures/fig3_null_test.html")

fig4 <- plot_ly()
for (k in names(tests)) {
  t <- tests[[k]]
  fig4 <- add_null(fig4, hist_bars(t$null, quantile(abs(t$null), 0.95)), k == "1 year")
}

labels <- paste0(names(tests), " (p = ", formatC(results$p_value, format = "f", digits = 3), ")")
shapes <- lapply(tests, function(t) list(vline(t$obs, width = 3)))

steps <- lapply(seq_along(tests), function(j) {
  list(method = "update",
       args = list(list(visible = as.list(rep(seq_along(tests) == j, each = 2))),
                   list(shapes = shapes[[j]])),
       label = labels[j])
})
sl <- slider(labels, 2, 1, "Data used: ")
sl$steps <- steps

fig4 <- fig4 |>
  style_fig("More data makes the null distribution narrower (α = 0.05)",
            "Average daily return (%)", "Number of simulations",
            bargap = 0.05,
            xaxis = list(title = "Average daily return (%)", gridcolor = light, zeroline = FALSE,
                         range = c(-0.25, 0.25)),
            shapes = shapes[[1]],
            sliders = list(sl))

htmlwidgets::saveWidget(fig4, "figures/fig4_more_data.html")
