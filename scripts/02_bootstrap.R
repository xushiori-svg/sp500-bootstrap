library(dplyr)
library(plotly)

dir.create("figures", showWarnings = FALSE)
dir.create("output", showWarnings = FALSE)

returns <- read.csv("data/sp500_returns.csv")
pop <- returns$ret
mu <- mean(pop)

set.seed(6400)

s <- sample(pop, 250, replace = TRUE)
boot <- replicate(5000, mean(sample(s, replace = TRUE)))
ci <- quantile(boot, c(0.025, 0.975))

fig1 <- plot_ly(x = boot, type = "histogram", nbinsx = 60,
                marker = list(color = "#9ecae1"), name = "Bootstrap means") |>
  layout(title = "Bootstrap distribution of the average daily return, one sample of 250 days",
         xaxis = list(title = "Average daily return (%)"),
         yaxis = list(title = "Count"),
         shapes = list(
           list(type = "line", x0 = ci[1], x1 = ci[1], y0 = 0, y1 = 1, yref = "paper",
                line = list(color = "#d95f02", dash = "dash")),
           list(type = "line", x0 = ci[2], x1 = ci[2], y0 = 0, y1 = 1, yref = "paper",
                line = list(color = "#d95f02", dash = "dash")),
           list(type = "line", x0 = mu, x1 = mu, y0 = 0, y1 = 1, yref = "paper",
                line = list(color = "black"))),
         showlegend = FALSE)

htmlwidgets::saveWidget(fig1, "figures/fig1_bootstrap.html")

ns <- c(60, 250, 1000)
levels <- c(0.80, 0.90, 0.95, 0.99)

cis <- list()
for (n in ns) {
  for (i in 1:100) {
    s <- sample(pop, n, replace = TRUE)
    b <- replicate(1000, mean(sample(s, replace = TRUE)))
    for (lv in levels) {
      q <- quantile(b, c((1 - lv) / 2, 1 - (1 - lv) / 2))
      cis[[length(cis) + 1]] <- data.frame(n = n, level = lv, i = i, lo = q[1], hi = q[2])
    }
  }
}

cis <- bind_rows(cis) |>
  mutate(cover = lo <= mu & hi >= mu,
         setting = paste0(n, " days, ", level * 100, "%"))

coverage <- cis |>
  group_by(n, level, setting) |>
  summarise(covered = sum(cover), .groups = "drop")

write.csv(cis, "output/ci_simulation.csv", row.names = FALSE)
write.csv(coverage, "output/ci_coverage.csv", row.names = FALSE)
coverage

settings <- coverage$setting
fig2 <- plot_ly()
for (k in settings) {
  d <- filter(cis, setting == k)
  yes <- filter(d, cover)
  no <- filter(d, !cover)
  if (nrow(no) == 0) no <- data.frame(lo = NA, hi = NA, i = NA)
  fig2 <- fig2 |>
    add_segments(data = yes, x = ~lo, xend = ~hi, y = ~i, yend = ~i,
                 line = list(color = "gray"), name = "Contains true mean",
                 visible = k == "250 days, 95%", hoverinfo = "none") |>
    add_segments(data = no, x = ~lo, xend = ~hi, y = ~i, yend = ~i,
                 line = list(color = "#d7301f", width = 2), name = "Misses true mean",
                 visible = k == "250 days, 95%", hoverinfo = "none")
}

steps <- lapply(seq_along(settings), function(j) {
  list(method = "restyle",
       args = list("visible", as.list(rep(seq_along(settings) == j, each = 2))),
       label = paste0(settings[j], ": ", coverage$covered[j], " of 100"))
})

fig2 <- fig2 |>
  layout(title = "100 confidence intervals for the average daily return",
         xaxis = list(title = "Average daily return (%)"),
         yaxis = list(title = "Sample", showticklabels = FALSE),
         shapes = list(list(type = "line", x0 = mu, x1 = mu, y0 = 0, y1 = 1, yref = "paper",
                            line = list(color = "black"))),
         sliders = list(list(active = which(settings == "250 days, 95%") - 1,
                             currentvalue = list(prefix = "Sample size and level: "),
                             steps = steps)))

htmlwidgets::saveWidget(fig2, "figures/fig2_ci_coverage.html")
