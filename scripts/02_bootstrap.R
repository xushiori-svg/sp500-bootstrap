library(dplyr)
library(plotly)
source("scripts/00_theme.R")

dir.create("figures", showWarnings = FALSE)
dir.create("output", showWarnings = FALSE)

returns <- read.csv("data/sp500_returns.csv")
pop <- returns$ret
mu <- mean(pop)

set.seed(6400)

s250 <- sample(pop, 250, replace = TRUE)
boot250 <- replicate(5000, mean(sample(s250, replace = TRUE)))

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

s60 <- sample(pop, 60, replace = TRUE)
boot60 <- replicate(5000, mean(sample(s60, replace = TRUE)))
s1000 <- sample(pop, 1000, replace = TRUE)
boot1000 <- replicate(5000, mean(sample(s1000, replace = TRUE)))

boots <- list("60 days" = list(s = s60, b = boot60),
              "250 days" = list(s = s250, b = boot250),
              "1000 days" = list(s = s1000, b = boot1000))

fig1_ci <- data.frame()
fig1 <- plot_ly()
for (k in names(boots)) {
  b <- boots[[k]]$b
  ci <- quantile(b, c(0.025, 0.975))
  fig1_ci <- rbind(fig1_ci, data.frame(sample = k, mean = mean(boots[[k]]$s), lo = ci[1], hi = ci[2]))
  inside <- b >= ci[1] & b <= ci[2]
  fig1 <- fig1 |>
    add_histogram(x = b[inside], xbins = list(size = 0.005), name = "Inside the 95% interval",
                  marker = list(color = teal, line = list(color = "white", width = 0.5)),
                  visible = k == "250 days",
                  hovertemplate = "Average return: %{x}<br>Count: %{y}<extra></extra>") |>
    add_histogram(x = b[!inside], xbins = list(size = 0.005), name = "Outside the interval",
                  marker = list(color = "#BDC3C7", line = list(color = "white", width = 0.5)),
                  visible = k == "250 days",
                  hovertemplate = "Average return: %{x}<br>Count: %{y}<extra></extra>")
}
write.csv(fig1_ci, "output/bootstrap_one_sample.csv", row.names = FALSE)

fig1 <- fig1 |>
  style_fig("Bootstrap distribution of the average daily return",
            "Average daily return (%)", "Number of resamples",
            barmode = "overlay", bargap = 0,
            xaxis = list(title = "Average daily return (%)", gridcolor = light,
                         zeroline = FALSE, range = c(-0.45, 0.55)),
            shapes = list(vline(mu), vline(0, color = red, dash = "dot")),
            annotations = list(
              list(x = mu, y = 1, yref = "paper", text = "True mean (10 years)",
                   showarrow = FALSE, xanchor = "left", yanchor = "bottom",
                   font = list(color = navy)),
              list(x = 0, y = 0.92, yref = "paper", text = "Zero", showarrow = FALSE,
                   xanchor = "right", font = list(color = red))),
            sliders = list(slider(names(boots), 2, 2, "Sample size: ")))

htmlwidgets::saveWidget(fig1, "figures/fig1_bootstrap.html")

to_lines <- function(d) {
  if (nrow(d) == 0) return(data.frame(x = NA, y = NA))
  data.frame(x = as.vector(rbind(d$lo, d$hi, NA)),
             y = as.vector(rbind(d$i, d$i, NA)))
}

settings <- coverage$setting
labels <- paste0(settings, " (", coverage$covered, "/100)")
start <- which(settings == "250 days, 95%")

fig2 <- plot_ly()
for (k in settings) {
  d <- filter(cis, setting == k)
  fig2 <- fig2 |>
    add_lines(data = to_lines(filter(d, cover)), x = ~x, y = ~y,
              line = list(color = teal, width = 2), name = "Contains the true mean",
              visible = k == settings[start], hoverinfo = "skip") |>
    add_lines(data = to_lines(filter(d, !cover)), x = ~x, y = ~y,
              line = list(color = red, width = 3), name = "Misses the true mean",
              visible = k == settings[start], hoverinfo = "skip")
}

fig2 <- fig2 |>
  style_fig("100 confidence intervals from 100 different samples",
            "Average daily return (%)", "",
            yaxis = list(title = "", showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE),
            shapes = list(vline(mu)),
            annotations = list(list(x = mu, y = 1, yref = "paper", text = "True mean",
                                    showarrow = FALSE, xanchor = "left", yanchor = "bottom")),
            sliders = list(slider(labels, 2, start, "Sample size and confidence level: ")))

htmlwidgets::saveWidget(fig2, "figures/fig2_ci_coverage.html")
