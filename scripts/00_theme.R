navy <- "#2C3E50"
teal <- "#18BC9C"
red <- "#E74C3C"
blue <- "#3498DB"
light <- "#ECF0F1"

style_fig <- function(p, title, xlab, ylab, ...) {
  p |>
    layout(title = list(text = title, x = 0.02, font = list(size = 18, color = navy)),
           font = list(family = "Lato, Helvetica, Arial, sans-serif", color = navy, size = 13),
           xaxis = list(title = xlab, gridcolor = light, zeroline = FALSE),
           yaxis = list(title = ylab, gridcolor = light, zeroline = FALSE),
           plot_bgcolor = "white", paper_bgcolor = "white",
           hoverlabel = list(bgcolor = "white", bordercolor = navy,
                             font = list(family = "Lato, Helvetica, Arial", color = navy)),
           legend = list(orientation = "h", x = 0, y = 1.08),
           margin = list(t = 90, b = 60),
           ...) |>
    config(displayModeBar = FALSE)
}

vline <- function(x, color = navy, dash = "solid", width = 2) {
  list(type = "line", x0 = x, x1 = x, y0 = 0, y1 = 1, yref = "paper",
       line = list(color = color, dash = dash, width = width))
}

band <- function(x0, x1, color = teal) {
  list(type = "rect", x0 = x0, x1 = x1, y0 = 0, y1 = 1, yref = "paper",
       fillcolor = color, opacity = 0.12, line = list(width = 0), layer = "below")
}

slider <- function(labels, n_traces, active, prefix) {
  steps <- lapply(seq_along(labels), function(j) {
    list(method = "update",
         args = list(list(visible = as.list(rep(seq_along(labels) == j, each = n_traces)))),
         label = labels[j])
  })
  list(active = active - 1, steps = steps, pad = list(t = 40),
       currentvalue = list(prefix = prefix, font = list(color = navy, size = 14)),
       bgcolor = light, activebgcolor = teal, bordercolor = light,
       font = list(color = navy))
}
