dummyPlot <- function(label = "Load data\nto display plot") {
  p <- ggplot(tibble(label = label,
                     x = 1,
                     y = 1),
              aes(x = x, y = y, label = label)) +
    geom_text(size = 5)

  p <- ggplotly(p)
  return(p)
}
