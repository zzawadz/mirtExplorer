plot_item <- function(fit, scores, i) {
  value <- data[,i]

  breaks <- seq(-6,6, length.out = 61)
  intervals <- cut(scores[,1], breaks = breaks)

  mid.point <- (breaks[-1] + head(breaks, -1)) / 2
  levels(intervals) <- round(mid.point, 2)

  result <- data_frame(value = value[[1]], intervals)
  result <- result %>% group_by(intervals) %>% summarise(Mean = mean(value))


  par(mar = c(2,2,2,2))
  layout(c(1,2) %>% t %>% t, heights = c(2,1))

  mids <- result$intervals %>% as.character() %>% as.numeric()
  plot(mids, result$Mean, xlim = c(-6,6), ylim = c(0,1))

  a <- itemplot(fit, item = i, CE = TRUE, CEdraws = 1000)
  bounds <- a$panel.args.common
  xx <- a$panel.args[[1]]$x
  lines(xx, bounds$upper, lty = 2, lwd = 2)
  lines(xx, bounds$lower, lty = 2, lwd = 2)

  probs <- probtrace(extract.item(fit, i), mids)[,2]
  lines(mids, probs)
  hist(scores, xlim = c(-6,6), breaks = mids, main = "")
  curve(dnorm, add = TRUE, lwd = 2, col = "red")
}
