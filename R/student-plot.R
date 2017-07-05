#' Title
#'
#' @param data data used to fit mirt model.
#' @param fit fitted model.
#' @param target.score raw score used to subset data.
#'
#' @return a list containing answers pattern
#' for best and worst student in term of fitted ability for
#' given raw score.
#'
#' @examples
diff_patterns <- function(data, fit, target.score) {

  final.score <- rowSums(data)
  irt.score   <- fscores(fit)[,1]

  idx <- which(final.score %in% target.score)

  max.id <- which.max(irt.score[idx])[1]
  min.id <- which.min(irt.score[idx])[1]


  best.pattern  <- data[idx,][max.id,]
  worst.pattern <- data[idx,][min.id,]

  result <- list(
    best.pattern = best.pattern,
    worst.pattern = worst.pattern,
    different.scores = which(best.pattern != worst.pattern)
  )

  return(result)

}

#' Compare the answers for the most distant students in terms of ability.
#'
#' @param data data used to fit mirt model.
#' @param fit fitted model.
#' @param target.score raw score used to subset data.
#'
#' @return
#' @export
#'
#' @examples
#'
#' data <- key2binary(SAT12,
#'           key = c(1,4,5,2,3,1,2,1,3,1,2,4,2,
#'           1,5,3,4,4,1,4,3,3,4,1,3,5,1,3,1,5,4,5))
#' fit <- mirt(data, 1)
#' plot_diff_students(data, fit, 10:20)
#'
plot_diff_students <- function(data, fit, target.score) {

  result <- diff_patterns(data, fit, target.score)

  item.values <- lapply(result$different.scores, function(i) {
    pl <- itemplot(fit, item = i)
    list(
      x = pl$panel.args[[1]]$x,
      y = pl$panel.args[[1]]$y
    )
  })

  plot(item.values[[1]], type = "n",
       xlab = "Ability", ylab = "Probability")

  best  <- result$best.pattern[result$different.scores]
  worst <- result$worst.pattern[result$different.scores]

  mapply(function(x, col) lines(x[[1]], x[[2]], col = ifelse(col == 1, "green", "red")), item.values, best)

  best.score  <- fscores(fit, response.pattern = result$best.pattern)
  worst.score <- fscores(fit, response.pattern = result$worst.pattern)

  abline(v = best.score[1,"F1"], col = "green")
  abline(v = worst.score[1,"F1"], col = "red")
}

#' Create simple plot to show the correlation between raw score and IRT ability.
#'
#' @param data data used to fit IRT model.
#' @param fit fitted IRT model.
#'
#' @return this function does not return anything.
#' @export
#'
#' @examples
#'
#' data <- key2binary(SAT12,
#'           key = c(1,4,5,2,3,1,2,1,3,1,2,4,2,
#'           1,5,3,4,4,1,4,3,3,4,1,3,5,1,3,1,5,4,5))
#' fit <- mirt(data, 1)
#' plot_score_ability_corr(data, fit)
#'
plot_score_ability_corr <- function(data, fit) {

  final.score <- rowSums(data)
  irt.score   <- fscores(fit)[,1]

  cor <- 100 * cor(final.score, irt.score)

  plot(final.score, irt.score, xlab = "Raw score", ylab = "IRT score", main = sprintf("Correlation: %.2f%%", cor))

  abline(lm(irt.score ~ final.score), col = "red")
}
