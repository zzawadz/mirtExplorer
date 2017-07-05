#' Title
#'
#' @param data data used to fit mirt model.
#' @param fit fitted model.
#' @param target.score raw score used to subset data.
#'
#' @return vector.
#'
#' @examples
distant_idx <- function(data, fit, target.score) {

  final.score <- rowSums(data)
  irt.score   <- fscores(fit)[,1]

  idx <- final.score %in% target.score

  max.id <- which(max(irt.score[idx])[1] == irt.score & idx)
  min.id <- which(min(irt.score[idx])[1] == irt.score & idx)

  c(max.id = max.id, min.id = min.id)
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
compare_most_distant_students <- function(data, fit, target.score) {

  idx <- distant_idx(data, fit, target.score)
  compare_students(data = data, fit = fit,
                   first.id = idx[[1]], second.id = idx[[2]])
}

compare_students <- function(data, fit, first.id, second.id) {

  first.pattern  <- data[first.id, ]
  second.pattern <- data[second.id, ]

  different.scores <- which(first.pattern != second.pattern)

  item.values <- lapply(different.scores, function(i) {
    pl <- itemplot(fit, item = i)
    list(
      x = pl$panel.args[[1]]$x,
      y = pl$panel.args[[1]]$y
    )
  })

  plot(item.values[[1]], type = "n",
       xlab = "Ability", ylab = "Probability")

  first  <- first.pattern[different.scores]
  second <- second.pattern[different.scores]

  mapply(function(x, col) lines(x[[1]], x[[2]], col = ifelse(col == 1, "green", "red")), item.values, first)

  first.score  <- fscores(fit, response.pattern = first.pattern)
  second.score <- fscores(fit, response.pattern = second.pattern)

  abline(v = first.score[1,"F1"], col = "green")
  abline(v = second.score[1,"F1"], col = "red")
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
