#' Simulate Dice Rolls
#'
#' @param n Number of rolls
#' @param sides Number of sides on the dice (default is 6)
#'
#' @return A tibble with frequency counts of outcomes
#' @export
#'
#' @examples
#' simulate_dice(1000)

simulate_dice <- function(n = 1000, sides = 6) {
  rolls <- sample(1:sides, n, replace = TRUE)
  outcome <- as.data.frame(table(rolls))
  colnames(outcome) <- c("face", "count")
  tibble::as_tibble(outcome)
}
