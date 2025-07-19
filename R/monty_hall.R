#' Simulate the Monty Hall Problem
#'
#' @param n Number of simulations
#' @param switch Logical, whether the player switches doors
#'
#' @return A tibble with win/loss counts and win rate
#' @export
#'
#' @examples
#' simulate_monty_hall(1000, switch = TRUE)

simulate_monty_hall <- function(n = 1000, switch = TRUE) {
  wins <- 0
  for (i in 1:n) {
    prize <- sample(1:3, 1)
    choice <- sample(1:3, 1)
    shown <- setdiff(1:3, c(choice, prize))
    shown <- sample(shown, 1)
    if (switch) {
      choice <- setdiff(1:3, c(choice, shown))
    }
    if (choice == prize) {
      wins <- wins + 1
    }
  }
  tibble::tibble(
    simulations = n,
    switch = switch,
    wins = wins,
    losses = n - wins,
    win_rate = wins / n
  )
}

