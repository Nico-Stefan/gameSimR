#' Simulate Rock-Paper-Scissors
#'
#' @param n Number of games to simulate
#'
#' @return A tibble summarizing win/loss/draw counts for player 1
#' @export
#'
#' @examples
#' simulate_rps(1000)

simulate_rps <- function(n = 1000) {
  moves <- c("rock", "paper", "scissors")
  outcomes <- replicate(n, {
    p1 <- sample(moves, 1)
    p2 <- sample(moves, 1)
    if (p1 == p2) "draw"
    else if ((p1 == "rock" && p2 == "scissors") ||
             (p1 == "scissors" && p2 == "paper") ||
             (p1 == "paper" && p2 == "rock")) {
      "win"
    } else {
      "loss"
    }
  })
  tibble::tibble(
    wins = sum(outcomes == "win"),
    losses = sum(outcomes == "loss"),
    draws = sum(outcomes == "draw")
  )
}
