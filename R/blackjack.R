#' Simulate Blackjack Games
#'
#' @param n_rounds Number of rounds to simulate
#'
#' @return A tibble summarizing win/loss/draw counts
#' @export
#'
#' @examples
#' simulate_blackjack(1000)

simulate_blackjack <- function(n_rounds = 1000) {
  deck <- rep(c(2:10, 10, 10, 10, 11), 4)
  draw_hand <- function() sample(deck, size = 2, replace = TRUE)
  play_hand <- function() {
    player <- draw_hand()
    while (sum(player) < 17) player <- c(player, sample(deck, 1))
    if (sum(player) > 21) return("loss")
    dealer <- draw_hand()
    while (sum(dealer) < 17) dealer <- c(dealer, sample(deck, 1))
    p <- sum(player)
    d <- sum(dealer)
    if (d > 21 || p > d) "win" else if (p == d) "draw" else "loss"
  }
  results <- replicate(n_rounds, play_hand())
  tibble::tibble(
    wins = sum(results == "win"),
    losses = sum(results == "loss"),
    draws = sum(results == "draw")
  )
}
