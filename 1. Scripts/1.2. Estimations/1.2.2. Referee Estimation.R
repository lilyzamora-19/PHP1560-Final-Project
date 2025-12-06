#'Estimating Red Card Probability of Referees
#'
#' @description estimates the probability that the referee gives a red card 
#' given the skin color of the players.
#' @param soccer_data A data frame with our cleaned data.
#' @return a data frame that will return referee number, skin tone of player,
#' the amount of cards given by a referee and the rate of different cards given.

estimation_referee <- function(soccer_data){
  referee <- soccer_data %>%
    group_by(refNum, skin_tone) %>%
    summarize(
      cards = n(),
      red_cards = sum(redCards, na.rm = TRUE),
      yellow_cards = sum(yellowCards, na.rm = TRUE),
      yellow_red_cards = sum(yellowReds, na.rm = TRUE),
      red_rate = red_cards / cards,
      yellow_rate = yellow_cards / cards,
      yellow_red_rate = yellow_red_cards / cards)

  return(referee)
}

referee_estimate <- estimation_referee(soccer_data)
