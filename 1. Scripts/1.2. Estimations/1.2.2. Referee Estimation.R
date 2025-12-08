#'Estimating Red Card Probability of Referees
#'
#' @description estimates the probability that the referee gives a red card 
#' given the skin color of the players.
#' @param soccer_data A data frame with our cleaned data.
#' @return a data frame that will return referee number, skin tone of player,
#' the amount of cards given by a referee and the rate of different cards given.

estimation_referee <- function(soccer_data){

  soccer_data$any_card <- (soccer_data$yellowCards + soccer_data$yellowReds + soccer_data$redCards) > 0
  
  referee <- soccer_data %>%
    group_by(refNum, skin_tone) %>%
    summarize(
      cards = n(),
      red_cards = sum(redCards > 0, na.rm = TRUE),
      yellow_cards = sum(yellowCards > 0, na.rm = TRUE),
      yellow_red_cards = sum(yellowReds > 0, na.rm = TRUE),
      any_card = sum(any_card, na.rm = TRUE),
      any_rate = any_card / cards,
      red_rate = red_cards / cards,
      yellow_rate = yellow_cards / cards,
      yellow_red_rate = yellow_red_cards / cards)
      

  referee_wide <- referee %>%
    pivot_wider(id_cols = refNum,
                names_from = skin_tone,
                values_from = c(yellow_rate, yellow_red_rate, red_rate, any_rate),
                names_glue = "{.value}_{skin_tone}")

  return(referee_wide)
}

referee_estimate <- estimation_referee(soccer_data)
