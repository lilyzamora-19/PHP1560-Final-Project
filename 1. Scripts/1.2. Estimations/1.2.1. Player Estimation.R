#' Estimating Red Cards for Players on Skin Tones
#' 
#' @description This function estimates the amount of red cards given to 
#' players given their skin color.
#' @param soccer_data A data frame with our cleaned data.
#' @return a data frame with an rate of red and yellow cards with position
#' and skin color average rating.

estimation_players <- function(soccer_data) {
  players <- soccer_data %>%
    group_by(player, position, avg_rating) %>%
    summarize(
      games = n(),
      red_cards = sum(redCards, na.rm = TRUE),
      yellow_cards = sum(yellowCards, na.rm = TRUE),
      red_rate = redCards / games,
      yellow_rate = yellowCards / games)
  
  return(players)
}

player_estimate <- estimation_players(soccer_data)

