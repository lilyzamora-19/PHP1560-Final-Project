#' Estimating Red Cards for Players on Skin Tones
#' 
#' @description This function estimates the amount of different cards given to 
#' players given their skin color.
#' @param soccer_data A data frame with our cleaned data.
#' @return a data frame with an rate of red and yellow cards with position
#' and skin color average rating.

estimation_players <- function(soccer_data) {
  players <- soccer_data %>%
    group_by(player, position, skin_tone) %>%
    summarize(
      games = n(),
      red_cards = sum(redCards, na.rm = TRUE),
      yellow_cards = sum(yellowCards, na.rm = TRUE),
      yellow_red_cards = sum(yellowReds, na.rm = TRUE),
      red_rate = red_cards / games,
      yellow_rate = yellow_cards / games,
      yellow_red_rate = yellow_red_cards / games,
      .groups = "drop")
  
  return(players)
}

player_estimate <- estimation_players(soccer_data)
player_estimate <- na.omit(player_estimate)
