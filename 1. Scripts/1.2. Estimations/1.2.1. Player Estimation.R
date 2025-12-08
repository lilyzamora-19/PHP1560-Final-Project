#' Estimating Red Cards for Players on Skin Tones
#' 
#' @description This function estimates the amount of red cards given to 
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
      red_rate = redCards / games,
      yellow_rate = yellowCards / games,
      yellow_red_rate = yellow_red_cards / games)
  
  return(players)
}

player_estimate <- estimation_players(soccer_data)

# 
estimation_players <- function(soccer_data) {
  players <- soccer_data %>%
    group_by(player, position) %>%
    summarize(avg_rating = mean(avg_rating),
              .groups = "drop") %>%
    mutate(skin_tone = case_when(avg_rating >= 0 & avg_rating < 0.25 ~ "very_light",
                                 avg_rating >= 0.25 & avg_rating < 0.50 ~ "light",
                                 avg_rating >= 0.50 & avg_rating < 0.75 ~ "dark",
                                 avg_rating >= 0.75 & avg_rating < 1 ~ "very_dark"))
  return(players)
}
player_estimate <- estimation_players(soccer_data)
