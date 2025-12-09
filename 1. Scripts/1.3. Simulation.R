#' Simulating Soccer Games
#' 
#' @description This function simulates soccer games with randomly selected players (following the 4-3-2-1 soccer formation) and referee
#' to examine whether referees disproportionately give out cards to players of different skin tones
#'
#' @param1 player_estimate A data frame containing the columns: player, position, and skin_tone.
#' @param2 referee_estimate A data frame containing the columns: refNum, any_rate_very_light, any_rate_light, any_rate_dark, any_rate_very_dark
#' @param3 games An integer that represents the number of games to simulate
#' @param4 seed A seed to reproduce simulation results
#' @return A data frame with the columns: game_id, refNum, any_cards_verylight, any_cards_light, any_cards_dark, any_cards_verydark, total_cards

sim_game <- function(player_estimate, referee_estimate, games, seed = NULL) {
  
  if (!is.null(seed)) set.seed(seed)
  
  # creating empty data frame to store game results
  results <- data.frame(game_id = integer(games),
                        refNum = integer(games),
                        total_cards = integer(games),
                        any_cards_light = integer(games),
                        any_cards_verylight = integer(games),
                        any_cards_dark = integer(games),
                        any_cards_verydark = integer(games))
  
  for (i in 1:games) {
    
    # pick a random referee
    ref_index <- sample(1:nrow(referee_estimate), 1)
    ref_row <- referee_estimate[ref_index, ]
    ref_id <- ref_row$refNum
    
    # picking random players with 4-3-2-1 formation
    def_index <- sample(which(player_estimate$position == "Defender"), 4)
    att_mid_index <- sample(which(player_estimate$position == "Attacking Midfielder"), 3)
    def_mid_index <- sample(which(player_estimate$position == "Defensive Midfielder"), 2)
    strike_index <- sample(which(player_estimate$position == "Striker"), 1)
    gk_index <- sample(which(player_estimate$position == "Goalie"), 1)
    
    player_index <- c(def_index, att_mid_index, def_mid_index, strike_index, gk_index)
    team <- player_estimate[player_index,]
    
    # starting off every player with 0 cards
    cards <- rep(0, nrow(team))
    any_cards_light <- 0
    any_cards_verylight <- 0
    any_cards_dark <- 0
    any_cards_verydark <- 0
    
    for (k in 1:nrow(team)) {
      player_skin_tone <- team$skin_tone[k]   # find skin tone for every player in team
      
      col_name <- paste0("any_rate_", player_skin_tone) 
      
      prob <- ref_row[[col_name]]  # take rate for getting a card
      if (is.na(prob) || length(prob) == 0) prob <- 0  # setting prob to 0 if NA
      
      cards[k] <- rbinom(1, size = 1, prob = prob) # card or no card

      if (cards[k] == 1) {
        if (player_skin_tone == "very_light") {
          any_cards_verylight <- any_cards_verylight + 1
        } else if (player_skin_tone == "light") {
          any_cards_light <- any_cards_light + 1
        } else if (player_skin_tone == "dark") {
          any_cards_dark <- any_cards_dark + 1
        } else if (player_skin_tone == "very_dark") {
          any_cards_verydark <- any_cards_verydark + 1
        }
      }
    }

    results$game_id[i] <- i
    results$refNum[i] <- ref_id
    results$total_cards[i] <- sum(cards)
    results$any_cards_verylight[i] <- any_cards_verylight
    results$any_cards_light[i] <- any_cards_light
    results$any_cards_dark[i] <- any_cards_dark
    results$any_cards_verydark[i] <- any_cards_verydark
    
  }
  return(results)
}

simulation_results <- sim_game(player_estimate, referee_estimate, games = 10, seed = 10)
