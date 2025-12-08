#' Simulation
#' 
#' @description
#' @param1
#' @return

sim_game <- function(player_estimate, referee_estimate, games, seed = NULL) {
  
  if (!is.null(seed)) set.seed(seed)
  
  # creating empty data frame to store game results
  results <- data.frame(game_id = integer(games),
                        refNum = integer(games),
                        total_cards = integer(games))
  
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
    
    for (k in 1:nrow(team)) {
      team_skin_tone <- team$skin_tone[k]   # find skin tone for every player in team
      
      col_name <- paste0("any_rate_", team_skin_tone) #take rate for getting a card
      
      prob <- ref_row[[col_name]]
      
      cards[k] <- rbinom(1, size = 1, prob = prob)
    }

    results$game_id[i] <- i
    results$refNum[i] <- ref_id
    results$total_cards[i] <- sum(cards)
    results$team_skin_tone[i] <- team_skin_tone
    
  }
  return(results)
}

simulation_results <- sim_game(player_estimate, referee_estimate, games = 10, seed = 1)