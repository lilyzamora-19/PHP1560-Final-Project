#' Results
#' 
#' @description
#' @param
#' @return

#Utilizing a simulation of 5000 games
simresults_5000 <- sim_game(player_estimate, referee_estimate, games = 5000, seed = 123)

#Taking the data frame from the results, let's look deeper into the results.

table_gt <- tbl_summary(simresults_5000[,3:4], 
            by = team_skin_tone,
            label = list(total_cards ~ "Total Cards"),
            statistic = list(
              all_continuous() ~ "{median} ({mean}) {min} ({max})"))

kable_table <- as_kable(table_gt)
