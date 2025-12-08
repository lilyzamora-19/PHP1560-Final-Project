#' Simulation Tests

#Run with different amount of games to simulate since there it is more
#likely for a team skin tone to be light or very light in comparison to
#dark or very dark. The more samples we have, the more we can observe in our
#results

test_10 <- sim_game(player_estimate, referee_estimate, games = 10)

test_50 <- sim_game(player_estimate, referee_estimate, games = 50)

test_100 <- sim_game(player_estimate, referee_estimate, games = 100)

test_500 <- sim_game(player_estimate, referee_estimate, games = 500)

test_1000 <- sim_game(player_estimate, referee_estimate, games = 1000)

test_5000 <- sim_game(player_estimate, referee_estimate, games = 5000)