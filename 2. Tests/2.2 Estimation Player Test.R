# estimation_player test
#
# creating a smaller mock soccer data set to test if estimation_player works.
# The code should run without error. 

# creating a test soccer data
player_test_data <- tibble(player = c("Bob", "Bob", "Bob", "Sam", "Sam"),
                           position = c("Defender", "Defender", "Defender", "Striker", "Striker"),
                           skin_tone = c("light", "light", "light", "dark", "dark"),
                           yellowCards = c(0, 2, 1, 0, 0),
                           yellowReds = c(0, 0, 0, 1, 0),
                           redCards = c(1, 0, 0, 0, 1))

# running estimation_players on test data
player_test_results <- estimation_players(player_test_data)

# testing to ensure that output is a data frame
expect_true(is.data.frame(player_test_results))

# testing to make sure there is are no duplicate rows for player, position, skin_tone group
expected_groups <- player_test_data %>%
  distinct(player, position, skin_tone)

expect_equal(nrow(player_test_results), nrow(expected_groups))
