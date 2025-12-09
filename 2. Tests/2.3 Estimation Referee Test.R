# estimation_referee test
#
# creating a smaller mock soccer data set to test if estimation_referee function
# works. Code should run without error if function runs correctly.


# creating a test soccer data
ref_test_data <- tibble(refNum = c(1, 1, 1, 2, 2),
                        skin_tone = c("light", "dark", "dark", "light", "dark"),
                        yellowCards = c(1, 0, 0, 0, 0),
                        yellowReds = c(0, 1, 0, 0, 0),
                        redCards = c(0, 0, 1, 0, 0))

# running estimation_referee on test data
ref_results <- estimation_referee(ref_test_data)

# testing to ensure output is a data frame
expect_true(is.data.frame(ref_results))

# testing to ensure that function should return one row per referee
expect_equal(nrow(ref_results), length(unique(ref_test_data$refNum)))
expect_true(all(ref_results$refNum %in% unique(ref_test_data$refNum)))
