#' Results

#Utilizing a simulation of 5000 games
simresults_5000 <- sim_game(player_estimate, referee_estimate, games = 5000, seed = 123)

#See the proportion of total cards to each skin tone for analysis
simresults_5000 <- simresults_5000 %>%
  mutate(proportion_cards_light = any_cards_light / total_cards) %>%
  mutate(proportion_cards_verylight = any_cards_verylight / total_cards) %>%
  mutate(proportion_cards_dark = any_cards_dark / total_cards) %>%
  mutate(proportion_cards_verydark = any_cards_verydark / total_cards)

#Prevent NAs from Dividing by 0 for Proportions
simresults_5000[is.na(simresults_5000)] <- 0

#Combining data frames to see if bias is a possible predictor
simresults_5000_bias <- full_join(simresults_5000, soccer_data, by = "refNum")

#Remove unnecessary columns
simresults_5000_bias <- subset(simresults_5000_bias, select = -c(12,13,14,15,16,17,18,19,26,27))

simresults_5000_bias <- na.omit(simresults_5000_bias)

#Taking the data frame from the results, let's look deeper into the results.

#------TABLES------#

#Get GT Summary Table For Analysis 
table_gt <- tbl_summary(simresults_5000, 
            label = list(total_cards ~ "Total Cards",
                         any_cards_light ~ "Light Skin Tone Total Cards",
                         any_cards_verylight ~ "Very Light Skin Tone Total Cards",
                         any_cards_dark ~ "Dark Skin Tone Total Cards",
                         any_cards_verydark ~ "Very Dark Skin Tone Total Cards",
                         game_id ~ "Game ID",
                         refNum ~ "Referee Number",
                         proportion_cards_light ~ "Prop. of Cards to Light Skin",
                         proportion_cards_verylight ~ "Prop. of Cards to Very Light Skin",
                         proportion_cards_dark ~ "Prop. of Cards to Dark Skin",
                         proportion_cards_verydark ~ "Prop. of Cards to Very Dark Skin"),
            statistic = list(
              all_continuous() ~ "{median}% ({mean}%) {sd}",
              all_categorical() ~ "{n} ({p}%)"))
print(table_gt)

#Kable Table for R Markdown
kable_table <- as_kable(table_gt)
print(kable_table) #Do this in R Markdown File if we want a kable version table

#------GRAPHS------#
ggplot(simresults_5000_bias, aes(meanIAT, any_cards_dark)) +
  geom_smooth()

#------TESTS------#

#Look at correlation between the proportion of total cards called by referees
#by skin tone and mean implicit bias score for the referees.

cor(simresults_5000_bias$meanIAT, simresults_5000_bias$proportion_cards_verylight)

cor(simresults_5000_bias$meanIAT, simresults_5000_bias$proportion_cards_light)

cor(simresults_5000_bias$meanIAT, simresults_5000_bias$proportion_cards_dark)

cor(simresults_5000_bias$meanIAT, simresults_5000_bias$proportion_cards_verydark)

#Look at correlation between the proportion of total cards called by referees
#by skin tone and mean explicit bias score for the referees.

cor(simresults_5000_bias$meanExp, simresults_5000_bias$proportion_cards_verylight)

cor(simresults_5000_bias$meanExp, simresults_5000_bias$proportion_cards_light)

cor(simresults_5000_bias$meanExp, simresults_5000_bias$proportion_cards_dark)

cor(simresults_5000_bias$meanExp, simresults_5000_bias$proportion_cards_verydark)

#NOTE: A higher meanIAT and meanExp means more bias against black people vs white people.