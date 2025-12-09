#' Results

#Utilizing a simulation of 5000 games
simresults_5000 <- sim_game(player_estimate, referee_estimate, games = 5000, seed = 123)

#Taking the data frame from the results, let's look deeper into the results.

#------TABLES------#

#Get GT Summary Table For Analysis 
table_gt <- tbl_summary(simresults_5000[,3:4], 
            by = team_skin_tone,
            label = list(total_cards ~ "Total Cards"),
            statistic = list(
              all_continuous() ~ "{median} ({mean}) {min} ({max})"))
print(table_gt)

#Kable Table for R Markdown
kable_table <- as_kable(table_gt)
print(kable_table) #Do this in R Markdown File if we want this table

#------GRAPHS------#

#Compare total cards for team skin tones with graph
ggplot(simresults_5000, aes(x = team_skin_tone, y = total_cards)) +
  geom_col(color = "navy") +
  labs(x = "Overall Team Skin Tone",
       y = "Total Cards",
       title = "Team Skin Tones vs. Total Cards For 5000 Game Simulation") +
  scale_x_discrete(labels = c("dark" = "Dark", "light" = "Light", 
                              "very_dark" = "Very Dark", "very_light" = "Very Light")) +
  theme(plot.title = element_text(face = "bold", size = 15)) +
  theme_minimal() 


#Deeper Questions
#With this simulation, is there a relationship

