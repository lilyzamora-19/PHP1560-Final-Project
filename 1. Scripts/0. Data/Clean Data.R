#'Load Data, Packages, and Clean Data

#Packages
library(tidyverse)
library(dplyr)
library(ggplot2)

#Load in Data
soccer_data <- read.csv("/Users/lilyzamora/Desktop/PHP 1560/Data/CrowdstormingDataJuly1st.csv")

#CLEAN DATA#

#Take specific columns
soccer_data <- soccer_data[, c("playerShort", "player", "position", "yellowCards", 
                               "yellowReds", "redCards", "rater1", "rater2",
                               "refNum", "meanIAT", "nIAT", "seIAT", "meanExp",
                               "nExp", "seExp")]

#Remove NAs
soccer_data <- na.omit(soccer_data)

#Remove small sample sizes (less than 100?)

soccer_data <- subset(soccer_data, nIAT > 100)
soccer_data <- subset(soccer_data, nExp > 100)

#Average skin tone ratings between rater 1 and rater 2, then categorize
soccer_data <- soccer_data %>%
  mutate(avg_rating = (rater1 + rater2) / 2) %>%
  mutate(skin_tone = case_when(
    avg_rating >= 0.5 ~ "dark",
    avg_rating < 0.5 ~ "light"))

#Rename positions to fit a 4-2-3-1 Soccer formation
#This formation consists of 4 defenders (two centerbacks and two fullbacks),
#two defensive midfielders, three attacking midfielders, and one striker, along
#with a goalie. Each team consists of 11 players with this formation and goalie.

#Center Forward is another name for Striker,
#Left and Right wingers are wide attacking midfielders, and so will be renamed such.
#Left and Right fullback and Center back will be given defender to simplify
#Left, Right, and Center Midfielders are offensive so are attacking to simplify.

unique(soccer_data$position) #This tells us all unique positions to rename

soccer_data <- soccer_data %>%
  mutate(position = case_when(
    position == "Center Forward" ~ "Striker",
    position == "Left Fullback" ~ "Defender",
    position == "Right Fullback" ~ "Defender",
    position == "Left Midfielder" ~ "Attacking Midfielder",
    position == "Right Midfielder" ~ "Attacking Midfielder",
    position == "Center Midfielder" ~ "Attacking Midfielder",
    position == "Left Winger" ~ "Attacking Midfielder",
    position == "Right Winger" ~ "Attacking Midfielder",
    position == "Goalkeeper" ~ "Goalie",
    position == "Center Back" ~ "Defender",
    position == "Defensive Midfielder" ~ "Defensive Midfielder",
    position == "Attacking Midfielder" ~ "Attacking Midfielder"))

unique(soccer_data$position) #Shows us 5 new position designations we made
