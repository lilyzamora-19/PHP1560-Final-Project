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