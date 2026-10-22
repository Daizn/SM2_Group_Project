#Analysis: Exploratory look at player training load and injuries
#Author: Nuuh Muhammad
#Purpose: Summarise training load by injury status and plot the comparison.

players <- read.csv("data/league_players.csv")

summary(players$training_load)

tapply(players$training_load, players$injured, mean)
#average training load for injured vs not injured players

boxplot(training_load ~ injured, data = players,
        xlab = "Injured (0 = no, 1 = yes)",
        ylab = "Training Load",
        main = "Training Load by Injury Status")
#compares the spread of training load between injured and non-injured players