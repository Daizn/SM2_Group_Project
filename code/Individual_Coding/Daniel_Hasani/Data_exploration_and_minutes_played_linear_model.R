# Analysis: Data exploration and a linear model for minutes played
# Author: Daniel Hasani
# Purpose: Summarise both datasets, classify each variable by type, and try a normal linear model
# for a continuous outcome (minutes played) with the usual residual checks.
players <- read.csv("data/league_players.csv", header = T)
matches <- read.csv("data/ashford_matches.csv", header = T)
#size and summaries of both datasets
dim(players)
dim(matches)
summary(players)
summary(matches)
#variable types
#continuous: age, minutes_played, bmi
#counts: prev_injuries, goals_scored, goals_conceded
#binary: injured, home_away
#ordinal: training_load (1 to 5), opponent_rank
#nominal: position, team
table(players$position)
table(players$training_load)
table(players$injured)
table(players$team)
#minutes played by position
players$position <- factor(players$position, levels = c("DEF", "GK", "MID", "FWD"))
boxplot(minutes_played ~ position, data = players, xlab = "Position", ylab = "Minutes played")
plot(players$age, players$minutes_played, xlab = "Age", ylab = "Minutes played")
#normal linear model for minutes played
mins.fit <- lm(minutes_played ~ position + age + bmi, data = players)
summary(mins.fit)
#F-tests
anova(mins.fit)
#residual checks
plot(fitted(mins.fit), rstudent(mins.fit), xlab = "Fitted values", ylab = "Studentised residuals")
abline(h = 0)
qqnorm(rstudent(mins.fit))
qqline(rstudent(mins.fit))
#possible outliers and influential players
rstudent(mins.fit)[abs(rstudent(mins.fit)) > 3]
plot(cooks.distance(mins.fit), type = "h", xlab = "Obs. number", ylab = "Cook's distance")
