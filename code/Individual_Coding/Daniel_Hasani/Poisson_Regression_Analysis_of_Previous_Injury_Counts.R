# Author: Daniel Hasani
# Purpose: Investigate factors associated with the number of previous injuries.
# Analysis: Do training load, age, minutes played, BMI or playing position relate to the number of previous injuries?
players <- read.csv("data/league_players.csv")
players$position <- factor(players$position)

#looks at dist of prev injury counts
table(players$prev_injuries)
mean(players$prev_injuries)
var(players$prev_injuries)

#Poisson model
injury_count_glm <- glm(prev_injuries ~ training_load + age + minutes_played + bmi + position,
  family = poisson(link = "log"),data = players)

summary(injury_count_glm)

#overdispersion
dispersion <- sum(
  residuals(injury_count_glm, type = "pearson")^2
) / injury_count_glm$df.residual

dispersion

#quasi poisson model from notes
injury_count_quasi <- glm(
  prev_injuries ~ training_load + age + minutes_played + bmi + position,
  family = quasipoisson(link = "log"),data = players)
summary(injury_count_quasi)

#Previous injury counts showed mild overdispersion (1.31)
#No predictors were significant in the quasi-Poisson model