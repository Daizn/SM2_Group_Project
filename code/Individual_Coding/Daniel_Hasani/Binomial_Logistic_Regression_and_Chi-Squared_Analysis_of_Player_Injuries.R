# Author: Daniel Hasani
# Purpose: Investigate factors associated with player injury.
# Analysis: Does age, previous injury history, training load, minutes played,BMI or playing position relate to whether a player is injured? 
# Also check whether training load's association with injury varies by position, and whether injury status is associated with position.
players <- read.csv("data/league_players.csv")
players$position <- factor(players$position)
# Logistic regression with interaction between training load and position
injury_glm <- glm(injured ~ training_load * position + prev_injuries +age + minutes_played + bmi,
  family = binomial(link = "logit"),data = players)
summary(injury_glm)
#converts the model coeff to odds ratios
exp(coef(injury_glm))
#shows confidence intervals for odds ratios
exp(confint(injury_glm))
#to compare the injury status across the positions played
injury_position <- table(players$position, players$injured)
injury_position
chisq.test(injury_position)

#Age and previous injuries were associated with injury status
# Position and injury status were not significantly associated (p = 0.223)