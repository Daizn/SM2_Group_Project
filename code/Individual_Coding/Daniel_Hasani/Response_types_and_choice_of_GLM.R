# Analysis: Which variables are responses and what kind of outcome each one is
# Author: Daniel Hasani
# Purpose: Compare a normal linear model with a Poisson GLM for a count response (goals conceded)
# to see why the type of response decides the model.

matches <- read.csv("data/ashford_matches.csv", header = T)
matches$home_away <- factor(matches$home_away)

#natural responses and the model family each one suggests
#injured (two options): binomial
#goals_scored, goals_conceded, prev_injuries (counts): Poisson
#training_load: ordinal model
#minutes_played, bmi (continuous): normal
#goals conceded is a small count: mean and variance
table(matches$goals_conceded)
mean(matches$goals_conceded)
var(matches$goals_conceded)
#normal linear model
conc.lm <- lm(goals_conceded ~ home_away + opponent_rank, data = matches)
summary(conc.lm)
#Poisson GLM with log link
conc.glm <- glm(goals_conceded ~ home_away + opponent_rank, family = poisson, data = matches)
summary(conc.glm)
#range of the fitted values: the linear model is not restricted to positive values, the Poisson model is
range(fitted(conc.lm))
range(fitted(conc.glm))
#fitted goals conceded at home against the strongest and weakest opponents
new.data <- data.frame(home_away = "Home", opponent_rank = c(1, 24))
predict(conc.lm, new.data)
predict(conc.glm, new.data, type = "response")
#the spread from the linear model against the fitted values
plot(fitted(conc.lm), rstudent(conc.lm), xlab = "Fitted values", ylab = "Studentised residuals")
abline(h = 0)
#opponent rank as rate ratio
exp(coef(conc.glm))
exp(confint.default(conc.glm))
