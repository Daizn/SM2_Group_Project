# Analysis: Does grouping players by club change the injury model?
# Author: Daniel Hasani
# Purpose: Add a random intercept for club to the logistic model for injury and compare the
# estimates and standard errors with the model that ignores clubs.

library(lme4)
players <- read.csv("data/league_players.csv", header = T)
players$team <- factor(players$team)
#grouped in clubs
table(players$team)
round(sort(tapply(players$injured, players$team, mean)), 2)
#complete pooling
glm.fit <- glm(injured ~ training_load + prev_injuries + age, family = binomial, data = players)
summary(glm.fit)
#no pooling
glm.team <- update(glm.fit, . ~ . + team)
anova(glm.fit, glm.team, test = "Chisq")
#partial pooling
glmer.fit <- glmer(injured ~ training_load + prev_injuries + age + (1 | team), family = binomial, data = players)
summary(glmer.fit)
#do fixed effects or standard errors change?
round(cbind(glm = coef(glm.fit), glmer = fixef(glmer.fit)), 3)
round(cbind(glm = sqrt(diag(vcov(glm.fit))), glmer = sqrt(diag(vcov(glmer.fit)))), 3)
#is the random int cpt needed?
anova(glmer.fit, glm.fit)
#club effects
re <- ranef(glmer.fit)$team
round(re[order(re[, 1]), , drop = F], 2)
#the same check for high training load
players$high <- ifelse(players$training_load >= 4, 1, 0)
glmer.high <- glmer(high ~ 1 + (1 | team), family = binomial, data = players)
summary(glmer.high)
re.high <- ranef(glmer.high)$team
qqnorm(re.high[, 1])
qqline(re.high[, 1])
