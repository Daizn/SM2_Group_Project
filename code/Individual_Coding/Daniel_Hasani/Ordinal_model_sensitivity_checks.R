# Analysis: Sensitivity checks for the ordinal model of training load
# Author: Daniel Hasani
# Purpose: Check whether the proportional odds model changes when the sparse level 1 is merged
# with level 2, and compare the effects at each cut-off of training load.
library(MASS)
players <- read.csv("data/league_players.csv", header = T)
players$position <- factor(players$position, levels = c("DEF", "GK", "MID", "FWD"))
players$mins <- players$minutes_played / 1000
#five levels
players$load <- factor(players$training_load, levels = 1:5, ordered = T)
table(players$load)
polr.fit <- polr(load ~ position + mins, data = players, Hess = T)
round(exp(cbind(summary(polr.fit)$coef[1:4, 1], summary(polr.fit)$coef[1:4, 1] - 1.96 * summary(polr.fit)$coef[1:4, 2], summary(polr.fit)$coef[1:4, 1] + 1.96 * summary(polr.fit)$coef[1:4, 2])), 2)
#1 and 2 merged
players$load4 <- factor(ifelse(players$training_load == 1, 2, players$training_load), levels = 2:5, ordered = T)
table(players$load4)
polr.fit.m <- polr(load4 ~ position + mins, data = players, Hess = T)
round(exp(cbind(summary(polr.fit.m)$coef[1:4, 1], summary(polr.fit.m)$coef[1:4, 1] - 1.96 * summary(polr.fit.m)$coef[1:4, 2], summary(polr.fit.m)$coef[1:4, 1] + 1.96 * summary(polr.fit.m)$coef[1:4, 2])), 2)
#a separate logistic model at each cut-off
glm.cut3 <- glm(I(training_load >= 3) ~ position + mins, family = binomial, data = players)
glm.cut4 <- glm(I(training_load >= 4) ~ position + mins, family = binomial, data = players)
glm.cut5 <- glm(I(training_load >= 5) ~ position + mins, family = binomial, data = players)
round(exp(cbind(coef(glm.cut3), confint.default(glm.cut3)))[-1, ], 2)
round(exp(cbind(coef(glm.cut4), confint.default(glm.cut4)))[-1, ], 2)
round(exp(cbind(coef(glm.cut5), confint.default(glm.cut5)))[-1, ], 2)
#is effect of minutes linear on logit scale?
polr.quad <- update(polr.fit, . ~ . + I(mins^2))
anova(polr.fit, polr.quad)
