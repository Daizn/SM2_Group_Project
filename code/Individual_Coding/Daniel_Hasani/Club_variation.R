# Analysis: Differences between clubs and Ashford United
# Author: Daniel Hasani
# Purpose: Tests for club differences, random intercept model for high load, club effects
# (BLUPs) with intervals and Ashford United's position (Weeks 9 and 10).

library(MASS)
library(lme4)
#data
players <- read.csv("data/league_players.csv", header = T)
players$load <- factor(players$training_load, levels = 1:5, ordered = T)
players$high <- ifelse(players$training_load >= 4, 1, 0)
players$position <- factor(players$position, levels = c("DEF", "GK", "MID", "FWD"))
players$mins <- players$minutes_played / 1000
players$team <- factor(players$team)
#club differences
anova(polr(load ~ 1, data = players), polr(load ~ team, data = players))
anova(polr(load ~ position + mins, data = players), polr(load ~ position + mins + team, data = players))
#random intercept for club
glmer.fit <- glmer(high ~ position + mins + (1 | team), family = binomial, data = players)
summary(glmer.fit)
glm.fit1 <- glm(high ~ position + mins, family = binomial, data = players)
anova(glmer.fit, glm.fit1)
#club effects (BLUPs) with approximate 95% intervals
re <- ranef(glmer.fit, condVar = T)$team
re.se <- sqrt(attr(re, "postVar")[1, 1, ])
club <- cbind(blup = re[, 1], lower = re[, 1] - 1.96 * re.se, upper = re[, 1] + 1.96 * re.se)
rownames(club) <- rownames(re)
round(club[order(club[, 1]), ], 2)
#odds ratio relative to an average club
round(exp(club["Ashford United", ]), 2)
#proportion of high load players
round(sort(tapply(players$high, players$team, mean)), 2)
mean(players$high)
#QQ-plot of the club effects
qqnorm(club[, 1])
qqline(club[, 1])

#95% conf int for the club sd (run last: profiling changes the fitted model, so ranef() must come first)
confint(glmer.fit, parm = "theta_", method = "profile")
