# Analysis: Q3 - differences between clubs and Ashford United
# Author: Daniel Hasani
# Purpose: Club tests, random intercept model and Ashford United's club effect (Weeks 9 and 10).

library(MASS)
library(lme4)

#data
players <- read.csv("data/league_players.csv", header = T)
players$load <- factor(players$training_load, levels = 1:5, ordered = T)
players$high <- ifelse(players$training_load >= 4, 1, 0)
players$position <- factor(players$position, levels = c("DEF", "GK", "MID", "FWD"))
players$mins <- players$minutes_played / 1000
players$team <- factor(players$team)

#clubs differ before accounting for player char
polr.null <- polr(load ~ 1, data = players)
polr.team <- polr(load ~ team, data = players)
anova(polr.null, polr.team)

#clubs differ after accounting for pos and min
polr.fit <- polr(load ~ position + mins, data = players)
polr.fit.team <- polr(load ~ position + mins + team, data = players)
anova(polr.fit, polr.fit.team)

#binary model
glmer.fit <- glmer(high ~ position + mins + (1 | team), family = binomial, data = players)
summary(glmer.fit)

#odds range for 95% of clubs relative to an average club
exp(c(-1.96, 1.96) * sqrt(VarCorr(glmer.fit)$team[1]))

#rand intercept compare with the model, no club effects
glm.fit1 <- glm(high ~ position + mins, family = binomial, data = players)
anova(glmer.fit, glm.fit1)

#club effects with  95% int
re <- ranef(glmer.fit, condVar = T)$team
re.se <- sqrt(attr(re, "postVar")[1, 1, ])
club <- cbind(blup = re[, 1], lower = re[, 1] - 1.96 * re.se, upper = re[, 1] + 1.96 * re.se)
rownames(club) <- rownames(re)
round(club[order(club[, 1]), ], 2)

#odds ratio relative to an average club
round(exp(club["Ashford United", ]), 2)

#proportion of high load players at each club and in the whole league
round(sort(tapply(players$high, players$team, mean)), 2)
mean(players$high)

#QQ-plot 
qqnorm(club[, 1])
qqline(club[, 1])

#95%  conf int for the club sd
confint(glmer.fit, parm = "theta_", method = "profile")
