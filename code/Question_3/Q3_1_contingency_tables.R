# Analysis: Q3 - contingency tables for position, minutes played and high training load
# Author: Daniel Hasani
# Purpose: Chi-squared tests and marginal vs adjusted odds ratios for position and high load (Weeks 6 and 8).

#data
players <- read.csv("data/league_players.csv", header = T)
players$high <- ifelse(players$training_load >= 4, 1, 0)
players$position <- factor(players$position, levels = c("DEF", "GK", "MID", "FWD"))

#min played three equal-sized groups
players$mins_group <- cut(players$minutes_played, breaks = quantile(players$minutes_played, c(0, 1/3, 2/3, 1)), labels = c("Low", "Mid", "High"), include.lowest = T)

#chi-squared test between pos and high load
pos.high <- table(players$position, players$high)
pos.high
chisq.test(pos.high, correct = F)

#chi-squared test between min group and high load
mins.high <- table(players$mins_group, players$high)
mins.high
prop.table(mins.high, 1)
chisq.test(mins.high, correct = F)

#chi-squared test between pos and min group
pos.mins <- table(players$position, players$mins_group)
pos.mins
chisq.test(pos.mins, correct = F)

#median min played by pos
tapply(players$minutes_played, players$position, median)

#marginal odds ratios high load compared to defenders
marg.fit <- glm(high ~ position, family = binomial, data = players)
exp(coef(marg.fit))
exp(confint.default(marg.fit))

#log-linear models for three way table
cells <- as.data.frame(table(players[, c("position", "high", "mins_group")]))

#pos and high load independent given min group
loglin1 <- glm(Freq ~ position * mins_group + high * mins_group, family = poisson, data = cells)
#same association between pos and high load in every min group
loglin2 <- update(loglin1, . ~ . + position:high)
#saturated model
loglin3 <- update(loglin2, . ~ . + position:high:mins_group)
anova(loglin1, loglin2, loglin3, test = "Chisq")

#odds ratios of high load for each pos, adjusted for min group
exp(coef(loglin2)[c("positionGK:high1", "positionMID:high1", "positionFWD:high1")])
exp(confint.default(loglin2)[c("positionGK:high1", "positionMID:high1", "positionFWD:high1"), ])
