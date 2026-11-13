# Analysis: Contingency tables - position, minutes played and high training load
# Author: Daniel Hasani
# Purpose: Chi-squared tests and marginal vs adjusted odds ratios (log-linear model) to show
# how minutes played changes the position-load association (Weeks 6 and 8).

#data
players <- read.csv("data/league_players.csv", header = T)
summary(players)
#high load (training load 4 or 5), position with defenders as baseline and minutes played in three equal-sized groups
players$high <- ifelse(players$training_load >= 4, 1, 0)
players$position <- factor(players$position, levels = c("DEF", "GK", "MID", "FWD"))
players$mins_group <- cut(players$minutes_played, breaks = quantile(players$minutes_played, c(0, 1/3, 2/3, 1)), labels = c("Low", "Mid", "High"), include.lowest = T)
#chi-squared tests
chisq.test(table(players$position, players$high), correct = F)
mins.high <- table(players$mins_group, players$high)
prop.table(mins.high, 1)
chisq.test(mins.high, correct = F)
chisq.test(table(players$position, players$mins_group), correct = F)
#median min played by pos
tapply(players$minutes_played, players$position, median)
#marginal odds ratios
marg.fit <- glm(high ~ position, family = binomial, data = players)
exp(cbind(coef(marg.fit), confint.default(marg.fit)))
#log-linear model
cells <- as.data.frame(table(players[, c("position", "high", "mins_group")]))
loglin <- glm(Freq ~ position * mins_group + high * mins_group + position:high, family = poisson, data = cells)
#odds ratios
terms <- c("positionGK:high1", "positionMID:high1", "positionFWD:high1")
exp(cbind(coef(loglin)[terms], confint.default(loglin)[terms, ]))
