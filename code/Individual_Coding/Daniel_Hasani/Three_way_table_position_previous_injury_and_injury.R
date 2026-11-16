# Analysis: Contingency tables for injury, previous injury and position
# Author: Daniel Hasani
# Purpose: Two-way chi-squared tests, then log-linear models for the three-way table to see
# which associations are needed (conditional independence).
players <- read.csv("data/league_players.csv", header = T)
players$position <- factor(players$position, levels = c("DEF", "GK", "MID", "FWD"))
players$prev <- ifelse(players$prev_injuries >= 1, "Yes", "No")
#two-way tables
inj.prev <- table(players$prev, players$injured)
inj.prev
prop.table(inj.prev, 1)
chisq.test(inj.prev, correct = F)
inj.pos <- table(players$position, players$injured)
inj.pos
chisq.test(inj.pos, correct = F)
prev.pos <- table(players$position, players$prev)
prev.pos
chisq.test(prev.pos, correct = F)
#odds ratio
(inj.prev[2, 2] * inj.prev[1, 1]) / (inj.prev[2, 1] * inj.prev[1, 2])
#three-way table
cells <- as.data.frame(table(position = players$position, prev = players$prev, injured = players$injured))
ftable(xtabs(Freq ~ position + prev + injured, data = cells))

#saturated model then drop terms one at a time
loglin1 <- glm(Freq ~ position * prev * injured, family = poisson, data = cells) drop1(loglin1, test = "Chisq")
loglin2 <- update(loglin1, . ~ . - position:prev:injured) drop1(loglin2, test = "Chisq")
#injury and position independent given previous injury
loglin3 <- update(loglin2, . ~ . - position:injured) drop1(loglin3, test = "Chisq")
1 - pchisq(loglin3$deviance, loglin3$df.residual)
#observed and fitted counts
cbind(cells, fitted = round(fitted(loglin3), 1))
#same odds ratio 
exp(coef(loglin3)["prevYes:injured1"])
