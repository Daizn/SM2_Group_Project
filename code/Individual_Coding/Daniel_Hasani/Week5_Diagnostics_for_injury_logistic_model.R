# Analysis: Week 5 lab - checking the logistic model for player injury
# Author: Daniel Hasani
# Purpose: Return to the injury model from the Week 4 lab and check it: drop terms by LRT,
# then leverage, Cook's distance and grouped observed vs expected counts.

players <- read.csv("data/league_players.csv", header = T)
players$position <- factor(players$position)

#model from the Week 4 lab
injury.glm <- glm(injured ~ training_load * position + prev_injuries + age + minutes_played + bmi, family = binomial, data = players)

#is the load by position interaction needed?
injury.add <- update(injury.glm, . ~ . - training_load:position)
anova(injury.add, injury.glm, test = "Chisq")

#which main effects are needed?
drop1(injury.add, test = "Chisq")

#smaller model
injury.fit <- glm(injured ~ training_load + prev_injuries + age, family = binomial, data = players)
anova(injury.fit, injury.add, test = "Chisq")
summary(injury.fit)

#the residual deviance is not a goodness of fit test here because the data are ungrouped 0/1
#leverages, average is p/n
mean(hatvalues(injury.fit))
4 / nrow(players)
sum(hatvalues(injury.fit) > 2 * 4 / nrow(players))

#Cook's distances
plot(cooks.distance(injury.fit), type = "h", xlab = "Obs. number", ylab = "Cook's distance")
max(cooks.distance(injury.fit))

#group the players into ten groups by fitted probability, compare observed and expected injuries
groups <- cut(fitted(injury.fit), quantile(fitted(injury.fit), seq(0, 1, 0.1)), include.lowest = T)
obs <- tapply(players$injured, groups, sum)
expd <- tapply(fitted(injury.fit), groups, sum)
n <- tapply(players$injured, groups, length)
cbind(n, observed = obs, expected = round(expd, 1))

#Pearson statistic for the grouped table
X2 <- sum((obs - expd)^2 / (expd * (1 - expd / n)))
X2
1 - pchisq(X2, 8)
