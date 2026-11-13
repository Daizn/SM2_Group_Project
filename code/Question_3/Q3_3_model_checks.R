# Analysis: Q3 - model checks (proportional odds and fit)
# Author: Daniel Hasani
# Purpose: Proportional odds check and fit checks for the binary model of high load (Weeks 9 and 4).

library(MASS)
library(nnet)
#data
players <- read.csv("data/league_players.csv", header = T)
players$load <- factor(players$training_load, levels = 1:5, ordered = T)
players$high <- ifelse(players$training_load >= 4, 1, 0)
players$position <- factor(players$position, levels = c("DEF", "GK", "MID", "FWD"))
players$mins <- players$minutes_played / 1000

#proportional odds model + multinomial model
polr.fit <- polr(load ~ position + mins, data = players)
polr.fit
mult.fit <- multinom(load ~ position + mins, data = players)
mult.fit

#check proportional odds
new.data <- expand.grid(position = levels(players$position), mins = quantile(players$mins, c(0.1, 0.3, 0.5, 0.7, 0.9)))
mult.pr <- predict(mult.fit, new.data, type = "probs")
mult.cpr <- apply(mult.pr, 1, cumsum)
logit <- function(x) log(x / (1 - x))
summary(logit(mult.cpr[2, ]) - logit(mult.cpr[1, ]))
summary(logit(mult.cpr[3, ]) - logit(mult.cpr[2, ]))
summary(logit(mult.cpr[4, ]) - logit(mult.cpr[3, ]))

#differences
diff(polr.fit$zeta)

#binary model for high load
glm.fit1 <- glm(high ~ position + mins, family = binomial, data = players)
summary(glm.fit1)
exp(cbind(coef(glm.fit1), confint.default(glm.fit1)))

#Cook's distances
plot(cooks.distance(glm.fit1), type = "h", xlab = "Obs. number", ylab = "Cook's distance")
max(cooks.distance(glm.fit1))

#group the players into ten groups by fitted probability compare observed and expected
groups <- cut(fitted(glm.fit1), quantile(fitted(glm.fit1), seq(0, 1, 0.1)), include.lowest = T)
cbind(observed = tapply(players$high, groups, sum), expected = round(tapply(fitted(glm.fit1), groups, sum), 1))
