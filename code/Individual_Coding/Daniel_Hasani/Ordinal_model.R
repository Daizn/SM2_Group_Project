# Analysis: Proportional odds model for training load
# Author: Daniel Hasani
# Purpose: Model selection for the proportional odds model, odds ratios with confidence
#intervals, proportional odds check and fit checks for the binary model (Weeks 9 and 4).

library(MASS)
library(nnet)
#data
players <- read.csv("data/league_players.csv", header = T)
players$load <- factor(players$training_load, levels = 1:5, ordered = T)
players$high <- ifelse(players$training_load >= 4, 1, 0)
players$position <- factor(players$position, levels = c("DEF", "GK", "MID", "FWD"))
players$mins <- players$minutes_played / 1000
table(players$load)
#start with all five variables, drop each term
polr.fit1 <- polr(load ~ position + mins + age + prev_injuries + bmi, data = players)
drop1(polr.fit1, test = "Chisq")
polr.fit2 <- update(polr.fit1, . ~ . - bmi)
drop1(polr.fit2, test = "Chisq")
polr.fit3 <- update(polr.fit2, . ~ . - age)
drop1(polr.fit3, test = "Chisq")
polr.fit4 <- update(polr.fit3, . ~ . - prev_injuries)
drop1(polr.fit4, test = "Chisq")
#final model vs full model, pos x min interaction
anova(polr.fit4, polr.fit1)
anova(polr.fit4, update(polr.fit4, . ~ . + position:mins))
#odds ratios and 95%  conf int
b <- summary(polr.fit4)$coef[1:4, ]
round(exp(cbind(b[, 1], b[, 1] - 1.96 * b[, 2], b[, 1] + 1.96 * b[, 2])), 2)
#estimated probabilities
new.data <- data.frame(position = "DEF", mins = quantile(players$mins, c(0.25, 0.75)))
round(predict(polr.fit4, new.data, type = "probs"), 2)
#proportional odds, compare AIC
polr.fit4
mult.fit <- multinom(load ~ position + mins, data = players)
mult.fit
#cumulative logits 
new.data2 <- expand.grid(position = levels(players$position), mins = quantile(players$mins, c(0.1, 0.3, 0.5, 0.7, 0.9)))
mult.cpr <- apply(predict(mult.fit, new.data2, type = "probs"), 1, cumsum)
logit <- function(x) log(x / (1 - x))
summary(logit(mult.cpr[2, ]) - logit(mult.cpr[1, ]))
summary(logit(mult.cpr[3, ]) - logit(mult.cpr[2, ]))
summary(logit(mult.cpr[4, ]) - logit(mult.cpr[3, ]))
diff(polr.fit4$zeta)
#binary model, Cook's distances + grouped observed vs expected
glm.fit1 <- glm(high ~ position + mins, family = binomial, data = players)
exp(cbind(coef(glm.fit1), confint.default(glm.fit1)))
max(cooks.distance(glm.fit1))
groups <- cut(fitted(glm.fit1), quantile(fitted(glm.fit1), seq(0, 1, 0.1)), include.lowest = T)
cbind(observed = tapply(players$high, groups, sum), expected = round(tapply(fitted(glm.fit1), groups, sum), 1))