# Analysis: Q3 - proportional odds model for training load
# Author: Daniel Hasani
# Purpose: Model selection and odds ratios for the proportional odds model of training load (Week 9).

library(MASS)
#data
players <- read.csv("data/league_players.csv", header = T)
players$load <- factor(players$training_load, levels = 1:5, ordered = T)
players$position <- factor(players$position, levels = c("DEF", "GK", "MID", "FWD"))
players$mins <- players$minutes_played / 1000

#fit the model with all five candidate explanatory variables
polr.fit1 <- polr(load ~ position + mins + age + prev_injuries + bmi, data = players, Hess = T)
polr.fit1

#test each term against the full model
drop1(polr.fit1, test = "Chisq")

#drop bmi
polr.fit2 <- update(polr.fit1, . ~ . - bmi)
drop1(polr.fit2, test = "Chisq")

#drop age 
polr.fit3 <- update(polr.fit2, . ~ . - age)
drop1(polr.fit3, test = "Chisq")

#drop prev injuries
polr.fit4 <- update(polr.fit3, . ~ . - prev_injuries)
drop1(polr.fit4, test = "Chisq")

#compare the final model with the full model
anova(polr.fit4, polr.fit1)

#effect of min depend on pos
polr.int <- update(polr.fit4, . ~ . + position:mins)
anova(polr.fit4, polr.int)

#final model
polr.fit4

#95%  conf int for odds ratios
round(exp(cbind(summary(polr.fit4)$coef[1:4, 1], summary(polr.fit4)$coef[1:4, 1] - 1.96 * summary(polr.fit4)$coef[1:4, 2], summary(polr.fit4)$coef[1:4, 1] + 1.96 * summary(polr.fit4)$coef[1:4, 2])), 2)

#estimated prob
new.data <- data.frame(position = "DEF", mins = quantile(players$mins, c(0.25, 0.75)))
cbind(new.data, round(predict(polr.fit4, new.data, type = "probs"), 2))
