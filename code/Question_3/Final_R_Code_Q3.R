# Analysis: Q3 final code - training load and club variation
# Author: Daniel Hasani
# Purpose: Complete analysis for Question 3, combining the four Q3 scripts: contingency tables,
# proportional odds model, model checks and differences between clubs.

library(MASS)
library(nnet)
library(lme4)

#data
players <- read.csv("data/league_players.csv", header = T)
players$load <- factor(players$training_load, levels = 1:5, ordered = T)
players$high <- ifelse(players$training_load >= 4, 1, 0)
players$position <- factor(players$position, levels = c("DEF", "GK", "MID", "FWD"))
players$mins <- players$minutes_played / 1000
players$team <- factor(players$team)

#min played three equal-sized groups
players$mins_group <- cut(players$minutes_played, breaks = quantile(players$minutes_played, c(0, 1/3, 2/3, 1)), labels = c("Low", "Mid", "High"), include.lowest = T)

#contingency tables 
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

#proportional odds model 
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

#95% conf int for odds ratios
round(exp(cbind(summary(polr.fit4)$coef[1:4, 1], summary(polr.fit4)$coef[1:4, 1] - 1.96 * summary(polr.fit4)$coef[1:4, 2], summary(polr.fit4)$coef[1:4, 1] + 1.96 * summary(polr.fit4)$coef[1:4, 2])), 2)

#estimated prob
new.data <- data.frame(position = "DEF", mins = quantile(players$mins, c(0.25, 0.75)))
cbind(new.data, round(predict(polr.fit4, new.data, type = "probs"), 2))

#estimated prob of high load (levels 4 and 5) for the same players
round(rowSums(predict(polr.fit4, new.data, type = "probs")[, 4:5]), 2)

#same model with the sparse level 1 merged into level 2
players$load4 <- factor(ifelse(players$training_load == 1, 2, players$training_load), levels = 2:5, ordered = T)
polr.fit.m <- polr(load4 ~ position + mins, data = players, Hess = T)
round(exp(cbind(summary(polr.fit.m)$coef[1:4, 1], summary(polr.fit.m)$coef[1:4, 1] - 1.96 * summary(polr.fit.m)$coef[1:4, 2], summary(polr.fit.m)$coef[1:4, 1] + 1.96 * summary(polr.fit.m)$coef[1:4, 2])), 2)

#model checks
#multinomial model with the same variables, compare AIC with the final model above
mult.fit <- multinom(load ~ position + mins, data = players)
mult.fit

#check proportional odds
new.data2 <- expand.grid(position = levels(players$position), mins = quantile(players$mins, c(0.1, 0.3, 0.5, 0.7, 0.9)))
mult.pr <- predict(mult.fit, new.data2, type = "probs")
mult.cpr <- apply(mult.pr, 1, cumsum)
logit <- function(x) log(x / (1 - x))
summary(logit(mult.cpr[2, ]) - logit(mult.cpr[1, ]))
summary(logit(mult.cpr[3, ]) - logit(mult.cpr[2, ]))
summary(logit(mult.cpr[4, ]) - logit(mult.cpr[3, ]))

#differences
diff(polr.fit4$zeta)

#binary model for high load
glm.fit1 <- glm(high ~ position + mins, family = binomial, data = players)
summary(glm.fit1)
exp(cbind(coef(glm.fit1), confint.default(glm.fit1)))

#is position needed in the binary model?
drop1(glm.fit1, test = "Chisq")

#same model at the other cut-offs: load 3 or more and load 5
glm.cut3 <- glm(I(training_load >= 3) ~ position + mins, family = binomial, data = players)
glm.cut5 <- glm(I(training_load >= 5) ~ position + mins, family = binomial, data = players)
round(exp(cbind(coef(glm.cut3), confint.default(glm.cut3))), 2)
round(exp(cbind(coef(glm.cut5), confint.default(glm.cut5))), 2)

#Cook's distances
plot(cooks.distance(glm.fit1), type = "h", xlab = "Obs. number", ylab = "Cook's distance")
max(cooks.distance(glm.fit1))

#group the players into ten groups by fitted probability compare observed and expected
groups <- cut(fitted(glm.fit1), quantile(fitted(glm.fit1), seq(0, 1, 0.1)), include.lowest = T)
obs <- tapply(players$high, groups, sum)
expd <- tapply(fitted(glm.fit1), groups, sum)
n <- tapply(players$high, groups, length)
cbind(n, observed = obs, expected = round(expd, 1))

#Pearson statistic for the grouped table
X2 <- sum((obs - expd)^2 / (expd * (1 - expd / n)))
X2
1 - pchisq(X2, 8)

#differences between clubs
#clubs differ before accounting for player char
polr.null <- polr(load ~ 1, data = players)
polr.team <- polr(load ~ team, data = players)
anova(polr.null, polr.team)

#clubs differ after accounting for pos and min
polr.fit.team <- update(polr.fit4, . ~ . + team)
anova(polr.fit4, polr.fit.team)

#binary model with a random intercept for club
glmer.fit <- glmer(high ~ position + mins + (1 | team), family = binomial, data = players)
summary(glmer.fit)

#odds range for 95% of clubs relative to an average club
exp(c(-1.96, 1.96) * sqrt(VarCorr(glmer.fit)$team[1]))

#rand intercept compare with the binary model above, no club effects
anova(glmer.fit, glm.fit1)

#proportion of the variation that is between clubs (intraclass correlation)
VarCorr(glmer.fit)$team[1] / (VarCorr(glmer.fit)$team[1] + pi^2 / 3)

#clubs as fixed effects in the same binary model
glm.team <- update(glm.fit1, . ~ . + team)
anova(glm.fit1, glm.team, test = "Chisq")

#club effects with 95% int
re <- ranef(glmer.fit, condVar = T)$team
re.se <- sqrt(attr(re, "postVar")[1, 1, ])
club <- cbind(blup = re[, 1], lower = re[, 1] - 1.96 * re.se, upper = re[, 1] + 1.96 * re.se)
rownames(club) <- rownames(re)
round(club[order(club[, 1]), ], 2)

#odds ratio relative to an average club
round(exp(club["Ashford United", ]), 2)

#number of Ashford United players not on and on high load
table(players$high[players$team == "Ashford United"])

#proportion of high load players at each club and in the whole league
round(sort(tapply(players$high, players$team, mean)), 2)
mean(players$high)

#QQ-plot
qqnorm(club[, 1])
qqline(club[, 1])

#95% conf int for the club sd (run last: profiling changes the fitted model, so ranef() must come first)
confint(glmer.fit, parm = "theta_", method = "profile")
