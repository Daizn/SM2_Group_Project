# Author: Daniel Hasani
# Purpose: Did Ashford score more goals at home than away?
# Analysis: Poisson regression with model fit and residual checks.

matches <- read.csv("data/ashford_matches.csv")
matches$home_away <- factor(matches$home_away)

#modelling goals scored by home or away stat and opponent rank included
ashford_glm <- glm(goals_scored ~ home_away + opponent_rank,
  family = poisson,data = matches)
summary(ashford_glm)

#checking for overall model fit
1 - pchisq(ashford_glm$deviance, ashford_glm$df.residual)

#looking for unusual residuals or patterns in the data
plot(fitted(ashford_glm), rstandard(ashford_glm), 
     xlab = "Fitted values", ylab = "Standardised deviance residuals")
abline(h = 0, lty = 2)

# Large infulencial data checks
plot(cooks.distance(ashford_glm), type = "h", xlab = "Match number", ylab = "Cook's distance")

#Ashford scored significantly more goals at home than away
#The G of F test showed no clear evidence of poor fit (p = 0.221)
#One match had much greater influence than the others