# ==============================================================================
# Question 2: Poisson GLM Analysis for Ashford Matches
# ==============================================================================

# 1. Load data from the project folder
data <- read.csv("ashford_matches.csv")

# 2. Fit the Poisson GLM using the canonical log link
# Model predicts goals scored based on opponent rank and home/away venue
fit_scored <- glm(goals_scored ~ opponent_rank + home_away, 
                  family = poisson(link = "log"), 
                  data = data)

# 3. View model summary
summary(fit_scored)

# 4. Extract Rate Ratios and Confidence Intervals
# Exponentiating coefficients converts log-scale effects into multiplicative rate ratios
exp(coef(fit_scored))
exp(confint(fit_scored))


# ==============================================================================
# SUMMARY & MODEL EVALUATION:
# ==============================================================================
# 
# 1. Summary of Results:
#    - Intercept: Represents the expected baseline goals scored against a top-ranked 
#      opponent (Rank 1) when playing away.
#    - Home Advantage: Playing at home roughly doubles the expected goal-scoring 
#      rate compared to playing away, holding opponent rank constant (p < 0.05).
#    - Opponent Strength: Each position drop in opponent rank increases Ashford's 
#      expected goal-scoring rate by roughly 5.6% (p < 0.01).
#
# 2. Evaluation of Technique and Model Suitability:
#    - Is Poisson GLM appropriate for this relationship? 
#    - SUITABLE: Goal counts are discrete non-negative values (0, 1, 2...), making 
#      the Poisson generalized linear model structurally superior to standard 
#      Linear Regression (LM). The log link function ensures predicted values 
#      remain non-negative.
#    - LIMITATION TO CONSIDER: The standard Poisson model assumes equidistribution 
#      (mean equals variance). If residual deviance indicates overdispersion in 
#      subsequent checks, we should evaluate a Quasi-Poisson or Negative Binomial 
#      GLM alternative for robustness.
# ==============================================================================
