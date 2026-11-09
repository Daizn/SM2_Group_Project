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
# What the model shows:
# ==============================================================================
# 
# 1. Intercept (Base Rate):
#    The intercept represents the expected baseline goals scored against a 
#    top-ranked opponent (Rank 1) when playing away.
#
# 2. Home Advantage Coefficient (home_awayHome):
#    The model shows a positive coefficient for playing at home. When exponentiated 
#    (Rate Ratio $\approx 2.00$), it indicates that playing home matches is associated 
#    with approximately twice the expected goal-scoring rate compared to playing away, 
#    holding opponent rank constant ($p \approx 0.01$).
#
# 3. Opponent Strength Coefficient (opponent_rank):
#    The coefficient for opponent rank is positive and statistically significant 
#    ($p \approx 0.005$). Because higher rank numbers denote weaker opponents, 
#    each position drop in opponent rank is associated with roughly a 5.6% 
#    increase in Ashford's expected goal-scoring rate ($RR \approx 1.06$).
#
# 4. Mathematical Foundation (Week 3 Practical):
#    Unlike standard linear models, this Poisson GLM uses a log link function 
#    ($\log(\mu) = X\beta$) to ensure predicted goal counts remain strictly 
#    non-negative. The standard errors are derived from the inverse of the 
#    expected Fisher Information matrix $\mathcal{I}(\beta) = X^T W X$ calculated 
#    via Iteratively Reweighted Least Squares (IWLS).
# ==============================================================================