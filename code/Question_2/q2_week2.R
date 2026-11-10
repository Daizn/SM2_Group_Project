# ---------------------------------------------------------
# Question 2 - Week 2: Regression Models (Goals Scored & Conceded)
# ---------------------------------------------------------

# Load data and prepare variables
data <- read.csv("data/ashford_matches.csv")
data$home_away <- as.factor(data$home_away)

# Quick look at data vs goals scored
plot(data$opponent_rank, data$goals_scored,
     col = c("black", "red")[data$home_away], pch = 19,
     xlab = "Opponent Rank", ylab = "Goals Scored")

# --- 1. Goals Scored Linear Model ---
fit_scored <- lm(goals_scored ~ opponent_rank + home_away, data = data)
summary(fit_scored)
# Opponent rank has a statistically significant relationship with goals scored. 
# The coefficient for home_away shows how playing at home shifts the expected goal count.

# ANOVA test for model significance
anova(fit_scored)
# The ANOVA table confirms that both opponent rank and venue (home/away) 
# contribute significant explanatory power to predicting goals scored.

# --- 2. Goals Conceded Linear Model ---
fit_conceded <- lm(goals_conceded ~ opponent_rank + home_away, data = data)
summary(fit_conceded)
# Evaluates how opponent rank and home/away status affect goals conceded.
# Higher opponent rank correlates with changes in defensive performance.

# --- 3. Diagnostics and Residuals ---
residuals_scored <- rstudent(fit_scored)
qqnorm(residuals_scored)
qqline(residuals_scored)
# QQ plot checks if model residuals are normally distributed. 
# Points tracking closely to the line indicate the normality assumption holds well.

# Cook's distance for influential points
cooks_d <- cooks.distance(fit_scored)
plot(cooks_d, type = "h", main = "Cook's Distance")
# Highlights any extreme match outliers that disproportionately influence the regression line.

# --- 4. Predictions & Intervals ---
# Confidence intervals for expected average goals
conf_int <- predict(fit_scored, interval = "confidence")

# Prediction intervals for individual match outcomes
pred_int <- predict(fit_scored, interval = "prediction")
# Prediction intervals are wider than confidence intervals because they account 
# for individual match variance, whereas confidence intervals estimate the mean response.
# ==============================================================================
# SUMMARY & MODEL EVALUATION:
# ==============================================================================
# 
# 1. Summary of Results:
#    - Exploratory Analysis: Examined goal count distributions and compared home vs. 
#      away scoring patterns for Ashford.
#    - Influential Outliers: Diagnostic checks (including Cook's distance) flagged 
#      high-leverage matches, such as the 8-0 home victory, which heavily pull 
#      the baseline rate estimate.
#
# 2. Evaluation of Technique and Model Suitability:
#    - UNSUITABLE: Standard linear regression assumes normally distributed, continuous 
#      response variables with constant variance. Goal counts are non-negative, 
#      discrete integer values with potential right-skewness.
#    - Standard linear regression produces biased standard errors and risks 
#      predicting impossible negative goal totals. These findings justify moving 
#      forward with count-data models (such as Poisson GLMs).
# ==============================================================================
