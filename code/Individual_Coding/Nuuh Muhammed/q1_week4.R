# ==============================================================================
# Question 1 - Week 4: Club Sensitivity Check (Logistic Model With and Without Club)
# ==============================================================================

# 1. Load data from the project folder
data <- read.csv("data/league_players.csv")
data$team <- factor(data$team)
data$age_c <- data$age - 25

# 2. Fit the prespecified model with and without club effects
# Model chosen before looking at the club results
model_noclub <- glm(injured ~ age_c * training_load + prev_injuries,
                    family = binomial, data = data)
model_club <- update(model_noclub, . ~ . + team)

# 3. Does adding club improve the model?
print("--- LIKELIHOOD RATIO TEST FOR CLUB ---")
print(anova(model_noclub, model_club, test = "Chisq"))

print("--- AIC COMPARISON ---")
AIC_table <- data.frame(
  Model = c("Without club", "With club"),
  AIC = c(AIC(model_noclub), AIC(model_club))
)
print(AIC_table)

# 4. Odds ratio of training load at ages 22, 26 and 30
ci <- function(m, L) {
  e <- sum(L * coef(m))
  se <- sqrt(t(L) %*% vcov(m) %*% L)
  round(exp(c(e, e - 1.96 * se, e + 1.96 * se)), 2)
}
or_age <- function(m, age) {
  nm <- names(coef(m))
  ci(m, as.numeric(nm == "training_load") +
       (age - 25) * as.numeric(nm == "age_c:training_load"))
}
print("--- TRAINING LOAD ODDS RATIO (OR, LOWER, UPPER), WITHOUT CLUB ---")
print(or_age(model_noclub, 22))
print(or_age(model_noclub, 26))
print(or_age(model_noclub, 30))
print("--- TRAINING LOAD ODDS RATIO (OR, LOWER, UPPER), WITH CLUB ---")
print(or_age(model_club, 22))
print(or_age(model_club, 26))
print(or_age(model_club, 30))

# 5. Observed and expected injuries for each club (model without club)
print("--- OBSERVED VS EXPECTED INJURIES BY CLUB ---")
club_table <- cbind(observed = tapply(data$injured, data$team, sum),
                    expected = round(tapply(fitted(model_noclub), data$team, sum), 1))
print(club_table[order(club_table[, 1] - club_table[, 2], decreasing = TRUE), ])

# ==============================================================================
# SUMMARY & MODEL EVALUATION:
# ==============================================================================
#
# 1. Summary of Results:
#    - Club Effects: Adding club effects did not improve the model (LRT
#      chi-squared 21.18 on 23 df, p = 0.57) and AIC was higher with them
#      (560.1 vs 535.3).
#    - Training Load Odds Ratios: Without club, the odds ratio per load unit
#      was 0.89 (0.65 to 1.22) at age 22, 1.39 (1.11 to 1.74) at 26 and 2.17
#      (1.54 to 3.05) at 30. With club it was 0.82 (0.59 to 1.15), 1.32 (1.04
#      to 1.68) and 2.13 (1.49 to 3.04), so the pattern with age is unchanged.
#    - Grantwick FC had the largest excess, with 12 injuries against about 7
#      expected.
#
# 2. Evaluation of Technique and Model Suitability:
#    - Is a fixed effects club term appropriate here?
#    - SUITABLE: The likelihood ratio test directly asks whether club
#      differences change the relationship, using the same logistic model.
#    - LIMITATION: Adding 23 club terms uses a lot of parameters for 115
#      injuries, so the club estimates are imprecise and the test has limited
#      power. Fitted probabilities warnings are possible for the same reason.
# ==============================================================================