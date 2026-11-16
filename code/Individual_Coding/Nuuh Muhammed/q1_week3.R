# ==============================================================================
# Question 1 - Week 3: Exploratory Analysis and Alternative Logistic Models
# ==============================================================================

# 1. Load data from the project folder
data <- read.csv("data/league_players.csv")
str(data)
colSums(is.na(data))

# 2. Create age bands, load groups and other variables
data$age_band <- cut(data$age, breaks = c(0, 22, 26, 100),
                     labels = c("<=22", "23-26", "27+"))
data$load_group <- factor(ifelse(data$training_load >= 4, "High", "Low"),
                          levels = c("Low", "High"))
data$high <- ifelse(data$training_load >= 4, 1, 0)
data$age_c <- data$age - 25
data$load_f <- factor(data$training_load)
table(data$age_band)
table(data$load_group)

# 3. Injury rates overall, by age band and by load group
mean(data$injured)
tapply(data$injured, data$age_band, mean)
tapply(data$injured, data$load_group, mean)
rates <- tapply(data$injured, list(data$load_group, data$age_band), mean)
round(rates, 3)

# 4. Exploratory plot of injury rate by age band and load group
barplot(rates, beside = TRUE, col = c("grey", "red"),
        xlab = "Age band", ylab = "Injury rate",
        legend.text = rownames(rates), args.legend = list(x = "topleft"))

# 5. Compare training load as a number, a factor and a high load indicator
model_num <- glm(injured ~ age_c + prev_injuries + training_load,
                 family = binomial, data = data)
model_fac <- update(model_num, . ~ . - training_load + load_f)
model_high <- update(model_num, . ~ . - training_load + high)

print("--- AIC COMPARISON ACROSS LOAD CODINGS ---")
AIC_table <- data.frame(
  Coding = c("Number", "Factor", "High load indicator"),
  AIC = c(AIC(model_num), AIC(model_fac), AIC(model_high))
)
print(AIC_table)

# 6. Age band by high load interaction (main effects kept in the model)
model_add <- glm(injured ~ age_band + high + prev_injuries,
                 family = binomial, data = data)
model_int <- glm(injured ~ age_band * high + prev_injuries,
                 family = binomial, data = data)
print("--- AGE BAND BY HIGH LOAD: LIKELIHOOD RATIO TEST ---")
print(anova(model_add, model_int, test = "Chisq"))
print(AIC(model_add, model_int))

# 7. Odds ratio of high load in each age band with 95% confidence intervals
ci <- function(m, L) {
  e <- sum(L * coef(m))
  se <- sqrt(t(L) %*% vcov(m) %*% L)
  round(exp(c(e, e - 1.96 * se, e + 1.96 * se)), 2)
}
nm <- names(coef(model_int))
print("--- HIGH LOAD ODDS RATIO (OR, LOWER, UPPER) ---")
print(ci(model_int, as.numeric(nm == "high")))
print(ci(model_int, as.numeric(nm %in% c("high", "age_band23-26:high"))))
print(ci(model_int, as.numeric(nm %in% c("high", "age_band27+:high"))))

# ==============================================================================
# SUMMARY & MODEL EVALUATION:
# ==============================================================================
#
# 1. Summary of Results:
#    - Exploratory Analysis: 115 of 620 players were injured (18.5%). Injury
#      rate rose with age band (11.9%, 16.7% and 25.7%) and was higher in the
#      high load group (25.9%) than the low load group (14.3%).
#    - Load Coding: The high load indicator had the lowest AIC (544.5), ahead
#      of load as a number (547.3) and load as a factor (549.1).
#    - Age Band Interaction: Adding the age band by high load interaction was
#      supported (LRT p = 0.029, AIC 546.1 vs 549.2). The odds ratio for high
#      load was 0.71 (0.24 to 2.15) aged 22 or under, 1.65 (0.82 to 3.35) for
#      23-26 and 3.60 (1.86 to 6.99) for 27+.
#
# 2. Evaluation of Technique and Model Suitability:
#    - Is logistic regression appropriate for the injury outcome?
#    - SUITABLE: Injured is binary (1/0), so a binomial GLM with a logit link
#      gives odds ratios with confidence intervals and keeps fitted
#      probabilities between 0 and 1.
#    - LIMITATION: Banding age and load throws away information, and only 19
#      players were injured in the youngest band, so those intervals are wide.
#      Training load and injury are observational, so results are associations.
# ==============================================================================