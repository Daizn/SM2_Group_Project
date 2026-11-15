
# Q1: Training load, age and injury
# My part of Q1 is checking whether training load and injury are linked differently at different ages.
# Step 1: Import data

players <- read.csv("data/league_players.csv")

dim(players)
head(players)

# Step 2: Check data structure
# Check data and see if any values are missing

str(players)

summary(players)

colSums(is.na(players))

# Step 3: Centre age at 25
# Used 25 as the reference age, so 25 becomes 0
# makes it easier to look at the interaction later

players$age_c <- players$age - 25
# Check the new variable
head(players[, c("age", "age_c")])

# Step 4: Additive Logistic Regression
# First tried a model without interaction
# included previous injuries as it could also be related to injury risk

model_add <- glm(injured ~ age_c + training_load + prev_injuries,
                 data = players, family = binomial)

summary(model_add)

# Step 5: Logistic regression with interaction
# Now checking if training load and injury are linked differently depending on age
# the * gives the interaction as well

model_int <- glm(injured ~ age_c * training_load + prev_injuries,
                 data = players, family = binomial)

summary(model_int)

# Step 6: Compare additive and interaction models
# Compare both models using LRT and AIC
# interaction seems to improve fit (p < 0.001)

anova(model_add, model_int, test = "Chisq")

AIC(model_add, model_int)

# Step 7: Odds ratios at different ages
# Checking how the training load OR changes at ages 22, 26 and 30
# OR is about odds, not probability

ages <- c(22, 26, 30)

beta <- coef(model_int)

log_or <- beta["training_load"] +
  (ages - 25) * beta["age_c:training_load"]

odds_ratios <- exp(log_or)

data.frame(Age = ages, Odds_Ratio = round(odds_ratios, 3))

# Step 8: 95% Confidence Intervals for Odds Ratios
# Added 95% intervals to check the uncertainty of the ORs
# at 22 it includes 1, while at 26 and 30 it doesn't

V <- vcov(model_int)

age_c_values <- ages - 25

SE <- sqrt(
  V["training_load", "training_load"] +
    age_c_values^2 *
    V["age_c:training_load", "age_c:training_load"] +
    2 * age_c_values *
    V["training_load", "age_c:training_load"]
)

OR_table <- data.frame(
  Age = ages,
  OR = round(exp(log_or), 3),
  Lower_95 = round(exp(log_or - 1.96 * SE), 3),
  Upper_95 = round(exp(log_or + 1.96 * SE), 3)
)

print(OR_table)

# Step 10: Quadratic age robustness check
# Tried adding age squared to see if the relationship is non linear
# didn't help much, so I kept the simpler model

model_quad <- glm(injured ~ age_c * training_load + I(age_c^2) + prev_injuries,
                  data = players, family = binomial)
# Compare the models
anova(model_int, model_quad, test = "Chisq")

AIC(model_int, model_quad)

summary(model_quad)


# Step 11: Cook's Distance
# Checking if some players influence the results too much
# 4/n is only a rough cutoff, not something to delete automatically

cook <- cooks.distance(model_int)

# Summary of Cook's Distance
summary(cook)

# Five most influential observations
head(sort(cook, decreasing = TRUE), 5)

# Number of observations above 4/n
sum(cook > 4 / nrow(players))

# Step 12: Examine influential observations
# Looking at the 5 players with the highest Cook's distance.
# I compare their actual injury outcome with the predicted risk.

top5 <- order(cook, decreasing = TRUE)[1:5]

data.frame(
  Row = top5,
  players[top5, c("age", "training_load",
                  "prev_injuries", "injured")],
  Predicted_Risk = round(
    predict(model_int, type = "response")[top5], 3
  ),
  Cooks_Distance = round(cook[top5], 4)
)

# Step 13: Sensitivity analysis
# Tried the model without the top 5 influential players
# interaction was still positive and significant

# Exclude the 5 most influential observations
players_sens <- players[-top5, ]

# Refit the same interaction model
model_int_sens <- update(model_int, data = players_sens)

# Compare interaction coefficients and p-values
data.frame(
  Model = c("Original", "Without_top_5"),
  Coefficient = c(
    coef(model_int)["age_c:training_load"],
    coef(model_int_sens)["age_c:training_load"]
  ),
  P_value = c(
    summary(model_int)$coefficients[
      "age_c:training_load", "Pr(>|z|)"
    ],
    summary(model_int_sens)$coefficients[
      "age_c:training_load", "Pr(>|z|)"
    ]
  )
)

# Step 14: Alternative interaction check
# Testing age with previous injuries too, but it didn't improve the fit

model_age_prev <- glm(injured ~ age_c * training_load + prev_injuries +
                         age_c:prev_injuries, data = players, family = binomial)

anova(model_int, model_age_prev, test = "Chisq")

AIC(model_int, model_age_prev)

# Step 15: Training load x previous injuries
# Checking load x previous injuries as another possible interaction
# p was around 0.061 and the AIC improved only a bit

model_load_prev <- glm(injured ~ age_c * training_load +
                         prev_injuries + training_load:prev_injuries,
                         data = players, family = binomial)

anova(model_int, model_load_prev, test = "Chisq")

AIC(model_int, model_load_prev)

# Step 16: Training load x position interaction
# Position is categorical, changed it to factor
# I checked if load is associated with injury differently by position

# Convert position to a categorical variable
players$position <- factor(players$position)

# Model with position but without load x position
model_pos <- glm(injured ~ age_c * training_load + prev_injuries + position,
                 data = players, family = binomial)

# Model with additional load x position interaction
model_pos_int <- glm(
  injured ~ age_c * training_load +
    prev_injuries + position +
    training_load:position,
  data = players,
  family = binomial
)

# Likelihood Ratio Test
anova(model_pos, model_pos_int, test = "Chisq")

# Compare AIC
AIC(model_pos, model_pos_int)

# Step 17: Training Load x Minutes Played
# Wanted to check minutes played as well
# used thousands so the coefficients are easier to read

# Convert minutes to thousands
players$minutes_k <- players$minutes_played / 1000

# Model with minutes played
model_minutes <- glm(injured ~ age_c * training_load + prev_injuries + minutes_k,
                     data = players, family = binomial)

# Add the interaction
model_minutes_int <- glm(
  injured ~ age_c * training_load +
    prev_injuries + minutes_k +
    training_load:minutes_k,
  data = players,
  family = binomial
)

# Likelihood Ratio Test
anova(model_minutes, model_minutes_int,
      test = "Chisq")

# Compare AIC
AIC(model_int, model_minutes, model_minutes_int)


# Step 18: Grouped goodness-of-fit
# Split players into 10 groups based on predicted injury probability
# compare how many were injured vs how many the model expected

# Predicted injury probabilities
p_hat <- predict(model_int, type = "response")

# Divide players into 10 risk groups
risk_decile <- ceiling(
  10 * rank(p_hat, ties.method = "first") / length(p_hat)
)

# Compare observed and expected injuries
gof_table <- aggregate(
  cbind(
    Observed = players$injured,
    Expected = p_hat
  ),
  by = list(Decile = risk_decile),
  FUN = sum
)

print(gof_table)

# Step 19: Grouped goodness-of-fit test
# Used an approximate grouped fit test
# p around 0.018 suggests calibration might be a problem
# but this depends on the grouping, so need to be careful

# Number of players in each risk group
gof_table$N <- as.numeric(table(risk_decile))

# Hosmer-Lemeshow-type statistic
HL_stat <- with(gof_table, sum(
  (Observed - Expected)^2 /
    (Expected * (1 - Expected / N))
))

# Degrees of freedom
df_HL <- nrow(gof_table) - 2

# Approximate p-value
p_HL <- pchisq(
  HL_stat,
  df = df_HL,
  lower.tail = FALSE
)
# Display results
data.frame(
  Statistic = HL_stat,
  DF = df_HL,
  P_value = p_HL
)

# Step 20: Residuals and Leverage
# Check residuals and leverage for unusual observations
# doesn't mean I should remove them straight away

# Standardised deviance residuals
residuals_std <- rstandard(
  model_int,
  type = "deviance"
)

# Leverage values
leverage <- hatvalues(model_int)

# Summary
summary(residuals_std)
summary(leverage)

# Five largest absolute residuals
head(sort(abs(residuals_std),
          decreasing = TRUE), 5)

# Five highest leverage values
head(sort(leverage,
          decreasing = TRUE), 5)

# Step 21: Age-band robustness check
# Also tried age groups instead of exact ages
# interaction was still significant but AIC was higher

# Create age categories
players$age_band <- cut(
  players$age,
  breaks = c(-Inf, 22, 26, Inf),
  labels = c("22_or_under", "23_to_26", "27_plus")
)

# Model without age-band interaction
model_band_add <- glm(injured ~ age_band + training_load + prev_injuries,
                      data = players, family = binomial)

# Model with age-band interaction
model_band_int <- glm(injured ~ age_band * training_load + prev_injuries,
                      data = players, family = binomial)

# Test the additional interaction
anova(model_band_add, model_band_int, test = "Chisq")

# Compare AIC values
AIC(model_int, model_band_add, model_band_int)

# Step 22: Goodness-of-fit diagnostic plot
# Plot observed and expected injuries for each risk group
# closer to the line means closer predictions

plot(
  gof_table$Expected,
  gof_table$Observed,
  pch = 19,
  xlab = "Expected number of injuries",
  ylab = "Observed number of injuries",
  main = "Q1: Observed vs Expected Injuries"
)

# Perfect agreement reference line
abline(a = 0, b = 1, lty = 2)

# Step 23: Save diagnostic figure
# Saving the same graph as an image for the group results.

png("outputs/Question_1/Q1_GOF_plot.png",
    width = 1200,
    height = 800,
    res = 150)

plot(
  gof_table$Expected,
  gof_table$Observed,
  pch = 19,
  xlab = "Expected number of injuries",
  ylab = "Observed number of injuries",
  main = "Q1: Observed vs Expected Injuries"
)

abline(a = 0, b = 1, lty = 2)

dev.off()

# Step 25: Club effects check
# Checking club differences as another test
# age x load coefficient stays quite similar

# Convert team into categorical variable
players$team <- factor(players$team)

# Model with club fixed effects
model_club <- glm(injured ~ age_c * training_load + prev_injuries + team,
                  data = players, family = binomial)

# Likelihood Ratio Test
anova(model_int, model_club, test = "Chisq")

# AIC comparison
AIC(model_int, model_club)

# Check the main interaction coefficient
coef(model_club)["age_c:training_load"]

# Step 26: Compare training load specifications
# Tried training load as numbers, categories and high vs low (4+ = high)

# Create high-load indicator
players$high_load <- as.integer(
  players$training_load >= 4
)

# Model with training load as a factor
model_load_factor <- glm(injured ~ age_c + factor(training_load) + prev_injuries,
                         data = players, family = binomial)

# Model with high-load indicator
model_load_high <- glm(injured ~ age_c + high_load + prev_injuries,
                       data = players, family = binomial)
# Compare additive models
AIC(model_add, model_load_factor, model_load_high)

# Step 27: Age x High Load Interaction
# Trying age x high load as well
# interaction is significant, but linear load has slightly lower AIC

# Fit interaction model with binary high load
model_high_int <- glm(injured ~ age_c * high_load + prev_injuries,
                      data = players, family = binomial)

# Test whether interaction improves the high-load model
anova(model_load_high, model_high_int, test = "Chisq")

# Compare with our current interaction model
AIC(model_int, model_high_int)

# Step 28: Factor Training Load Interaction
# Here each training load category can have its own interaction with age
# but it also adds more parameters

# Model with categorical training load interaction
model_factor_int <- glm(injured ~ age_c * factor(training_load) + prev_injuries,
                        data = players, family = binomial)

# Test whether interactions improve the factor model
anova(model_load_factor, model_factor_int, test = "Chisq")

# Compare all three interaction specifications
AIC(model_int, model_high_int, model_factor_int)

# Step 29: Final Model Comparison Table
# Put all main models into one table to compare AIC and deviance

models <- list(
  "Additive Linear" = model_add,
  "Additive Factor" = model_load_factor,
  "Additive High Load" = model_load_high,
  "Interaction Linear" = model_int,
  "Interaction High Load" = model_high_int,
  "Interaction Factor" = model_factor_int
)

model_table <- data.frame(
  Model = names(models),
  AIC = round(sapply(models, AIC), 2),
  Deviance = round(sapply(models, deviance), 2)
)

rownames(model_table) <- NULL

print(model_table)

# Step 30: Save final model comparison table
# Saving the comparison table for the group to use later.

write.csv(model_table, "outputs/Question_1/Q1_model_comparison.csv",  row.names = FALSE)

# Check that the file exists
file.exists("outputs/Question_1/Q1_model_comparison.csv")



# Check script location
getwd()

# Check saved outputs
file.exists("outputs/Question_1/Q1_GOF_plot.png")
file.exists("outputs/Question_1/Q1_model_comparison.csv")
