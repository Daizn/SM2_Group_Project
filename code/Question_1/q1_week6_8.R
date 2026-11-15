# ==============================================================================
# Question 1 - Weeks 6-8: Contingency Tables (Age Band x Training Load x Injury)
# ==============================================================================

# 1. Load data from the project folder
data <- read.csv("data/league_players.csv")

# 2. Create age bands and load groups
data$age_band <- cut(data$age, breaks = c(0, 22, 26, 100),
                     labels = c("<=22", "23-26", "27+"))
data$load_group <- factor(ifelse(data$training_load >= 4, "High", "Low"),
                          levels = c("Low", "High"))

# 3. Three way table (load group, injured, age band) and the marginal table
tab3 <- table(data$load_group, data$injured, data$age_band)
print(tab3)
marg <- apply(tab3, c(1, 2), sum)
print(marg)

# 4. Odds ratios of high vs low load with 95% confidence intervals
# Odds ratio from a 2 x 2 table, interval from the log odds ratio
or <- function(t) (t[2, 2] * t[1, 1]) / (t[2, 1] * t[1, 2])
or_ci <- function(t) {
  l <- log(or(t))
  se <- sqrt(sum(1 / t))
  round(exp(c(l, l - 1.96 * se, l + 1.96 * se)), 2)
}
print("--- ODDS RATIOS (OR, LOWER, UPPER) ---")
print(or_ci(marg))
print(or_ci(tab3[, , 1]))
print(or_ci(tab3[, , 2]))
print(or_ci(tab3[, , 3]))

# 5. Tests on the marginal table and within each age band
print("--- MARGINAL CHI-SQUARED TEST ---")
print(chisq.test(marg, correct = FALSE))
print("--- FISHER EXACT TESTS WITHIN AGE BANDS ---")
print(fisher.test(tab3[, , 1]))
print(fisher.test(tab3[, , 2]))
print(fisher.test(tab3[, , 3]))

# 6. Mantel-Haenszel test of load and injury given age band
print("--- MANTEL-HAENSZEL TEST ---")
print(mantelhaen.test(tab3))

# 7. Do the odds ratios differ across age bands? (Woolf test, 2 df)
lor <- c(log(or(tab3[, , 1])), log(or(tab3[, , 2])), log(or(tab3[, , 3])))
w <- 1 / c(sum(1 / tab3[, , 1]), sum(1 / tab3[, , 2]), sum(1 / tab3[, , 3]))
x2 <- sum(w * (lor - sum(w * lor) / sum(w))^2)
print("--- WOOLF HOMOGENEITY TEST ---")
print(c(X2 = x2, p = 1 - pchisq(x2, 2)))

# 8. Are age band and load group related?
print(chisq.test(table(data$age_band, data$load_group), correct = FALSE))

# ==============================================================================
# SUMMARY & EVALUATION
# ==============================================================================
#
# 1. Summary of Results:
#    - Marginal Table: Ignoring age, the odds of injury were about twice as
#      high in the high load group (OR 2.09, 95% CI 1.39 to 3.16, chi-squared
#      p < 0.001).
#    - Age Specific Tables: The odds ratio was 0.71 (0.24 to 2.10) aged 22 or
#      under, 1.74 (0.88 to 3.43) for 23-26 and 3.40 (1.79 to 6.45) for 27+.
#      Fisher exact p-values were 0.612, 0.112 and below 0.001.
#    - Homogeneity: The Woolf test (X2 = 6.30 on 2 df, p = 0.043) gives some
#      evidence that the load and injury association differs across age bands.
#      The Mantel-Haenszel common odds ratio was 2.01 (p = 0.001).
#    - Age band and load group were not clearly related (p = 0.165).
#
# 2. Evaluation of Technique and Model Suitability:
#    - Are contingency tables appropriate for this question?
#    - SUITABLE: Splitting by age band shows directly how the odds ratio
#      changes, and conditioning on age band checks the marginal result.
#    - LIMITATION: Counts in the youngest band are small (19 injuries), so
#      its odds ratio is imprecise. The age cut-offs are chosen by us, the
#      Woolf test has low power, and the tables cannot adjust for other
#      variables such as previous injuries.
# ==============================================================================