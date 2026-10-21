#Purpose: To investigate how training load, previous injuries, age and minutes played relate to player injury risk.
#Author: Rayan Suleman
#Analysis: Fit a logistic regression model, estimate odds ratios and confidence intervals, assess model fit, and produce diagnostic plots.

players <- read.csv("data/league_players.csv")

table(players$injured)

injury_glm <- glm(injured ~ training_load + prev_injuries + age + minutes_played, 
                  family = binomial(link = "logit"), 
                  data = players)

summary(injury_glm)

exp(coef(injury_glm))
exp(confint(injury_glm))

cat("Residual Deviance:", deviance(injury_glm), "on", injury_glm$df.residual, "degrees of freedom\n")

plot(fitted(injury_glm), rstandard(injury_glm),
     xlab = "Fitted values",
     ylab = "Standardised Deviance Residuals",
     main = "Diagnostics: Residuals vs Fitted")
abline(h = 0, lty = 2)

qqnorm(rstandard(injury_glm))
qqline(rstandard(injury_glm))

png("outputs/injury_diagnostics/injury_diagnostics.png", width = 800, height = 600)
plot(fitted(injury_glm), rstandard(injury_glm),
     xlab = "Fitted values", ylab = "Standardised Deviance Residuals")
abline(h = 0, lty = 2)
dev.off()

