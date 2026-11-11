ashford_matches <- read.csv("ashford_matches.csv")

goals_scored <- ashford_matches$goals_scored
opp_rank <- ashford_matches$opponent_rank

model <- lm(goals_scored ~ opp_rank)

plot(goals_scored ~ opp_rank, ylab = "Amount of Goals Scored", xlab = "Opposition Rank")
#plotted goals scored in the game aginst the rank of the opponent to look for any correlation, doesn't look like there is any

qqnorm(residuals(model), main = "QQ Plot", xlab = "", ylab = "")
qqline(residuals(model), col = "red")
#plotted QQ plot to check normality assumption
#majority of points seem to fall on qqline, assumption fine

std_residuals <- rstandard(model)
fitted_vals <- fitted(model)
plot(fitted_vals, std_residuals, xlab = "Fitted Values", ylab = "Standardised Residuals")
#standardised residuals vs fitted values plot to check linearity and constant variance assumption
#the plot doesnt show a random scatter which means the assumptions aren't fine.
