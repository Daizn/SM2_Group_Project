# Analysis: Goals scored vs goals conceded
# Author: Daniel Hasani
# Purpose: Explore the relationship between goals scored
# and goals conceded and investigate potential outliers.


ashford_matches <- read.csv('ashford_matches.csv', header = T)

goals_scored <- ashford_matches$goals_scored
goals_conceded <- ashford_matches$goals_conceded
#Scatterplot
plot(goals_scored~goals_conceded, main = 'Scored vs Conceded', xlab = 'Conceded', ylab = 'Scored')
#Fit linear regression model
goals_fit <- lm(goals_scored~goals_conceded)
summary(goals_fit)
#Studentised residuals vs fitted values
student_r <- rstudent(goals_fit)

plot(goals_fit$fitted, rstudent(goals_fit), main = 'Studentised Residuals vs Fitted values', xlab = 'Fitted Values', ylab = 'Studentised Residuals')
abline(h = 0, lty = 2)
#Q-Q plot
qqnorm(student_r)
qqline(student_r)
#Potential outliers
outliers <- which(abs(student_r)>3)
plot(student_r, main = 'Studentised Residuals by Observation', xlab = 'Observation Number', ylab = 'Studentised Residual')
abline(h = c(-3,3), col = 'red', lty = 2)
text(outliers, student_r[outliers], labels = outliers, pos = 1 , col = 'red')

#Goals scored had a weak, non-significant negative relationship with goals conceded (p = 0.26, R-squared = 0.03) 
#Match 8 was an outlier as Ashford scored 8 goals and conceded none