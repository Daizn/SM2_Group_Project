ashford_matches <- read.csv('ashford_matches.csv', header = T)
league_players <- read.csv('league_players.csv', header = T)
# find correlation between goals scored/ conceded
#importing info for convinience
if FALSE {
age <- league_players$age
minutes_played <-league_players$minutes_played
training_load <- league_players$training_load
previous_injuries <- league_players$prev_injuries
bmi <- league_players$bmi
injured <- league_players$injured
}
goals_scored <- ashford_matches$goals_scored
goals_conceded <- ashford_matches$goals_conceded
plot(goals_scored~goals_conceded)
goals_fit <- lm(goals_scored~goals_conceded)
summary(goals_fit)
student_r <- rstudent(goals_fit)
plot(goals_fit$fitted, rstudent(goals_fit))
qqnorm(rstudent(goals_fit))
qqline(rstudent(goals_fit))
max(abs(rstudent(goals_fit)))
outliers <- which(abs(rstudent(goals_fit))>3)
plot(student_r)
text(outliers, student_r[outliers], labels = outliers, pos = 1 , col = 'red')
# observation 8 is an outlier as it exceeds 3 standard deviations