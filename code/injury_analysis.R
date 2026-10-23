players <- read.csv("data/league_players.csv")

table(players$injured)

injury_glm <- glm(injured ~ training_load + prev_injuries + age + minutes_played, family = binomial(link = "logit"), data = players)

summary(injury_glm)

exp(coef(injury_glm))
exp(confint(injury_glm))

plot(fitted(injury_glm), residuals(injury_glm, type = "deviance"))
abline(h = 0, lty = 2)

qqnorm(residuals(injury_glm, type = "deviance"))
qqline(residuals(injury_glm, type = "deviance"))
