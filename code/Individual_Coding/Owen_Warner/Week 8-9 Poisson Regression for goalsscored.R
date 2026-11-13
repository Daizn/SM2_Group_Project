ashford <- read.csv("data/ashford_matches.csv")
head(ashford)
ashford$home_away <- as.factor(ashford$home_away)

intercept <- glm(goals_scored ~ 1, family = poisson, data = ashford)
#intercept only
additive <- glm(goals_scored ~ opponent_rank + home_away, family = poisson, data = ashford)
#additive model
anova(intercept, additive, test = "Chisq")
#comparing the models
#pvalue of 0.000537 shows that additive the model is not signficantly worse

drop1(additive, test = "Chisq")
#both variables are signifiant

interaction <- glm(goals_scored ~ opponent_rank * home_away,family = poisson, data = ashford)
anova(additive, interaction, test = "Chisq")
#pvalue of 0.5479 shows the interaction is not statistically significant

deviance(additive)
df.residual(additive)
pchisq(deviance(additive), df = df.residual(additive),lower.tail = FALSE)
#our p value of 0.221428 suggest we do not have signifiacnt evidnce of a lack of fit
summary(additive)


deviance(additive) / df.residual(additive)
#1.157779 is pretty close to 1 so no obvious indcation of overdispersion and suports use of poission model

predict(additive),newdata = data.frame(opponent_rank = 20,home_away = "Away"),type = "response")
#expect 1.207021 goals against a team ranked 20th away
predict(additive,newdata = data.frame(opponent_rank = 20,home_away = "Home"),type = "response")
#double our expected goals for when we play away against the same team as we expect
#as our coeffecient for home/away variable is 0.69315, so exp(0.69315) is approx 2

AIC(additive, interaction)
#the additive model has a lower AIC meaning it provides a better balance between goodness of fit and model complexity
#this is a good statistical justification for retaining the additive model