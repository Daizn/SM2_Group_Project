ashford <- read.csv("data/ashford_matches.csv")
ashford$win <- as.numeric(ashford$goals_scored > ashford$goals_conceded)
ashford$home_away <- as.factor(ashford$home_away)

ashford.glm <- glm(win ~ opponent_rank + home_away,family = binomial, data = ashford)
#additive model

plot(fitted(ashford.glm),rstandard(ashford.glm, type = "deviance"),xlab = "Fitted values",ylab = "Standardized deviance residuals",
     main = "St Deviance Residuals vs Fitted Values")
abline(h = 0, col = "red", lty = 2)


qqnorm(rstandard(ashford.glm, type = "deviance"),
       main = "QQ-plot of standardized deviance residuals")
qqline(rstandard(ashford.glm, type = "deviance"), col = "red")
#the standardised deviance residuals do not closely follow a normal distribution


plot(cooks.distance(ashford.glm),type = "h",main = "Cook's Distances",xlab = "Observation",ylab = "Cook's Distance")
abline(h = 4/length(ashford.glm$fitted.values), col = "red", lty = 2)
#there is one potentailly influential observation that needs
#further investigation


plot(hatvalues(ashford.glm),rstandard(ashford.glm, type = "deviance"),xlab = "Leverage",ylab = "Standardized deviance residuals",main = "Residuals vs Leverage")
abline(h = c(-2, 0, 2), lty = 2)
abline(v = 2 * 3/length(ashford.glm$fitted.values),col = "red", lty = 2)
#red line doesnt appear because it is 0.13 which is beyond the graph on the right
#there is no evidence of unusually high leverage observations or large
#standardised residuals. however observation 27 may remain worth investigating
#due to our cooks distances plot


#POISSON REGRESSION FOR GOALS SCORED
#using logistic regressions before for win as our response variable
#made sense as the response is binary (win or loss), however this doesn't
#translate to a poisson regression so we will analyze goals scored
#as the response variable and keeping explantory variables mostly the same

ashford.poisson <- glm(goals_scored ~ opponent_rank + home_away,family = poisson, data = ashford)
summary(ashford.poisson)
#additive model with nothing else
#opponent_rank and home_away both statistically significnat at 5% with pvalues
#0.00507 and 0.01137 respectively and since they both have postive coeffecients 
#suggests that as the opponent rank increases (team gets worse) the expected number of goals scored increases
#also suggests that when they play at home, they score more
#residual devinace of 49.785 on 43 d.f

ashford.poisson.interaction <- glm(goals_scored ~ opponent_rank * home_away,family = poisson, data = ashford)
summary(ashford.poisson.interaction)
#model with interaction (betwwen opponent_rank and home_away obviously)
#interaction has pvalue 0.547 which is much greater than 0.05, not signifiacnt
#residual deviance of 49.423 on 42d.f which shows very little imporvement in model fit

drop1(ashford.poisson.interaction, test = "Chisq")
#insufficient evidence that the interaction improves our model
#therefore we should remove the interaction and retain our simpler additive model

1 - pchisq(deviance(ashford.poisson),df.residual(ashford.poisson))
#goodness of fit test
#pvalue of 0.221428 means we fail to reject our null hypothesis
#that the additive poission model proves an adequate fit to the data


plot(fitted(ashford.poisson),rstandard(ashford.poisson, type = "deviance"),xlab = "Fitted values",ylab = "Standardized deviance residuals",main = "St Deviance Residuals vs Fitted Values")
abline(h = 0, col = "red", lty = 2)
#no smooth curved pattern suggesting a missing polynomial term

qqnorm(rstandard(ashford.poisson, type = "deviance"),main = "QQ-plot of standardized deviance residuals")
qqline(rstandard(ashford.poisson, type = "deviance"), col = "red")
#most plots seem to lie on the plot

plot(cooks.distance(ashford.poisson),type = "h",main = "Cook's Distances",xlab = "Observation",ylab = "Cook's Distance")
abline(h = 4/length(ashford.poisson$fitted.values),col = "red", lty = 2)
#observation 8 is potentially highly influential and could have an effect on our poisson 
#regression model

plot(hatvalues(ashford.poisson),rstandard(ashford.poisson, type = "deviance"),xlab = "Leverage",ylab = "Standardized deviance residuals",main = "Residuals vs Leverage")
abline(h = c(-2, 0, 2), lty = 2)
abline(v = 2 * 3/length(ashford.poisson$fitted.values),col = "red", lty = 2)
#there is one observation that has both unusually high leverage
#and larg residual, which requires further investigation about whether its affecting our results

#we will investigate this influential observation:

ashford[8, ]#looking at what happened in this observaion/game
ashford.poisson.no8 <- glm(goals_scored ~ opponent_rank + home_away,family = poisson, data = ashford[-8, ])
summary(ashford.poisson.no8)
#our model without observation 8
#removing the match changes both explantory variables to insignificant
#at 5% level, which suggests that our orignial conclusions are senitive to an unusually high scoring
#match, but we cant simply get rid of an observation just because it reduces our residual deviance.