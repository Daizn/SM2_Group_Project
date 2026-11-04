ashford <- read.csv("data/ashford_matches.csv")
ashford$win <- as.numeric(
  ashford$goals_scored > ashford$goals_conceded)

plot(ashford$opponent_rank, ashford$win,
     xlab = "Opponent rank",
     ylab = "Win (1 = Yes, 0 = No)",
     main = "Ashford match outcomes",
     pch = 16)
#simple plot for opponent rank against wether they win or not



#additive model:
ashford$home_away <- as.factor(ashford$home_away)
additive <- glm(win ~ opponent_rank + home_away,
            data = ashford,
            family = binomial)#simple additive model
summary(additive)
#residual deviance of 54.409 against 43 degrees of freedom
#suggest that under, a chi-squared test with 43 degrees of
#freedom leads us to reject null hypothesis that this reduced
#model is not suitable compared to saturated model

plot(fitted(additive), resid(additive, type = "deviance"),
     xlab = "Fitted",
     ylab = "Deviance Residual",
     main = "Additive model")

plot(fitted(additive), resid(additive, type = "pearson"),
     xlab = "Fitted",
     ylab = "Pearson Residual",
     main = "Additive model")
#both residual plots here are showing a 
#clear pattern when what we want is a random
#scatter, this might suggest that we need to try a 
#new distrubtuion or transformation.



#interaction model:
interaction <- glm(win ~ opponent_rank * home_away,
            data = ashford,
            family = binomial)
summary(interaction)
#residual deviance of 54.1 against 42 degrees of freedom
#same takeaway we made for the additive model
#that the reduced model is not suitable compared to the
#saturated model. But if we compare it to the additive
#model, the interaction model has a residual deviance
#which is lower, but not enough lower to lose a degree of freedom for

plot(fitted(interaction), resid(interaction, type = "deviance"),
     xlab = "Fitted",
     ylab = "Deviance Residual",
     main = "Interaction model")

plot(fitted(interaction), resid(interaction, type = "pearson"),
     xlab = "Fitted",
     ylab = "Pearson Residual",
     main = "Interaction model")
#again the residual vs fitted plots look
#the same and draw the same conclusions


#comparison:
anova(additive, interaction, test = "Chisq")
#p-value is way higher than 0.05
#insufficient evidence that the interaction
#improves the model fit


#quadratic model: 
#fitting this model because of residual plots
# quadratic model:
quadratic <- glm(win ~ poly(opponent_rank, 2) + home_away,
                 data = ashford,
                 family = binomial)
summary(quadratic)

plot(fitted(quadratic), resid(quadratic, type = "deviance"),
     xlab = "Fitted",
     ylab = "Deviance Residual",
     main = "Quadratic model")

plot(fitted(quadratic), resid(quadratic, type = "pearson"),
     xlab = "Fitted",
     ylab = "Pearson Residual",
     main = "Quadratic model")
#still see the same pattern in residual plot

# comparison:
anova(additive, quadratic, test = "Chisq")
#pvalue of 0.2917 still doesnt show sufficient
#evidence that the quadratic term improves the fit
#and no need to retain the quadratic term


# cubic model:
cubic <- glm(win ~ poly(opponent_rank, 3) + home_away,
             data = ashford,
             family = binomial)

summary(cubic)
anova(quadratic, cubic, test = "Chisq")
#we see a drop of 3 residual deviance for 1 
#degree of freedom but the p-value says there
#is not quite enough evidence to retain the cubic term

# cubic model residual plots:
par(mfrow = c(1, 2))

plot(fitted(cubic), resid(cubic, type = "deviance"),
     xlab = "Fitted",
     ylab = "Deviance Residual",
     main = "Cubic model")

plot(fitted(cubic), resid(cubic, type = "pearson"),
     xlab = "Fitted",
     ylab = "Pearson Residual",
     main = "Cubic model")

par(mfrow = c(1, 1))
#again we are seeing the same patterns for residual plots



# quartic model:
quartic <- glm(win ~ poly(opponent_rank, 4) + home_away,
               data = ashford,
               family = binomial)

summary(quartic)
# compare cubic and quartic models:
anova(cubic, quartic, test = "Chisq")


#Final Conclusion: polynomial terms were introduced
#to see if the relationship were non linear
#but the tests showed that there was no significant improvements
#at the 5% level. 