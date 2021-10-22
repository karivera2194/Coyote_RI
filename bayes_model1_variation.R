# Bayesian Ordinal Regression Model 1
# This refers to our value model with dimension 1 from PCA results which considers all 'value' questions
library("devtools")
devtools::install_github("paul-buerkner/brms")
install.packages("brms")
library("brms")
library("loo")


bay.mod1.1 <- brm(
  formula = value_rate ~ 1 + age + county + gender + image.relation.nature + fear + 
    knowledge.score + animal.owner + animal.incident + (1|value_question) + (1|ID),
  data = coyote_bayes, 
  family = cumulative("probit"),
  chains = 1,
  iter = 10000,
  inits = "0",
)

summary(bay.mod1.1)
save(bay.mod1.1, file="bay.mod1.1")

coyote_bayes$age2 = scale(coyote_bayes$age)
  
bay.mod1.1 <- brm(
  formula = value_rate ~ 1 + age2 + county + gender + image.relation.nature + fear + 
    knowledge.score + animal.owner + animal.incident + (1|value_question) + (1|ID),
  data = coyote_bayes, 
  family = cumulative("probit"),
  chains = 1,
  iter = 1000,
  inits = "0",
)

hist(bay.mod1.10$fit@sim$samples[[1]]$b_age)
# Send to Brian

### Trials with interactions

bay.mod1.5 <- brm(
  formula = value_rate ~ 1 + age+gender+county+image.relation.nature*fear+knowledge.score+
    animal.incident+animal.owner + (1|value_question)+(1|ID),
  data = coyote_bayes, 
  family = cumulative("probit"),
  chains = 1,
  iter = 10000,
  inits = "0",
)
summary(bay.mod1.5)
save(bay.mod1.5, file="bay.mod1.5")


bay.mod1.6 <- brm(
  formula = value_rate ~ 1 + age+county+gender+image.relation.nature*knowledge.score+fear+
    animal.owner+animal.incident + (1|value_question)+(1|ID),
  data = coyote_bayes, 
  family = cumulative("probit"),
  chains = 1,
  iter = 10000,
  inits = "0",
)
summary(bay.mod1.6)
save(bay.mod1.6, file="bay.mod1.6")

bay.mod1.10 <- brm(
  formula = value_rate ~ 1 + age+county+gender+image.relation.nature+knowledge.score+
    fear*animal.incident+animal.owner + (1|value_question)+(1|ID),
  data = coyote_bayes, 
  family = cumulative("probit"),
  chains = 1,
  iter = 10000,
  inits = "0",
)
summary(bay.mod1.10)
save(bay.mod1.10, file="bay.mod1.10")

# Let's compare models now------------------------------------------------------
model_comp = loo(bay.mod1.1, bay.mod1.5, bay.mod1.6, bay.mod1.10)
model_comp$diffs

# Models perfromance = 
# Mod1.10, mod1.6, mod1.5, then mod1.1

summary(bay.mod1.10)
summary(bay.mod1.6) #large est. error
summary(bay.mod1.5) # Upper CI & lower CI huge diff
summary(bay.mod1.1) # Prob with Rhat?

# Warning messages:
#   1: Found 1 observations with a pareto_k > 0.7 in model 'bay.mod1.5'. It is recommended to set 
# 'moment_match = TRUE' in order to perform moment matching for problematic observations. 

model_comp = loo(bay.mod1.1, bay.mod1.5, bay.mod1.6, bay.mod1.10)
model_comp$loos
model_comp$diffs
#model_comp_match= loo_moment_match(bay.mod1.1, bay.mod1.5, bay.mod1.6, bay.mod1.10)
