# Baysian model fitting resources-----------------------------------------------
# https://osf.io/vxw73/
# https://osf.io/tasu5
# https://github.com/paul-buerkner/brms/blob/master/R/distributions.R
# https://github.com/lottybrand/GH_Kernow (this is for Brand et al., 2019)

# Bayesian Ordinal Regression 'peoples value of coyotes' e.g. 'value'
library("devtools")
devtools::install_github("paul-buerkner/brms")
install.packages("brms")
library("brms")
library("loo")

## Additive model---------------------------------------------------------------
# '(1|value_question) + (1|ID)' allows for the incorporation of variation amongst 
# people and questions in the Likert scale used to measure 'value'

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
hist(bay.mod1.1$fit@sim$samples[[1]]$b_age)


## Models with interactions-----------------------------------------------------

bay.mod1.2 <- brm(
  formula = value_rate ~ 1 + age+gender+county+image.relation.nature*fear+knowledge.score+
    animal.incident+animal.owner + (1|value_question)+(1|ID),
  data = coyote_bayes, 
  family = cumulative("probit"),
  chains = 1,
  iter = 10000,
  inits = "0",
)
summary(bay.mod1.2)
save(bay.mod1.2, file="bay.mod1.2")
hist(bay.mod1.2$fit@sim$samples[[1]]$b_age)

bay.mod1.3 <- brm(
  formula = value_rate ~ 1 + age+county+gender+image.relation.nature*knowledge.score+fear+
    animal.owner+animal.incident + (1|value_question)+(1|ID),
  data = coyote_bayes, 
  family = cumulative("probit"),
  chains = 1,
  iter = 10000,
  inits = "0",
)
summary(bay.mod1.3)
save(bay.mod1.3, file="bay.mod1.3")

bay.mod1.4 <- brm(
  formula = value_rate ~ 1 + age+county+gender+image.relation.nature+knowledge.score+
    fear*animal.incident+animal.owner + (1|value_question)+(1|ID),
  data = coyote_bayes, 
  family = cumulative("probit"),
  chains = 1,
  iter = 10000,
  inits = "0",
)
summary(bay.mod1.4)
save(bay.mod1.4, file="bay.mod1.4")

# Let's compare models now------------------------------------------------------
model_comp = loo(bay.mod1.1, bay.mod1.2, bay.mod1.3, bay.mod1.4)
model_comp$diffs
model_comp$loos



