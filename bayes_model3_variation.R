# Libraries
library("devtools")
devtools::install_github("paul-buerkner/brms")
library("brms")

# Bayesian Ordinal Regression Model 3: Predicting coyote sightings by 
# participant characteristics

bay.mod3.1 <- brm(
  formula = fq.see ~ 1 + county+image.relation.nature+fear+knowledge.score+
    animal.owner,
  data = coyote, 
  family = cumulative("probit"),
  chains = 3,
  iter = 2000,
  inits = "0",
)

summary(bay.mod3.1)

bay.mod3.2 <- brm(
  formula = fq.see ~ 1 + county+image.relation.nature+fear*knowledge.score+
    animal.owner,
  data = coyote, 
  family = cumulative("probit"),
  chains = 3,
  iter = 2000,
  inits = "0",
)
summary(bay.mod3.2)
# this interaction doesn't make sense to me, drop

bay.mod3.3 <- brm(
  formula = fq.see ~ 1 + county+image.relation.nature*fear+knowledge.score+
    animal.owner,
  data = coyote, 
  family = cumulative("probit"),
  chains = 3,
  iter = 2000,
  inits = "0",
)

summary(bay.mod3.3)

bay.mod3.4 <- brm(
  formula = fq.see ~ 1 + county+image.relation.nature*knowledge.score+fear+
    animal.owner,
  data = coyote, 
  family = cumulative("probit"),
  chains = 3,
  iter = 2000,
  inits = "0",
)
summary(bay.mod3.4)
# Not sure this makes sense either

bay.mod3.5 <- brm(
  formula = fq.see ~ 1 + county+image.relation.nature+knowledge.score+fear*
    animal.owner,
  data = coyote, 
  family = cumulative("probit"),
  chains = 3,
  iter = 2000,
  inits = "0",
)
summary(bay.mod3.5)
# not sure I have a hypothesis for this, lets drop

bay.mod3.6 <- brm(
  formula = fq.see ~ 1 + county+fear+knowledge.score+image.relation.nature*
    animal.owner,
  data = coyote, 
  family = cumulative("probit"),
  chains = 3,
  iter = 2000,
  inits = "0",
)
summary(bay.mod3.6)
# not sure I have a hypothesis for this, lets drop

model_comp = loo(bay.mod3.1, bay.mod3.3, bay.mod3.4)
model_comp$loos
model_comp$diffs
# Model 3.1 is the best, this model does not consider any interactions
