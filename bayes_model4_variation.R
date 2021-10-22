# Libraries
library("devtools")
devtools::install_github("paul-buerkner/brms")
library("brms")

# Bayesian Ordinal Regression Model 4: Predicting coyote incidence by 
# participant characteristics

bay.mod4.1 <- brm(
  formula = fq.aggressive ~ 1 + county+image.relation.nature+fear+knowledge.score+
    animal.owner,
  data = coyote, 
  family = cumulative("probit"),
  chains = 3,
  iter = 2000,
  inits = "0",
)

summary(bay.mod4.1)

bay.mod4.2 <- brm(
  formula = fq.aggressive ~ 1 + county+image.relation.nature+fear*knowledge.score+
    animal.owner,
  data = coyote, 
  family = cumulative("probit"),
  chains = 3,
  iter = 2000,
  inits = "0",
)
summary(bay.mod4.2)
# Not sure how to interpret this interaction

bay.mod4.3 <- brm(
  formula = fq.aggressive ~ 1 + county+image.relation.nature*fear+knowledge.score+
    animal.owner,
  data = coyote, 
  family = cumulative("probit"),
  chains = 3,
  iter = 2000,
  inits = "0",
)
# Not sure how to interpret this interaction

summary(bay.mod4.3)

bay.mod4.4 <- brm(
  formula = fq.aggressive ~ 1 + county+image.relation.nature*knowledge.score+fear+
    animal.owner,
  data = coyote, 
  family = cumulative("probit"),
  chains = 3,
  iter = 2000,
  inits = "0",
)

summary(bay.mod4.4)

bay.mod4.5 <- brm(
  formula = fq.aggressive ~ 1 + county+image.relation.nature+knowledge.score+fear*
    animal.owner,
  data = coyote, 
  family = cumulative("probit"),
  chains = 3,
  iter = 2000,
  inits = "0",
)
summary(bay.mod4.5)


initials = function() list("")

bay.mod4.6 <- brm(
  formula = fq.aggressive ~ 1 + county+fear+knowledge.score+image.relation.nature*
    animal.owner,
  data = coyote, 
  family = cumulative("probit"),
  chains = 3,
  iter = 2000,
  inits = "0",
)

summary(bay.mod4.6)
# Warning message:
# Parts of the model have not converged (some Rhats are > 1.05). Be careful when analysing the results! 
# We recommend running more iterations and/or setting stronger priors.

# we will not consider this interaction important as we think their intercept are not greatly differetn
model_comp = loo(bay.mod4.1, bay.mod4.4, bay.mod4.5)
model_comp$loos
model_comp$diffs
# Model 4.5 is the best, this model considers an interaction btw fear and animal owner


