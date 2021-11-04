# Baysian model fitting resources-----------------------------------------------
# https://osf.io/vxw73/
# https://osf.io/tasu5
# https://github.com/paul-buerkner/brms/blob/master/R/distributions.R
# https://github.com/lottybrand/GH_Kernow (this is for Brand et al., 2019)

# Bayesian Ordinal Regression 'frequency of coyote incidents' e.g. 'fq.aggressive'
library("devtools")
devtools::install_github("paul-buerkner/brms")
install.packages("brms")
library("brms")
library("loo")

## Additive model---------------------------------------------------------------

bay.mod3.1 <- brm(
  formula = fq.aggressive ~ 1 + county+image.relation.nature+fear+knowledge.score+
    animal.owner,
  data = coyote, 
  family = cumulative("probit"),
  chains = 3,
  iter = 2000,
  inits = "0",
)

summary(bay.mod3.1)
save(bay.mod3.1, file="bay.mod3.1")
hist(bay.mod3.1$fit@sim$samples[[1]]$b_age)

## Model with interactions------------------------------------------------------
bay.mod3.2 <- brm(
  formula = fq.aggressive ~ 1 + county+image.relation.nature*knowledge.score+fear+
    animal.owner,
  data = coyote, 
  family = cumulative("probit"),
  chains = 3,
  iter = 2000,
  inits = "0",
)

summary(bay.mod3.2)
save(bay.mod3.2, file="bay.mod3.2")
hist(bay.mod3.2$fit@sim$samples[[1]]$b_age)

bay.mod3.3 <- brm(
  formula = fq.aggressive ~ 1 + county+image.relation.nature+knowledge.score+fear*
    animal.owner,
  data = coyote, 
  family = cumulative("probit"),
  chains = 3,
  iter = 2000,
  inits = "0",
)
summary(bay.mod3.3)
save(bay.mod3.3, file="bay.mod3.3")
hist(bay.mod3.3$fit@sim$samples[[1]]$b_age)

# Let's compare models now------------------------------------------------------
model_comp = loo(bay.mod3.1, bay.mod3.2, bay.mod3.3)
model_comp$loos
model_comp$diffs

