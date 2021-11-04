# Baysian model fitting resources-----------------------------------------------
# https://osf.io/vxw73/
# https://osf.io/tasu5
# https://github.com/paul-buerkner/brms/blob/master/R/distributions.R
# https://github.com/lottybrand/GH_Kernow (this is for Brand et al., 2019)

# Bayesian Ordinal Regression 'frequency of seeing coyotes' e.g. 'fq.see'
library("devtools")
devtools::install_github("paul-buerkner/brms")
install.packages("brms")
library("brms")
library("loo")

## Additive model---------------------------------------------------------------

bay.mod2.1 <- brm(
  formula = fq.see ~ 1 + county+image.relation.nature+fear+knowledge.score+
    animal.owner,
  data = coyote, 
  family = cumulative("probit"),
  chains = 3,
  iter = 2000,
  inits = "0",
)

summary(bay.mod2.1)
save(bay.mod2.1, file="bay.mod2.1")
hist(bay.mod2.1$fit@sim$samples[[1]]$b_age)

## Models with interactions-----------------------------------------------------
bay.mod2.2 <- brm(
  formula = fq.see ~ 1 + county+image.relation.nature*fear+knowledge.score+
    animal.owner,
  data = coyote, 
  family = cumulative("probit"),
  chains = 3,
  iter = 2000,
  inits = "0",
)

summary(bay.mod2.2)
save(bay.mod2.2, file="bay.mod2.2")
hist(bay.mod2.2$fit@sim$samples[[1]]$b_age)


bay.mod2.3 <- brm(
  formula = fq.see ~ 1 + county+image.relation.nature*knowledge.score+fear+
    animal.owner,
  data = coyote, 
  family = cumulative("probit"),
  chains = 3,
  iter = 2000,
  inits = "0",
)
summary(bay.mod2.3)
save(bay.mod2.3, file="bay.mod2.3")
hist(bay.mod2.3$fit@sim$samples[[1]]$b_age)

# Let's compare models now------------------------------------------------------
model_comp = loo(bay.mod2.1, bay.mod2.2, bay.mod2.3)
model_comp$loos
model_comp$diffs

