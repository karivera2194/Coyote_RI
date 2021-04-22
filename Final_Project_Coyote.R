## Coyote Survey ##

# Clean Up ---------------------------------------------------------------------
rm(list=ls())
gc() #garbage collection

# Libraries---------------------------------------------------------------------
library(tidyverse) # data manipulation (includes ggplot and forcats)
library(lubridate) # dates 
library(dplyr)
library(psych)
library(janitor)
library(corrr)
#library(MASS)

# Read in data------------------------------------------------------------------
setwd("C:/Users/Kim Rivera/Documents/Coyotes/Questionnaire Data/live survey/r_data")
coyote_raw = read_csv("Coyote Questionnaire_final_dataset.csv") # OG survey data

# Organize data-----------------------------------------------------------------
coyote <- coyote_raw %>% mutate(date = round_date(mdy_hm(`End Date`))) %>% 
  select(-c(1,2,4,7,8,10:13, 16, 17, 158:162)) 
coyote<- coyote[-c(1:12),] # 'Official' survey started Oct 6

## Rename columns---------------------------------------------------------------
names <- colnames(coyote)
colnames(coyote) <- c("response.type", "progress","time","ID","lat","lon","resident","years.residency","not.resident",
                      "not.resident.others","months.RI", "months.RI.text","county","town","town.text",
                      "street","street.text","outdoor","outdoor.text","nature.person","image.relation.nature",
                      "important.ecosystem","beneficial.humans", "beneficial.wildlife","important.hunters","risk.humans","risk.pets",
                      "negative.wildlife","diseases","reaction","reaction.text","fearful","problem.when",
                      "problem.when.text","coyote.population","all.removed","problem.removed","problem.relocated",
                      "all.sterilized","problem.sterilized","preventive.measures","more.research",
                      "education.program","footholds","native","ri.past.10.years","litter.food",
                      "nocturnal","carnivores","10.human.rabies.year","furbearers","NAMWC","place.coyotes",
                      "place.should","fq.hear","fq.see","fq.see.pups","fq.see.roadkill","fq.see.scat",
                      "fq.see.neighbour","fq.see.eat", "fq.aggressive","fq.fed","pet.owner","pet","pet.text","risk.outdoor.cat",
                      "encounter.outdoor.cat","attack.outdoor.cat","missing.outdoor.cat","kill.outdoor.cat",
                      "risk.large.dog","encounter.large.dog","attack.large.dog","missing.large.dog",
                      "kill.large.dog","risk.small.dog","encounter.small.dog","attack.small.dog",
                      "missing.small.dog","kill.small.dog","risk.pet","encounter.pet","attack.pet",
                      "missing.pet","kill.pet","livestock.owner","livestock","livestock.text",
                      "risk.goat","encounter.goat","attack.goat","missing.goat","kill.goat",
                      "risk.sheep","encounter.sheep","attack.sheep","missing.sheep","kill.sheep",
                      "risk.poultry","encounter.poultry","attack.poultry","missing.poultry","kill.poultry",
                      "risk.pig","encounter.pig","attack.pig","missing.pig","kill.pig","risk.cattle",
                      "encounter.cattle","attack.cattle","missing.cattle","kill.cattle","risk.other",
                      "encounter.other","attack.other","missing.other","kill.other",
                      "protect.from.coyote","measures","measures.text","take.care","not.to.worry",
                      "human.ingenuity","plenty.resources","equal.rights","laws.of.nature","humans.rule","delicate.balance",
                      "envt.cata","age","ethnicity","ethnicity.text","education","household.income",
                      "gender","share","seek.info","contact.envt.mgt.nat.resources","contact.police","contact.uri",
                      "contact.wildlife.center","contact.nature.center","contact.animal.control",
                      "contact.envt.mgt.law.enforce","date")

# Convert all characters to factors, can change back to numeric, etc later if needed
coyote = coyote %>% mutate_if(sapply(coyote, is.character), as.factor)

# create new column which ID's unusable data, 0 = good data, 1's = flagged 
# start with survey.type and flag 'Survey Preview' and 'Spam'
coyote = coyote %>% mutate(flagged = ifelse(response.type == "Spam", 1,
                                            ifelse(response.type == "Survey Preview", 1, 0)))
# Now remove these from our cleaned dataset
coyote = filter(coyote, flagged != 1)


# Project Goal: What independent variables impact the relationship (positive or negative) with coyotes in Rhode Island?

# Hypothesis: 
# 1) Older individuals will have more negative view of coyotes
# 2) Individuals with more education will have a more positive view of coyotes
# 3) Pet owners with pet-coyote interactions will have a more negative view of coyotes
# 4) Livestock owners with livestock-coyote interactions will have a more negative view of coyotes

## Age --------------------------------------------------------------------------
## Check Out Age Distribution
ggplot(coyote, aes(x=age)) + geom_histogram(fill ='yellow4', color = 'yellow4') +
  labs(x = "Age", y = "Counts", title = "Participant Age Distribution") +
  theme_minimal() +
  theme(legend.position="none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(family = "LM Roman 10 Bold", size=10))
  

# Hmm, I believe it is unlikely a participant is over 110 y/o. Either this was entered wrong or perhaps the 
# participant doesn't want to share their age, or did not take this survey seriously. Let's look at the data 
# point and investigate other responses. 
view = coyote %>% filter(age > 100) # it looks like other data is legitimate, we will therefor change age to NA
coyote <- coyote %>% mutate(age = na_if(age, 120))

## Gender-----------------------------------------------------------------------
levels(coyote$gender)

# Let's convert unusable entries to NA
coyote <- coyote %>% mutate(gender = recode_factor(gender, "Apache attack helicopter" = NA_character_)) %>% 
  mutate(gender = recode_factor(gender, "mmm, no." = NA_character_)) %>% 
  mutate(gender = recode_factor(gender, "I don't believe in gender" = NA_character_)) %>%
  mutate(gender = recode_factor(gender, "Bi curious Buffalo." = NA_character_))

# collapse and correct level labels
coyote <- coyote %>% mutate(gender = fct_collapse(gender, 
                                                  male = c("m", "M", "Male", "boy", "MALE","Make", "Dude",
                                                           "male.  However it is interesting that the previous question offered selections but this question did not.",
                                                           "Man", "masculine", "male"),
                                                  female = c("Femail", "More or less female", "femal", "woman", "Woman", "F", "f", "Female", "female", "is that still a thing? female"),
                                                  non.binary = c("Female, non-binary/fluid" ,"non binary", "Genderqueer/Nonbinary", "Non-Binary","Non-binary","non-binary")))

## Check Out Distribution
ggplot(coyote, aes(x= age, fill = gender)) + geom_bar() +
  labs(title = 'Participant Gender Distribution by Age')+
  scale_x_continuous(breaks = seq(10, 100, by = 5))+
theme_minimal()
   

## Education--------------------------------------------------------------------
levels(coyote$education)
# relevel education
coyote <- coyote %>% mutate(education = factor(education)) %>%
  mutate(education = fct_relevel(education, c("Elementary School", "Middle School","High School",  "Technical or vocational degree", "2-year college degree or certificate", "4-year college Degree", "Graduate Degree")))

ggplot(coyote, aes(education, fill = education)) +
  geom_bar() +
  labs(x = "Education", y = "Counts", title = "Level of Education in RI") + 
  theme_minimal() +
  theme(legend.position="none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(family = "LM Roman 10 Bold", size=15)) 

## Household Income-------------------------------------------------------------
# relevel income
coyote <- coyote %>% mutate(household.income = factor(household.income)) %>%
  mutate(household.income = fct_relevel(household.income, c("Under $15,000", "Between $15,000 and $29,999","Between $30,000 and $49,999", "Between $50,000 and $74,999","Between $75,000 and $99,999","Between $100,000 and $149,999","Between $150,000 and $199,999", "$200,000 or more" )))

ggplot(coyote, aes(household.income, fill = household.income)) +
  geom_bar() +
  labs(x = "Household Income", y = "Counts", title = "Household Income RI") + 
  theme_minimal() +
  theme(legend.position="none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(family = "LM Roman 10 Bold", size=15)) 

# Let's see if income is related to education
ggplot(coyote, aes(household.income, fill = education)) +
  geom_bar() +
  labs(x = "Household Income", y = "Counts", title = "Education by Household Income") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(family = "LM Roman 10 Bold", size=15)) 

# County--------------------------------------------------------------------------------
levels(factor(coyote$county))

coyote <- mutate(coyote,county = replace(county, county=="I do not wish to share this information","Not_shared"))

# get the levels ordered by counts 
levels <- coyote %>% 
  group_by(county) %>% 
  summarize(count=n()) %>% 
  arrange(count) %>% 
  select(county) %>% 
  as_vector()

coyote %>% 
  mutate(county = factor(county, levels = levels)) %>% 
  ggplot(aes(x = county, fill = county)) + 
  geom_bar() + 
  labs(x = "County", y = "Counts", title = "Counts per county") + 
  theme_minimal() + 
  theme(legend.position="none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(family = "LM Roman 10 Bold", size=15)) 

# Let's see if county is related to education
ggplot(coyote, aes(county, fill = education)) +
  geom_bar() +
  labs(x = "County", y = "Counts", title = "Education by County") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(family = "LM Roman 10 Bold", size=15)) 


## Pet Demographics--------------------------------------------------------------

coyote <- coyote %>% 
  mutate(pet.owner = ifelse(str_detect(pet.owner,"(?i)yes"),1,0))
         
# correct non-informative levels to NAs
coyote <- coyote %>% mutate(pet = recode_factor(pet, "Tamed Coyote" = NA_character_)) %>% 
  mutate(gender = recode_factor(pet, "Two" = NA_character_)) %>% 
  mutate(gender = recode_factor(pet, "I have 9 indoor pets of various sizes and species" = NA_character_))%>% 
  mutate(gender = recode_factor(pet, "All indoors and never unattended" = NA_character_))

# Make selection wider data
coyote <- coyote %>% 
  mutate(pet.outdoor.cat = ifelse(str_detect(pet,"(?i)Outdoor.*Cat"),1,0),
         pet.small.dog = ifelse(str_detect(pet,"(?i)Small.*dog"),1,0),
         pet.lrg.dog = ifelse(str_detect(pet,"Large.*dog"),1,0))

# Change data from factor to character for mutate and case_when
coyote <- coyote %>% 
  mutate(pet = as.character(pet)) 
class(coyote$pet)

coyote <- coyote %>% 
  mutate(pet.text = as.character(pet.text)) 
class(coyote$pet.text)

# categorize the input text from participants
coyote <- coyote %>% 
  mutate(pet.text = case_when(pet.text %in% c("2 cats","cats (indoor only)",
                                              "i am active in cat rescue and have previously had outdoor feral cats. I moved them indoors.", 
                                              "Lost a cat to coyotes now indoor only") ~ "indoor cat",
                              pet.text %in% c("2 dogs","my dog is 45#, which I consider medium sized.") ~ "large dogs",
                              pet.text %in% c("3cats, 1. Corgi") ~ "outdoor cat, Corgi",
                              pet.text %in% c("cat goes out only if I am in yard") | str_detect(pet.text,"(?i)indoor/outdoor|cat.*night") ~ "int/out cat",
                              TRUE ~ pet.text)) %>% 
  mutate(pet.indoor.cat = ifelse(str_detect(pet.text,"(?i)indoor.*cat|house.*cat|door.*cat|indoor.*animal|inside.*cat"),1,0),
         pet.outdoor.cat = replace(pet.outdoor.cat,str_detect(pet.text,"(?i)outdoor.*cat|stray.*cat|out.*cat"),1),
         pet.small.dog = replace(pet.small.dog,str_detect(pet.text,"(?i)Corgi|small.*dog"),1),
         pet.lrg.dog = replace(pet.lrg.dog,str_detect(pet.text,"(?i)lab|Medium-sized.*dog|lbdog|Border|large.*dog"),1),
         pet.bird = ifelse(str_detect(pet.text, "(?i)bird|macaw|parrot|cockatoo"),1,0),
         pet.rabbit = ifelse(str_detect(pet.text, "(?i)\\brabbit\\b|house.*rabbit|bunny|indoor.*rabbits"),1,0),
         pet.reptile = ifelse(str_detect(pet.text, "(?i)reptile|snake|herp|lizard|tortoise|turtle"),1,0),
         pet.ferret = ifelse(str_detect(pet.text, "(?i)ferret"),1,0), 
         pet.rodent = ifelse(str_detect(pet.text, "(?i)gerbil|guinea.*pig|hamter"),1,0)) 

# Lets check out population of pet owners
levels <- coyote %>% 
  pivot_longer(pet.outdoor.cat:pet.rodent, names_to="index",values_to="value") %>% 
  filter(value == 1) %>% 
  group_by(index) %>% 
  summarize(count=n()) %>% 
  arrange(count) %>% 
  select(index) %>% 
  as_vector()

# data with including others answers 
coyote %>% 
  pivot_longer(pet.outdoor.cat:pet.rodent, values_to="value",names_to="index") %>%
  filter(value==1) %>% 
  mutate(index = factor(index, levels=levels)) %>% 
  ggplot(aes(x = index, fill = index)) + 
  geom_bar() + 
  labs(x = "Pets", y = "Counts", title = "Pet ownership") + 
  theme_minimal() + 
  theme(legend.position="none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(family = "LM Roman 10 Bold", size=15))

## Pet-coyote interactions------------------------------------------------------
# Create new column for having had any pet-coyote interaction (only focus on outdoor cats and dogs for now)
pet.int = coyote %>%
  select(encounter.outdoor.cat:kill.small.dog ) %>% # select all columns about pet interactions with coyotes
  select(-contains("risk")) %>%
  mutate_all(~case_when(. == "Yes" ~ 1,
                        . == "No" ~ 0)) %>% # change yes/no for 1/0
  add_column(pet.int = rowSums(., na.rm=TRUE)) %>% # sum all the interactions 
  mutate(pet.int = ifelse(pet.int > 0,"Yes","No")) # # if more than one interaction, assign Yes 

## Add the column to the dataset 
coyote$pet.int <- pet.int$pet.int 
coyote <- coyote %>% 
  mutate(pet.int = replace(pet.int, pet.owner == "No", NA))
# Change pet.int from yes/no, to 1/0
coyote = coyote %>%
  mutate(pet.int = case_when(
    pet.int == "Yes" ~ 1,
    pet.int == "No" ~ 0))

## Livestock Demographics-------------------------------------------------------

# Make selection wider data
coyote <- coyote %>% 
  mutate(livestock.cattle = ifelse(str_detect(livestock,"(?i)cattle"),1,0),
         livestock.goat = ifelse(str_detect(livestock,"(?i)goat"),1,0),
         livestock.poultry = ifelse(str_detect(livestock,"(?i)poultry"),1,0),
         livestock.pig = ifelse(str_detect(livestock,"(?i)pig"),1,0),
         livestock.sheep = ifelse(str_detect(livestock,"(?i)sheep"),1,0))

coyote <- coyote %>% 
  mutate(livestock.bee = ifelse(str_detect(livestock.text, "(?i)bees"),1,0),
         livestock.camelid = ifelse(str_detect(livestock.text, "(?i)llama|alpaca"),1,0),
         livestock.poultry = replace(livestock.poultry,str_detect(livestock.text,"(?i)peacock|hen"),1),
         livestock.equine = ifelse(str_detect(livestock.text,"(?i)horse|donkey"),1,0),
         livestock.cattle = replace(livestock.cattle,str_detect(livestock.text,"(?i)cattle|goat"),1),
         livestock.shellfish = ifelse(str_detect(livestock.text, "(?i)quoahog"),1,0),
         livestock.rabbit = ifelse(str_detect(livestock.text, "(?i)outdoor.*rabbits|rabbits"),1,0))

# counts by levels 
levels <- coyote %>% 
  pivot_longer(livestock.cattle:livestock.rabbit, names_to="index",values_to="value") %>% 
  filter(value == 1) %>% 
  group_by(index) %>% 
  summarize(count=n()) %>% 
  arrange(count) %>% 
  select(index) %>% 
  as_vector()

# all data 
coyote %>% 
  pivot_longer(livestock.cattle:livestock.rabbit, names_to="index",values_to="value") %>% 
  mutate(index = factor(index, levels = levels)) %>% 
  filter(value == 1) %>% 
  ggplot(aes(x = index, fill = index)) + 
  geom_bar() + 
  labs(x = "Livestock Ownership", y = "Counts", title = "Livestock Ownership") + 
  theme_minimal() + 
  theme(legend.position="none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(family = "LM Roman 10 Bold", size=15)) 

## Livestock-coyote interaction-------------------------------------------------
## Create new column for having had any pet-coyote interaction (only focus on outdoor cats and dogs for now)
livestock.int = coyote %>%
  select(encounter.goat:kill.cattle) %>% # select all columns about livestock interactions with coyotes
  select(-contains("risk")) %>%
  mutate_all(~case_when(. == "Yes" ~ 1,
                        . == "No" ~ 0)) %>% # change yes/no for 1/0
  add_column(livestock.int = rowSums(., na.rm=TRUE)) %>% # sum all the interactions 
  mutate(livestock.int = ifelse(livestock.int > 0,"Yes","No")) # # if more than one interaction, assign Yes 

## Add the column to the dataset 
coyote$livestock.int <- livestock.int$livestock.int 
coyote <- coyote %>% 
  mutate(livestock.int = replace(livestock.int, pet.owner == "No", NA))
# Change livestock.int from yes/no, to 1/0
coyote = coyote %>%
  mutate(livestock.int = case_when(
    livestock.int == "Yes" ~ 1,
    livestock.int == "No" ~ 0))

# Lets focus on participants thoughts on coyotes, e.g. ranking of benefits and risks---------------------------------
# Starting with benefits strong disagree [1] -> strong agree [5]
coyote <- coyote  %>%
  mutate(important.ecosystem = fct_relevel(important.ecosystem, c("Strongly Disagree", "Disagree", "Neither agree nor disagree",  "Agree", "Strongly Agree")))
coyote <- coyote  %>%
  mutate(beneficial.humans = fct_relevel(beneficial.humans, c("Strongly Disagree", "Disagree", "Neither agree nor disagree",  "Agree", "Strongly Agree")))
coyote <- coyote  %>%
  mutate(beneficial.wildlife = fct_relevel(beneficial.wildlife, c("Strongly Disagree", "Disagree", "Neither agree nor disagree",  "Agree", "Strongly Agree")))
coyote <- coyote  %>%
  mutate(important.hunters = fct_relevel(important.hunters, c("Strongly Disagree", "Disagree", "Neither agree nor disagree",  "Agree", "Strongly Agree")))

# Now risks strong disagree [5] -> strongly agree [1]
coyote <- coyote  %>%
  mutate(risk.humans = fct_relevel(risk.humans, c("Strongly Agree", "Agree", "Neither agree nor disagree", "Disagree", "Strongly disagree")))
coyote <- coyote  %>%
  mutate(risk.pets = fct_relevel(risk.pets, c("Strongly Agree", "Agree", "Neither agree nor disagree",  "Disagree", "Strongly disagree")))
coyote <- coyote  %>%
  mutate(negative.wildlife = fct_relevel(negative.wildlife, c("Strongly Agree", "Agree", "Neither agree nor disagree",  "Disagree", "Strongly disagree")))
coyote <- coyote  %>%
  mutate(diseases = fct_relevel(diseases, c("Strongly Agree", "Agree", "Neither agree nor disagree",  "Disagree", "Strongly disagree")))

# Now we need to create a table of these columns for Cronbach's alpha test

coyote.attitude = coyote %>%
  select(c(important.ecosystem, beneficial.humans, beneficial.wildlife, important.hunters,
           risk.humans, risk.pets, negative.wildlife, diseases)) %>%
  mutate_at(c("important.ecosystem", "beneficial.humans", "beneficial.wildlife", "important.hunters",
              "risk.humans", "risk.pets", "negative.wildlife", "diseases"), ~unclass(.))

# This test will tell us how well items here correlate
# ideally want an alpha coefficient of 0.08 or higher for short lists of items 
psych::alpha(coyote.attitude)
# this gives us a raw alpha of .84, a .8 correlation is considered sufficient for short lists

# Let's look at our hypothesis and start by analyzing the ordinal level data, education
# Hyp 2) Individuals with more education will have a more positive view of coyotes

kendall = coyote %>% select(c(beneficial.humans, beneficial.wildlife, important.ecosystem, important.hunters, 
                              diseases, negative.wildlife, risk.humans, risk.pets, education)) %>%
  mutate_all(~unclass(.))


eco.ed = cor.test(kendall$important.ecosystem, kendall$education, method="kendall")
ben.hum.ed = cor.test(kendall$beneficial.humans, kendall$education, method="kendall")
ben.wild.ed = cor.test(kendall$beneficial.wildlife, kendall$education, method="kendall")
hunt.ed = cor.test(kendall$important.hunters, kendall$education, method="kendall")
dis.ed = cor.test(kendall$diseases, kendall$education, method="kendall")
neg.wild.ed = cor.test(kendall$negative.wildlife, kendall$education, method="kendall")
risk.hum.ed = cor.test(kendall$risk.humans, kendall$education, method="kendall")
risk.pet.ed = cor.test(kendall$risk.pets, kendall$education, method="kendall")

# Hyp 3) Pet owners pet-coyote interactions will have a more negative view of coyotes

eco.pet = cor.test(kendall$important.ecosystem, coyote$pet.int, method="kendall")
ben.hum.pet = cor.test(kendall$beneficial.humans, coyote$pet.int, method="kendall")
ben.wild.pet = cor.test(kendall$beneficial.wildlife, coyote$pet.int, method="kendall")
hunt.pet = cor.test(kendall$important.hunters, coyote$pet.int, method="kendall")
dis.pet = cor.test(kendall$diseases, coyote$pet.int, method="kendall")
neg.wild.pet = cor.test(kendall$negative.wildlife, coyote$pet.int, method="kendall")
risk.hum.pet = cor.test(kendall$risk.humans, coyote$pet.int, method="kendall")
risk.pet.pet = cor.test(kendall$risk.pets, coyote_2$pet.int, method="kendall")

# Hyp 4) Pet owners livestock-coyote interactions will have a more negative view of coyotes

eco.livestock = cor.test(kendall$important.ecosystem, coyote$livestock.int, method="kendall")
ben.hum.livestock = cor.test(kendall$beneficial.humans, coyote$livestock.int, method="kendall")
ben.wild.livestock = cor.test(kendall$beneficial.wildlife, coyote$livestock.int, method="kendall")
hunt.livestock = cor.test(kendall$important.hunters, coyote$livestock.int, method="kendall")
dis.livestock = cor.test(kendall$diseases, coyote$livestock.int, method="kendall")
neg.wild.livestock = cor.test(kendall$negative.wildlife, coyote$livestock.int, method="kendall")
risk.hum.livestock = cor.test(kendall$risk.humans, coyote$livestock.int, method="kendall")
risk.pet.livestock = cor.test(kendall$risk.pets, coyote$livestock.int, method="kendall")


# Ordinal Regression------------------------------------------------------------

kendall = kendall %>% rowwise() %>%
  mutate(coyote.attitude = mean(c(important.ecosystem, beneficial.humans, beneficial.wildlife, important.hunters)), na.rm = TRUE) %>%
  select(-na.rm)

coyote$coyote.attitude <- kendall$coyote.attitude 

coyote <- coyote %>%
  mutate(coyote.attitude = ordered(coyote.attitude, levels = c("1", "1.5", "2","2.5","3","3.5","4","4.5","5"))) 
  #        %>%
  # mutate_at(c("pet.owner","livestock.owner","reaction","gender","education","county","nature.person","animal.high.int"), factor)

library(MASS) #this masks 'select' from dplyr, to use dplyr use doplyr::select or unload MASS package
#str(coyote)
?polr
library(effects)

# What is ordinal regression?
# We use this when the dependent variable we want to predict is ordinal while the independent variables are ordinal or continuous.
# Ordinal regression analysis assumes a dependence or causal relaitonship btw 1 or more independent and one dependent variable.
# This can be used for 1) causal analysis 2) forecasting an effect OR 3) trend forecasting.
# 

# This model includes the variables of interest for our hypotheses
mod1 = polr(coyote.attitude ~ age + education + pet.int + livestock.int, data = coyote, method="logistic", Hess=TRUE)
AIC(mod1)
summary(mod1)
coef(mod1) #extract model coef 
ctable = coef(summary(mod1))
p = pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2  ## add the p-values to the table
ctable = cbind(ctable, "p value" = p)
ctable
(ci <- confint(mod1))
exp(cbind(odd_ratio = coef(mod1), ci))  ## porportional odds ratios (success/failure)
                                        ## example, success = .8, failure = .2; OR = .8/.2 = 4; odds of success are 4 to 1

# interpretting results,
# for each one unit increase in age, the odds of a participant being more likely to have a positive attitude 
# is multipled .988 times, holding other variables  

#Let's try variations of these variables and compare AIC's
mod2 = polr(coyote.attitude ~ education + pet.int + livestock.int, data = coyote, method="logistic", Hess=TRUE)
AIC(mod2)

mod3 = polr(coyote.attitude ~ age + pet.int + livestock.int, data = coyote, method="logistic", Hess=TRUE)
AIC(mod3)

mod4 = polr(coyote.attitude ~ pet.int + livestock.int, data = coyote, method="logistic", Hess=TRUE)
AIC(mod4)

AIC(mod1, mod2, mod3, mod4) #Looks like our best model, model 4, includes all of our independent variables of interest!

# We can also explore our hypothesis via plotting-------------------------------
# Histograms and barplots of variable values and ranking of attitude
coyote %>% filter(is.na(age)== FALSE) %>%
  ggplot(aes(x = age, y = coyote.attitude, fill = coyote.attitude))+
  geom_jitter(color = "darkgray", size = .3, alpha =.9)+
  geom_boxplot()

ggplot(coyote, aes(education, fill = coyote.attitude)) +
  geom_bar() +
  labs(x = "Education", y = "Counts", title = "Coyote Attitude by Education") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(family = "LM Roman 10 Bold", size=15))

ggplot(coyote, aes(pet.int, fill = coyote.attitude)) +
  geom_bar() +
  labs(x = "Pet Interaction", y = "Counts", title = "Coyote Attitude by Pet Interaction") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(family = "LM Roman 10 Bold", size=15))

ggplot(coyote, aes(livestock.int, fill = coyote.attitude)) +
  geom_bar() +
  labs(x = "Livestcok Interaction", y = "Counts", title = "Coyote Attitude by Livestock Interaction") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(family = "LM Roman 10 Bold", size=15))

ggplot(coyote, aes(x = coyote.attitude, y = age)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5) +
  facet_grid(livestock.int ~ pet.int, margins = TRUE) +
  theme(axis.text.x = element_text
        (angle = 45, hjust = 1, vjust = 1))

# Let's play around with a few more variables we measured
# mod6 = polr(coyote.attitude ~ age + gender + education + household.income, data = coyote, method="logistic", Hess=TRUE)
# AIC(mod6)
# 
# mod7 = polr(coyote.attitude ~ age + gender + education + household.income + pet.owner, data = coyote, method="logistic", Hess=TRUE)
# AIC(mod7)
#  
# e.out = Effect(focal.predictors = c("age", "pet.int"), mod1)
# 
# plot(e.out, rug = FALSE)
