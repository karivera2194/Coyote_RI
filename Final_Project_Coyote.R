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
library(psych)

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
ggplot(coyote, aes(x=age)) +
  geom_histogram()

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
ggplot(coyote, aes(x= gender)) +
  geom_bar()

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

## Pet Demographics--------------------------------------------------------------
# Lets check out population of pet owners
ggplot(coyote, aes(x = pet.owner, color = pet)) +
  geom_bar(fill="white")

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

## Pet-coyote interaction
## Create new column for having had any pet-coyote interaction
pet.int = coyote %>%
  select(c(matches("risk.|encounter.|attack.|missing.|kill."))) %>%
  select(-c(1:2, 23:length(.)))

# Change pet.int from yes/no, to 1/0
pet.int = pet.int %>%
  mutate_all(~case_when(
    . == "Yes" ~ 1,
    . == "No" ~ 0))

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

## Create new column for having had any livestock-coyote interaction
livestock.int = coyote %>%
  select(matches("risk.|encounter.|attack.|missing.|kill.")) %>%
  select(-c(1:22))

##FIGURING OUT 
# coyote2 = coyote %>% select(matches("risk.|encounter.|attack.|missing.|kill.")) %>%
#   mutate_all(~case_when(
#     . == "Yes" ~ 1,
#     . == "No" ~ 0)) %>% 
#   mutate(livestock.int = ifelse(rowSums(., na.rm=TRUE) >= 1, 1, 0))


  ifelse(rowSums(livestock.int.perc, na.rm=TRUE) >= 1, 1, 0)) %>%
  select(livestock.int.perc)
  # if marches these then put 1 in column

# Change livestock.int from yes/no, to 1/0
livestock.int = livestock.int %>%
  mutate_all(~case_when(
    . == "Yes" ~ 1,
    . == "No" ~ 0))

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
# This test will tell us how well items here correlate
# ideally want an alpha coefficient of 0.08 or higher for short lists of items 

coyote.values = coyote %>%
  select(c(important.ecosystem, beneficial.humans, beneficial.wildlife, important.hunters,
           risk.humans, risk.pets, negative.wildlife, diseases)) %>%
  mutate_at(c("important.ecosystem", "beneficial.humans", "beneficial.wildlife", "important.hunters",
              "risk.humans", "risk.pets", "negative.wildlife", "diseases"), ~unclass(.))

psych::alpha(coyote.values)


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

coyote_2 = coyote %>%
  mutate(pet.int = case_when(
    pet.int == "Yes" ~ 1,
    pet.int == "No" ~ 0))

coyote_2 = coyote %>%
  mutate(animal.high.int = case_when(
    animal.high.int == "Yes" ~ 1,
    animal.high.int == "No" ~ 0))

eco.agg = cor.test(kendall$important.ecosystem, pet.int, method="kendall")
ben.hum.agg = cor.test(kendall$beneficial.humans, coyote$animal.high.int, method="kendall")
ben.wild.agg = cor.test(kendall$beneficial.wildlife, coyote$animal.high.int, method="kendall")
hunt.agg = cor.test(kendall$important.hunters, coyote$animal.high.int, method="kendall")
dis.agg = cor.test(kendall$diseases, coyote$animal.high.int, method="kendall")
neg.wild.agg = cor.test(kendall$negative.wildlife, coyote$animal.high.int, method="kendall")
risk.hum.agg = cor.test(kendall$risk.humans, coyote$animal.high.int, method="kendall")
risk.pet.agg = cor.test(kendall$risk.pets, coyote_2$animal.high.int, method="kendall")

# hypothesis

