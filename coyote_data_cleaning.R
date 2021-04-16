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
# Read in data------------------------------------------------------------------
# Before I read this in, I deleted two nonsensical rows 1 and 3 (not sure how to code)
setwd("C:/Users/Kim Rivera/Documents/Coyotes/Questionnaire Data/live survey/r_data")
coyote_raw = read_csv("Coyote Questionnaire_final_dataset.csv") # OG survey data

#colnames(coyote_raw)
#coyote_raw = coyote_raw[-c(1,3),] # not sure how to remove nonsenical 1st and 3rd row in raw data


# Organize data-----------------------------------------------------------------
coyote <- coyote_raw %>% mutate(date = round_date(mdy_hm(`End Date`))) %>% 
  select(-c(1,2,4,7,8,10:13, 16, 17, 158:162)) 
  #slice(-(1:12)) 
  coyote<- coyote[-c(1:12),] # 'Official' survey started Oct 6
  
  
# Check that this worked 
write.csv(coyote, file = "C:/Users/Kim Rivera/Documents/Coyotes/Questionnaire Data/live survey/r_data/coyote_tibble.csv")
  
#?select

## Rename columns
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

# Convert all characters to factor for simplicity, can change back to numeric, etc later if needed
coyote = coyote %>% mutate_if(sapply(coyote, is.character), as.factor)

# create new column which ID's unusable data, 0 = good data, 1's = unusable/ flagged 
# start with survey.type and flag'Survey Preview' and 'Spam'
coyote = coyote %>% mutate(flagged = ifelse(response.type == "Spam", 1,
                                            ifelse(response.type == "Survey Preview", 1, 0)))

# Check that all unusable survey.types were flagged with 1's (we will filter this out later)
coyote %>% filter(response.type %in% c("Spam", "Survey Preview")) %>% select(response.type, flagged)
#Great, if we find other 'bad' data we will turn it to a 1 and filter out later for analysis

#write.csv(coyote, file = "C:/Users/Kim Rivera/Documents/Coyotes/Questionnaire Data/live survey/r_data/coyote_tibble.csv")

coyote = filter(coyote, flagged != 1)
# Participant Demographics------------------------------------------------------

## Age --------------------------------------------------------------------------
## Check Out Distribution
ggplot(coyote, aes(x=age)) +
  geom_histogram()

# Hmm, I believe it is unlikely a participant is over 110 y/o. Either this was entered wrong or perhaps the participant doesn't want 
# to share their age or did not take this survey seriously. Let's look at the data point and investigate other responses
# I'm going to be using exam as a place holder for data I am examening
exam = coyote %>% filter(age > 100) # it looks like other data is legitamate and part took time to fill out everything, 
# we will therefor change age to NA
coyote <- coyote %>% mutate(age = na_if(age, 120))
exam = coyote %>% filter(ID == "R_2rrvT7uos1Y64ZH") # looks like this worked

## Gender-----------------------------------------------------------------------
levels(coyote$gender)

# Let's convert unusable entries to NA
coyote <- coyote %>% mutate(gender = recode_factor(gender, "Apache attack helicopter" = NA_character_)) %>% 
  mutate(gender = recode_factor(gender, "mmm, no." = NA_character_)) %>% 
  mutate(gender = recode_factor(gender, "I don't believe in gender" = NA_character_)) %>%
  mutate(gender = recode_factor(gender, "Bi curious Buffalo." = NA_character_))

## collapse and correct level labels
coyote <- coyote %>% mutate(gender = fct_collapse(gender, 
                                                  male = c("m", "M", "Male", "boy", "MALE","Make", "Dude",
                                                                   "male.  However it is interesting that the previous question offered selections but this question did not.",
                                                                   "Man", "masculine", "male"),
                                                  female = c("Femail", "More or less female", "femal", "woman", "Woman", "F", "f", "Female", "female", "is that still a thing? female"),
                                                  non.binary = c("Female, non-binary/fluid" ,"non binary", "Genderqueer/Nonbinary", "Non-Binary","Non-binary","non-binary")))
levels(coyote$gender)

## Check Out Distribution
ggplot(coyote, aes(x= gender)) +
  geom_bar()

# Now let's turn this into wider data
coyote <- coyote %>% 
  mutate(gender.male = ifelse(str_detect(gender,"\\bmale\\b"),1,0),  #\\b sets a boundary around the word so it is an exact match
         gender.female = ifelse(str_detect(gender,"\\bfemale\\b"),1,0),
         gender.non.binary = ifelse(str_detect(gender,"\\bnon.binary\\b"),1,0)) %>%
  select(-gender)


view = coyote %>% select(gender, gender.male, gender.female, gender.non.binary)
## Education--------------------------------------------------------------------
levels(coyote$education)
coyote <- coyote %>% mutate(education = factor(education)) %>%
  mutate(education = fct_relevel(education, c("Elementary School", "Middle School","High School",  "Technical or vocational degree", "2-year college degree or certificate", "4-year college Degree", "Graduate Degree")))

ggplot(coyote, aes(education, fill = education)) +
  geom_bar() +
  labs(x = "Education", y = "Counts", title = "Level of Education in RI") + 
  theme_minimal() +
  theme(legend.position="none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(family = "LM Roman 10 Bold", size=15)) 

# Now let's turn this into wider data
# coyote <- coyote %>% 
#   mutate(education.elementary = ifelse(str_detect(education,"\\bElementary School\\b"),1,0),  #\\b sets a boundary around the word so it is an exact match
#          education.middle = ifelse(str_detect(education,"\\bMiddle School\\b"),1,0),
#          education.high = ifelse(str_detect(education,"\\bHigh School\\b"),1,0),
#          education.tech = ifelse(str_detect(education,"\\bTechnical or vocational degree\\b"),1,0),
#          education.2.year = ifelse(str_detect(education,"\\b2-year college degree or certificate\\b"),1,0),
#          education.4.year = ifelse(str_detect(education,"\\b4-year college Degree\\b"),1,0),
#          education.grad = ifelse(str_detect(education,"\\bGraduate Degree\\b"),1,0)) %>%
#   select(-education)

view = coyote %>% select(education, education.elementary, education.middle, education.high, education.tech, education.2.year,education.4.year, education.grad)
## Household Income-------------------------------------------------------------
## Turn into factor
coyote <- coyote %>% mutate(household.income= factor(household.income))

## Organize levels
coyote <- coyote %>% mutate(household.income = factor(household.income)) %>%
  mutate(household.income = fct_relevel(household.income, c("Under $15,000", "Between $15,000 and $29,999","Between $30,000 and $49,999", "Between $50,000 and $74,999","Between $75,000 and $99,999","Between $100,000 and $149,999","Between $150,000 and $199,999", "$200,000 or more" )))

ggplot(coyote, aes(household.income, fill = household.income)) +
  geom_bar() +
  labs(x = "Household Income", y = "Counts", title = "Household Income RI") + 
  theme_minimal() +
  theme(legend.position="none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(family = "LM Roman 10 Bold", size=15)) 

# Now let's turn this into wider data
# coyote <- coyote %>% 
#   mutate(household.income.under.15000 = ifelse(str_detect(household.income,"\\bUnder $15,000\\b"),1,0),  #\\b sets a boundary around the word so it is an exact match
#          household.income.15000.29999 = ifelse(str_detect(household.income,"\\bBetween $15,000 and $29,999\\b"),1,0),
#          household.income.30000.49999 = ifelse(str_detect(household.income,"\\bBetween $30,000 and $49,999\\b"),1,0),
#          household.income.50000.74999 = ifelse(str_detect(household.income,"\\bBetween $50,000 and $74,999\\b"),1,0),
#          household.income.75000.99999 = ifelse(str_detect(household.income,"\\bBetween $75,000 and $99,999\\b"),1,0),
#          household.income.100000.149999 = ifelse(str_detect(household.income,"\\bBetween $100,000 and $149,999\\b"),1,0),
#          household.income.150000.200000 = ifelse(str_detect(household.income,"\\bBetween $150,000 and $199,999\\b"),1,0),
#          household.income.over.200000 = ifelse(str_detect(household.income,"\\b$200,000 or more\\b"),1,0)) %>%
#   select(-household.income)

view = coyote %>% select(household.income,household.income.under.15000, household.income.15000.29999, household.income.30000.49999, household.income.50000.74999, household.income.75000.99999, household.income.100000.149999,household.income.150000.200000, household.income.over.200000)
# Ethnic Group-----------------------------------------------------------------------------
levels(factor(coyote$ethnicity))
ggplot(coyote, aes(x=ethnicity)) +
  geom_bar()

# **NEED TO FIGURE OUT IF THIS IS WORKING

# Not sure fo the best way to aggregate these...
exam = filter(coyote, ethnicity == "Asian,Other (please specify)")
exam = filter(coyote, ethnicity == "Native American,Other (please specify)")
exam = filter(coyote, ethnicity == "White/ Caucasian,Other (please specify)")
exam = filter(coyote, ethnicity == "Other (please specify)")

coyote <- coyote %>% 
  mutate(ethnicity.african.american = ifelse(str_detect(ethnicity,"African American"),1,0),
         ethnicity.asian = ifelse(str_detect(ethnicity,"Asian"),1,0),
         ethnicity.white = ifelse(str_detect(ethnicity,"White/ Caucasian"),1,0),
         ethnicity.native.american = ifelse(str_detect(ethnicity,"Native American"),1,0),
         ethnicity.islander = ifelse(str_detect(ethnicity,"Hawaiian or Pacific Islander"),1,0),
         ethnicity.hispanic = ifelse(str_detect(ethnicity,"Hispanic"),1,0))

# check if it is working
sum(coyote$ethnicity.islander, na.rm = TRUE) #10 hispanic, 9 asian, 1 aa, 770 white,
# 6 nat am, 2 islander

levels(factor(coyote$ethnicity.text))
coyote <- coyote %>% mutate(ethnicity.text = recode_factor(ethnicity.text, "why does it matter?" = NA_character_)) %>% 
  mutate(ethnicity.text = recode_factor(ethnicity.text, "Local" = NA_character_)) %>% 
  mutate(ethnicity.text = recode_factor(ethnicity.text, "Human race" = NA_character_)) %>% 
  mutate(ethnicity.text = recode_factor(ethnicity.text, "American" = NA_character_))

#(?i) = ignore CASE 
coyote <- coyote %>% 
  mutate(ethnicity.black = ifelse(str_detect(ethnicity.text,"I'm not African American I'm just Black"),1,0),
         ethnicity.jewish = ifelse(str_detect(ethnicity.text, "(?i)jewish"),1,0),
         ethnicity.middle.eastern = ifelse(str_detect(ethnicity.text,"(?i)middle eastern"),1,0),
         ethnicity.hispanic = replace(ethnicity.hispanic,str_detect(ethnicity.text, "(?i)latino"),1),
         ethnicity.asian = replace(ethnicity.asian,str_detect(ethnicity.text, "South Asian Indian"),1),
         ethnicity.white = replace(ethnicity.white,str_detect(ethnicity.text, "(?i)white|caucasian|Italian|irish|french" ),1))

sum(coyote$ethnicity.white, na.rm = TRUE) #10 hispanic, 9 asian, 1 aa, 770 white,
# 6 nat am, 2 islander      

# Drop the old longer columns, we now have wider columns
coyote = coyote %>% 
  select(-c(ethnicity,ethnicity.text))

## collapse and correct level labels
# coyote <- coyote %>% mutate(ethnicity.text = fct_collapse(ethnicity.text, 
#                                                   "White/ Caucasian" = c("White", "French/Portuguese", "Irish American", "WHITE AMERICAN IRISH.", "Italian American:)"),
#                                                   "Hispanic" = c("White Latino"),
#                                                   "Asian" = c("South Asian Indian")))
# levels(factor(coyote$ethnicity.text))

# Length of stay in RI for non-residents----------------------------------------

#convert factor to string then to a numeric once everything is a number
class(coyote$months.RI.text)

#coyote <- coyote %>% 
# mutate(as.string(months.RI.text))

levels(coyote$months.RI.text)
exam = coyote %>% filter(months.RI.text == "zero")
exam = coyote %>% filter(months.RI.text == 0)
exam = coyote %>% filter(ID == "R_Rlf2IvahxBnmqBP")

coyote <- coyote %>% 
  mutate(flagged = case_when(months.RI.text == "0" ~ 1,
                                months.RI.text == "zero" ~ 1,
                             TRUE ~ as.numeric(flagged))) 

# filter out flagged data again
coyote = filter(coyote, flagged != 1)

# since we already converted to a factor, we need to do this to make numbers numerical 
coyote <- coyote %>% 
  mutate(months.RI.text = as.character(months.RI.text)) 
class(coyote$months.RI.text)

coyote <- coyote %>% 
  mutate(months.RI = case_when(months.RI.text == "7 (from last year)" ~ 7,
                               months.RI.text == "About 7 months a year" ~ 28, #This was a 4 year student that lives in Mass otherwise
                               months.RI.text == "I went to college at URI and we vacation several weeks per year." ~ 29, 
                               months.RI.text == "All my life until Nov. 2019" ~ age*12-3,
                               months.RI.text %in% c("6 months each year", "Several", "Twice a week", "I live on the edge of RI and work in East Providence.", "I just visit RI occasionally") ~ NA_real_,
                               as.numeric(months.RI.text) >=2000 ~ (2020-as.numeric(months.RI.text))*12+10,
                               str_detect(months.RI.text, "year") ~ as.numeric(str_extract(months.RI.text,"(\\d)+"))*12,
                               TRUE ~ as.numeric(months.RI.text))) %>% 
                                select(-months.RI.text)

# Now that everything is a proper month, we can convert to a numeric value
coyote <- coyote %>% 
  mutate(months.RI = as.numeric(months.RI))
class(coyote$months.RI)

#here remove people how have put zero or '0' months, they have basically never spent time in RI

coyote %>% 
  ggplot(aes(x = months.RI)) + 
  geom_histogram(color="darkblue", fill="lightblue") +
  labs(title="Months in RI",x="Months", y="Counts")+
  theme_minimal() + 
  theme(legend.position="none",
        text = element_text(family = "LM Roman 10 Bold", size=15)) 

# Particpant Location-----------------------------------------------------------

# Residency
coyote %>% 
  ggplot(aes(x = resident, fill = resident)) + 
  geom_bar() + 
  labs(x = "Residency", y = "Counts", title = "Counts of residents vs. non-residents") + 
  theme_minimal() + 
  theme(legend.position="none",
        text = element_text(family = "LM Roman 10 Bold", size=15)) 

# Years of Residency (if resident)
summary(coyote$years.residency)

coyote %>% 
  ggplot(aes(x = years.residency)) + 
  geom_histogram(color="darkblue", fill="lightblue") +
  labs(title="Years of residency",x="Years", y="Counts")+
  theme_minimal() + 
  theme(legend.position="none",
        text = element_text(family = "LM Roman 10 Bold", size=15))

#Non-residents
levels(factor(coyote$not.resident))
levels(factor(coyote$not.resident.others))

# Let's treat peoples RI activities as we did ethnicity, people may fall into various columns which
# we will turn into wider data
coyote <- coyote %>% 
  mutate(not.resident.student = ifelse(str_detect(not.resident,"I am a student in RI"),1,0),
         not.resident.rent = ifelse(str_detect(not.resident,"I rent a home in RI"),1,0),
         not.resident.work = ifelse(str_detect(not.resident,"I work in RI"),1,0),
         not.resident.vacation = ifelse(str_detect(not.resident,"I own a vacation home or property in RI"),1,0),
         not.resident.tourist = ifelse(str_detect(not.resident,"I am a tourist in RI"),1,0))

# Now let's recode the other column 'others' to fit in the above columns or new columns
coyote <- coyote %>% 
  mutate(not.resident.family = ifelse(str_detect(not.resident.others,"(?i)family|My in-laws in in RI and I'm there almost every weekend"),1,0),
         not.resident.prev.resident = ifelse(str_detect(not.resident.others, "(?i)Grew up|Owned 3 properties|Recently moved|I am a resident currently living in Mexico|I live in de, taking online classes|I once resided for three years in Pawtucket"),1,0),
         not.resident.work = replace(not.resident.work,str_detect(not.resident.others, "(?i)held jobs in RI in the past|worked"),1),
         not.resident.prev.student = ifelse(str_detect(not.resident.others,"previous student"),1,0))

coyote <- coyote %>% 
  mutate(not.resident.family = ifelse(str_detect(not.resident.others,"(?i)family"),1,0),
         not.resident.prev.resident = ifelse(str_detect(not.resident.others, "(?i)Owned 3 properties|Recently moved|I am a resident currently living in Mexico|I live in de, taking online classes|I once resided for three years in Pawtucket"),1,0),
         not.resident.work = replace(not.resident.work,str_detect(not.resident.others, "(?i)held jobs in RI in the past|worked"),1),
         not.resident.family = replace(not.resident.family,str_detect(not.resident.others, "\\bMy in-laws in in RI and I'm there almost every weekend\\b"),1),
         not.resident.prev.resident = replace(not.resident.prev.resident,str_detect(not.resident.others, "\\bGrew up here\\b"),1),
         not.resident.prev.resident = replace(not.resident.prev.resident,str_detect(not.resident.others, "\\bgrew up there"),1),
         not.resident.prev.resident = replace(not.resident.prev.resident,str_detect(not.resident.others, "\\bI grew up in R"),1),
         not.resident.prev.student = ifelse(str_detect(not.resident.others,"previous student"),1,0))

view = coyote %>% select(not.resident, not.resident.others,not.resident.family, not.resident.prev.student, not.resident.prev.resident, not.resident.work,not.resident.student,not.resident.rent, not.resident.vacation, not.resident.tourist)
                         
# Flag participants that have not spent a significant amount of time in RI
coyote <- coyote %>% 
  mutate(flagged = case_when(not.resident.others == "Living at Home in MA" ~ 1,
                             TRUE ~ as.numeric(flagged)))

view = filter(coyote, not.resident.others == "Living at Home in MA")

view = coyote %>%
  filter(str_detect(not.resident.others,"previous student"))

# filter out flagged data again
coyote = filter(coyote, flagged != 1)

coyote = coyote %>% 
  select(-c(not.resident,not.resident.others))

# # tourist + other (appears this value was filtered out in the beginning)
# view = filter(coyote, not.resident == "I am a tourist in RI,Other (please specify)")
# coyote <- coyote %>% 
#    mutate(not.resident = replace(not.resident, not.resident == "I am a tourist in RI,Other (please specify)"
#                                  "family"))
# 
# # work + other 
# view <- filter(coyote, not.resident == "I work in RI,Other (please specify)")
# coyote <- coyote %>% 
#   mutate(not.resident = replace(not.resident, not.resident == "I work in RI,Other (please specify)" & not.resident.others == "Business owner", "work")) %>% 
#   mutate(not.resident = replace(not.resident, not.resident == "I work in RI,Other (please specify)" & not.resident.others == "I have family in RI and I live only a mile or so away from the kids e.", "work.family"))
# 
# # other
# # reminder: these "character strings" must be exact matches or the function does't work
# # need to delete rows with non.resident's that spent zero, '0' months in RI****
# view <- filter(coyote, not.resident == "Other (please specify)")
# coyote <- coyote %>% 
#   mutate(not.resident = replace(not.resident, not.resident.others %in% c("Owned 3 properties in RI...1998-2020","Recently moved from RI","I once resided for three years in Pawtucket", "Grew up here","I grew up in Rhode Island.", "I grew up in RI and family farm still there.  I lived and worked in RI most of my life. It", "I am a resident currently living in Mexico", "Originally from Rhode Island grew up there for 24 years"), "previous.resident")) %>% 
#   mutate(not.resident = replace(not.resident, not.resident.others == "My in-laws in in RI and I'm there almost every weekend. I grew up in a MA border town and have held jobs in RI in the past.", "family")) %>%
#   mutate(not.resident = replace(not.resident, not.resident.others == "previous student in RI", "previous.student")) %>%
#   mutate(not.resident = replace(not.resident, not.resident.others %in% c("I live in de, taking online classes","Living at Home in MA","I attended an online seminar."),"remote"))
# 
# # change the other categories names 
# coyote <- coyote %>% 
#   mutate(not.resident = case_when(not.resident %in% c("I am a student in RI","I am a student in RI,I rent a home in RI") ~ "student",
#                                   not.resident == "I am a student in RI,I work in RI" ~ "student.work",
#                                   not.resident %in% c("I own a vacation home or property in RI","I own a vacation home or property in RI,I rent a home in RI") ~ "vacation",
#                                   not.resident %in% c("I work in RI", "I work in RI,I rent a home in RI") ~ "work",
#                                   TRUE ~ not.resident))
# levels(factor(coyote$not.resident))
# 
# coyote <- coyote %>% 
#   mutate(not.resident.childhood = ifelse(not.resident == "childhood",1,0),
#          not.resident.family = ifelse(not.resident %in% c("family","work.family"),1,0),
#          not.resident.previous.resident = ifelse(not.resident == "previous.resident",1,0),
#          not.resident.remote = ifelse(not.resident == "remote",1,0),
#          not.resident.student = ifelse(not.resident %in% c("student", "student.work"),1,0),
#          not.resident.work = ifelse(not.resident %in% c("student.work","work","work.family"),1,0),
#          not.resident.vacation = ifelse(not.resident == "vacation",1,0))

levels(factor(coyote$not.resident))

coyote %>% 
  drop_na(not.resident) %>% 
  ggplot(aes(x = not.resident, fill = not.resident)) + 
  geom_bar() + 
  labs(x = "Reason", y = "Counts", title = "Counts per non-residency reason") + 
  theme_minimal() + 
  theme(legend.position="none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(family = "LM Roman 10 Bold", size=15))
#The above isn't work, seems to not be replacing, what is happening? ***

#coyote <- select(coyote, -not.resident.others)


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

# Town------------------------------------------------------------------------------

levels(factor(coyote$town.text))

# Change the mispelling for the towns/villages 
coyote <- coyote %>% 
  mutate(town.text = str_to_title(town.text)) %>% 
  mutate(town.text = case_when(town.text == "Edgewood Cranston" ~ "Cranston",
                               town.text %in% c("Elmhurst","Elmhurst Providence","Hope", "Providence (Hope)","Olneyville (Providence)","Onyville","Summit") ~ "Providence",
                               town.text == "N Smithfield" ~ "North Smithfield",
                               town.text %in% c("Naragansett","Narragansett, But Moving To Coventry") ~ "Narragansett",
                               town.text == "Newp" ~ "Newport",
                               town.text == "No Kingstown" ~ "North Kingstown",
                               town.text %in% c("South Kingston","Kingston Fortin Rd","Kingston") ~ "South Kingstown",
                               town.text == "Scituare" ~ "Scituate",
                               town.text == "Tiverron" ~ "Tiverton",
                               TRUE ~ as.character(town.text)))

levels <- coyote %>% 
  group_by(town.text) %>% 
  summarize(count=n()) %>% 
  arrange(count) %>% 
  select(town.text) %>% 
  as_vector()

coyote %>% 
  mutate(town.text = factor(town.text, levels = levels)) %>% 
  ggplot(aes(x = town.text, fill = town.text)) + 
  geom_bar() + 
  labs(x = "Town and villages", y = "Counts", title = "Counts per town and villages") + 
  theme_minimal() + 
  theme(legend.position="none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(family = "LM Roman 10 Bold", size=5)) 

# combine by town 
coyote <- coyote %>% 
  mutate(town.text = case_when(town.text == "Arnold Mills" ~ "Cumberland",
                               town.text %in% c("Escoheag", "Exeter, West Kingston Area") ~ "Exeter",
                               town.text %in% c("Coventry-Western","Greene") ~ "Coventry",
                               town.text == "Greenville" ~ "Smithfield",
                               town.text %in% c("Harmony/Glocester","Chepachet") ~ "Glocester",
                               town.text == "Hog Island From April Through October" ~ "Portsmouth",
                               town.text %in% c("Hope Valley", "Burdickville In Hopkinton") ~ "Hopkinton",
                               town.text == "Jamestown /Clarkes Village" ~ "Jamestown",
                               town.text %in% c("No Kingstown","Nk","North Kingstown/Wickford", "North Kingstown/Hamilton","Wickford","Saunderstown") ~ "North Kingstown",
                               town.text %in% c("North Scituate","Scituate/Hope") ~ "Scituate",
                               town.text %in% c("Pascoag", "Burrillville / Glendale") ~ "Burrillville",
                               town.text %in% c("Pawtuxet Village","Warwick / Pilgrim Park","Warwick /Greenwood","Warwick...Pojac Area","Gaspee Point", "Warwick - Budlong Farm") ~ "Warwick",
                               town.text %in% c("Peace Dale, South Kingstown","South Kingstown/Peace Dale","Green Hill","Green Hill, Wakefield","South Kingston","Matunuck","South Kingstown/Matunuck","Wakefield","Kingston Fortin Rd","West Kingston","Wordens Pond","East Matunuck","Kingston", "Perryville", "South Kingstown/Kingston", "South Kingstown/Wakefield") ~ "South Kingstown",
                               town.text %in% c("Carolina","Richmond/Carolina","Wyoming") ~ "Richmond",
                               town.text == "Scituate/ Clayville" ~ "Foster",
                               town.text == "Manville" ~ "Lincoln",
                               town.text == "Misquamicut" ~ "Westerly",
                               town.text %in% c("Riverside","Rumford") ~ "East Providence",
                               TRUE ~ as.character(town.text))) %>% 
  select(-town)

view = coyote %>% select(town.text, town)

levels <- coyote %>% 
  group_by(town.text) %>% 
  summarize(count=n()) %>% 
  arrange(count) %>% 
  select(town.text) %>% 
  as_vector()

coyote %>% 
  mutate(town.text = factor(town.text, levels = levels)) %>% 
  ggplot(aes(x = town.text, fill = town.text)) + 
  geom_bar() + 
  labs(x = "Town and villages", y = "Counts", title = "Counts per town") + 
  theme_minimal() + 
  theme(legend.position="none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(family = "LM Roman 10 Bold", size=10)) 


# Relationship with the Outdoors------------------------------------------------
levels(factor(coyote$outdoor))

coyote <- coyote %>% 
  mutate(outdoor.backpacking = ifelse(str_detect(outdoor,"Backpacking"),1,0),
         outdoor.car.rvcamping = ifelse(str_detect(outdoor,"Car or RV camping"),1,0),
         outdoor.fish = ifelse(str_detect(outdoor,"Fishing"),1,0),
         outdoor.hunt = ifelse(str_detect(outdoor,"Hunting or Trapping"),1,0),
         outdoor.other = ifelse(str_detect(outdoor,"Other"),1,0),
         outdoor.mountainbiking = ifelse(str_detect(outdoor,"Mountain biking"),1,0),
         outdoor.road_run.jog = ifelse(str_detect(outdoor,"Road running or jogging"),1,0),
         outdoor.water_sports = ifelse(str_detect(outdoor,"Water"),1,0),
         outdoor.wildlife_viewing = ifelse(str_detect(outdoor,"Wildlife viewing"),1,0),
         outdoor.roadbiking = ifelse(str_detect(outdoor,"Road biking"),1,0),
         outdoor.hike.trailrun = ifelse(str_detect(outdoor,"Hiking or trail running"),1,0),
         outdoor.horseriding = ifelse(str_detect(outdoor,"Horseback riding"),1,0))

coyote %>% 
  pivot_longer(outdoor.backpacking:outdoor.horseriding, names_to="index",values_to="value") %>% 
  filter(value == 1) %>% 
  ggplot(aes(x = index, fill = index)) + 
  geom_bar() + 
  labs(x = "Outdoor activities", y = "Counts", title = "Practicants of outdoor activities") + 
  theme_minimal() + 
  theme(legend.position="none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(family = "LM Roman 10 Bold", size=15)) 

levels(factor(coyote$outdoor.text))

coyote <- coyote %>% 
  mutate(outdoor.walk = ifelse(str_detect(outdoor.text,"wal"),1,0),
         outdoor.sport = ifelse(str_detect(outdoor.text, "(?i)basketball|golf|climb|sport|tennis"),1,0),
         outdoor.water_sports = replace(outdoor.water_sports,str_detect(outdoor.text,"(?i)beach|boat|swim|canoe|sail|kayak|scuba"),1),
         outdoor.snow_sports = ifelse(str_detect(outdoor.text, "(?i)snow|ski"),1,0),
         outdoor.gardening_yard = ifelse(str_detect(outdoor.text,"(?i)garden|yeard|yard"),1,0),
         outdoor.farm = ifelse(str_detect(outdoor.text,"(?i)farm"),1,0),
         outdoor.bike = ifelse(str_detect(outdoor.text,"(?i)bike"),1,0),
         outdoor.monitoring = ifelse(str_detect(outdoor.text,"(?i)sampl|camera|monitor"),1,0),
         outdoor.wildlife_viewing = replace(outdoor.wildlife_viewing,str_detect(outdoor.text,"(?i)bird|geese"),1),
         outdoor.children = ifelse(str_detect(outdoor.text,"(?i)child"),1,0),
         outdoor.foraging = ifelse(str_detect(outdoor.text,"(?i)forag|mushroom"),1,0),
         outdoor.photo_film = ifelse(str_detect(outdoor.text,"(?i)photo|film"),1,0))

levels <- coyote %>% 
  pivot_longer(outdoor.backpacking:outdoor.photo_film, names_to="index",values_to="value") %>% 
  filter(value == 1) %>% 
  group_by(index) %>% 
  summarize(count=n()) %>% 
  arrange(count) %>% 
  select(index) %>% 
  as_vector()

coyote %>% 
  pivot_longer(outdoor.backpacking:outdoor.photo_film, names_to="index",values_to="value") %>% 
  mutate(index = factor(index, levels = levels)) %>% 
  filter(value == 1) %>% 
  ggplot(aes(x = index, fill = index)) + 
  geom_bar() + 
  labs(x = "Outdoor activities", y = "Counts", title = "Practicants of outdoor activities") + 
  theme_minimal() + 
  theme(legend.position="none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(family = "LM Roman 10 Bold", size=15)) 

# Attitude towards Coyotes------------------------------------------------------
## Value of coyotes?

# Pet Demographics--------------------------------------------------------------
## Lets check out population of pet owners
ggplot(coyote, aes(x = pet.owner, color = pet)) +
  geom_bar(fill="white")

## correct non-informative levels to NAs
coyote <- coyote %>% mutate(pet = recode_factor(pet, "Tamed Coyote" = NA_character_)) %>% 
  mutate(gender = recode_factor(pet, "Two" = NA_character_)) %>% 
  mutate(gender = recode_factor(pet, "I have 9 indoor pets of various sizes and species" = NA_character_))%>% 
  mutate(gender = recode_factor(pet, "All indoors and never unattended" = NA_character_))

## Separate multiple selections
coyote <- coyote %>% 
  mutate(pet.outdoor.cat = ifelse(str_detect(pet,"(?i)Outdoor.*Cat"),1,0),
         pet.small.dog = ifelse(str_detect(pet,"(?i)Small.*dog"),1,0),
         pet.lrg.dog = ifelse(str_detect(pet,"Large.*dog"),1,0))
         

# coyote <- coyote %>%
#   mutate(pet.outdoor.cat = ifelse(str_detect(pet,"Outdoor Cat", "\\b"),1,0),
#          pet.small.dog = ifelse(str_detect(pet,"Small dog", "\\b"),1,0),
#          pet.lrg.dog = ifelse(str_detect(pet,"Large dog","\\b"),1,0),
#          pet.other = ifelse(str_detect(pet,"Other", "\\b"),1,0))

## Look at data
coyote %>% 
  pivot_longer(pet.outdoor.cat:pet.lrg.dog, names_to="index",values_to="value") %>% 
  filter(value == 1) %>% 
  ggplot(aes(x = index, fill = index)) + 
  geom_bar() + 
  labs(x = "Pet Ownership", y = "Counts", title = "Pet Ownership") + 
  theme_minimal() + 
  theme(legend.position="none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(family = "LM Roman 10 Bold", size=15))


## recategroize 'other' text
levels(coyote$pet.text)
view = coyote %>% select(pet, pet.text)

# coyote <- coyote %>% 
#   mutate(pet.indoor.cat = ifelse(str_detect(pet.text,"(?i)indoor.*cats|indoor.*cat|house.*cats|indoor.*only.*cats||in.*door.*cat|Indoor-only.*cats"),1,0),
#          pet.indoor.night.cat = ifelse(str_detect(pet.text, "(?i)Cat.*comes.*in.*at.*night.*|Indoor/outdoor.*cat.*(inside.*at.*night)|indoor/outdoor.*cat.*locked.*in.*at.*night"),1,0),
#          pet.outside.tort = ifelse(str_detect(pet.text, "(?i)outdoor.*tortoise"),1,0),
#          pet.outdoor.cat = replace(pet.outdoor.cat,str_detect(pet.text,"(?i)outdoor.*feral.*cats|Lost.*a.*cat|stray.*feral.*cats|outdoor.*feral.*cats|Indoor/outdoor.*cat|Lost.*a.*cat|Mostly.*indoor.*cat"),1),
#          pet.small.dog = replace(pet.small.dog,str_detect(pet.text,"(?i)Corgi|small.*dogs"),1),
#          pet.lrg.dog = replace(pet.lrg.dog,str_detect(pet.text,"(?i)Two.*Labs|Medium-sized.*dogs|lbdog|Border.*Collies"),1),
#          pet.bird = ifelse(str_detect(pet.text, "(?i)bird|macaw|parrot"),1,0))
#          pet.other = ifelse(str_detect(pet.txt,"),1,0)))                     


# coyote <- coyote %>% 
#   mutate(pet.indoor.cat = ifelse(str_detect(pet.text,"(?i)2 indoor cats|3 indoor cats|Chickens, rabbit, house cats|House rabbit, macaw, indoor only cats|i am active in cat rescue and have previously had outdoor feral cats. I moved them indoors.|in door cat, fish, hamters, turtles, snake|Indoor-only cats|indoor cat|Indoor cat|Indoor Cat|INDOOR cat|Indoor cat also|indoor cat that I walk on a leash|Indoor cat, chickens|Indoor cat.  2 small dogs|indoor cats|Indoor cats|INDOOR CATS|Indoor cats also|indoor cats, herps|indoor cats, occasional escape|Indoor cats.|indoor cats.  Previously, large dog.|Lost a cat to coyotes now indoor only|Chickens, ducks, geese, peafowl, guineafowl, indoor cats, reptiles, outdoor rabbits"),1,0),
#          pet.indoor.night.cat = ifelse(str_detect(pet.text, "(?i)Cat comes in at night because coyote|Indoor/outdoor cat (inside at night)|indoor/outdoor cat locked in at night|"),1,0),
#          pet.outside.tort = ifelse(str_detect(pet.text, "(?i)2 large dogs, bird and outdoor tortoise"),1,0),
#          pet.outdoor.cat = replace(pet.outdoor.cat,str_detect(pet.text,"(?i)Have owned outdoor cats|i am active in cat rescue and have previously had outdoor feral cats. I moved them indoors.|I have several stray feral cats that live in and around my property.|Indoor/outdoor cat|indoor/outdoor cats|Lost a cat to coyotes now indoor only|Mostly indoor cat|mostly indoor cat, mostly|outdoor cat|"),1),
#          pet.small.dog = replace(pet.small.dog,str_detect(pet.text,"(?i)3cats, 1. Corgi|Indoor cat.  2 small dogs"),1),
#          pet.lrg.dog = replace(pet.lrg.dog,str_detect(pet.text,"(?i)Two Labs|Medium-sized dogs- 33 pounds and 30 pounds|indoor cats.  Previously, large dog.|30 lbdog|3 Border Collies"),1),
#          pet.bird = ifelse(str_detect(pet.text, "(?i)2 large dogs, bird and outdoor tortoise|House rabbit, macaw, indoor only cats|parrot|Parrot|Small parrot"),1,0))                     

coyote <- coyote %>% 
  mutate(pet = as.character(pet)) 
class(coyote$pet)

coyote <- coyote %>% 
  mutate(pet.text = as.character(pet.text)) 
class(coyote$pet.text)

coyote <- coyote %>% 
  mutate(pet.text = case_when(pet.text %in% c("2 cats","cats (indoor only)",
                                              "i am active in cat rescue and have previously had outdoor feral cats. I moved them indoors.", 
                                              "Lost a cat to coyotes now indoor only") ~ "indoor cat",
                              pet.text %in% c("2 dogs","my dog is 45#, which I consider medium sized.") ~ "large dogs",
                              pet.text %in% c("3cats, 1. Corgi") ~ "outdoor cat, Corgi",
                              pet.text %in% c("cat goes out only if I am in yard") | str_detect(pet.text,"(?i)indoor/outdoor|cat.*night") ~ "int/out cat",
                              TRUE ~ pet.text)) %>% 
  mutate(pet.indoor.cat = ifelse(str_detect(pet.text,"(?i)indoor.*cat|house.*cat|door.*cat|indoor.*animal|inside.*cat"),1,0),
         #pet.indoor.outdoor.cat = ifelse(str_detect(pet.text, "(?i)int/out.*cat"),1,0), # I'm just calling this outdoor
         pet.outdoor.cat = replace(pet.outdoor.cat,str_detect(pet.text,"(?i)outdoor.*cat|stray.*cat|out.*cat"),1),
         pet.small.dog = replace(pet.small.dog,str_detect(pet.text,"(?i)Corgi|small.*dog"),1),
         pet.lrg.dog = replace(pet.lrg.dog,str_detect(pet.text,"(?i)lab|Medium-sized.*dog|lbdog|Border|large.*dog"),1),
         pet.bird = ifelse(str_detect(pet.text, "(?i)bird|macaw|parrot|cockatoo"),1,0),
         pet.rabbit = ifelse(str_detect(pet.text, "(?i)\\brabbit\\b|house.*rabbit|bunny|indoor.*rabbits"),1,0),
         pet.reptile = ifelse(str_detect(pet.text, "(?i)reptile|snake|herp|lizard|tortoise|turtle"),1,0),
         pet.ferret = ifelse(str_detect(pet.text, "(?i)ferret"),1,0), 
         pet.rodent = ifelse(str_detect(pet.text, "(?i)gerbil|guinea.*pig|hamter"),1,0)) 

view = coyote %>% select(pet, pet.text,pet.ferret, pet.rodent,pet.bird,pet.rabbit,pet.reptile, pet.indoor.cat,pet.outdoor.cat,pet.small.dog, pet.lrg.dog)
write.csv(view, file = "C:/Users/Kim Rivera/Documents/Coyotes/Questionnaire Data/live survey/r_data/view_tibble.csv")


##Lets plot pet type ownership distribution
levels <- coyote %>% 
  pivot_longer(pet.outdoor.cat:pet.bird, names_to="index",values_to="value") %>% 
  filter(value == 1) %>% 
  group_by(index) %>% 
  summarize(count=n()) %>% 
  arrange(count) %>% 
  select(index) %>% 
  as_vector()

coyote %>% 
  pivot_longer(pet.outdoor.cat:pet.bird, names_to="index",values_to="value") %>% 
  mutate(index = factor(index, levels = levels)) %>% 
  filter(value == 1) %>% 
  ggplot(aes(x = index, fill = index)) + 
  geom_bar() + 
  labs(x = "Pet Ownership", y = "Counts", title = "Pet Ownership") + 
  theme_minimal() + 
  theme(legend.position="none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(family = "LM Roman 10 Bold", size=15)) 

#dev.off()
# Livestock-Coyote Interactions-------------------------------------------------
## Let's start with ownership

# coyote = coyote %>%
#   select(matches("risk.")) %>% 
#   mutate(factor(.))


ggplot(coyote, aes(x = livestock.owner, color = livestock)) +
  geom_bar(fill="white")

# ## Change to factor
# coyote <- coyote %>% mutate(livestock= factor(livestock))
# coyote <- coyote %>% mutate(livestock.text= factor(livestock.text))

## correct levels to NAs
# coyote <- coyote %>% mutate(pet = recode_factor(pet, "Tamed Coyote" = NA_character_)) %>% 
#   mutate(gender = recode_factor(pet, "Two" = NA_character_)) %>% 
#   mutate(gender = recode_factor(pet, "I have 9 indoor pets of various sizes and species" = NA_character_))%>% 
#   mutate(gender = recode_factor(pet, "All indoors and never unattended" = NA_character_))

## Separate multiple selections
coyote <- coyote %>% 
  mutate(livestock.cattle = ifelse(str_detect(livestock,"(?i)cattle"),1,0),
         livestock.goat = ifelse(str_detect(livestock,"(?i)goat"),1,0),
         livestock.poultry = ifelse(str_detect(livestock,"(?i)poultry"),1,0),
         livestock.pig = ifelse(str_detect(livestock,"(?i)pig"),1,0),
         livestock.sheep = ifelse(str_detect(livestock,"(?i)sheep"),1,0))

## Look at data
coyote %>% 
  pivot_longer(livestock.cattle:livestock.sheep, names_to="index",values_to="value") %>% 
  filter(value == 1) %>% 
  ggplot(aes(x = index, fill = index)) + 
  geom_bar() + 
  labs(x = "Livestock Ownership", y = "Counts", title = "Livestock Ownership") + 
  theme_minimal() + 
  theme(legend.position="none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(family = "sans", size=15))

## recategroize 'other' text

coyote <- coyote %>% 
  mutate(livestock.bee = ifelse(str_detect(livestock.text, "(?i)bees"),1,0),
         livestock.camelid = ifelse(str_detect(livestock.text, "(?i)llama|alpaca"),1,0),
         livestock.poultry = replace(livestock.poultry,str_detect(livestock.text,"(?i)peacock|hen"),1),
         livestock.equine = ifelse(str_detect(livestock.text,"(?i)horse|donkey"),1,0),
         livestock.cattle = replace(livestock.cattle,str_detect(livestock.text,"(?i)cattle|goat"),1),
         livestock.shellfish = ifelse(str_detect(livestock.text, "(?i)quoahog"),1,0),
         livestock.rabbit = ifelse(str_detect(livestock.text, "(?i)outdoor.*rabbits|rabbits"),1,0))

view = coyote %>% select(livestock, livestock.text,livestock.bee,livestock.camelid, livestock.poultry,livestock.equine,livestock.cattle,livestock.shellfish)
write.csv(view, file = "C:/Users/Kim Rivera/Documents/Coyotes/Questionnaire Data/live survey/r_data/view_tibble.csv")

view = coyote %>% select(livestock.text,pet.text, pet.rabbit, livestock.rabbit)

levels <- coyote %>% 
  pivot_longer(livestock.cattle:shellfish, names_to="index",values_to="value") %>% 
  filter(value == 1) %>% 
  group_by(index) %>% 
  summarize(count=n()) %>% 
  arrange(count) %>% 
  select(index) %>% 
  as_vector()

coyote %>% 
  pivot_longer(livestock.cattle:shellfish, names_to="index",values_to="value") %>% 
  mutate(index = factor(index, levels = levels)) %>% 
  filter(value == 1) %>% 
  ggplot(aes(x = index, fill = index)) + 
  geom_bar() + 
  labs(x = "Livestock Ownership", y = "Counts", title = "Livestock Ownership") + 
  theme_minimal() + 
  theme(legend.position="none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(family = "LM Roman 10 Bold", size=15)) 




## Lets get into the interactions now!------------------------------------------
##Change to factor levels if needed
# 
# coyote <- coyote %>% mutate(risk.outdoor.cat = factor(risk.outdoor.cat))
# coyote <- coyote %>% mutate(encounter.outdoor.cat= factor(encounter.outdoor.cat))
# coyote <- coyote %>% mutate(attack.outdoor.cat= factor(attack.outdoor.cat))
# coyote <- coyote %>% mutate(missing.outdoor.cat= factor(missing.outdoor.cat))
# coyote <- coyote %>% mutate(kill.outdoor.cat= factor(kill.outdoor.cat))
# coyote <- coyote %>% mutate(risk.large.dog= factor(risk.large.dog))
# coyote <- coyote %>% mutate(encounter.large.dog= factor(encounter.large.dog))
# coyote <- coyote %>% mutate(attack.large.dog= factor(attack.large.dog))
# coyote <- coyote %>% mutate(missing.large.dog= factor(missing.large.dog))
# coyote <- coyote %>% mutate(kill.large.dog= factor(kill.large.dog))
# coyote <- coyote %>% mutate(risk.small.dog= factor(risk.small.dog))
# coyote <- coyote %>% mutate(attack.small.dog= factor(attack.small.dog))
# coyote <- coyote %>% mutate(missing.small.dog= factor(missing.small.dog))
# coyote <- coyote %>% mutate(kill.small.dog= factor(kill.small.dog))
# coyote <- coyote %>% mutate(risk.pet= factor(risk.pet))
# coyote <- coyote %>% mutate(encounter.pet= factor(encounter.pet))
# coyote <- coyote %>% mutate(attack.pet= factor(attack.pet))
# coyote <- coyote %>% mutate(missing.pet= factor(missing.pet))
# coyote <- coyote %>% mutate(kill.pet= factor(kill.pet))
# coyote <- coyote %>% mutate(risk.goat = factor(risk.goat))
# coyote <- coyote %>% mutate(encounter.goat= factor(encounter.goat))
# coyote <- coyote %>% mutate(attack.goat= factor(attack.goat))
# coyote <- coyote %>% mutate(missing.goat= factor(missing.goat))
# coyote <- coyote %>% mutate(kill.goat= factor(kill.goat))
# coyote <- coyote %>% mutate(risk.sheep= factor(risk.sheep))
# coyote <- coyote %>% mutate(encounter.sheep= factor(encounter.sheep))
# coyote <- coyote %>% mutate(attack.sheep= factor(attack.sheep))
# coyote <- coyote %>% mutate(missing.sheep= factor(missing.sheep))
# coyote <- coyote %>% mutate(kill.sheep= factor(kill.sheep))
# coyote <- coyote %>% mutate(risk.poultry= factor(risk.poultry))
# coyote <- coyote %>% mutate(attack.poultry= factor(attack.poultry))
# coyote <- coyote %>% mutate(missing.poultry= factor(missing.poultry))
# coyote <- coyote %>% mutate(kill.poultry= factor(kill.poultry))
# coyote <- coyote %>% mutate(risk.pig= factor(risk.pig))
# coyote <- coyote %>% mutate(encounter.pig= factor(encounter.pig))
# coyote <- coyote %>% mutate(attack.pig= factor(attack.pig))
# coyote <- coyote %>% mutate(missing.pig= factor(missing.pig))
# coyote <- coyote %>% mutate(kill.pig= factor(kill.pig))
# coyote <- coyote %>% mutate(risk.cattle= factor(risk.cattle))
# coyote <- coyote %>% mutate(encounter.cattle= factor(encounter.cattle))
# coyote <- coyote %>% mutate(attack.cattle= factor(attack.cattle))
# coyote <- coyote %>% mutate(missing.cattle= factor(missing.cattle))
# coyote <- coyote %>% mutate(kill.cattle= factor(kill.cattle))
# coyote <- coyote %>% mutate(risk.other= factor(risk.other))
# coyote <- coyote %>% mutate(encounter.other= factor(encounter.other))
# coyote <- coyote %>% mutate(attack.other= factor(attack.other))
# coyote <- coyote %>% mutate(missing.other= factor(missing.other))
# coyote <- coyote %>% mutate(kill.other= factor(kill.other))

# data <- data.frame(matrix(sample(1:40), 4, 10, dimnames = list(1:4, LETTERS[1:10])))
# cols <- c("A", "C", "D", "H")
# 
# data %<>% mutate_at(cols, funs(factor(.)))
# str(data)

# Pet-Coyote Interactions-------------------------------------------------------
## Create new column for having had any pet-coyote interaction
pet.int = coyote %>%
  select(c(matches("risk.|encounter.|attack.|missing.|kill."))) %>%
  select(-c(1:2, 23:length(.)))

# Change pet.int from yes/no, to 1/0
pet.int = pet.int %>%
  mutate_all(~case_when(
    . == "Yes" ~ 1,
    . == "No" ~ 0))
# how do I keep ID for future joining? Need to add back ID to pet.int
# pet.int = pet.int %>%
#   mutate_at(vars(-ID), max~case_when(
#     . == "Yes" ~ 1,
#     . == "No" ~ 0))
# pet.int = pet.int %>%
#   mutate_at(vars(c(risk.outdoor.cat:risk.outdoor.dog, -ID))~case_when(
#     . == "Yes" ~ 1,
#     . == "No" ~ 0))

## Create new column of all percieved pet-coyote int
pet.int.perc = pet.int %>%
  select(c(matches("risk.|missing.")))

## Turn column in 1/0 if percieved pet-coyote int occured (1 = Yes, 0 = NA or No)--This will be a later problem, I want NA's to stay NA's***
pet.int.perc = pet.int.perc %>% 
  mutate(pet.int.perc = ifelse(rowSums(pet.int.perc, na.rm=TRUE) >= 1, 1, 0)) %>%
  select(pet.int.perc)
# pet.int.perc = rowSums(pet.int.perc, na.rm=TRUE)  
# pet.int.perc = pet.int.perc %>% 
#   ifelse(rowSums(pet.int.perc>= 1), 1, 0)
# 
# pet.int.perc = pet.int.perc %>%
#   mutate_at(paste(risk.outdoor.cat:risk.other), ifelse(. == 1, 1, 0))


## Create new column of all low pet-coyote int
pet.int.low = pet.int %>%
  select(matches("encounter.")) 

pet.int.low = pet.int.low %>% 
  mutate(pet.int.low = ifelse(rowSums(pet.int.low, na.rm=TRUE) >= 1, 1, 0)) %>%
  select(pet.int.low)

## Create new column of all high pet-coyote int
pet.int.high = pet.int %>%
  select(matches("attack.|kill.")) 

pet.int.high = pet.int.high %>% 
  mutate(pet.int.high = ifelse(rowSums(pet.int.high, na.rm=TRUE) >= 1, 1, 0)) %>%
  select(pet.int.high)

# can use mutate_at (for all columns, replace yes with 1, else 0) (for serveral columns not combining)
# pet.livestock.perc.risk = pet.livestock.perc.risk %<>% rowwise() %>%
#   ifelse(pet.livestock.perc.risk == 1,1,0)
# livestock.percieved.risk = livestock.percieved.risk %>%
#   mutate_at(paste(risk.outdoor.cat:risk.other), ifelse(. == "Yes", 1, 0))
# ?ifelse
# pet.interaction <- coyote %>% 
#   group_by(ID) %>% 
#   summarize(pet, pet) %>% 
#   arrange(ID)
# livestock.percieved.risk = livestock.percieved.risk %>%
#   mutate_at(paste(risk.outdoor.cat:risk.other), ifelse(. == "Yes", 1, 0))
# livestock.percieved.risk = livestock.percieved.risk %>%
#   mutate(livestock.percieved.risk = recode(livestock.percieved.risk, "No" = 0, "Yes" = 1))



## Now let's deal with livestock-coyote interactions----------------------------
## Create new column for having had any livestock-coyote interaction
pet.int = coyote %>%
  select(risk.outdoor.cat:kill.pet) %>% # select all columns about pet interactions with coyotes
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
pet.int = pet.int %>%
  mutate_all(~case_when(
    . == "Yes" ~ 1,
    . == "No" ~ 0))

# livestock.int = coyote %>%
#   select(matches("risk.|encounter.|attack.|missing.|kill.")) %>%
#   select(-c(1:22))
# 
# # Change livestock.int from yes/no, to 1/0
# livestock.int = livestock.int %>%
#   mutate_all(~case_when(
#     . == "Yes" ~ 1,
#     . == "No" ~ 0))
          
## Create new column of all percieved livestock-coyote int
livestock.int.perc = livestock.int %>%
  select(matches("risk.|missing.")) 
## Turn column in 1/0 if percieved livestock-coyote int occured (1 = Yes, 0 = NA or No)--This will be a later problem, I want NA's to stay NA's***
livestock.int.perc = livestock.int.perc %>% 
  mutate(livestock.int.perc = ifelse(rowSums(livestock.int.perc, na.rm=TRUE) >= 1, 1, 0)) %>%
  select(livestock.int.perc)
         
## Create new column of all low livestock-coyote int
livestock.int.low = livestock.int %>%
  select(matches("encounter.")) 
livestock.int.low = livestock.int.low %>% 
  mutate(livestock.int.low = ifelse(rowSums(livestock.int.low, na.rm=TRUE) >= 1, 1, 0)) %>%
  select(livestock.int.low)

## Create new column of all high livestock-coyote int
livestock.int.high = livestock.int %>%
  select(matches("attack.|kill.")) 
livestock.int.high = livestock.int.high %>% 
  mutate(livestock.int.high = ifelse(rowSums(livestock.int.high, na.rm=TRUE) >= 1, 1, 0)) %>%
  select(livestock.int.high)


# Lets focus on Peoples Thoughts on coyotes, e.g. ranking of benefits and risks---------------------------------
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

# These are now leveled so that the heigher the level number, the more value ppl
# have of coyotes


# Now we need to create a table of these columns for Cronbach's alpha test
# Unclass likert scales to be numeric
coyote.values = coyote %>%
  select(c(important.ecosystem, beneficial.humans, beneficial.wildlife, important.hunters,
           risk.humans, risk.pets, negative.wildlife, diseases, ID)) %>% # This ID should be fine to unclass right? They are still indiv. #?
  mutate_at(c("important.ecosystem", "beneficial.humans", "beneficial.wildlife", "important.hunters",
               "risk.humans", "risk.pets", "negative.wildlife", "diseases"), ~unclass(.))
#  mutate_all(~unclass(.))
# coyote.values = coyote %>%
#   select(c(important.ecosystem, beneficial.humans, beneficial.wildlife, important.hunters,
#            risk.humans, risk.pets, negative.wildlife, diseases, ID)) 
            
psych::alpha(values)
# Our raw alpha is .84, values > .7 indicate good reliability so it appears our
# questions are well coorelated to represent value of coyotes. The question that 
# appears most out of place is 'important.hunters'. Perhaps this is b/c ppl who values 
# coyotes do not consider them game species. We could consider removing this variables to
# increase our alpha to .87, although .84 should be sufficient in data < 25 items

# Lets take the median for each person or ID
# coyote.values = coyote.values %>% rowwise() %>%
#   mutate(values.median = median(c(important.ecosystem, beneficial.humans, beneficial.wildlife, important.hunters, risk.humans,        
#                         risk.pets, negative.wildlife, diseases)), na.rm = TRUE) %>%
#   add_column()
# OK lets do the median for values and risks
# coyote.values.only = coyote.values %>% select(important.ecosystem, beneficial.humans, beneficial.wildlife, important.hunters) %>% rowwise() %>%
#   mutate(values.median = median(c(important.ecosystem, beneficial.humans, beneficial.wildlife, important.hunters)), na.rm = TRUE)
# 
# coyote.risk.only = coyote.values %>% select(risk.humans, risk.pets, negative.wildlife, diseases) %>% rowwise() %>%
#   mutate(risk.median = median(c(risk.humans, risk.pets, negative.wildlife, diseases)), na.rm = TRUE)
# 
# values.plot = tibble(coyote.risk.only,coyote.values.only)

# Knopff et al. 2016 Methods------------------------------------------------------------------------------------------
# Lets take the mean of coyote value as done in Knopff et al. 2016 (we think this is not a statistically sound analysis--but lets see...)
coyote.values = coyote.values %>% rowwise() %>%
 mutate(values.mean = mean(c(important.ecosystem, beneficial.humans, beneficial.wildlife, important.hunters)), na.rm = TRUE)


# Moving on to the ordinal logistic regression, let's get our response variable married to our predictors of interest
ord.reg = coyote %>% select(education) %>%
  tibble(.,pet.int.high, pet.int.low, coyote.values)

install.packages("MASS")
library(MASS)

ord.reg.1 = polr(important.ecosystem~education, data = ord.reg)
summary(ord.reg.1)

ord.reg.2 = polr(risk.pets~pet.int.high, data = ord.reg)
summary(ord.reg.2)

ggplot()
# Lets check out fear and relevel------------------------------------------------------------------------------

coyote <- coyote  %>%
  mutate(fearful = fct_relevel(fearful, c("Strongly disagree", "Disagree", "Neither agree nor disagree",  "Agree", "Strongly Agree")))

plot(coyote$fearful)

# Population of Coyotes----------------------------------------------------------------------------------------
# View on coyote population, these levels are not necessarily representative but we can focus on different levels for different questions
coyote <- coyote  %>%
  mutate(coyote.population = fct_relevel(coyote.population, c("Too many", "Just right", "I do not have an opinion",  "I do not know", "Too few")))

# Knowledge of Coyotes-----------------------------------------------------------------------------------------
# Lets score participants by summing the correct reponses to questions about coyotes
# First lets tell R whats correctly true and false 
coyote = coyote %>% mutate(native = ifelse(str_detect(native,"TRUE"),1,0)) # Correct answer is TRUE
coyote = coyote %>% mutate(ri.past.10.years = ifelse(str_detect(ri.past.10.years,"FALSE"),1,0)) # Correct answer is FALSE
coyote = coyote %>% mutate(litter.food = ifelse(str_detect(litter.food,"TRUE"),1,0)) # Correct answer is TRUE
coyote = coyote %>% mutate(nocturnal = ifelse(str_detect(nocturnal,"FALSE"),1,0)) # Correct answer is FALSE
coyote = coyote %>% mutate(carnivores = ifelse(str_detect(carnivores,"FALSE"),1,0)) # Correct answer is FALSE
coyote = coyote %>% mutate(human.rabies.year = ifelse(str_detect(human.rabies.year,"FALSE"),1,0)) # Correct answer is FALSE
coyote = coyote %>% mutate(furbearers = ifelse(str_detect(furbearers,"TRUE"),1,0)) # Correct answer is TRUE

match("native", names(coyote)) #44
match("furbearers", names(coyote)) #50

 # trying to summaize specific columns by for all rows in certain column names
#coyote = coyote %>% mutate(knowledge.score = ifelse(risk.large.dog == 1 | missing.large.dog == 1, 1, 0))
###This is for creating new column of multi column

coyote2 = coyote %>%
  mutate(knowledge.score = rowSums(select(.,native:furbearers), na.rm=TRUE))

?grep
livestock.equine = ifelse(str_detect(livestock.text,"(?i)horse|donkey"),1,0)

# Going to explore some data related to coyote values---------------------------
education = unclass(coyote$education)
test = tibble(values, education)
chisq.test(values, unclass(coyote$education))
hist(values)

# Summarize coyote values into one column which is the sum of appropriate cols
values = values %>% 
  mutate(values = rowSums(values, na.rm=TRUE)) %>%
  select(values)



# Values vs. Education Analysis-------------------------------------------------
# ?polr
# library(MASS)
# education = coyote %>% select(c(education, ID))
# coyote.values = coyote.values %>% left_join(education) %>%
#   mutate_at("values.median", ~as.factor(.))
# polr(values.median~education, data = coyote.values)


# Lets create a table for ordered data of important.eco value of coyotes and human education 
val.eco = coyote %>% select(c(important.ecosystem, education)) %>%
  mutate_all(~unclass(.))
#val.eco = coyote %>% tabyl(important.ecosystem, education)
cor(val.eco, method="kendall", use="pairwise")
# kendalls tau = .064 indicating that there is no relationship between rankings

eco.ed = cor.test(val.eco$important.ecosystem, val.eco$education, method="kendall")
eco.ed$estimate
eco.ed$p.value
# p < .05, p = .04449, this value indicates that we cannot reject the null 
# and our kendall tau value is statistically significant

#correlate(important.ecosystem, education, data = val.eco, method = "kendall")

# imporant.hunt and ed
val.hunt = coyote %>% select(c(important.hunters, education)) %>%
  mutate_all(~unclass(.))
#val.eco = coyote %>% tabyl(important.ecosystem, education)
cor(val.hunt, method="kendall", use="pairwise")
# kendalls tau = .064 indicating that there is no relationship between rankings

hunt.ed = cor.test(val.eco$important.ecosystem, val.eco$education, method="kendall")
hunt.ed$estimate
hunt.ed$p.value

# imporant.hum and ed
val.hum = coyote %>% select(c(beneficial.humans, education)) %>%
  mutate_all(~unclass(.))
#val.eco = coyote %>% tabyl(important.ecosystem, education)
cor(val.hum, method="kendall", use="pairwise")
# kendalls tau = .064 indicating that there is no relationship between rankings

hunt.ed = cor.test(val.eco$, val.eco$education, method="kendall")
hunt.ed$estimate
hunt.ed$p.value

