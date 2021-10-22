## Coyote Survey ##

# dependent variables of interest
# 1) value
# 2) interactions
#  a. sighting
#  b. incidents


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
setwd("C:/Users/karivera/Documents/Rivera_K/Coyote_Models/Data")
coyote_raw = read_csv("Coyote Questionnaire_final_dataset.csv") # OG survey data

# Organize data-----------------------------------------------------------------
coyote <- coyote_raw %>% mutate(date = round_date(mdy_hm(`End Date`))) %>% 
  select(-c(1,2,4,7,8,10:13, 16, 17, 158:162)) 
#slice(-(1:12)) 
coyote<- coyote[-c(1:12),] # 'Official' survey started Oct 6

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
                      "nocturnal","carnivores","human.rabies.year","furbearers","NAMWC","place.coyotes",
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
# start with survey.type and flag 'Survey Preview' and 'Spam'
coyote = coyote %>% mutate(flagged = ifelse(response.type == "Spam", 1,
                                            ifelse(response.type == "Survey Preview", 1, 0)))
coyote = filter(coyote, flagged != 1)
# Participant Demographics------------------------------------------------------

## Age --------------------------------------------------------------------------
## Check Out Distribution
ggplot(coyote, aes(x=age)) +
  geom_histogram()

# Hmm, I believe it is unlikely a participant is over 110 y/o. Either this was entered wrong or perhaps the participant doesn't want 
# to share their age or did not take this survey seriously. Let's look at the data point and investigate other responses
# I'm going to be using exam as a place holder for data I am examining

# We will therefor change age unrealistic ages to NA
coyote <- coyote %>% mutate(age = na_if(age, 120))

## Gender-----------------------------------------------------------------------
levels(coyote$gender)

# Let's convert unusable entries to NA
coyote <- coyote %>% mutate(gender = recode_factor(gender, "Apache attack helicopter" = NA_character_)) %>% 
  mutate(gender = recode_factor(gender, "mmm, no." = NA_character_)) %>% 
  mutate(gender = recode_factor(gender, "I don't believe in gender" = NA_character_)) %>%
  mutate(gender = recode_factor(gender, "Bi curious Buffalo." = NA_character_))

## collapse and correct level labels
coyote <- coyote %>% mutate(gender = fct_collapse(gender, 
                                                  male = c("m", "M", "Male", "boy", "MALE","Make", "Dude","cisgender male",
                                                           "male.  However it is interesting that the previous question offered selections but this question did not.",
                                                           "Man", "masculine", "male"),
                                                  female = c("Femail", "More or less female", "femal", "woman", "Woman", "F", "f", "Female", "female", "is that still a thing? female"),
                                                  non.binary = c("Female, non-binary/fluid" ,"non binary", "Genderqueer/Nonbinary", "Non-Binary","Non-binary","non-binary")))
levels(coyote$gender)

## Check Out Distribution
ggplot(coyote, aes(x= gender)) +
  geom_bar()

length(which(coyote$gender=="male")) # 324
(324/971)*100 # 33.37%
length(which(coyote$gender=="female")) #451
(451/971)*100 # 46.45%
length(which(coyote$gender=="non.binary")) #7 # look into how to report non binary population
(7/971)*100 # .72%
length(which(is.na(coyote$gender))) # 189

sum(324+451+7+189) #971
# Actual RI %'s: 
# Pop estimate 2019 = 1,059,361 OR 1,057,231 based on county totals
# female = 51.3%
119/1059361


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

# STATS to RI POP
length(which(coyote$education=="Elementary School")) #1
length(which(coyote$education=="Middle School")) #0

length(which(coyote$education=="High School")) #88
length(which(coyote$education=="Technical or vocational degree")) # 32
length(which(coyote$education=="2-year college degree or certificate")) # 89
length(which(coyote$education=="4-year college Degree")) # 274
length(which(coyote$education=="Graduate Degree")) # 321
length(which(is.na(coyote$education))) # 166
sum(88+32+89+274+321+166)

sum(88+32+89+274+321) # 804: Highschool or higher
(804/971)*100 # 82.80%
sum(274+321+166) # 761: Bachelors or higher 
(761/971)*100 # 78.37%

# Actual RI %'s: 
# Pop estimate 2019 = 1,059,361 OR 1,057,231 based on county totals
# https://www.census.gov/quickfacts/RI
# https://www.rhodeisland-demographics.com/counties_by_population
#
# Highschool or higher = 88.8%
# Bachelors of higher = 34.2%
# pop total = 1,059,361



## Household Income-------------------------------------------------------------
# Turn into factor
coyote <- coyote %>% mutate(household.income= factor(household.income))

## Organize levels
coyote <- coyote %>% mutate(household.income = factor(household.income)) %>%
  mutate(household.income = fct_relevel(household.income, c("Under $15,000", "Between $15,000 and $29,999","Between $30,000 and $49,999",
                                                            "Between $50,000 and $74,999","Between $75,000 and $99,999","Between $100,000 and $149,999","Between $150,000 and $199,999", "$200,000 or more" )))

ggplot(coyote, aes(household.income, fill = household.income)) +
  geom_bar() +
  labs(x = "Household Income", y = "Counts", title = "Household Income RI") + 
  theme_minimal() +
  theme(legend.position="none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(family = "LM Roman 10 Bold", size=15)) 

# STATS to RI POP
length(which(coyote$household.income=="Under $15,000")) #17
length(which(coyote$household.income=="Between $15,000 and $29,999")) # 49
length(which(coyote$household.income=="Between $30,000 and $49,999")) # 64
length(which(coyote$household.income=="Between $50,000 and $74,999")) # 109
length(which(coyote$household.income=="Between $75,000 and $99,999")) # 141
length(which(coyote$household.income=="Between $100,000 and $149,999")) # 176
length(which(coyote$household.income=="Between $150,000 and $199,999")) # 101
length(which(coyote$household.income=="$200,000 or more")) # 72
length(which(is.na(coyote$household.income))) # 242
sum(17+49+64+109+141+176+101+72+242) # 971
(971/2)-242 # 243.5
sum(17+49+64+109)
sum(17+49+64+109+141) # So based on the this...I think the median income is btw 75,000 and 99,999

# Actual RI %'s: 
# Pop estimate 2019 = 1,059,361 OR 1,057,231 based on county totals
# Median household income 2015-2019 = 67,167


## Ethnic Group-----------------------------------------------------------------
levels(factor(coyote$ethnicity))
ggplot(coyote, aes(x=ethnicity)) +
  geom_bar()

# Not sure fo the best way to aggregate these...
exam = filter(coyote, ethnicity == "Asian,Other (please specify)")
exam = filter(coyote, ethnicity == "Native American,Other (please specify)")
exam = filter(coyote, ethnicity == "White/ Caucasian,Other (please specify)")
exam = filter(coyote, ethnicity == "Other (please specify)")
exam = filter(coyote, ethnicity == "White/ Caucasian") #754  

coyote <- coyote %>% 
  mutate(ethnicity.african.american = ifelse(str_detect(ethnicity,"African American"),1,0),
         ethnicity.asian = ifelse(str_detect(ethnicity,"Asian"),1,0),
         ethnicity.white = ifelse(str_detect(ethnicity,"White/ Caucasian"),1,0),
         ethnicity.native.american = ifelse(str_detect(ethnicity,"Native American"),1,0),
         ethnicity.islander = ifelse(str_detect(ethnicity,"Hawaiian or Pacific Islander"),1,0),
         ethnicity.hispanic = ifelse(str_detect(ethnicity,"Hispanic"),1,0))

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

sum(coyote$ethnicity.white, na.rm = TRUE) 

# Drop the old longer columns, we now have wider columns
coyote = coyote %>% 
  select(-c(ethnicity,ethnicity.text))

## Length of stay in RI for non-residents---------------------------------------

#convert factor to string then to a numeric once everything is a number
class(coyote$months.RI.text)

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

coyote %>% 
  ggplot(aes(x = months.RI)) + 
  geom_histogram(color="darkblue", fill="lightblue") +
  labs(title="Months in RI",x="Months", y="Counts")+
  theme_minimal() + 
  theme(legend.position="none",
        text = element_text(family = "LM Roman 10 Bold", size=15)) 

## Participant Location---------------------------------------------------------

# Residency
coyote %>% 
  ggplot(aes(x = resident, fill = resident)) + 
  geom_bar() + 
  labs(x = "Residency", y = "Counts", title = "Counts of residents vs. non-residents") + 
  theme_minimal() + 
  theme(legend.position="none",
        text = element_text(family = "LM Roman 10 Bold", size=15)) 

## Years of Residency (if resident)---------------------------------------------
summary(coyote$years.residency)

coyote %>% 
  ggplot(aes(x = years.residency)) + 
  geom_histogram(color="darkblue", fill="lightblue") +
  labs(title="Years of residency",x="Years", y="Counts")+
  theme_minimal() + 
  theme(legend.position="none",
        text = element_text(family = "LM Roman 10 Bold", size=15))

# Is years or residency related to age?
fit = lm(years.residency ~ age, data = coyote)
summary(fit)

plot(years.residency ~ age, data = coyote)
abline(fit)

ggplot(coyote, aes(x = age, y = years.residency)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")
# Something is weird here... why is there a perfect linear relationship for some participants
exam = select(coyote, c(age, years.residency))
# I think this is because residents have lives in RI the same as their age (since they have been born)
# and you can't be a resident longer than you've been alive!


## Non-residents----------------------------------------------------------------
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

exam = coyote %>% select(not.resident, not.resident.others,not.resident.family, not.resident.prev.student, not.resident.prev.resident, not.resident.work,not.resident.student,not.resident.rent, not.resident.vacation, not.resident.tourist)

# Flag participants that have not spent a significant amount of time in RI
coyote <- coyote %>% 
  mutate(flagged = case_when(not.resident.others == "Living at Home in MA" ~ 1,
                             TRUE ~ as.numeric(flagged)))

# filter out flagged data again
coyote = filter(coyote, flagged != 1)

coyote = coyote %>% 
  select(-c(not.resident.others))

coyote %>% 
  drop_na(not.resident) %>% 
  ggplot(aes(x = not.resident, fill = not.resident)) + 
  geom_bar() + 
  labs(x = "Reason", y = "Counts", title = "Counts per non-residency reason") + 
  theme_minimal() + 
  theme(legend.position="none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(family = "LM Roman 10 Bold", size=15))

## County-----------------------------------------------------------------------
levels(factor(coyote$county))

coyote <- coyote %>% mutate(county = case_when(county == "I do not wish to share this information" ~ "Not_shared",
                                               TRUE ~ as.character(county))) #remember when you use case when, other goes to NA unless stated otherwise


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

# I wonder if there is a relationship between income and county?
coyote %>% 
  mutate(county = factor(county, levels = levels)) %>% 
  ggplot(aes(x = county, fill = household.income)) + 
  geom_bar() + 
  labs(x = "County", y = "Counts", title = "Counts per county") + 
  theme_minimal() 
# Hmm, nothing strikingly obvious, would have to investigate this more


length(which(coyote$county=="Newport")) # 143 
(143/971)*100 #14.73% (over represented)
length(which(coyote$county=="Kent")) # 94
(94/971)*100 #9.68% (under represented)
length(which(coyote$county=="Washington")) #293
(293/971)*100 #30.18% (over represented)
length(which(coyote$county=="Bristol")) # 63
(63/971)*100 #6.49% (slightly over represented)
length(which(coyote$county=="Providence")) # 270
(270/971)*100 #27.81% (under represented)
length(which(coyote$county=="Not_shared")) # 15
(15/971)*100 #1.54% 
length(which(is.na(coyote$county))) # 93
(93/971)*100 #9.58%
sum(143+94+293+63+270+15+93) # Good this is our total sum of 'filtered' participants

# Actual RI %'s: 
# Pop estimate 2019 = 1,059,361
# https://www.census.gov/quickfacts/RI
# https://www.rhodeisland-demographics.com/counties_by_population
#
# Newport: 82,801 
(82801/1057231)*100 # 7.83% 
# Kent: 163,869
(163869/1057231)*100 # 15.50% 
# Providence: 635,737 
(635737/1057231)*100 # 60.13% ()
# Washington: 126,060
(126060/1057231)*100 # 11.92%
# Bristol: 48,764
(48764/1057231)*100 # 4.61%

82801+163869+635737+126060+48764 # 1,057,231 WE WILL USE THIS IS THE POP TOTAL



## Town-------------------------------------------------------------------------

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
# There is a lot of varied distribution for towns, probably best not to use this as a 
# potential predcitor variable 

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
         outdoor.photo_film = ifelse(str_detect(outdoor.text,"(?i)photo|film"),1,0)) %>%
  select(-outdoor.text)

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

# It may be interested to relate these activities to people's relationship with nature

# Relationship with nature------------------------------------------------------
# Circles are coded so that the increasing number is a higher relationships ship with nature
# e.g. 1 = lowest, 6 = closest relationship
coyote = coyote %>% mutate(image.relation.nature= factor(image.relation.nature))
view = coyote %>% select(image.relation.nature)
levels(coyote$image.relation.nature)

# Change pet.int from yes/no, to 1/0
nature.person = coyote %>% select(nature.person) %>%
  mutate_all(~case_when(
    . == "Yes" ~ 1,
    . == "No" ~ 0))

# How do I interpret binary and capped numeric data?
# Based on a glm model, it looks like these binary data are only useful in predicting 
# nature relationships levels 4, 5, and 6 

coyote %>% 
  ggplot(aes(x = nature.person, fill = image.relation.nature)) + 
  geom_bar() 
# This shows us that people who consider themselves a nature person most
# scored themselves a 3 or above on the factorial relationship scale, 
# those who chose no, are more varied with all options having been chosen, most commonly
# levels 2 and 3

model <- glm(nature.person ~ image.relation.nature, data = coyote, family = "binomial")
summary(model)
# looks like only level 4,5, and 6 have a significant influence on the outcome 
# being successful e.g. a person is nature oriented
# https://stats.idre.ucla.edu/r/dae/logit-regression/


# Now let's compare this to the 
# New Ecological Paradigm results-----------------------------------------------
# Increasing environmental value goes from strongly disagree [1] -> strongly agree [5]
# Some are reverse coded based on which way value moves e.g. strongly agree [1] -> strongly disagree [5]
levels(coyote$take.care)
class(coyote$take.care)

coyote <- coyote %>% 
  mutate(take.care = fct_relevel(take.care, c("Strongly disagree", "Neither agree nor disagree",  "Agree", "Strongly agree")))
coyote <- coyote  %>%
  mutate(not.to.worry = fct_relevel(not.to.worry, c("Strongly agree", "Agree", "Neither agree nor disagree", "Disagree", "Strongly disagree")))
coyote <- coyote  %>%
  mutate(human.ingenuity = fct_relevel(human.ingenuity, c("Strongly agree", "Agree", "Neither agree nor disagree", "Disagree", "Strongly disagree")))
coyote <- coyote  %>%
  mutate(plenty.resources = fct_relevel(plenty.resources, c("Strongly agree", "Agree", "Neither agree nor disagree", "Disagree", "Strongly disagree")))
coyote <- coyote  %>%
  mutate(equal.rights = fct_relevel(equal.rights, c("Strongly disagree", "Disagree", "Neither agree nor disagree",  "Agree", "Strongly agree")))
coyote <- coyote  %>%
  mutate(laws.of.nature = fct_relevel(laws.of.nature, c("Strongly disagree", "Disagree", "Neither agree nor disagree",  "Agree", "Strongly agree")))
coyote <- coyote  %>%
  mutate(humans.rule = fct_relevel(humans.rule, c("Strongly agree", "Agree", "Neither agree nor disagree", "Disagree", "Strongly disagree")))
coyote <- coyote  %>%
  mutate(delicate.balance = fct_relevel(delicate.balance, c("Strongly disagree", "Disagree", "Neither agree nor disagree",  "Agree", "Strongly agree")))
coyote <- coyote  %>%
  mutate(envt.cata = fct_relevel(envt.cata, c("Strongly disagree", "Disagree", "Neither agree nor disagree",  "Agree", "Strongly agree")))

# Cronbach's alpha test for environmental values--------------------------------
# Unclass likert scales to be numeric
envt.concern = coyote %>%
  select(c(take.care, not.to.worry, human.ingenuity, plenty.resources,
           equal.rights, laws.of.nature, humans.rule, delicate.balance, envt.cata)) %>%
  mutate_at(c("take.care", "not.to.worry", "human.ingenuity", "plenty.resources",
              "equal.rights", "laws.of.nature", "humans.rule", "delicate.balance","envt.cata"), ~unclass(.))

# This test will tell us how well items here correlate
# ideally want an alpha coefficient of 0.7 or higher for short lists of items 
psych::alpha(envt.concern)
# It looks like we have an acceptable alpha but just so, we should considering using PCA as done by Amburgey et al., 2012
pca_envt.concern <- envt.concern %>%
  select(where(is.numeric)) %>%
  drop_na() %>%
  scale() %>%
  prcomp()

summary(pca_envt.concern) # looks like PC1 only explains 50%, it seems we need to include more dimensions
# PC2 captures 16.7%
# *Kaiser criterion = If the eigenvalues are > 1, retain that component
# sum of eigenvalues = total variance 
# https://www.youtube.com/watch?v=oZ2nfIPdvjY
eigenvalues = pca_envt.concern$sdev ^2 # This shows we should consider the first three dimensions
screeplot(pca_envt.concern, main = "Screen Plot", xlab = "Components")
pca_loadings = pca_envt.concern$rotation # indicate extend each variables is correlated with each component

# We have three key dimensions, the first considers all questions,
# the second: -c(humans.rule, not.to.worry)
# the third: -c(human.ingenuity, plenty.resources, envt.cata)


envt.concern1 = coyote %>%
  select(c(take.care, not.to.worry, human.ingenuity, plenty.resources,
           equal.rights, laws.of.nature, humans.rule, delicate.balance, envt.cata, ID)) %>%
  mutate_at(c("take.care", "not.to.worry", "human.ingenuity", "plenty.resources",
              "equal.rights", "laws.of.nature", "humans.rule", "delicate.balance","envt.cata"), ~unclass(.))

envt.concern1 = envt.concern1 %>% mutate(envt.concern1.sum = rowSums(envt.concern1[,1:9], na.rm = TRUE))

envt.concern2 = coyote %>%
  select(c(take.care, human.ingenuity, plenty.resources,
           equal.rights, laws.of.nature, delicate.balance, envt.cata, ID)) %>%
  mutate_at(c("take.care", "human.ingenuity", "plenty.resources",
              "equal.rights", "laws.of.nature", "delicate.balance","envt.cata"), ~unclass(.))
envt.concern2 = envt.concern2 %>% mutate(envt.concern2.sum = rowSums(envt.concern2[,1:7], na.rm = TRUE))


envt.concern3 = coyote %>%
  select(c(take.care, not.to.worry,
           equal.rights, laws.of.nature, humans.rule, delicate.balance, ID)) %>%
  mutate_at(c("take.care", "not.to.worry",
              "equal.rights", "laws.of.nature", "humans.rule", "delicate.balance"), ~unclass(.))

envt.concern3 = envt.concern3 %>% mutate(envt.concern3.sum = rowSums(envt.concern1[,1:6], na.rm = TRUE))


coyote = envt.concern1 %>% select(ID, envt.concern1.sum) %>%
  left_join(coyote, envt.concern1, by = "ID")
coyote = envt.concern2 %>% select(ID, envt.concern2.sum) %>%
  left_join(coyote, envt.concern2, by = "ID")
coyote = envt.concern3 %>% select(ID, envt.concern3.sum) %>%
  left_join(coyote, envt.concern3, by = "ID")


model <- glm(envt.concern1.sum ~ image.relation.nature, family = poisson(link = log), data = coyote)
summary(model)

# Wilkinson-Rogers Method
envt.concern1.fail = coyote %>% select(ID, envt.concern1.sum) %>%
  mutate(envt.concern1.sum.fail = 45-envt.concern1.sum) %>%
  select(-envt.concern1.sum)

coyote = envt.concern1.fail %>% select(ID, envt.concern1.sum.fail) %>%
  left_join(coyote, envt.concern1.fail, by = "ID")

model = glm(formula = cbind(envt.concern1.sum, envt.concern1.sum.fail) ~ image.relation.nature, family = binomial, data = coyote)
summary(model)
plot(model)
ggplot(aes(x=envt.concern1.sum), y = image.relation.nature)

# no constrant in the model
# maybe think about as a porportion (logistic regression--use glm binomial)
# each number is success, diff is failure 
# see website

coyote %>%
  ggplot(aes(x = image.relation.nature, fill = envt.concern1.sum)) + 
  geom_bar()

coyote %>% 
  ggplot(aes(x = nature.person, fill = image.relation.nature)) + 
  geom_bar()

# Pet Demographics--------------------------------------------------------------
## Lets check out population of pet owners
ggplot(coyote, aes(x = pet.owner, color = pet)) +
  geom_bar(fill="white")

## correct non-informative levels to NAs
coyote <- coyote %>% mutate(pet = recode_factor(pet, "Tamed Coyote" = NA_character_)) %>% 
  mutate(pet = recode_factor(pet, "Two" = NA_character_)) %>% 
  mutate(pet = recode_factor(pet, "I have 9 indoor pets of various sizes and species" = NA_character_))%>% 
  mutate(pet = recode_factor(pet, "All indoors and never unattended" = NA_character_))

## Separate multiple selections
coyote <- coyote %>% 
  mutate(pet.outdoor.cat = ifelse(str_detect(pet,"(?i)Outdoor.*Cat"),1,0),
         pet.small.dog = ifelse(str_detect(pet,"(?i)Small.*dog"),1,0),
         pet.lrg.dog = ifelse(str_detect(pet,"Large.*dog"),1,0))

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

ggplot(coyote, aes(x = livestock.owner, color = livestock)) +
  geom_bar(fill="white")

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
         livestock.shellfish = ifelse(str_detect(livestock.text, "(?i)quoahogs"),1,0),
         livestock.rabbit = ifelse(str_detect(livestock.text, "(?i)outdoor.*rabbits|rabbits"),1,0))

levels <- coyote %>% 
  pivot_longer(livestock.cattle:livestock.shellfish, names_to="index",values_to="value") %>% 
  filter(value == 1) %>% 
  group_by(index) %>% 
  summarize(count=n()) %>% 
  arrange(count) %>% 
  select(index) %>% 
  as_vector()

coyote %>% 
  pivot_longer(livestock.cattle:livestock.shellfish, names_to="index",values_to="value") %>% 
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

# Pet-Coyote Interactions-------------------------------------------------------
## Create new column for having had any pet-coyote interaction
coyote = coyote %>% 
  mutate(coyote,
         pet.owner = if_else(pet.owner == "Yes", 1, 0)) 

pet.int = coyote %>% 
  select(c(matches("risk.|encounter.|attack.|missing.|kill."))) %>%
  select(-c(1:2, 23:length(.))) %>%
  mutate_all(~case_when(
    . == "Yes" ~ 1,
    . == "No" ~ 0))

pet.int = coyote %>%
  select(c(matches("risk.|encounter.|attack.|missing.|kill."))) %>%
  select(-c(1:2, 23:length(.)))

# Change pet.int from yes/no, to 1/0
pet.int = pet.int %>%
  mutate_all(~case_when(
    . == "Yes" ~ 1,
    . == "No" ~ 0))

## Create new column of all percieved pet-coyote int
pet.int.perc = pet.int %>%
  select(c(matches("risk.|missing.")))

## Turn column in 1/0 if percieved pet-coyote int occured (1 = Yes, 0 = NA or No)--This will be a later problem, I want NA's to stay NA's***
pet.int.perc = pet.int.perc %>% 
  mutate(pet.int.perc = ifelse(rowSums(pet.int.perc, na.rm=TRUE) >= 1, 1, 0)) %>%
  select(pet.int.perc)

## Create new column of all low pet-coyote int
pet.int.low = pet.int %>%
  select(matches("encounter.")) 

pet.int.low = pet.int.low %>% 
  mutate(pet.int.low = ifelse(rowSums(pet.int.low, na.rm=TRUE) >= 1, 1, 0)) %>%
  select(pet.int.low)

## Create new column of all high pet-coyote int and keep in coyote dataset
#We want to add this column to the coyote dataset so we can use this as an indp variable
pet.int.high = pet.int.high %>% 
  mutate(pet.int.high = ifelse(rowSums(pet.int.high, na.rm=TRUE) >= 1, 1, 0)) %>%
  select(pet.int.high)

# Problem here, need to translate yes and no to 1,0 then summarize all by 1, 0
# animal.incident = coyote %>%
#   select(c(matches("attack.|kill."), ID)) %>%
#   mutate(ifelse(.[,1:20])
#     
#     ~case_when(
#     . == "Yes" ~ 1, 
#     . == "No" ~ 0,
#     TRUE ~ as.factor(.)))

animal.incident = coyote %>%
  select(c(matches("attack.|kill."), ID)) %>%
  mutate_if(sapply(., is.factor), as.character)

animal.incident = animal.incident %>%
  mutate_at(c("attack.outdoor.cat", "kill.outdoor.cat", "attack.large.dog", "kill.large.dog", "attack.small.dog",
              "kill.small.dog", "attack.pet", "kill.pet", "attack.goat", "kill.goat", "kill.sheep", "attack.sheep",
              "attack.poultry", "kill.poultry", "attack.pig", "kill.pig", "attack.cattle", "kill.cattle", 
              "kill.other", "attack.other"), 
            ~case_when(
              . == "Yes" ~ 1,
              . == "No" ~ 0))

animal.incident = animal.incident %>% 
  mutate(animal.incident = ifelse(rowSums(.[,1:20], na.rm=TRUE) >= 1, 1, 0)) %>%
  select(animal.incident, ID)

coyote = left_join(coyote, animal.incident, by = "ID")

table(coyote$animal.incident, coyote$animal.owner)
## Now let's deal with livestock-coyote interactions----------------------------
coyote = coyote %>% 
  mutate(coyote,
         livestock.owner = if_else(livestock.owner == "Yes", 1, 0)) 


livestock.int = coyote %>%
  select(matches("risk.|encounter.|attack.|missing.|kill.")) %>%
  select(-c(1:22))

# Change livestock.int from yes/no, to 1/0
livestock.int = livestock.int %>%
  mutate_all(~case_when(
    . == "Yes" ~ 1,
    . == "No" ~ 0))

## Create new column of all percieved livestock-coyote int
livestock.int.perc = livestock.int %>%
  select(matches("risk.|missing.")) 

## Turn column in 1/0 if perceived livestock-coyote int occurred (1 = Yes, 0 = NA or No)--This will be a later problem, I want NA's to stay NA's***
livestock.int.perc = livestock.int.perc %>% 
  mutate(livestock.int.perc = ifelse(rowSums(livestock.int.perc, na.rm=TRUE) >= 1, 1, 0)) %>%
  select(livestock.int.perc)

## Create new column of all low livestock-coyote int
livestock.int.low = livestock.int %>%
  select(matches("encounter.")) 
## Turn column in 1/0 if low int of livestock-coyote occurred 
livestock.int.low = livestock.int.low %>% 
  mutate(livestock.int.low = ifelse(rowSums(livestock.int.low, na.rm=TRUE) >= 1, 1, 0)) %>%
  select(livestock.int.low)

## Create new column of all high livestock-coyote int
livestock.int.high = livestock.int %>%
  select(matches("attack.|kill.")) 
## Turn column in 1/0 if high int of livestock-coyote occurred 
livestock.int.high = livestock.int.high %>% 
  mutate(livestock.int.high = ifelse(rowSums(livestock.int.high, na.rm=TRUE) >= 1, 1, 0)) %>%
  select(livestock.int.high)

# We will also create a column for animal ownership (either own pets or livestock or both)
animal.owner = coyote %>% select(ID, pet.owner, livestock.owner) %>% 
  mutate(animal.owner = ifelse(pet.owner == 1 | livestock.owner ==1, 1, 0)) %>%
  select(-c(pet.owner, livestock.owner))

coyote = left_join(coyote, animal.owner, by = "ID")

# Now we consider our dependent variables
# First Value of coyotes
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

# These are now leveled so that the higher the level number, the more value ppl
# have of coyotes


# Lets check out fear and relevel------------------------------------------------------------------------------

coyote <- coyote  %>%
  mutate(fearful = fct_relevel(fearful, c("Strongly disagree", "Disagree", "Neither agree nor disagree",  "Agree", "Strongly Agree")))

plot(coyote$fearful)
# How does fear influence various aspects of how people view coyotes?
# first let's bin those who 'agree' and those who 'disagree'
fear <- coyote %>%
  select(fearful, ID) %>%
  mutate(fear = case_when(
    fearful == "Strongly disagree" ~ 0,
    fearful == "Disagree" ~ 0,
    fearful == "Agree" ~ 1,
    fearful == "Strongly Agree" ~ 1)) %>% select(-fearful)

coyote = left_join(coyote, fear, by = "ID") #Remember you need to join on a unique variable

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

# This is a bit tricky as a majority of people skipped at least some knowledge questions. If we remove anyone with NA's
# we will have very little data. I presume that the majority of participants who skippd questions did not know the answer
# I also wonder if there was an error with the survey because I don't see any 'I don't know's in the raw data.
# I will say that if all columns are NA, it is a true NA and if some are NA, they are really "I don't knows'
knowledge.score = coyote %>% select(native:furbearers, ID) %>% 
  mutate(all.na = ifelse(rowSums(is.na(.[,1:7])) == 7,TRUE,FALSE)) %>%
  filter(., all.na == FALSE) %>%
  select(-all.na) 

# lets convert, those who answered true/false to some and NA to some will go from NA to 0
knowledge.score = knowledge.score %>% replace_na(list(native = 0, ri.past.10.years = 0, litter.food = 0, nocturnal = 0, carnivores = 0, human.rabies.year = 0, furbearers = 0))

# trying to summarize specific columns by for all rows in certain column names
knowledge.score = knowledge.score %>% mutate(knowledge.score = rowSums(.[,1:7], na.rm = TRUE)) %>% select(-c(native:furbearers))

coyote = left_join(coyote, knowledge.score, by = "ID")
exam = coyote %>% select(native:furbearers, ID, knowledge.score) # check this worked

# Let's save our data set and start analysing data elsewhere!-------------------

write.csv(coyote, file = "C:/Users/Kim Rivera/Documents/Coyotes/Questionnaire Data/live survey/r_data/coyote_cleaned.csv")


########################   STOP HERE FOR CLEANING   ############################

# Now let's look at analyzing data----------------------------------------------
# First let's focus on organizing and checking validity of coyote.value scales

# Let's first create a table of these columns for 
# Cronbach's alpha test---------------------------------------------------------
# Unclass likert scales to be numeric
coyote.values = coyote %>%
  select(c(important.ecosystem, beneficial.humans, beneficial.wildlife, important.hunters,
           risk.humans, risk.pets, negative.wildlife, diseases)) %>%
  mutate_at(c("important.ecosystem", "beneficial.humans", "beneficial.wildlife", "important.hunters",
              "risk.humans", "risk.pets", "negative.wildlife", "diseases"), ~unclass(.))

# This test will tell us how well items here correlate
# ideally want an alpha coefficient of 0.7 or higher for short lists of items 
psych::alpha(coyote.values)

# Our raw alpha is .84, values > .7 indicate good reliability so it appears our
# questions are well correlated to represent value of coyotes. The question that 
# appears most out of place is 'important.hunters'. Perhaps this is b/c ppl who values 
# coyotes do not consider them game species. We could consider removing this variables to
# increase our alpha to .87, although .84 should be sufficient in data < 25 items

# Principal Component Analysis for Values---------------------------------------
# as recommended by Amburgey et al., 2012
# https://clauswilke.com/blog/2020/09/07/pca-tidyverse-style/
# https://towardsdatascience.com/understanding-pca-fae3e243731d
# https://online.stat.psu.edu/stat505/lesson/11/11.4
# https://personality-project.org/r/psych/help/principal.html
# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/

library(broom)
library(cowplot)
library(factoextra)

pca_fit <- coyote.values %>%
  select(where(is.numeric)) %>%
  drop_na() %>%
  scale() %>%
  prcomp()

summary(pca_fit) # looks like PC1 only explains 50%, it seems we need to include more dimensions
# PC2 captures 16.7%
# *Kaiser criterion = If the eigenvalues are > 1, retain that component
# sum of eigenvalues = total variance 
# https://www.youtube.com/watch?v=oZ2nfIPdvjY
eigenvalues = pca_fit$sdev ^2
screeplot(pca_fit, main = "Screen Plot", xlab = "Components")
pca_loadings = pca_fit$rotation # indicate extend each variables is correlated with each component


# Ordinal Regression------------------------------------------------------------
# we need to conduct two ordinal regressions based on PCA
# Model 1: all variables
# Model 2: all variables except negative.wildlife (coyotes negatively impact other wildlife in RI)
# Model 3* We could consider this as important.hunters seems to have a completely different relationship 
# to value to other variables
# Moving on to the ordinal logistic regression, let's get our response variable married to our predictors of interest
# ord.reg = coyote %>% select(education) %>%
#   tibble(.,pet.int.high, pet.int.low, coyote.values)

# We are going to conduct two regressions for each important component
# let's review all the potential the variables we want to test

# Indep vars = age, gender, income, residency, environmental values, relationship with
# nature, animal ownership (pets/ livestock), knowledge of coyotes, fear of coyotes,
# and experience with coyotes

# Now let's recall what the variables are in R!
coyote$age # continuous
coyote$gender # categorical
coyote$county #categorical 
coyote$nature.person # binary
coyote$image.relation.nature # leveled factor 1-6
# IP coyote$envt.concern
coyote$fear # binary
coyote$knowledge.score # level factor 1-7
coyote$animal.owner # binary
coyote$animal.incident #binary
coyote$envt.concern1.sum
coyote$envt.concern2.sum
coyote$envt.concern3.sum

# Maybe don't include frequencies into model, too complicated and looking at this separatly anyway
# IP coyote$human.incident #binary (not sure I asked this well enough to distinguish coyote human or animal aggression--ask Brian opinion)
# IP coyote$encounter (perhaps include sightings and eating garbage etc)

# Organizing dependent variable 1a & b: value of coyotes
# Knopff et al. 2016 Methods------------------------------------------------------------------------------------------
# Lets take the mean of coyote value as done in Knopff et al. 2016 (we are not sure if this is statistically sound analysis--but lets see...)
coyote.values = coyote %>%
  select(c(important.ecosystem, beneficial.humans, beneficial.wildlife, important.hunters,
           risk.humans, risk.pets, negative.wildlife, diseases, ID)) %>%
  mutate_at(c("important.ecosystem", "beneficial.humans", "beneficial.wildlife", "important.hunters",
              "risk.humans", "risk.pets", "negative.wildlife", "diseases"), ~unclass(.))

# Unlike Knopff, let's use medians so we have fewer factors to split
# Lets take the median for each person or ID for model 1 which includes all values questions
coyote.values.mod1 = coyote.values %>% rowwise() %>%
  mutate(values.median.mod1 = median(c(important.ecosystem, beneficial.humans, beneficial.wildlife, important.hunters, risk.humans,
                                       risk.pets, negative.wildlife, diseases)), na.rm = TRUE) %>%
  select(-c(na.rm, important.ecosystem, beneficial.humans, beneficial.wildlife, important.hunters, risk.humans,
            risk.pets, negative.wildlife, diseases)) 

coyote = left_join(coyote, coyote.values.mod1, by = "ID") #Remember you need to join on a unique variable
coyote = coyote %>% mutate(values.median.mod1 = factor(values.median.mod1)) #need for ordinal reg

# Let's first work with model 1-------------------------------------------------
# We can also subet this data to examine it more closely
mod1.data = coyote %>% select(c(values.median.mod1,age,county,gender,image.relation.nature,fear,knowledge.score,
                                animal.owner,animal.incident))
?complete.cases
comp = complete.cases(mod1.data)
mean(comp) 
971 * .561277 # = 545 complete responses

# Install packages for model
library(MASS)

# Our first model includes all variables except environtmnetal concern. Given the complexity
# of the dimensions, I don't feel comfortable including it as I do not know how to interpret it
# Therefor we have 8 variables (coefficients)
mod1.1 = polr(values.median.mod1~age+county+gender+image.relation.nature+fear+knowledge.score+
                animal.owner+animal.incident, data = coyote, method="logistic", Hess=TRUE)
summary(mod1.1)
AIC(mod1.1)
# AIC = 1953.662
# Hess = true
# Hessian Matrix no ordinary matrix; it is a matrix with functions as entries. 
# In other words, it is meant to be evaluated at some point. This is used to get the
# standard errors

# add p-value
ctable = coef(summary(mod1.1))
p = pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2  ## add the p-values to the table
ctable = cbind(ctable, "p value" = p)
ctable

# interesting that county Not_shared is significant, is this the majority?
length(which(coyote$county=="Not_shared")) # Only 15! Myabe there's something different about this group of folks


# odds ratio 
exp(coef(mod1.1))

# confidence intervals
ci <- confint(mod1.1)
#If the 95% CI does not cross (include) 0, the parameter estimate is statistically significant.

# Regression Interpretation: 
# https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/
# https://stats.idre.ucla.edu/r/faq/ologit-coefficients/

# mathematical formula of the Propoprtional odds Model:
# logit[P(Y</=j)] = alphaj - sum[BetaiXi]
# j = 1, dependent variable 1 (coyote value of 1)
# j = 2, (coyote value of 2)
# i = 1, refers to age
# i = 2, refers to countyKent...

# Equation examples
## countyKent
# logit(P(Y</=1)) = -.09 - .58*x1
# logit(P(Y</=1)) = -.44 - .58*x1
# logit(P(Y</=1)) = 1.14 - .58*x1
# logit(P(Y</=1)) = 1.86 - .58*x1
# logit(P(Y</=1)) = 3.27 - .58*x1
# logit(P(Y</=1)) = 4.16 - .58*x1
# logit(P(Y</=1)) = 5.79 - .58*x1
# logit(P(Y</=1)) = 6.54 - .58*x1

# Kent County value = .58
exp.Kent = exp(.58) # 1.79
# Participants from Kent have 1.79 times the odds of highly valuing coyotes (value level = 5; vs. 1-4.5) 
# compared to other counties
# QUESTION: Does 1.79 refer to only the highest value state (5) or as we move through factors (1->1.5, 1.5->2 ...?)


# Let's also look into model variations-----------------------------------------
# random forests? 
# step-wise regression?


# REMEMBER, DF = # of parameters for the model, so df for  mod1.1 with all parms = 25



# https://cran.r-project.org/web/packages/sjPlot/vignettes/plot_interactions.html
# library("sjPlot")
# library("sjmisc")
# plot_model(mod1.10, type = "pred", terms = c("fear", "knowledge.score"))

mod1.2 = polr(values.median.mod1~age+gender+county+image.relation.nature*fear*knowledge.score+
                animal.owner*animal.incident, data = coyote, method="logistic", Hess=TRUE)
summary(mod1.2)
# This model is rank-deficient, so dropping coefs

mod1.3 = polr(values.median.mod1~age+gender+county+image.relation.nature+fear+knowledge.score+
                animal.owner*animal.incident, data = coyote, method="logistic", Hess=TRUE)
summary(mod1.3)
# This model is rank-deficient, so dropping coefs

mod1.4 = polr(values.median.mod1~age+gender+county+image.relation.nature*fear+knowledge.score+
                animal.incident*animal.owner, data = coyote, method="logistic", Hess=TRUE)
summary(mod1.4)
# This model is rank-deficient, so dropping coefs

mod1.5 = polr(values.median.mod1~age+gender+county+image.relation.nature*fear+knowledge.score+
                animal.incident+animal.owner, data = coyote, method="logistic", Hess=TRUE)
summary(mod1.5)

mod1.6 = polr(values.median.mod1~age+county+gender+image.relation.nature*knowledge.score+fear+
                animal.owner+animal.incident, data = coyote, method="logistic", Hess=TRUE)
summary(mod1.6)

mod1.7 = polr(values.median.mod1~age+county+gender*fear+image.relation.nature*knowledge.score+
                animal.owner+animal.incident, data = coyote, method="logistic", Hess=TRUE)
summary(mod1.7)
# This model is rank-deficient, so dropping coefs

mod1.8 = polr(values.median.mod1~age+county+gender+image.relation.nature+knowledge.score+
                fear*animal.owner*animal.incident, data = coyote, method="logistic", Hess=TRUE)
summary(mod1.8)
# This model is rank-deficient, so dropping coefs

mod1.9 = polr(values.median.mod1~age+county+gender+image.relation.nature+knowledge.score+
                fear*animal.owner+animal.incident, data = coyote, method="logistic", Hess=TRUE)
summary(mod1.9)

mod1.10 = polr(values.median.mod1~age+county+gender+image.relation.nature+knowledge.score+
                 fear*animal.incident+animal.owner, data = coyote, method="logistic", Hess=TRUE)
summary(mod1.10)

AIC(mod1.1, mod1.5, mod1.6, mod1.9, mod1.10)
# Certain models had insufficient combinations to properly run the model, therefor we are left 
# with the above model possibilities
# Model 1, which does not consider any interactions outperforms the above models which do

# Let's settle with mod1.1 
summary(mod1.1)
# This model uses 971-426 = 545 obs, df = 25, AIC = 1952.811
ctable = coef(summary(mod1.1))
p = pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2  ## add the p-values to the table
ctable = cbind(ctable, "p value" = p)
ctable

# odds ratio 
# This helps look at which variables impact the increasing value participants have of coyotes 
exp(coef(mod1.1))

# confidence intervals
ci <- confint(mod1.1)
# If the 95% CI's do not cross zero, the parameter estimate is significant
# These include: countyNot_shared, genderfemale, image.relation.nature (all), fear, and 
# animal.incident

# What do the 'Values' mean?----------------------------------------------------
# example, animal.incident:
# for one unit increase (going from 0 to 1), we expect a .100 decrease in the expected value of 
# valuing coyotes on the log odds scale, given all other variables are held constant 

# non-ordered categorical predictors ex) gender (male, female, non.binary)


# ordered categorical predictors ex) fear or image.relation.nature

# What do the exp(coef)'s mean?-------------------------------------------------

# numeric predictors ex) age
# for every one unit increase in age, the odds of valuing coyotes MORE is multiplied by 1.00,
# holding all other variables constant

# binary predictors ex) animal.owner & animal.incident
# for those that own animals, the odds of valuing coyotes MORE, is 1.27 times those who do not own animals,
# holding all other variables in the model are constant 
# 
# for those that have had animal incidents, the odds of valuing coyotes MORE, is .37 less times or 62.1 % (1-.379) lower than those 
# who have not had an animal incident, holding all other variables in the model are constant  
# OR those who have had animal incidents have a 63.0% lower odds of being less likely to value coyotes


# non-ordered categorical predictors ex) gender (male, female, non.binary)


# ordered categorical predictors ex) fear or image.relation.nature



# Let's now work with model 2----------------------------------------------------

# OK lets do the median for values for model 2 which does not include 'negative.wildlife'
coyote.values.mod2 = coyote.values %>% select(., -negative.wildlife) %>% rowwise() %>% 
  mutate(values.median.mod2 = median(c(important.ecosystem, beneficial.humans, beneficial.wildlife, important.hunters, risk.humans,
                                       risk.pets, diseases)), na.rm = TRUE) %>%
  select(-c(na.rm, important.ecosystem, beneficial.humans, beneficial.wildlife, important.hunters, risk.humans,
            risk.pets, diseases))

coyote = left_join(coyote, coyote.values.mod2, by = "ID") #Remember you need to join on a unique variable
coyote = coyote %>% mutate(values.median.mod2 = factor(values.median.mod2)) #need for ordinal reg

# Let's get to modeling! Don't forget to reload MASS package
mod2.data = coyote %>% select(c(values.median.mod2,age,county,gender,image.relation.nature,fear,knowledge.score,
                                animal.owner,animal.incident))

# Our first model includes all predictors of interest with no interactions
mod2.1 = polr(values.median.mod2~age+county+gender+image.relation.nature+fear+knowledge.score+
                animal.owner+animal.incident, data = coyote, method="logistic", Hess=TRUE)
summary(mod2.1)

# And now some more model variations
mod2.2 = polr(values.median.mod2~age+gender+county+image.relation.nature*fear*knowledge.score+
                animal.owner*animal.incident, data = coyote, method="logistic", Hess=TRUE)
summary(mod2.2)
# This model is rank-deficient, so dropping coefs

mod2.3 = polr(values.median.mod2~age+gender+county+image.relation.nature+fear+knowledge.score+
                animal.owner*animal.incident, data = coyote, method="logistic", Hess=TRUE)
summary(mod2.3)
# This model is rank-deficient, so dropping coefs

mod2.4 = polr(values.median.mod2~age+gender+county+image.relation.nature*fear+knowledge.score+
                animal.incident*animal.owner, data = coyote, method="logistic", Hess=TRUE)
summary(mod2.4)
# This model is rank-deficient, so dropping coefs

mod2.5 = polr(values.median.mod2~age+gender+county+image.relation.nature*fear+knowledge.score+
                animal.incident+animal.owner, data = coyote, method="logistic", Hess=TRUE)
summary(mod2.5)

mod2.6 = polr(values.median.mod2~age+county+gender+image.relation.nature*knowledge.score+fear+
                animal.owner+animal.incident, data = coyote, method="logistic", Hess=TRUE)
summary(mod2.6)

mod2.7 = polr(values.median.mod2~age+county+gender*fear+image.relation.nature*knowledge.score+
                animal.owner+animal.incident, data = coyote, method="logistic", Hess=TRUE)
summary(mod2.7)
# This model is rank-deficient, so dropping coefs

mod2.8 = polr(values.median.mod2~age+county+gender+image.relation.nature+knowledge.score+
                fear*animal.owner*animal.incident, data = coyote, method="logistic", Hess=TRUE)
summary(mod2.8)
# This model is rank-deficient, so dropping coefs

mod2.9 = polr(values.median.mod2~age+county+gender+image.relation.nature+knowledge.score+
                fear*animal.owner+animal.incident, data = coyote, method="logistic", Hess=TRUE)
summary(mod2.9)

mod2.10 = polr(values.median.mod2~age+county+gender+image.relation.nature+knowledge.score+
                 fear*animal.incident+animal.owner, data = coyote, method="logistic", Hess=TRUE)
summary(mod2.10)

AIC(mod2.1, mod2.5, mod2.6, mod2.9, mod2.10)
# Similarly to the previous model, the first addative model outperforms the rest

# add p-value
ctable = coef(summary(mod2.1))
p = pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2  ## add the p-values to the table
ctable = cbind(ctable, "p value" = p)
ctable

# odds ratio 
exp(coef(mod2.1))

# confidence intervals
ci <- confint(mod2.1)
#If the 95% CI does not cross (include) 0, the parameter estimate is statistically significant.
# The significant variables are then: image.relation.nature3 +, fear, and animal.incident

# for one unit increase in animal.incident (from 0 to 1), we expect a .98 decrease in expected
# value of coyote
#
# for people who have had animal.incidents, the odds of valuing coyotes more is .37 that of people 
# who did not have incidents 


# Now let's consider our model which predicts the frequency of coyote interaction
# this will be two separate models which consider the frequency of observations and
# frequency of incidents (this refers to an aggressive coyote interaction)
coyote$fq.see
coyote$fq.aggressive

# First let's correctly level the frequencies so the lowest factor [1] is 'Never' followed by 
# 'Less than once a year'... [6] Daily
coyote <- coyote  %>%
  mutate(fq.see = fct_relevel(fq.see, c("Never", "Less than once a year", "Yearly", "Monthly", "Weekly", "Daily")))

unclass(coyote$fq.see) # Great this worked!

coyote <- coyote  %>%
  mutate(fq.aggressive = fct_relevel(fq.aggressive, c("Never", "Less than once a year", "Yearly", "Monthly", "Weekly", "Daily")))

unclass(coyote$fq.aggressive) 

# Let's also maybe a new column that is binary for fq.aggressive. The models will not run bc
# of deficient data

binary.aggressive = coyote %>% select(ID, fq.aggressive) %>% 
  mutate(binary.aggressive = ifelse(fq.aggressive == "Never", 0, 1)) %>%
  select(-c(fq.aggressive))

coyote = left_join(coyote, binary.aggressive, by = "ID")

# Now we will work through our third model which considers frequency of observations and our 
# hypothesized predictors
mod3.1 = polr(fq.see~county+image.relation.nature+fear+knowledge.score+
                animal.owner, data = coyote, method="logistic", Hess=TRUE)
summary(mod3.1)

mod3.2 = polr(fq.see~county+image.relation.nature+fear*knowledge.score+
                animal.owner, data = coyote, method="logistic", Hess=TRUE)
summary(mod3.2)

mod3.3 = polr(fq.see~county+image.relation.nature*fear+knowledge.score+
                animal.owner, data = coyote, method="logistic", Hess=TRUE)
summary(mod3.3)

mod3.4 = polr(fq.see~county+image.relation.nature*knowledge.score+fear+
                animal.owner, data = coyote, method="logistic", Hess=TRUE)
summary(mod3.4)

mod3.5 = polr(fq.see~county+image.relation.nature+knowledge.score+fear*
                animal.owner, data = coyote, method="logistic", Hess=TRUE)
summary(mod3.5)

mod3.6 = polr(fq.see~county+fear+knowledge.score+image.relation.nature*
                animal.owner, data = coyote, method="logistic", Hess=TRUE)
summary(mod3.6)

AIC(mod3.1, mod3.2, mod3.3, mod3.4, mod3.5, mod3.6)
# According to our model variations, the best model with AIC is:
# mod3.1
# add p-value
ctable = coef(summary(mod3.1))
p = pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2  ## add the p-values to the table
ctable = cbind(ctable, "p value" = p)
ctable

# odds ratio 
exp(coef(mod3.1))

# confidence intervals
ci <- confint(mod3.1)
# If the 95% CI does not cross (include) 0, the parameter estimate is statistically significant.
# Thus: countyKent, countNotshares, CountyProvidence, countyWashington, and animal.owner are significant
# Remember the categorical values don't mean much since they are just comparing to a reference category

# Values
# for one unit increase (0 to 1), we expect a .52 increase in seeing a coyote 

# Exp(coef)
# People that have pets have 1.69 odds of seeing coyotes than those who don't

# Now we will work through our fourth model which considers frequency of observations and our 
# hypothesized predictors

mod4.1 = polr(fq.aggressive~county+image.relation.nature+fear+knowledge.score+
                animal.owner, data = coyote, method="logistic", Hess=TRUE)
summary(mod4.1)

# add p-value
ctable = coef(summary(mod4.1))
p = pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2  ## add the p-values to the table
ctable = cbind(ctable, "p value" = p)
ctable

# odds ratio 
exp(coef(mod4.1))

# confidence intervals
ci <- confint(mod4.1)
#If the 95% CI does not cross (include) 0, the parameter estimate is statistically significant.


mod4.2 = polr(fq.aggressive~county+image.relation.nature+fear*knowledge.score+
                animal.owner, data = coyote, method="logistic", Hess=TRUE)
summary(mod3.2)

mod4.3 = polr(fq.aggressive~county+image.relation.nature*fear+knowledge.score+
                animal.owner, data = coyote, method="logistic", Hess=TRUE)
summary(mod3.3)

mod4.4 = polr(fq.aggressive~county+image.relation.nature*knowledge.score+fear+
                animal.owner, data = coyote, method="logistic", Hess=TRUE)
summary(mod4.4)

mod4.5 = polr(fq.aggressive~county+image.relation.nature+knowledge.score+fear*
                animal.owner, data = coyote, method="logistic", Hess=TRUE)
summary(mod4.5)

mod4.6 = polr(fq.aggressive~county+fear+knowledge.score+image.relation.nature*
                animal.owner, data = coyote, method="logistic", Hess=TRUE)
summary(mod4.6)

AIC(mod4.1, mod4.2, mod4.3, mod4.4, mod4.5, mod4.6)

length(which(coyote$fq.aggressive!="Never"))

# Hmm, let's do a logistic regression instead,
mod5.1 = glm(binary.aggressive~county+fear+knowledge.score+image.relation.nature+
               animal.owner, family = binomial(link = "logit"), data = coyote)
summary(mod5.1)
plot(mod5.1)
# Interesting, only fear comes up as significant, and very so!


# Predicting with our models----------------------------------------------------
# create a new dataset for mod1.1 with every possibility 
# Create new data 
# then use expand.grid function
summary(mod1.1)

age = seq(18,100, by = 2)
county = c("Bristol","Newport","Washington","Providence","Kent")
gender = c("male", "female", "non.binary")
image.relation.nature = as.factor(seq(1,6, 1))
fear = seq(0,1, 1)
knowledge.score = seq(1,45, by = 5)
animal.owner = seq(0,1, by = 1)
animal.incident = seq(0,1, by = 1)

newdat = expand.grid(age = age, county = county, gender = gender, image.relation.nature = image.relation.nature,
                     fear = fear, knowledge.score = knowledge.score, animal.owner = animal.owner, animal.incident = animal.incident)
newdat <- cbind(newdat, predict(mod1.1, newdat, type = "probs"))

# Our significant variables were: countyNot_shared, genderfemale, image.relation.nature (all), fear, and 
# animal.incident
newdat %>% 
  pivot_longer(9:16, names_to = "Level", values_to="Probability") %>% 
  ggplot(aes(x = age, y = Probability, color=factor(Level))) +
  geom_line(alpha=0.7) + 
  scale_color_brewer(palette = "RdYlBu") + 
  facet_grid(fear~image.relation.nature) + theme_minimal() + 
  labs(color="Level")



# Baysian model fitting--------------------------------------
# file:///C:/Users/Kim%20Rivera/Downloads/ordinal_analysis.html
# https://osf.io/vxw73/
# https://osf.io/tasu5
# https://github.com/paul-buerkner/brms/blob/master/R/distributions.R
# https://github.com/lottybrand/GH_Kernow (this is for Brand et al., 2019)

library("devtools")
devtools::install_github("paul-buerkner/brms")
library("brms")

# Model 1: This refers to our value model with dimension 1 from PCA results which considers all 'value' questions
# First we need our response data in 'longer data' e.g. there will be column headers
# for each Question and rows are a value for each question
coyote.values.bays = coyote %>%
  select(c(important.ecosystem, beneficial.humans, beneficial.wildlife, important.hunters,
           risk.humans, risk.pets, negative.wildlife, diseases, ID)) %>%
  mutate_at(c("important.ecosystem", "beneficial.humans", "beneficial.wildlife", "important.hunters",
              "risk.humans", "risk.pets", "negative.wildlife", "diseases"), ~unclass(.))


coyote.values.bays = coyote.values.bays %>% pivot_longer(cols = important.ecosystem:diseases,
                                                         names_to = "value_question",
                                                         values_to = "value_rate")
# Intercepts refer to the rating with rating 5 being the reference category

coyote_bayes = inner_join(coyote, coyote.values.bays, by = "ID") #Remember you need to join on a unique variable




# Model 2: This refers to our value model with dimension 2 from PCA results which does not considers the 'negative.wildlife' question

# First we need our response data in 'longer data' e.g. there will be column headers
# for each Question and rows are a value for each question
coyote.values.bays2 = coyote %>%
  select(c(important.ecosystem, beneficial.humans, beneficial.wildlife, important.hunters,
           risk.humans, risk.pets, diseases, ID)) %>%
  mutate_at(c("important.ecosystem", "beneficial.humans", "beneficial.wildlife", "important.hunters",
              "risk.humans", "risk.pets", "diseases"), ~unclass(.))


coyote.values.bays2 = coyote.values.bays2 %>% pivot_longer(cols = important.ecosystem:diseases,
                                                         names_to = "value_question",
                                                         values_to = "value_rate")
# Intercepts refer to the rating with rating 5 being the reference category

coyote_bayes2 = inner_join(coyote, coyote.values.bays2, by = "ID") #Remember you need to join on a unique variable

# Model 3: This refers to our sight model 
# We are only using one leveled question to predict so we don't need to pivot or join a 
# new data set
coyote <- coyote %>%  
  mutate(fq.see = fct_relevel(fq.see, c("Never", "Less than once a year", "Yearly", "Monthly", "Weekly", "Daily"))) %>%
  mutate (fq.see = unclass(fq.see))

# Model 4: This refers to our incidence model 
# We are only using one leveled question to predict so we don't need to pivot or join a 
# new data set
coyote <- coyote %>%  
  mutate(fq.aggressive = fct_relevel(fq.aggressive, c("Never", "Less than once a year", "Yearly", "Monthly", "Weekly", "Daily"))) %>%
  mutate(fq.aggressive = unclass(fq.aggressive))
