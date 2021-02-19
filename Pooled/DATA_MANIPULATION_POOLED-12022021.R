library(tidyverse)
library(car)

setwd("C:/Users/Thijs/surfdrive/COVID vaccine/git/Pooled")

raw.IE <- read.csv("C:/Users/Thijs/surfdrive/COVID vaccine/git/EN/DATA_EN-18022021.csv") %>%
  dplyr::mutate_if(is.character, .funs = function(x){return(`Encoding<-`(x, "UTF-8"))}) %>%
  mutate(country = "IE",
         check = "no")
raw.FR <- read.csv("C:/Users/Thijs/surfdrive/COVID vaccine/git/FR/DATA_FR-18022021.csv") %>%
  dplyr::mutate_if(is.character, .funs = function(x){return(`Encoding<-`(x, "UTF-8"))}) %>%
  mutate(country = "FR",
         check = "no")
raw.NL <- read.csv("C:/Users/Thijs/surfdrive/COVID vaccine/git/NL/DATA_NL-12022021-FINAL.csv") %>%
  dplyr::mutate_if(is.character, .funs = function(x){return(`Encoding<-`(x, "UTF-8"))}) %>%
  mutate(country = "NL",
         check = "no")
raw.SE <- read.csv("C:/Users/Thijs/surfdrive/COVID vaccine/git/SE/DATA_SE-12022021-FINAL.csv") %>%
  dplyr::mutate_if(is.character, .funs = function(x){return(`Encoding<-`(x, "UTF-8"))}) %>%
  mutate(country = "SE",
         check = "no") 
raw.NL.check <- read.csv("C:/Users/Thijs/surfdrive/COVID vaccine/git/Control/NL - control check/DATA_CONTROL_CHECK_NL-12022021-FINAL.csv") %>%
  dplyr::mutate_if(is.character, .funs = function(x){return(`Encoding<-`(x, "UTF-8"))})%>%
  mutate(country = "NL",
         check = "yes")
raw.SE.check <- read.csv("C:/Users/Thijs/surfdrive/COVID vaccine/git/Control/SE - control check/DATA_CONTROL_CHECK_SE-12022021-FINAL.csv") %>%
  dplyr::mutate_if(is.character, .funs = function(x){return(`Encoding<-`(x, "UTF-8"))}) %>%
  mutate(country = "SE",
         check = "yes")%>%
  select(-Q19.7_2_TEXT) # This var has numeric values,
                        # which causes an error when merging
                        # the different dataframes. No data loss.

# Binding dataframes
raw.total <- bind_rows(raw.IE,
                  raw.FR,
                  raw.NL,
                  raw.SE,
                  raw.NL.check,
                  raw.SE.check) %>%
  select(-contains("Click")) # Some values are NA by default.
                            # I removed them to prevent errors.

# Missing data                            
raw.total[is.na(raw.total)] <- 0 # Set NA to 0

# Subset pc and mobile
raw.mobile <- raw.total %>%
  filter(Q2.2_1>0)
raw.pc<- raw.total %>%
  filter(Q3.2_1>0)
raw.unknown <- raw.total %>%
  filter(Q3.2_1==0 & Q2.2_1==0) # "unknown" are cases that stopped before Q3.2_1==0 or Q2.2_1==0.

# Add tag
raw.pc["device"] <- "pc"
raw.mobile["device"] <- "mobile"

# Recode mobile
raw.mobile <- raw.mobile %>% 
  mutate(Q3.2_1 = Q2.2_1,
         Q3.2_2 = Q2.2_2,
         Q3.2_3 = Q2.2_3,
         Q3.2_4 = Q2.2_4,
         Q3.2_5 = Q2.2_5,
         Q3.2_6 = Q2.2_6,
         Q3.3_1 = Q2.3_1,
         Q3.3_2 = Q2.3_2,
         Q3.3_3 = Q2.3_3) # Opinion about EU

raw.mobile <- raw.mobile %>% 
  mutate(Q6.2_1 = Q5.3,
         Q6.2_2 = Q5.4,
         Q6.2_3 = Q5.5,
         Q6.2_4 = Q5.6,
         Q6.3_1 = Q5.8,
         Q6.3_2 = Q5.9,
         Q6.4 = Q5.10) # Pre-manipulation (Mobile)

# Bind rows
df.total <- rbind(raw.mobile, raw.pc)

# Manipulation variable
df.total <- df.total %>%
  mutate(experimental.group = case_when(
  Q8.7 > 0  ~ "independence",
  Q10.7 > 0 ~ "advice",
  Q421 > 0 | Q142 > 0 ~ "no text")) %>%
     mutate(independence = case_when(
          experimental.group == "independence" ~ 1,
          experimental.group == "advice" ~ 0,
          experimental.group == "no text" ~ 0),
     advice = case_when(
          experimental.group == "independence" ~ 0,
          experimental.group == "advice" ~ 1,
          experimental.group == "no text" ~ 0),
     no.text = case_when(
       experimental.group == "independence" ~ 0,
       experimental.group == "advice" ~ 0,
       experimental.group == "no text" ~ 1)) %>%
  drop_na(experimental.group) # Note that this code drops all cases
                              # that stopped before the question about the perceived
                              # independence EMA.



# Bind answers from manipulation 
df.total <- df.total %>%  mutate(
  intro.submit = (Q8.3_Page.Submit + Q10.3_Page.Submit + Q138_Page.Submit + Q417_Page.Submit) %>% na_if(0),
  manipulation.submit = (Q8.5_Page.Submit + Q10.5_Page.Submit) %>% na_if(0),
  perceived.independence = (Q8.7 + Q10.7 + Q142 + Q421) %>% na_if(0),
  safety = (Q8.6 + Q10.6 + Q141 + Q420) %>% na_if(0))

## IMCs
df.total <- df.total %>% mutate(Q17.1 = Q17.1 %>% na_if(0)) # Set 0 to NA

df.total <-df.total %>%
  mutate(Q17.1_7_TEXT = ifelse(is.na(Q17.1), NA_real_, Q17.1_7_TEXT),
         IMC =ifelse(str_detect(Q17.1_7_TEXT,c("9|Nine|nine|negen|Negen|Neuf|neuf|nittionio|Nittionio"))==T,1,0))

df.total <- df.total %>% 
  mutate(manipulation.check = case_when(
   check == "no" & Q298 == 1 & experimental.group == "independence" ~ 1,
   check == "no" & Q298 == 2 & experimental.group == "advice" ~ 1,

      check == "yes" & country == "SE" & Q298 == 1 & experimental.group == "independence" ~ 1,
      check == "yes" & country == "SE" & Q298 == 2 & experimental.group == "advice" ~ 1,
      check == "yes" & country == "SE" & Q298 == 4 & experimental.group == "no text" ~ 1,
    
   check == "yes" & country == "NL" & Q298 == 2 & experimental.group == "advice" ~ 1,
   check == "yes" & country == "NL" & Q298 == 3 & experimental.group == "no text" ~ 1,
   
   Q298 == 0 ~ NA_real_,
   TRUE ~ 0)) # Coding needs to be double-checked in Qualtrics!

df.total <- df.total %>% 
  mutate(manipulation.check.failed = case_when(
    manipulation.check == 1 ~ "passed",
    manipulation.check == 0 ~ "failed"))

# Demographics
df.total <- df.total %>% 
  mutate(female = Q19.3 %>% Recode("1=0;2=1;3=NA; 0=NA"),
         age = (Q19.2_8 + Q19.2_1) %>% na_if(0),
         healthcare = Q19.5 %>% na_if(0) %>% recode("1=1;  2=0"),
         healthcare.recoded = Q19.5 %>% recode("1='yes';  2='no'") %>% na_if(0),
         native.language = Q19.9 %>% na_if(0))

# Outcome variables
df.total <- df.total %>% 
  mutate(benefits.vaccines = Q6.2_2 %>% na_if(0),
         comments.general = Q20.6 %>% na_if(0),
         intent.vaccine = Q16.2  %>% na_if(0),
         intent.vaccine.recoded = Q16.2 %>% recode("1=0; 2=0; 3=1; 4=1"),
         credibility.item1 = Q15.2 %>% na_if(0),
         credibility.item2.reversed = Q15.3 %>% recode("7=1; 6=2; 5=3; 4=4; 3=5; 2=6; 1=7") %>% na_if(0),
         credibility.item3.reversed = Q15.4 %>% recode("7=1; 6=2; 5=3; 4=4; 3=5; 2=6; 1=7") %>% na_if(0),
         credibility.item4 = Q15.5 %>% na_if(0),
         credibility.item5 = Q15.6 %>% na_if(0),
         credibility.item6 = Q15.7 %>% na_if(0),
         stability = Q15.7 %>% na_if(0)) 
  
# Credibility index
df.total <- df.total %>% 
  mutate(
    expertise =
      rowMeans(cbind(credibility.item4,
                     credibility.item5), na.rm=T),
    noninterference =
      rowMeans(cbind(credibility.item1,
                     credibility.item2.reversed,
                     credibility.item3.reversed), na.rm=T),
    credibility.index =
      rowMeans(cbind(credibility.item1,
                     credibility.item2.reversed,
                     credibility.item3.reversed,
                     credibility.item4,
                     credibility.item5,
                     credibility.item6), na.rm=T))

# controls
df.total <- df.total %>%
  mutate(trust.EC = Q3.2_1 %>% na_if(0),
         trust.EP = Q3.2_2 %>% na_if(0),
         trust.council = Q3.2_3 %>% na_if(0),
         trust.scientist = Q3.2_4 %>% na_if(0),
         trust.politicians = Q3.2_5 %>% na_if(0),
         trust.media = Q3.2_6 %>% na_if(0),
         political.ideology = Q4.2 %>% na_if(0),
         EU.integration = Q4.3 %>% na_if(0),
         interpersonal.trust = Q4.4 %>% na_if(0),
         familiarity.EMA = Q18.2 %>% na_if(0),
         familiarity.advice = Q18.3 %>% na_if(0),
         credibility.EMA.pre = Q3.3_2 %>% na_if(0),
         credibility.EMA.post = (Q160 + Q140 + Q86) %>% na_if(0),
         trust.health.authorities = Q6.4 %>% na_if(0),
         perceived.independence.reversed = perceived.independence %>% car::recode("7=1; 6=2; 5=3; 4=4; 3=5; 2=6; 1=7") %>% na_if(0),
         consequences.health = Q6.3_1 %>% na_if(0),
         consequences.economic =  Q6.3_2 %>% na_if(0),
         importance.EMA = Q16.7_1 %>% na_if(0),
         importance.FDA = Q16.7_2 %>% na_if(0),
         importance.NRA = Q16.7_3 %>% na_if(0),
         private.providers = Q20.2 %>% na_if(0),
         decision.submit = Q14.2_Page.Submit %>% na_if(0),
         duration = Duration..in.seconds./60 %>% na_if(0))

# Knowledge
df.total <- df.total %>% mutate(knowledge = case_when(
  Q16.5 == 2 & Q16.6 == 2 ~ 1,
  Q16.5 == 1 | Q16.6 == 1 ~ 0)) # Coding needs to be double-checked in Qualtrics!

#create "master" dataset
pooled <-  df.total %>%  
  filter(IMC=="1" | age <18)

# Add variables

## Time submission manipulation page
pooled <- pooled %>% mutate(manipulation.submit.quartile = case_when(
  manipulation.submit <= quantile(manipulation.submit, probs = 0.25, na.rm = TRUE) ~ "Q1",
  manipulation.submit > quantile(manipulation.submit, probs = 0.25, na.rm = TRUE) &
    manipulation.submit <= quantile(manipulation.submit, probs = 0.5, na.rm = TRUE)  ~ "Q2",
  manipulation.submit > quantile(manipulation.submit, probs = 0.5, na.rm = TRUE) &
    manipulation.submit <= quantile(manipulation.submit, probs = 0.75, na.rm = TRUE)  ~ "Q3",
  manipulation.submit > quantile(manipulation.submit, probs = 0.75, na.rm = TRUE) ~ "Q4"),
  manipulation.submit.quartile = factor(manipulation.submit.quartile, levels = c("Q1", "Q2", "Q3", "Q4")))

pooled <- pooled %>% mutate(manipulation.submit.30 = case_when(
  manipulation.submit <= 30 ~ "Under 30 secs",
  manipulation.submit > 30 ~ "30 secs or more"),
  manipulation.submit.30 = factor(manipulation.submit.30,
                                  levels = c("Under 30 secs",
                                             "30 secs or more")))

# Age in 10 yrs segments
agecut.10 <- c(-Inf, 30, 40, 50, 60,  70, 80, Inf)

pooled <- pooled %>%
  mutate(age.10 = cut(age, agecut.10,
                           c("younger than 30",
                             "30-39",
                             "40-49",
                             "50-59",
                             "60-69",
                             "70-79",
                             "80 or older"),
                           right = FALSE,
                           ordered_result = TRUE))

# Age in 5 yrs segments per country
agecut.5 <- c(-Inf, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, Inf)
age.country <- pooled %>% group_by(country) %>%
  transmute(age.5 = cut(age, agecut.5,
                         c("younger than 25",
                           "25-29",
                           "30-34",
                           "35-39",
                           "40-44",
                           "45-49",
                           "50-54",
                           "55-59",
                           "60-64",
                           "65-69",
                           "70-74",
                           "75-79",
                           "80 or older"),
                          right=FALSE,
                         ordered_result = TRUE)) %>% 
  table() %>%
  as_tibble()



age.sample.FR <- age.country %>%
  filter(country == "FR") %>% 
  mutate(percentage = n / sum(n))

age.sample.IE <- age.country %>%
  filter(country == "IE")  %>% 
  mutate(percentage = n / sum(n))

age.sample.NL <- age.country %>%
  filter(country == "NL")  %>% 
  mutate(percentage = n / sum(n))

age.sample.SE <- age.country %>%
  filter(country == "SE")  %>% 
  mutate(percentage = n / sum(n))

# Create subsets
pooled.experiment <- pooled %>%  
  filter(experimental.group != "no text")

check <-  pooled %>%  
  filter(manipulation.check=="1")

NL <-  pooled %>%  
  filter(country == "NL")

SE <-  pooled %>%  
  filter(country == "SE")

IE <-  pooled %>%  
  filter(country == "IE")

FR <-  pooled %>%  
  filter(country == "FR")

# Save data     
save.image()

