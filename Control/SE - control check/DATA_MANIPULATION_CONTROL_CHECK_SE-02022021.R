library(tidyverse)
library(car)


setwd("C:/Users/Thijs/surfdrive/COVID vaccine/git/Control/SE - control check")
test.SE <- read.csv("C:/Users/Thijs/surfdrive/COVID vaccine/Data/DATA_SE_CHECK-11032021-FINAL-CORRECTED-PID.csv") %>%
  dplyr::mutate_if(is.character, .funs = function(x){return(`Encoding<-`(x, "UTF-8"))})
test.SE[is.na(test.SE)] <- 0
df.prep.SE <- test.SE %>% filter(Progress > 80) %>%  select(-c(Q19.7_2_TEXT, Q.20.4, Q19.4_9_TEXT, Q19.4_8_TEXT, X, contains("StartDate")))

# Subset pc and mobile
df.prep.mobile.SE  <- df.prep.SE  %>% filter(Q2.2_1>0)
df.prep.pc.SE <- df.prep.SE %>% filter(Q3.2_1>0)


# Add tag
df.prep.pc.SE ["device"] <- "pc"
df.prep.mobile.SE ["device"] <- "mobile"

# Recode mobile
df.prep.mobile.SE  <- df.prep.mobile.SE  %>% 
  mutate(Q3.2_1 = Q2.2_1,
         Q3.2_2 = Q2.2_2,
         Q3.2_3 = Q2.2_3,
         Q3.2_4 = Q2.2_4,
         Q3.2_5 = Q2.2_5,
         Q3.3_1 = Q2.3_1,
         Q3.3_2 = Q2.3_2,
         Q3.3_3 = Q2.3_3) # Opinion about EU

df.prep.mobile.SE  <- df.prep.mobile.SE  %>% 
  mutate(Q6.2_1 = Q5.3,
         Q6.2_2 = Q5.4,
         Q6.2_3 = Q5.5,
         Q6.2_4 = Q5.6,
         Q6.3_1 = Q5.8,
         Q6.3_2 = Q5.9,
         Q6.4 = Q5.10) # Pre-manipulation (Mobile)

# Bind rows
df.total.SE  <- rbind(df.prep.mobile.SE, df.prep.pc.SE)
df.total.SE["country"] <- "SE"

# Manipulation variable
df.total.SE <- df.total.SE %>% mutate(experimental.group = case_when(
  Q142 > 0  ~ "no text",
  Q10.7 > 0 ~ "advice",
  Q8.7 > 0 ~ "independence")) %>% 
  mutate(advice = case_when(
  experimental.group == "advice" ~ 1,
  experimental.group == "no text" ~ 0,
  experimental.group == "independence" ~ 2))

# Bind answers from manipulation 
df.total.SE <- df.total.SE %>%  mutate(
  intro.submit = Q138_Page.Submit + Q10.3_Page.Submit + Q8.3_Page.Submit ,
  manipulation.submit =  Q10.5_Page.Submit + Q8.5_Page.Submit %>% na_if(0),
  perceived.independence = Q142 + Q10.7 + Q8.7,
  safety = Q141 + Q10.6 + Q8.6)

## IMCs
df.total.SE <- df.total.SE %>% 
  mutate(IMC =ifelse(str_detect(Q17.1_7_TEXT,c("9|nittionio|Nittionio"))==T,1,0))

df.total.SE <- df.total.SE %>% drop_na(Q298)  %>% 
mutate(manipulation.check = case_when(
    Q298 == 1 & experimental.group == "independence" ~ 1,
    Q298 == 2 & experimental.group == "advice" ~ 1,
    Q298 == 3 ~ 0,
    Q298 == 4 & experimental.group == "no text" ~ 1,
    TRUE ~ 0))

df.total.SE <- df.total.SE %>% 
  mutate(manipulation.check.failed = case_when(
    manipulation.check == 1 ~ "passed",
    manipulation.check == 0 ~ "failed"))

# Demographics
df.total.SE <- df.total.SE %>% 
    mutate(female = Q19.3 %>% Recode("1=0;2=1;3=NA; 0=NA"),
           age = Q19.2_1 %>% na_if(0),
           healthcare = Q19.5 %>% recode("1='yes';  2='no'"),
           education.recoded = Q19.4 %>% recode("1='1. Grundskola (mindre än 9 år)';
                                      2='2. Grundskola (9 år)';
                                      3='3. Gymnasiestudier (mindre än 3 år)';
                                      4='4. Gymnasiestudier (3 år)';
                                      5='5. Eftergymnasial utbildning (mindre än 3 år)';
                                      6='6. Eftergymnasial utbildning (3 år eller mer)';
                                      7='7. Forskarutbildning';
                                      12='8. Annan';
                                      0=NA"),
           province = Q19.8 %>% Recode(
             "1 = 'Region Blekinge';
         2 = 'Region Dalarna';
         3 = 'Region Gotland';
         4 = 'Region Gävleborg';
         5 = 'Region Halland';
         6 = 'Region Jämtland Härjedalen';
         7 = 'Region Jönköpings län';
         8 = 'Region Kalmar län';
         9 = 'Region Kronoberg';
         10 = 'Region Norrbotten';
         11 = 'Region Skåne';
         12 = 'Region Stockholm';
         13 = 'Region Sörmland';
         14 = 'Region Uppsala';
         15 = 'Region Värmland';
         16 = 'Region Västerbotten';
         17 = 'Region Västernorrland';
         18 = 'Region Västmanland';
         19 = 'Region Örebro län';
         20 = 'Region Östergötland';
         21 = 'Västra Götalandsregionen';
         0 = NA"),
           income = Q19.6  %>% recode("1='<200K';
                                    2='200K-250K SEK';
                                    3='250K-300K SEK';
                                    4='300K-350K SEK';
                                    5='350K-400K SEK';
                                    6='400K-450K SEK';
                                    7='450K-500K SEK';
                                    8='500K> SEK';
                                    0=NA"),
           native.language = Q19.9%>% na_if(0))

# Outcome variables
df.total.SE <- df.total.SE %>% 
  mutate(benefits.vaccines = Q6.2_2%>% na_if(0),
         comments.general = Q20.6 %>% na_if(0),
         intent.vaccine = Q16.2 %>% na_if(0),
         intent.vaccine.recoded = Q16.2 %>% recode("1=0; 2=0; 3=1; 4=1"),
         credibility.item1 = Q15.2 %>% na_if(0),
         credibility.item2.reversed = Q15.3 %>% recode("7=1; 6=2; 5=3; 4=4; 3=5; 2=6; 1=7") %>% na_if(0),
         credibility.item3.reversed = Q15.4 %>% recode("7=1; 6=2; 5=3; 4=4; 3=5; 2=6; 1=7") %>% na_if(0),
         credibility.item4 = Q15.5 %>% na_if(0),
         credibility.item5 = Q15.6 %>% na_if(0),
         credibility.item6 = Q15.7 %>% na_if(0),
         credibility.stability = Q15.7 %>% na_if(0)) 

# Credibility index
df.total.SE$credibility.index <- df.total.SE %>% dplyr::select(credibility.item1:credibility.item6) %>% base::rowMeans()  %>% na_if(0)
df.total.SE$credibility.expert<- df.total.SE %>% dplyr::select(credibility.item4,credibility.item5) %>% base::rowMeans() %>% na_if(0)
df.total.SE$credibility.noninterference<- df.total.SE %>% dplyr::select(credibility.item1:credibility.item3.reversed) %>% base::rowMeans() %>% na_if(0)

# Controls
df.total.SE <- df.total.SE %>%
  mutate(trust.EC = Q3.2_1 %>% na_if(0),
         trust.EP = Q3.2_2 %>% na_if(0),
         trust.council = Q3.2_3 %>% na_if(0),
         trust.scientist = Q3.2_4 %>% na_if(0),
         trust.politicians = Q3.2_5 %>% na_if(0),
         political.ideology = Q4.2 %>% na_if(0),
         EU.integration = Q4.3 %>% na_if(0),
         interpersonal.trust = Q4.4 %>% na_if(0),
         familiarity.EMA = Q18.2 %>% na_if(0),
         familiarity.advice = Q18.3 %>% na_if(0),
         credibility.ECB = Q3.3_1 %>% na_if(0),
         credibility.EMA = Q3.3_2 %>% na_if(0),
         credibility.EFSA = Q3.3_3 %>% na_if(0),
         credibility.post = Q140 %>% na_if(0),
         trust.health.authorities = Q6.4 %>% na_if(0),
         perceived.independence.reversed = perceived.independence %>% recode("7=1; 6=2; 5=3; 4=4; 3=5; 2=6; 1=7") %>% na_if(0),
         consequences.health = Q6.3_1 %>% na_if(0),
         consequences.economic =  Q6.3_2 %>% na_if(0),
         importance.EMA = Q16.7_1 %>% na_if(0),
         importance.FDA = Q16.7_2 %>% na_if(0),
         importance.NRA = Q16.7_3 %>% na_if(0),
         private.providers = Q20.2 %>% na_if(0),
         decision.submit = Q14.2_Page.Submit %>% na_if(0),
         duration = Duration..in.seconds./60 %>% na_if(0))

# Knowledge
df.total.SE <- df.total.SE %>% mutate(knowledge = case_when(
  Q16.5 == 2 & Q16.6 == 2 ~ 1,
  Q16.5 == 1 | Q16.6 == 1 ~ 0))

# Create subsets
df.def.SE <-  df.total.SE %>%  
  filter(age >= 18 & IMC=="1")

agecut.5 <- c(-Inf, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, Inf)
agecut.10 <- c(-Inf, 30, 40, 50, 60,  70, 80, Inf)

df.def.SE <- df.def.SE %>% mutate(age.recoded = cut(age, agecut.10, c("younger than 30", "30-39", "40-49", "50-59", "60-69", "70-79", "80 or older"), right=FALSE))
age.sample.SE <- df.def.SE %>% transmute(agecat = cut(age, agecut.5, c("younger than 25", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80 or older"), right=FALSE))  %>% table() %>% as_tibble()

age.sample.SE <- age.sample.SE %>% 
  mutate(percentage = n / sum(n))

df.mobile.SE <-  df.def.SE %>% 
  filter(device=="mobile")

df.pc.SE <-  df.def.SE %>% 
  filter(device=="pc")

df.check.SE <-  df.def.SE %>% 
  filter(manipulation.check.failed=="passed")

# Save data     
save.image()

