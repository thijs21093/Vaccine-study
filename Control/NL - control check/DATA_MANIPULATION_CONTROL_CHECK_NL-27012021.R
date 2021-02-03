library(tidyverse)
library(car)


setwd("C:/Users/Thijs/surfdrive/COVID vaccine/R data/git/Control/NL - control check")
test.NL <- read.csv("DATA_CONTROL_CHECK_NL-01022021.csv")
test.NL[is.na(test.NL)] <- 0
df.prep.NL <- test.NL %>% filter(Progress > 80) %>%  select(-Q19.7_2_TEXT) 

# Subset pc and mobile
df.prep.mobile.NL <- df.prep.NL %>% filter(Q2.2_1>0)
df.prep.pc.NL <- df.prep.NL %>% filter(Q3.2_1>0)

# Add tag
df.prep.pc.NL["device"] <- "pc"
df.prep.mobile.NL["device"] <- "mobile"

# Recode mobile
df.prep.mobile.NL <- df.prep.mobile.NL %>% 
  mutate(Q3.2_1 = Q2.2_1,
         Q3.2_2 = Q2.2_2,
         Q3.2_3 = Q2.2_3,
         Q3.2_4 = Q2.2_4,
         Q3.2_5 = Q2.2_5,
         Q3.3_1 = Q2.3_1,
         Q3.3_2 = Q2.3_2,
         Q3.3_3 = Q2.3_3) # Opinion about EU

df.prep.mobile.NL <- df.prep.mobile.NL %>% 
  mutate(Q6.2_1 = Q5.3,
         Q6.2_2 = Q5.4,
         Q6.2_3 = Q5.5,
         Q6.2_4 = Q5.6,
         Q6.3_1 = Q5.8,
         Q6.3_2 = Q5.9,
         Q6.4 = Q5.10) # Pre-manipulation (Mobile)

# Bind rows
df.total.NL <- rbind(df.prep.mobile.NL, df.prep.pc.NL)
df.total.NL["country"] <- "NL"

# Manipulation variable
df.total.NL <- df.total.NL %>% mutate(experimental.group = case_when(
  Q421 > 0  ~ "no text",
  Q10.7 > 0 ~ "advice")) %>% 
  mutate(advice = case_when(
  experimental.group == "advice" ~ 1,
  experimental.group == "no text" ~ 0)) %>%
  drop_na(experimental.group)

# Bind answers from manipulation 
df.total.NL <- df.total.NL %>%  mutate(
  intro.first.click = Q417_First.Click + Q10.3_First.Click,
  intro.last.click = Q417_Last.Click + Q10.3_Last.Click,
  intro.submit = Q417_Page.Submit + Q10.3_Page.Submit,
  intro.click.count = Q417_Click.Count + Q10.3_Click.Count,
  manipulation.first.click =   Q10.5_First.Click,
  manipulation.last.click =  Q10.5_Last.Click,
  manipulation.submit =  Q10.5_Page.Submit %>% na_if(0),
  manipulation.count = Q10.5_Click.Count,
  perceived.independence = Q421 + Q10.7,
  safety = Q420 + Q10.6)

## IMCs
df.total.NL <- df.total.NL %>% 
  mutate(IMC =ifelse(str_detect(Q17.1_7_TEXT,c("9|Negen|negen"))==T,1,0))

df.total.NL <- df.total.NL %>% 
  mutate(manipulation.check = case_when(
    Q298 == 2 & experimental.group == "advice" ~ 1,
    Q298 == 3 & experimental.group == "no text" ~ 1,
    Q298 == 1 ~ 0,
    Q298 == 2 & experimental.group == "no text" ~ 0,
    Q298 == 3 & experimental.group == "advice" ~ 0))

df.total.NL <- df.total.NL %>% 
  mutate(manipulation.check.failed = case_when(
    manipulation.check == 1 ~ "passed",
    manipulation.check == 0 ~ "failed"))

# Demographics
df.total.NL <- df.total.NL %>% 
  dplyr::mutate(female = Q19.3 %>% car::recode("1=0; 2=1; 3=NA; 0=NA"),
         gender = Q19.3 %>% car::recode("1='male';2='female';3=NA; 0=NA"),
         age = Q19.2_8,
         healthcare = Q19.5 %>% car::recode("1='yes';  2='no'"),
         education = Q19.4 %>% car::recode("1='1. VMBO/Mavo';
                                      2='2. Havo';
                                      3='3. Vwo';
                                      4='4. MBO';
                                      5='5. HBO Bachelor';
                                      6='6. WO Bachelor';
                                      7='7. HBO Master';
                                      8='8. WO Master of hoger';
                                      9='9. Anders';
                                      0=NA"),
         education.recoded = Q19.4 %>% car::recode("1='1. VMBO/Mavo';
                                      2='2. Havo';
                                      3='3. Vwo';
                                      4='4. MBO';
                                      5='5. HBO';
                                      6='6. WO';
                                      7='5. HBO';
                                      8='6. WO';
                                      9= '7. Anders';
                                      0=NA"),
         income = Q19.6  %>% car::recode("2='<20K';
                                    3='20K-25K';
                                    4='25K-30K';
                                    5='30K-35K';
                                    6='35K-40K';
                                    7='40K-45K';
                                    8='45K-50K';
                                    9='50K>';
                                    0=NA"),
         province = Q19.8 %>% car::recode("2='Noord-Holland';
                                      3='Zuid-Holland';
                                      4='Groningen';
                                     5='Friesland';
                                     6='Drenthe';
                                     7='Overijssel';
                                     8='Flevoland';
                                     9='Gelderland';
                                     10='Utrecht';
                                     11='Zeeland';
                                     12='Noord-Branbant';
                                     13='Limburg';
                                     0=NA"),
         native.language = Q19.9%>% na_if(0))


# Outcome variables
df.total.NL <- df.total.NL %>% 
  mutate(benefits.vaccines = Q6.2_2%>% na_if(0),
         comments.general = Q20.6 %>% na_if(0),
         intent.vaccine = Q16.2 %>% na_if(0),
         intent.vaccine.recoded = Q16.2 %>% car::recode("1=0; 2=0; 3=1; 4=1"),
         credibility.item1 = Q15.2 %>% na_if(0),
         credibility.item2.reversed = Q15.3 %>% car::recode("7=1; 6=2; 5=3; 4=4; 3=5; 2=6; 1=7") %>% na_if(0),
         credibility.item3.reversed = Q15.4 %>% car::recode("7=1; 6=2; 5=3; 4=4; 3=5; 2=6; 1=7") %>% na_if(0),
         credibility.item4 = Q15.5 %>% na_if(0),
         credibility.item5 = Q15.6 %>% na_if(0),
         credibility.item6 = Q15.7 %>% na_if(0),
         credibility.stability = Q15.7 %>% na_if(0)) 
  
# Credibility index
df.total.NL$credibility.index <- df.total.NL %>% dplyr::select(credibility.item1:credibility.item6) %>% base::rowMeans()
df.total.NL$credibility.expert<- df.total.NL %>% dplyr::select(credibility.item4,credibility.item5) %>% base::rowMeans() %>% na_if(0)
df.total.NL$credibility.noninterference<- df.total.NL %>% dplyr::select(credibility.item1:credibility.item3.reversed) %>% base::rowMeans() %>% na_if(0)

# controls
df.total.NL <- df.total.NL %>%
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
         trust.health.authorities = Q6.4 %>% na_if(0),
         perceived.independence.reversed = perceived.independence %>% car::recode("7=1; 6=2; 5=3; 4=4; 3=5; 2=6; 1=7") %>% na_if(0),
         consequences.health = Q6.3_1 %>% na_if(0),
         consequences.economic =  Q6.3_2 %>% na_if(0),
         importance.EMA = Q16.7_1 %>% na_if(0),
         importance.FDA = Q16.7_2 %>% na_if(0),
         importance.NRA = Q16.7_3 %>% na_if(0),
         private.providers = Q20.2 %>% na_if(0),
         decision.first.click = Q14.2_First.Click %>% na_if(0),
         decision.last.click = Q14.2_Last.Click %>% na_if(0),
         decision.submit = Q14.2_Page.Submit %>% na_if(0),
         decision.click.count = Q14.2_Click.Count %>% na_if(0),
         duration = Duration..in.seconds./60 %>% na_if(0))

# Knowledge
df.total.NL <- df.total.NL %>% mutate(knowledge = case_when(
  Q16.5 == 2 & Q16.6 == 2 ~ 1,
  Q16.5 == 1 | Q16.6 == 1 ~ 0))

# Create subsets
df.def.error.NL <-  df.total.NL %>%  
  filter(age >= 18 & IMC=="1")

df.def.NL <-  df.total.NL %>%  
  filter(error == 0  & age >= 18 & IMC=="1")

agecut.5 <- c(-Inf, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, Inf)
agecut.10 <- c(-Inf, 30, 40, 50, 60,  70, 80, Inf)

df.def.NL <- df.def.NL %>% mutate(age.recoded = cut(age, agecut.10, c("younger than 30", "30-39", "40-49", "50-59", "60-69", "70-79", "80 or older"), right=FALSE))
age.sample.NL <- df.def.NL %>% transmute(agecat = cut(age, agecut.5, c("younger than 25", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80 or older"), right=FALSE))  %>% table() %>% as_tibble()

age.sample.NL <- age.sample.NL %>% 
  mutate(percentage = n / sum(n))

df.advice.NL <-  df.def.NL %>% 
  filter(advice == 1)

df.no.text.NL <-  df.def.NL %>% 
  filter(advice == 0)

df.mobile.NL <-  df.def.NL %>% 
  filter(device=="mobile")

df.pc.NL <-  df.def.NL %>% 
  filter(device=="pc")

df.check.NL <-  df.def.NL %>% 
  filter(manipulation.check.failed=="passed")

# Save data     
save.image()

