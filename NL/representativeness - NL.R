# Library
library(dplyr)
library(tidyr)
library(flextable)
library(officer)
library(readr)
library(car)

# Load data
setwd("~/ema_study_check/code/Vaccine-study/NL")
load("~/ema_study_check/code/Vaccine-study/pooled.RData")

rm(list=setdiff(ls(), "NL"))

# Load CBS data
province <- read_delim("~/ema_study_check/data/NL stats/province NL.csv", 
                       delim = ";",
                       escape_double = FALSE,
                       col_types = cols(Country_mean = col_number()),
                       locale = locale(decimal_mark = ","),
                       trim_ws = TRUE) %>%
  select(Variable, Country_mean) %>%
  drop_na()

education <- read_delim("~/ema_study_check/data/NL stats/education NL.csv", 
                       delim = ";",
                       escape_double = FALSE,
                       col_types = cols(Country_mean = col_number()),
                       locale = locale(decimal_mark = ","),
                       trim_ws = TRUE) %>%
  select(Variable, Country_mean) %>%
  drop_na()

combined <- bind_rows(province, education)

# Recode variables
NL2 <-NL %>% 
  mutate(education = Q19.4 %>% car::recode("1='1. VMBO/Mavo';
                                      2='2. Havo';
                                      3='3. Vwo';
                                      4='4. MBO';
                                      5='5. HBO Bachelor';
                                      6='6. WO Bachelor';
                                      7='7. HBO Master';
                                      8='8. WO Master of hoger';
                                      9='9. Anders';
                                      0=NA"),
         education.recoded = Q19.4 %>% car::recode("1='Pre-vocational secondary education';
                                      2='Senior general secondary education, pre-university education or vocational education';
                                      3='Senior general secondary education, pre-university education or vocational education';
                                      4='Senior general secondary education, pre-university education or vocational education';
                                      5='Bachelor';
                                      6='Bachelor';
                                      7='Master/doctoral';
                                      8='Master/doctoral';
                                      9='Do not know/Unknown/Other';
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
                                     12='Noord-Brabant';
                                     13='Limburg';
                                     0=NA"))


# Convert the 'education.recoded' variable to a factor
NL2 <- NL2 %>%
  mutate(education.factor = as.factor(education.recoded),
  province.factor = as.factor(province))


# Create binary columns for each category in 'education.recoded'
education_wide <- NL2 %>%
  select(education.factor) %>%
  drop_na() %>%
  mutate(id = row_number()) %>%  # Add an ID column to keep track of rows
  pivot_wider(
    names_from = education.factor,  # The unique categories will become column names
    values_from = education.factor,  # Values come from the original variable
    values_fn = list(education.factor = ~ as.integer(!is.na(.)))  # Convert to 1/0
  ) %>%
  mutate(across(where(is.numeric), ~replace_na(., 0)))   %>% 
  select(-id)  # Remove the ID column if not needed

# Create binary columns for each category in 'province'
province_wide <- NL2 %>%
  select(province.factor) %>%
  drop_na() %>%
  mutate(id = row_number()) %>%  # Add an ID column to keep track of rows
  pivot_wider(
    names_from = province.factor,  # The unique categories will become column names
    values_from = province.factor,  # Values come from the original variable
    values_fn = list(province.factor = ~ as.integer(!is.na(.)))  # Convert to 1/0
  ) %>%
  mutate(across(where(is.numeric), ~replace_na(., 0)))   %>% 
  select(-id)  # Remove the ID column if not needed


# Create summary table for province and education
province_sum <- province_wide %>%
  reframe(across(where(is.numeric), list(
    Min = ~min(.x, na.rm = TRUE),
    Max = ~max(.x, na.rm = TRUE),
    Mean = ~mean(.x, na.rm = TRUE),
    Stddev = ~sd(.x, na.rm = TRUE)
  )))  %>%
  pivot_longer(cols = everything(), 
               names_to = c("Variable", ".value"), 
               names_pattern = "(.*)_(.*)") %>%
  select(Variable, Min, Max, Mean, Stddev)

x <- c("Pre-vocational secondary education",
       "Senior general secondary education, pre-university education or vocational education",
       "Bachelor",
       "Master/doctoral",
       "Do not know/Unknown/Other")

education_sum <- education_wide %>%
  reframe(across(where(is.numeric), list(
    Min = ~min(.x, na.rm = TRUE),
    Max = ~max(.x, na.rm = TRUE),
    Mean = ~mean(.x, na.rm = TRUE),
    Stddev = ~sd(.x, na.rm = TRUE)
  )))  %>%
  pivot_longer(cols = everything(), 
               names_to = c("Variable", ".value"), 
               names_pattern = "(.*)_(.*)") %>%
  select(Variable, Min, Max, Mean, Stddev) %>%
  slice(match(x, Variable))


# Summarize age and gender
summary_tables_other <- NL2 %>%
  reframe(
    Variable = c("Age", "Female"),
    Min = c(min(age, na.rm = TRUE), min(female, na.rm = TRUE)),
    Max = c(max(age, na.rm = TRUE), max(female, na.rm = TRUE)),
    Mean = c(mean(age, na.rm = TRUE), mean(female, na.rm = TRUE)),
    Stddev = c(sd(age, na.rm = TRUE), sd(female, na.rm = TRUE))
  )

# Add all tables together and add label
all_sum_tables <- bind_rows(summary_tables_other, province_sum, education_sum)%>%
  mutate(Category = case_when(
    Variable %in% c("Noord-Brabant", "Groningen", "Utrecht", "Noord-Holland", "Zeeland", 
                    "Overijssel", "Gelderland", "Zuid-Holland", "Flevoland", 
                    "Limburg", "Drenthe", "Friesland") ~ "Province",
    Variable %in% x ~ "Education",
    TRUE ~ ""
  )) %>% relocate(Category)

all_sum_tables <- all_sum_tables %>%
  left_join(combined)  %>%
  mutate(Country_mean = case_when(
    Variable == "Age" ~ 49.55946586,
    Variable == "Female" ~ 0.502928199,
    TRUE ~ Country_mean)) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))


# Create table
ft <- flextable(all_sum_tables) %>%
  set_header_labels(Variable = "Variable", Min = "Min", Max = "Max", Mean = "Mean", Stddev = "Std. Deviation", Country_mean = "Netherlands
                    (mean)", Category = "") %>%
  merge_v(j = "Category") %>%  # Merge the Category column for grouping
  theme_vanilla() %>%  # Apply a clean theme
  align(j = "Category", align = "left", part = "body") %>%
  align(j = c("Min", "Max", "Mean", "Stddev", "Country_mean"), align = "center", part = "body")  %>%
  
  # Add a title to the table
  set_caption(caption = "TABLE XX Summary Statistics of the Experimental Sample and Mean Values for the General Population (Netherlands, N=1317).") %>%
  
  # Add a footnote
  add_footer_lines(values = "Source: aStatistics Netherlands, 2021, Bevolking op 1 januari en gemiddeld; geslacht, leeftijd en regio, https://opendata.cbs.nl/#/CBS/nl/dataset/03759ned/table?dl=AE555. bStatistics Netherlands, 2021, Bevolking; hoogstbehaald onderwijsniveau en onderwijsrichting, https://opendata.cbs.nl/#/CBS/nl/dataset/85313NED/table?dl=AE647. Note: Mean age for the Dutch population pertains to the adult population (18 years and older). The answer options in the survey differ somewhat from the categorisation used by Netherlands Statistics. Comparison should be made cautiously.")

ft

# Export to a Word document
doc <- read_docx() %>%
  body_add_flextable(ft)


print(doc, target = paste0(getwd(), "/NL table out.docx"))
