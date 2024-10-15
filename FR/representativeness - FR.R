# Library
library(dplyr)
library(tidyr)
library(flextable)
library(officer)
library(readr)
library(car)

# Load data
setwd("~/ema_study_check/code/Vaccine-study/FR")
load("~/ema_study_check/code/Vaccine-study/pooled.RData")

rm(list=setdiff(ls(), "FR"))

# Load CBS data
province <- read_delim("~/ema_study_check/data/FR stats/province FR.csv", 
                       delim = ";",
                       escape_double = FALSE,
                       col_types = cols(Country_mean = col_number()),
                       locale = locale(decimal_mark = ",", encoding = "WINDOWS-1252"),
                       trim_ws = TRUE) %>%
  select(Variable, Country_mean) %>%
  drop_na()

education <- read_delim("~/ema_study_check/data/FR stats/education FR.csv", 
                       delim = ";",
                       escape_double = FALSE,
                       col_types = cols(Country_mean = col_number()),
                       locale = locale(decimal_mark = ",", encoding = "WINDOWS-1252"),
                       trim_ws = TRUE) %>%
  select(Variable, Country_mean) %>%
  drop_na()

combined <- bind_rows(province, education)

# Recode variables
FR2 <-FR %>% 
  mutate(education.recoded = Q19.4 %>%
         car::recode("1='Less than primary, primary and lower secondary education';
         2='Less than primary, primary and lower secondary education';
         3='Upper secondary and post-secondary non-tertiary education';
         4='Tertiary education';
         5='Tertiary education';
         6='Tertiary education';
         7='Tertiary education';
         8='Other'"),
         province = Q19.8 %>% na_if(0) %>%
         car::recode("1 = 'Auvergne-Rhône-Alpes';
                  2 = 'Bourgogne-Franche-Comté';
                  3 = 'Bretagne';
                  4 = 'Centre-Val de Loire';
                  5 = 'Corse';
                  6 = 'Grand Est';
                  7 = 'Hauts-de-France';
                  8 = 'Île-de-France';
                  9 = 'Normandie';
                  10 = 'Nouvelle-Aquitaine';
                  11 = 'Occitanie';
                  12 = 'Pays de la Loire';
                  13 = 'Provence-Alpes-Côte d`Azur'"))


# Convert the 'education.recoded' variable to a factor
FR2 <- FR2 %>%
  mutate(education.factor = as.factor(education.recoded),
  province.factor = as.factor(province))


# Create binary columns for each category in 'education.recoded'
education_wide <- FR2 %>%
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
province_wide <- FR2 %>%
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

x <- c('Less than primary, primary and lower secondary education',
       'Upper secondary and post-secondary non-tertiary education',
       'Tertiary education')

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
summary_tables_other <- FR2 %>%
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
    Variable %in% c('Auvergne-Rhône-Alpes',
                    'Bourgogne-Franche-Comté',
                    'Bretagne',
                    'Centre-Val de Loire',
                    'Corse',
                    'Grand Est',
                    'Hauts-de-France',
                    'Île-de-France',
                    'Normandie',
                    'Nouvelle-Aquitaine',
                    'Occitanie',
                    'Pays de la Loire',
                    'Provence-Alpes-Côte d`Azur') ~ "Province",
    Variable %in% x ~ "Education",
    TRUE ~ ""
  )) %>% relocate(Category)

all_sum_tables <- all_sum_tables %>%
  left_join(combined)  %>%
  mutate(Country_mean = case_when(
    Variable == "Age" ~ 50.46383977,
    Variable == "Female" ~ 0.516414375,
    TRUE ~ Country_mean)) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))


# Create table
ft <- flextable(all_sum_tables) %>%
  set_header_labels(Variable = "Variable", Min = "Min", Max = "Max", Mean = "Mean", Stddev = "Std. Deviation", Country_mean = "France
                    (mean)", Category = "") %>%
  merge_v(j = "Category") %>%  # Merge the Category column for grouping
  theme_vanilla() %>%  # Apply a clean theme
  align(j = "Category", align = "left", part = "body") %>%
  align(j = c("Min", "Max", "Mean", "Stddev", "Country_mean"), align = "center", part = "body")  %>%
  
  # Add a title to the table
  set_caption(caption = paste0("TABLE XX Summary Statistics of the Experimental Sample and Mean Values for the General Population (France, N=", FR2 %>% count(),").")) %>%
  
  # Add a footnote
  add_footer_lines(values = "Source: aNational Institute of Statistics and Economic Studies, 2021, Age structure of the population: Demographic balance sheet 2021, https://www.insee.fr/en/statistiques/6040016.
                                    bNational Institute of Statistics and Economic Studies, 2021, Estimations de population, https://www.insee.fr/en/statistiques/6040016.
                                    cNational Institute of Statistics and Economic Studies, 2021, Formation et diplômes, https://catalogue-donnees.insee.fr/fr/explorateur/DS_RP_FORMATION_PRINC
                            Note: Mean age for the French population pertains to the adult population (18 years and older). ")

ft

# Export to a Word document
doc <- read_docx() %>%
  body_add_flextable(ft)


print(doc, target = paste0(getwd(), "/FR table out.docx"))
