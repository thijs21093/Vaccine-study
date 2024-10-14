# Library
library(dplyr)
library(tidyr)
library(flextable)
library(officer)
library(readr)
library(car)

# Load data
setwd("~/ema_study_check/code/Vaccine-study/EN")
load("~/ema_study_check/code/Vaccine-study/pooled.RData")

rm(list=setdiff(ls(), "IE"))

# Load CBS data
province <- read_delim("~/ema_study_check/data/IE stats/province IE.csv", 
                       delim = ";",
                       escape_double = FALSE,
                       col_types = cols(Country_mean = col_number()),
                       locale = locale(decimal_mark = ","),
                       trim_ws = TRUE) %>%
  select(Variable, Country_mean) %>%
  drop_na()

education <- read_delim("~/ema_study_check/data/IE stats/education IE.csv", 
                       delim = ";",
                       escape_double = FALSE,
                       col_types = cols(Country_mean = col_number()),
                       locale = locale(decimal_mark = ","),
                       trim_ws = TRUE) %>%
  select(Variable, Country_mean) %>%
  drop_na()

combined <- bind_rows(province, education)

# Recode variables
IE2 <-IE %>% 
  mutate(education.recoded = Q19.4 %>% car::recode("1='Primary';
                                                    2='Lower secondary';
                                                    3='Upper secondary';
                                                    4='Post leaving cert';
                                                    5='Higher certificate';
                                                    6='Ordinary bachelor degree/professional qualification or both';
                                                    7='Honours bachelor degree/professional qualification or both';
                                                    8='Postgraduate diploma/degree or Doctorate (Ph.D.)';
                                                    10='Postgraduate diploma/degree or Doctorate (Ph.D.)';
                                                    9='Other'"),
         province = Q19.8 %>%
           na_if(0) %>%
           car::recode("1 = 'Carlow County Council';
         2 = 'Cavan County Council';
         3 = 'Clare County Council';
         4 = 'Cork City Council';
         5 = 'Cork County Council';
         6 = 'Donegal County Council';
         7 = 'Dublin City Council';
         8 = 'Dún Laoghaire Rathdown County Council';
         9 = 'Fingal County Council';
         10 = 'Galway City Council';
         11 = 'Galway County Council';
         12 = 'Kerry County Council';
         13 = 'Kildare County Council';
         14 = 'Kilkenny County Council';
         15 = 'Laois County Council';
         16 = 'Leitrim County Council';
         17 = 'Limerick City & County Council';
         18 = 'Longford County Council';
         19 = 'Louth County Council';
         20 = 'Mayo County Council';
         21 = 'Meath County Council';
         22 = 'Monaghan County Council';
         23 = 'Offaly County Council';
         24 = 'Roscommon County Council';
         25 = 'Sligo County Council';
         26 = 'South Dublin County Council';
         27 = 'Tipperary County Council';
         28 = 'Waterford City & County Council';
         29 = 'Westmeath County Council';
         30 = 'Wexford County Council';
         31 = 'Wicklow County Council'")
         )



# Convert the 'education.recoded' variable to a factor
IE2 <- IE2 %>%
  mutate(education.factor = as.factor(education.recoded),
  province.factor = as.factor(province))


# Create binary columns for each category in 'education.recoded'
education_wide <- IE2 %>%
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
province_wide <- IE2 %>%
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

x <- c("Primary",
       "Lower secondary",
       "Upper secondary",
       "Post leaving cert",
       "Higher certificate",
       "Ordinary bachelor degree/professional qualification or both",
       "Honours bachelor degree/professional qualification or both",
       "Postgraduate diploma/degree or Doctorate (Ph.D.)",
       "Other")

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
summary_tables_other <- IE2 %>%
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
    Variable %in% c('Carlow County Council',
                    'Cavan County Council',
                    'Clare County Council',
                    'Cork City Council',
                    'Cork County Council',
                    'Donegal County Council',
                    'Dublin City Council',
                    'Dún Laoghaire Rathdown County Council',
                    'Fingal County Council',
                    'Galway City Council',
                    'Galway County Council',
                    'Kerry County Council',
                    'Kildare County Council',
                    'Kilkenny County Council',
                    'Laois County Council',
                    'Leitrim County Council',
                    'Limerick City & County Council',
                    'Longford County Council',
                    'Louth County Council',
                    'Mayo County Council',
                    'Meath County Council',
                    'Monaghan County Council',
                    'Offaly County Council',
                    'Roscommon County Council',
                    'Sligo County Council',
                    'South Dublin County Council',
                    'Tipperary County Council',
                    'Waterford City & County Council',
                    'Westmeath County Council',
                    'Wexford County Council',
                    'Wicklow County Council') ~ "Province",
    Variable %in% x ~ "Education",
    TRUE ~ ""
  )) %>% relocate(Category)

all_sum_tables <- all_sum_tables %>%
  left_join(combined)  %>%
  mutate(Country_mean = case_when(
    Variable == "Age" ~ 47.22254975,
    Variable == "Female" ~ 0.504591495,
    TRUE ~ Country_mean)) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))


# Create table
ft <- flextable(all_sum_tables) %>%
  set_header_labels(Variable = "Variable", Min = "Min", Max = "Max", Mean = "Mean", Stddev = "Std. Deviation", Country_mean = "Ireland
                    (mean)", Category = "") %>%
  merge_v(j = "Category") %>%  # Merge the Category column for grouping
  theme_vanilla() %>%  # Apply a clean theme
  align(j = "Category", align = "left", part = "body") %>%
  align(j = c("Min", "Max", "Mean", "Stddev", "Country_mean"), align = "center", part = "body")  %>%
  
  # Add a title to the table
  set_caption(caption = "TABLE XX Summary Statistics of the Experimental Sample and Mean Values for the General Population (Ireland, N=980).") %>%
  
  # Add a footnote
  add_footer_lines(values = "Source: aCentral Statistics Office, Ireland, 2021, Population estimates from 1926, https://data.cso.ie/table/PEA11.
                   bCentral Statistics Office, Ireland, 2021, Population Estimates (Persons in April), https://data.cso.ie/table/PEA01.
                   cCentral Statistics Office, Ireland, 2022, Population, https://data.cso.ie/table/F1004A.
                   dCentral Statistics Office, Ireland, 2021, Persons Aged 15-64, https://data.cso.ie/table/EDQ01.
                   Note: Mean age for the Irish population pertains to the adult population (18 years and older). Educational attainment for Irish population pertains to age group 15-64.")

ft

# Export to a Word document
doc <- read_docx() %>%
  body_add_flextable(ft)


print(doc, target = paste0(getwd(), "/IE table out.docx"))
