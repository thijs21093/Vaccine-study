# Library
library(dplyr)
library(tidyr)
library(flextable)
library(officer)
library(readr)
library(car)

# Load dcar# Load data
setwd("~/ema_study_check/code/Vaccine-study/SE")
load("~/ema_study_check/code/Vaccine-study/pooled.RData")

rm(list=setdiff(ls(), "SE"))

# Load national stats data
province <- read_delim("~/ema_study_check/data/SE stats/province SE.csv", 
                       delim = ";",
                       escape_double = FALSE,
                       col_types = cols(Country_mean = col_number()),
                       locale = locale(decimal_mark = ",", encoding = "WINDOWS-1252"),
                       trim_ws = TRUE) %>%
  select(Variable, Country_mean) %>%
  drop_na()

education <- read_delim("~/ema_study_check/data/SE stats/education SE.csv", 
                       delim = ";",
                       escape_double = FALSE,
                       col_types = cols(Country_mean = col_number()),
                       locale = locale(decimal_mark = ","),
                       trim_ws = TRUE) %>%
  select(Variable, Country_mean) %>%
  drop_na()

combined <- bind_rows(province, education)


# Recode variables
SE2 <-SE %>% 
  filter(check == "no") %>%
  mutate(education.recoded = Q19.4 %>%
  car::recode("1='Primary and secondary education less than 9 years';
        2='Primary and secondary education 9-10 years';
        3='Upper secondary education, 2 years or less';
        4='Upper secondary education 3 years';
        5='Post-secondary education, less than 3 years';
        6='Post-secondary education 3 years or more';
        7='Post-graduate education';
        12='No information about level of educational attainment/Else';
               0=NA"),
  province = Q19.8 %>% car::recode(
               "1 = 'Blekinge';
                2 = 'Dalarna';
                3 = 'Gotland';
                4 = 'Gävleborg';
                5 = 'Halland';
                6 = 'Jämtland';
                7 = 'Jönköping';
                8 = 'Kalmar';
                9 = 'Kronoberg';
                10 = 'Norrbotten';
                11 = 'Skåne';
                12 = 'Stockholm';
                13 = 'Södermanland';
                14 = 'Uppsala';
                15 = 'Värmland';
                16 = 'Västerbotten';
                17 = 'Västernorrland';
                18 = 'Västmanland';
                19 = 'Örebro';
                20 = 'Östergötland';
                21 = 'Västra Götaland';
                0 = NA"))

# Convert the 'education.recoded' variable to a factor
SE2 <- SE2 %>%
  mutate(education.factor = as.factor(education.recoded),
  province.factor = as.factor(province))

# Create binary columns for each category in 'education.recoded'
education_wide <- SE2 %>%
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
province_wide <- SE2 %>%
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

x <- c("Primary and secondary education less than 9 years",
       "Primary and secondary education 9-10 years",
       "Upper secondary education, 2 years or less",
       "Upper secondary education 3 years",
       "Post-secondary education, less than 3 years",
       "Post-secondary education 3 years or more",
       "Post-graduate education",
       "No information about level of educational attainment/Else")

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
summary_tables_other <- SE2 %>%
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
    Variable %in% c('Blekinge',
                'Dalarna',
                'Gotland',
                'Gävleborg',
                'Halland',
                'Jämtland',
                'Jönköping',
                'Kalmar',
                'Kronoberg',
                'Norrbotten',
                'Skåne',
                'Stockholm',
                'Södermanland',
                'Uppsala',
                'Värmland',
                'Västerbotten',
                'Västernorrland',
                'Västmanland',
                'Örebro',
                'Östergötland',
                'Västra Götaland') ~ "Province",
    Variable %in% x ~ "Education",
    TRUE ~ "")) %>%
  relocate(Category)

all_sum_tables <- all_sum_tables %>%
  left_join(combined)  %>%
  mutate(Country_mean = case_when(
    Variable == "Age" ~ 49.69350695,
    Variable == "Female" ~ 0.49669509,
    TRUE ~ Country_mean)) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

# Create table
ft <- flextable(all_sum_tables) %>%
  set_header_labels(Variable = "Variable", Min = "Min", Max = "Max", Mean = "Mean", Stddev = "Std. Deviation", Country_mean = "Sweden
                    (mean)", Category = "") %>%
  merge_v(j = "Category") %>%  # Merge the Category column for grouping
  theme_vanilla() %>%  # Apply a clean theme
  align(j = "Category", align = "left", part = "body") %>%
  align(j = c("Min", "Max", "Mean", "Stddev", "Country_mean"), align = "center", part = "body")  %>%
  
  # Add a title to the table
  set_caption(caption = paste0("TABLE XX Summary Statistics of the Experimental Sample and Mean Values for the General Population (Sweden, N=", SE2 %>% count(),").")) %>%
  
  # Add a footnote
  add_footer_lines(values = "Source: aStatistics Sweden, 2021, Folkmängden efter region, civilstånd, ålder och kön. År 1968 - 2023, https://www.statistikdatabasen.scb.se/pxweb/en/ssd/START__BE__BE0101__BE0101A/BefolkningNy/.
                    bStatistics Sweden, 2021, Befolkning 16-95+ år efter utbildningsnivå och år, https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__UF__UF0506__UF0506B/UtbBefRegionR/.
                   Note: Mean age for the Swedish population pertains to the adult population (18 years and older).")

ft

# Export to a Word document
doc <- read_docx() %>%
  body_add_flextable(ft)


print(doc, target = paste0(getwd(), "/SE table out.docx"))
