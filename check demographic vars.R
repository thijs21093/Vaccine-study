# Library
library(dplyr)
library(tidyr)
library(flextable)
library(officer)
library(readr)
library(car)

load("~/ema_study_check/code/Vaccine-study/pooled.RData")
rm(list=setdiff(ls(), "pooled"))
analytic_sample <- read_csv("analytic_sample.csv")

pooled <- pooled %>% filter(check == "no") %>%
  mutate(income =
           case_when(
             country == "NL" ~ Q19.6-1,
             country == "IE" ~ Q19.6-1,
             TRUE ~ Q19.6
           ))

pooled <- pooled %>% 
  mutate(migration_background = Q19.10 %>% na_if(0)%>% recode("1=0; 2=1; 3=1"))

# Income
bind_cols(
  pooled %>% group_by(income) %>% count() %>% ungroup() %>% add_row(income = 9, n = NA),
  analytic_sample %>% group_by(income) %>% count()
  )

# Migration background
bind_cols(
  pooled %>% group_by(migration_background) %>% count(),
  analytic_sample %>% group_by(migration_background) %>% count())

# Education
bind_cols(
  pooled %>% group_by(Q19.4) %>% count(),
  analytic_sample %>% group_by(Q19.4) %>% count(),
  analytic_sample %>% group_by(education) %>% count()) %>%
    rename(pooled_Q19.4 = Q19.4...1,
           analytic_sample_Q19.4 = Q19.4...3,
           analytic_sample_education = education)

analytic_sample %>% filter(is.na(education)) %>%
  count(country, Q19.4)

analytic_sample %>%
  group_by(country) %>% count(income) %>%
  ungroup() %>%
  pivot_wider(names_from = country, values_from = n)
