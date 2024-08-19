# Load libraries
library(tidyverse)
library(readxl)

# Set working directory
setwd('~/Documents/repos/R/mexico_pop')
dir  <- getwd()

# Load dataset with the population per muncipality, age groups, and sex
pop <- readxl::read_excel(paste0(dir,'/datasets','/','1_Grupo_Quinq_00_RM.xlsx'))

# Create a vector to translate columns names into English
columns <- c('key', 'key_ent', 'ent', 'mun', 'sex', 'year',
             'pop_00_04', 'pop_05_09', 'pop_10_14', 'pop_15_19', 'pop_20_24',
             'pop_25_29', 'pop_30_34', 'pop_35_39', 'pop_40_44', 'pop_45_49',
             'pop_50_54', 'pop_55_59', 'pop_60_64', 'pop_65_69', 'pop_70_74',
             'pop_75_79', 'pop_80_84', 'pop_85_mm', 'pop_total')

# Change the columns names
colnames(pop) <- columns

# Clean the data to leave only the total for the period 2016-2024
pop_t <- pop %>% 
  filter(year > 2015) %>%
  filter(year < 2025) %>% 
  select(-c(7:24)) %>% 
  group_by(key, key_ent, ent, mun, year) %>% 
  summarise(pop_total = sum(pop_total))

# Create a female-only dataset
pop_f <- pop %>% 
  filter(sex == 'MUJERES') %>% 
  filter(year > 2015) %>%
  filter(year < 2025) %>% 
  select(-c(7:24)) %>% 
  group_by(key, key_ent, ent, mun, year) %>% 
  summarise(pop_total = sum(pop_total))

# Create a male-only dataset
pop_m <- pop %>% 
  filter(sex == 'HOMBRES') %>% 
  filter(year > 2015) %>%
  filter(year < 2025) %>% 
  select(-c(7:24)) %>% 
  group_by(key, key_ent, ent, mun, year) %>% 
  summarise(pop_total = sum(pop_total))

