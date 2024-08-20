# Load libraries
library(tidyverse)
library(scales)
library(readxl)
library(patchwork)

# Set working directory
setwd('~/Documents/repos/R/mexico_pop')
dir  <- getwd()

# # Initial setup to choose what year and what state is to be analyzed
# state <- 'Baja California'
# yearan <- 2024

# Load dataset with the population per municipality, age groups, and sex
data <- read_excel(paste0(dir,'/datasets/1_Grupo_Quinq_00_RM.xlsx'))

# Create a vector to translate columns names into English
columns <- c('key', 'key_ent', 'ent', 'mun', 'sex', 'year',
             'pop_00_04', 'pop_05_09', 'pop_10_14', 'pop_15_19', 'pop_20_24',
             'pop_25_29', 'pop_30_34', 'pop_35_39', 'pop_40_44', 'pop_45_49',
             'pop_50_54', 'pop_55_59', 'pop_60_64', 'pop_65_69', 'pop_70_74',
             'pop_75_79', 'pop_80_84', 'pop_85_mm', 'pop_total')

# Change the columns names
colnames(data) <- columns

# Pivot the dataset from wide to long, add percentage column
pop <- data %>% 
  pivot_longer(
  cols = starts_with('pop_') & !ends_with('total'),
  names_to = 'age_group',
  values_to = 'population'
) %>% 
  group_by(year) %>% 
  mutate(pct = round(population/pop_total, 5)) %>% 
  ungroup() %>%
  mutate(pct = if_else(sex == 'HOMBRES', pct, -pct)) %>% # Make female values negative to show them on the left side of the plot
  select(-7) # Remove total_pop column to clean the table

# Create a female-only dataset
pop_f <- pop %>% 
  filter(sex == 'MUJERES')

# Create a male-only dataset
pop_m <- pop %>% 
  filter(sex == 'HOMBRES')

