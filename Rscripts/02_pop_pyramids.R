# Load libraries
library(tidyverse)
library(scales)
library(readxl)
library(patchwork)

# Set working directory
setwd('~/Documents/repos/R/mexico_pop')
dir  <- getwd()

# Load dataset with the population per municipality, age groups, and sex
data <- read_excel(paste0(dir,'/datasets/1_Grupo_Quinq_00_RM.xlsx'))

# Create a vector to translate columns names into English
columns <- c('key', 'key_state', 'state', 'mun', 'sex', 'year',
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
  mutate(sex = case_when( #Change the values from SPANISH to English
    sex == 'HOMBRES' ~ 'Men',
    sex == 'MUJERES' ~ 'Women'
  )) %>% 
  mutate(pct = if_else(sex == 'Men', pct, -pct)) %>% # Make female values negative to show them on the left side of the plot
  select(-7) # Remove total_pop column to clean the table

# Create a function to produce sex plots
single_sex_plot <- 
  function(
    year_to_filter,
    state_to_filter,
    mun_to_filter,
    sex_to_filter
  ) {
    max_pct <- pop %>% 
      filter(year == year_to_filter) %>% 
      filter(state == state_to_filter) %>% 
      filter(mun == mun_to_filter) %>% 
      slice_max(
        order_by = pct,
        n = 1
      ) %>% 
      pull(pct)
    
    if (sex_to_filter == 'Men') {
      x_limits <- c(0, max_pct * 1.1)
      gender_label_x_position <- 0.05
      fill_color <- '#6A3D9A'
      sex_text_color <-  '#f5f5f5'
    }
    
    if (sex_to_filter == 'Women') {
      x_limits <- c(-max_pct * 1.1, 0)
      gender_label_x_position <- -0.05
      fill_color <- '#FF7F00'
      sex_text_color <-  '#f5f5f5'
    }
    
    pop %>% 
      filter(year == year_to_filter) %>% 
      filter(state == state_to_filter) %>% 
      filter(mun == mun_to_filter) %>% 
      filter(sex == sex_to_filter) %>%
      ggplot(aes(
        x = pct,
        y = age_group
      )) +
      geom_col(fill = fill_color) +
      annotate(
        geom = 'label',
        x = gender_label_x_position,
        y = 17,
        label = sex_to_filter,
        fill = fill_color,
        color = sex_text_color,
        label.size = 0,
        label.padding = unit(0.3, 'lines')
      ) +
      scale_x_continuous(
        labels = function(x) label_percent(accuracy = 1)(abs(x)),
        breaks = breaks_pretty(),
        limits = x_limits
      ) +
      coord_cartesian(clip = 'off') +
      theme_void() +
      theme(
        axis.text.x = element_text(),
        panel.grid.major.x = element_line(color = '#e0e0e0')
      )
  }

# Create a second function to produce the populaton pyramid plot
pyramid_plot <- function(
    year_to_filter,
    state_to_filter,
    mun_to_filter
) {
  w_plot <-
    single_sex_plot(
      year_to_filter = year_to_filter,
      state_to_filter = state_to_filter,
      mun_to_filter = mun_to_filter,
      sex_to_filter = 'Women'
    )
  
  m_plot <-
    single_sex_plot(
      year_to_filter = year_to_filter,
      state_to_filter = state_to_filter,
      mun_to_filter = mun_to_filter,
      sex_to_filter = 'Men'
    )
  
  w_plot +
    age_labels_plot +
    m_plot +
    plot_layout(
      widths = c(7.5, 1, 7.5)
      ) +
        plot_annotation(
          title = paste0('Population Distribution in the Municipality of ', mun_to_filter,', ',state_to_filter,' (',year_to_filter,')'),
          subtitle = 'Age group breakdown by sex as assigned at birth',
          caption = 'Source: Reconstrucción y proyecciones de la población de los municipios de México 1990-2040, CONAPO, 2024\nPlot: zer.ø Data Viz',
          theme = theme(
            plot.title = element_text(hjust = 0, size = 14, face = "bold"),
            plot.subtitle = element_text(hjust = 0, size = 12),
            plot.caption = element_text(hjust = 1, size = 10)
          )
        )
}

# Test the function
pyramid_plot(2024, 'Jalisco', 'Zapopan')
