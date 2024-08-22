# Load libraries
library(tidyverse)
library(scales)
library(readxl)
library(patchwork)

# Set working directory
setwd('~/Documents/repos/R/mexico_pop')
dir  <- getwd()

# Create a value with the current year
current_year <- as.numeric(format(Sys.Date(), "%Y"))

# Load dataset with the population per municipality, age groups, and sex
data <- read_excel(paste0(dir,'/datasets/1_Grupo_Quinq_00_RM.xlsx')) %>% 
  select(-c(7:24))

# Create a vector to translate columns names into English
columns <- c('key', 'key_state', 'state', 'mun', 'sex', 'year', 'total')

# Change the columns names
colnames(data) <- columns

# Prepare the data for the plots
pop <- data %>% 
  group_by(key, key_state, state, mun, year) %>% 
  summarise(total = sum(total)) %>% 
  group_by(key) %>%
  mutate(
    pop_1990 = total[year == 1990],
         pop_current = total[year == current_year],
         pop_2040 = total[year == 2040],
         change_1990_current = round((pop_current - pop_1990) / pop_1990,5),
         change_current_2040 = round((pop_2040 - pop_current) / pop_current,5),
         change_1990_2040 = round((pop_2040 - pop_1990) / pop_1990 * 100,5),
         period = ifelse(year <= current_year, 'past', 'future'),
         trend = ifelse(year <= current_year, 'past', 
                        ifelse(change_current_2040 > 0, 'increase_future', 'decrease_future'))
    )

# Define colors
color_past <- '#7f7f7f'  # Blue for the past (1990 to 2024)
color_increase_future <- '#2ca02c'  # Green for increasing in the future (2025 to 2040)
color_decrease_future <- '#d62728'  # Red for decreasing in the future (2025 to 2040)
mid_grey <- '#b0b0b0'  # Light grey for the vertical line
light_grey <-  '#f5f5f5' # Lighter grey for the lable of the vertical line

# Create a function to produce the plots
line_plot_function <-
  function(
    state_to_filter, #Since there are instances of municipalities withthe same name, it´s important to filter by State as well
    mun_to_filter
    ) {
    # This creates a max value for the y axis to place the label of the current year  
    y_max <- pop %>% 
        filter(state == state_to_filter) %>% 
        filter(mun == mun_to_filter) %>% 
        slice_max(
          order_by = total,
          n = 1
        ) %>% 
        pull(total)

      y_max <- y_max/1.02 # Reduce the value of y_max to better place the label
   
     line_plot <-  pop %>% 
        filter(state == state_to_filter) %>% 
        filter(mun == mun_to_filter) %>% 
        ggplot(aes(x = year, y = total, color = trend)) +
        geom_line(size = 1) +
        scale_color_manual(values = c(
          'past' = color_past,
          'increase_future' = color_increase_future,
          'decrease_future' = color_decrease_future
        )) +
        theme_minimal() +
        theme_minimal() +
        theme(
          axis.title.x = element_blank(),  
          axis.title.y = element_blank(),  
          panel.grid.major = element_line(color = '#e5e5e5'),
          panel.grid.minor = element_line(color = 'grey95'),
          legend.position = 'none'
        ) +
        geom_vline(xintercept = current_year, linetype = 'dashed', color = mid_grey) +
        annotate(
          geom = 'label',
          x = current_year-2,
          y = y_max,
          label = current_year,
          fill = mid_grey,
          color = light_grey,
          label.size = 0,
          label.padding = unit(0.3, 'lines')) +
        scale_x_continuous(breaks = seq(1990, 2040, 5)) +
        scale_y_continuous(labels = scales::comma)
     
     line_plot +
       plot_annotation(
         title = paste0('Population Trends in the Municipality of ', mun_to_filter,', ',state_to_filter,' (1990-2040)'),
         caption = 'Source: Reconstrucción y proyecciones de la población de los municipios de México 1990-2040, CONAPO, 2024\nPlot: zer.ø Data Viz',
         theme = theme(
           plot.title = element_text(hjust = 0, size = 14, face = "bold"),
           plot.subtitle = element_text(hjust = 0, size = 12),
           plot.caption = element_text(hjust = 0, size = 8)
         )
       )
    }

# Test the function
line_plot_function('Baja California', 'Mexicali')
