library(tidyverse)
library(reactable)
library(htmltools)
library(readxl)

# Load the data
data <- read_xlsx('~/Documents/repos/R/mexico_pop/datasets/1_Grupo_Quinq_00_RM.xlsx')

# Filter to only leave the desired state
bc <- data %>% 
  filter(CLAVE_ENT == 2,
         AÑO %in% c(1990, 2024)) %>% 
  select(-c(1:3),-5,-c(7:24)) %>% 
  group_by(NOM_MUN, AÑO) %>% 
  summarise(POB = sum(POB_TOTAL)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = AÑO, values_from = POB, names_prefix = 'POB_') %>% 
  mutate(PCT_CHANGE = (POB_2024-POB_1990)/POB_1990)

columns <- c('Mun', 'Pop_1990', 'Pop_2024', 'Pct_Change')

colnames(bc) <- columns
  
# Render a bar chart with a label on the left
bar_chart <- function(label, width = '100%', height = '0.875rem', fill = '#00bfc4', background = NULL) {
  bar <- div(style = list(background = fill, width = width, height = height))
  chart <- div(style = list(flexGrow = 1, marginLeft = '0.375rem', background = background), bar)
  div(style = list(display = 'flex', alignItems = 'center'), label, chart)
}


# Create the reactable
reactable(
  bc,
  defaultSorted = 'Pct_Change',
  columns = list(
    Mun = colDef(
      name = 'Municipality',
      format = colFormat()
    ),
    Pop_1990 = colDef(
      name = '1990 Population',
      defaultSortOrder = 'desc',
      # Render the bar charts using a custom cell render function
      cell = function(value) {
        width <- paste0(value * 100 / max(bc$Pop_1990), '%')
        value <- format(value, big.mark = ',')
        value <- format(value, width = 9, justify = 'right')
        bar_chart(value, width = width, fill = '#3fc1c9')
      },
      # And left-align the columns
      align = 'left',
      # Use the operating system's default monospace font, and
      # preserve white space to prevent it from being collapsed by default
      style = list(fontFamily = 'monospace', whiteSpace = 'pre')
          ),
    Pop_2024 = colDef(
      name = '2024 Population',
      # Render the bar charts using a custom cell render function
      cell = function(value) {
        width <- paste0(value * 100 / max(bc$Pop_2024), '%')
        value <- format(value, big.mark = ',')
        value <- format(value, width = 9, justify = 'right')
        bar_chart(value, width = width, fill = '#3fc1c9')
      },
          align = 'left',
          style = list(fontFamily = 'monospace', whiteSpace = 'pre')
    ),
    Pct_Change = colDef(
      name = 'Percent Change 1990-2024',
      defaultSortOrder = 'desc',
      # Format and render the cell with a JavaScript render function
      cell = JS('function(cellInfo) {
        // Format as a percentage with 1 decimal place
        const pct = (cellInfo.value * 100).toFixed(1) + "%"
        // Fix width of numeric labels
        let value = pct.padStart(5)
        // Show percent sign on first row only
        if (cellInfo.viewIndex > 0) {
          value = value.replace("%", " ")
        }
        // Render bar chart
        return `
          <div style="display: flex; align-items: center;">
            <span style="font-family: monospace; white-space: pre;">${value}</span>
            <div style="flex-grow: 1; margin-left: 0.375rem; height: 0.875rem; background-color: #e1e1e1">
              <div style="height: 100%; width: ${pct}; background-color: #fc5185"></div>
            </div>
          </div>
        `
      }'),
      # Render this column as HTML
      html = TRUE,
      align = "left"
    )
  )
)
        
#         
#         function(value) {
#         # Scale the width relative to the maximum value
#         width <- paste0(value * 100 / max(bc$Pct_Change), '%')
#         # Format as percentages with 1 decimal place
#         value <- paste0(format(value * 100, nsmall = 1), '%')
#         bar_chart(value, width = width, fill = '#fc5185', background = '#e1e1e1')
#       },
#       # And left-align the columns
#       align = 'left',
#       # Use the operating system's default monospace font, and
#       # preserve white space to prevent it from being collapsed by default
#       style = list(fontFamily = 'monospace', whiteSpace = 'pre')
#     )
#   )
# )