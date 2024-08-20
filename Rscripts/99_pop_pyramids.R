pop <- pop %>% 
  filter(year == 2024)

max_pct <- pop %>% 
  filter(mun == 'Aguascalientes') %>% 
  slice_max(
    order_by = pct,
    n = 1
  ) %>% 
  pull(pct)

pop_f_plot <-
  pop |>
  filter(mun == 'Aguascalientes') |>
  filter(sex == 'MUJERES') |>
  filter(year == 2024) |>
  ggplot(aes(
    x = pct,
    y = age_group
  )) +
  geom_col(fill = '#FF7F00') +
  annotate(
    geom = 'label',
    x = -max_pct + 0.05 * max_pct,
    y = 17,
    label = 'Women',
    fill = '#FF7F00',
    color = '#FFFFFF',
    label.size = 0,
    label.padding = unit(0.3, 'lines')
  ) +
  scale_x_continuous(
    labels = function(x) label_percent(accuracy = 1)(abs(x)),
    breaks = breaks_pretty(),
    limits = c(-max_pct, 0)
  ) +
  theme_void() +
  theme(
    axis.text.x = element_text(),
    panel.grid.major.x = element_line(color = "grey90")
  )

pop_m_plot <-
  pop |>
  filter(mun == 'Aguascalientes') |>
  filter(sex == 'HOMBRES') |>
  filter(year == 2024) |>
  ggplot(aes(
    x = pct,
    y = age_group
  )) +
  geom_col(fill = '#6A3D9A') +
  annotate(
    geom = 'label',
    x = max_pct -0.05 * max_pct,
    y = 17,
    label = 'Men',
    fill = '#6A3D9A',
    color = '#FFFFFF',
    label.size = 0,
    label.padding = unit(0.3, 'lines')
  ) +
  scale_x_continuous(
    labels = function(x) label_percent(accuracy = 1)(abs(x)),
    breaks = breaks_pretty(),
    limits = c(0, max_pct)
  ) +
  theme_void() +
  theme(
    axis.text.x = element_text(),
    panel.grid.major.x = element_line(color = "grey90")
  )

# Create an age labels tibble for the plot
age_labels <-
  tibble(
    age = c(
      '0-4',
      '5-9',
      '10-14',
      '15-19',
      '20-24',
      '25-29',
      '30-34',
      '35-39',
      '40-44',
      '45-49',
      '50-54',
      '55-59',
      '60-64',
      '65-69',
      '70-74',
      '75-79',
      '80-84',
      '85+'
    )
  ) |>
  mutate(
    age = fct_inorder(age)
  )

# Age labels plot
age_labels_plot <- age_labels |>
  ggplot(
    aes(
      x = 1,
      y = age,
      label = age
    )
  ) +
  geom_text() +
  theme_void()


pop_f_plot +
  age_labels_plot +
  pop_m_plot +
  plot_layout(
    widths = c(7.5, 1, 7.5)
  ) +
  plot_annotation(
    title = 'Population Distribution in the Municipality of Aguascalientes (2024)',
    subtitle = 'Age group breakdown by sex as assigned at birth',
    caption = 'Source: Reconstrucción y proyecciones de la población de los municipios de México 1990-2040, CONAPO, 2024\nPlot: zer.ø Data Viz',
    theme = theme(
      plot.title = element_text(hjust = 0, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0, size = 12),
      plot.caption = element_text(hjust = 1, size = 10)
    )
  )


