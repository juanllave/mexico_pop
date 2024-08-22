past <- pop %>% 
  filter(year <= current_year)


future <- pop %>% 
  filter(year > current_year)

past_plot <- past %>% 
  filter(mun == 'Guadalajara') %>% 
  ggplot(aes(x = year, y = total, color = mun)) +
  geom_line(size = 1) +
   theme_minimal() +
  theme(legend.position = 'none')

print(past_plot)

future_plot <- future %>% 
  filter(mun == 'Guadalajara') %>% 
  ggplot(aes(x = year, y = total, color = mun)) +
  geom_line(size = 1) +
  theme_minimal() +
  theme(legend.position = 'none')

print(future_plot)

line_plot <- past_plot / future_plot

print(line_plot)
