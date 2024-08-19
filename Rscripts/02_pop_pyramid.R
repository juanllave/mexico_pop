library(tidyverse)

mex_pop <- read_csv('~/Desktop/SNSP/datasets/02_mexico_pop.csv') %>% 
  filter(year == 2024 & cve_geo == 0) %>% 
  select(-c(1:4))

# Define age groups
age_breaks <- c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,Inf)
age_labels <- c('0-4','5-9','10-14','15-19','20-24','25-29','30-34','35-39',
                '40-44','45-49','50-54','55-59','60-64','65-69','70-74','75-79',
                '80-84','85+')

mex_pop$age_groups <- cut(mex_pop$age, breaks = age_breaks, labels = age_labels, right = FALSE)

pop <- mex_pop %>% 
    group_by(age_groups, sex) %>% 
  summarise(population = sum(population)) %>% 
  mutate(pct = population/sum(population)) %>% 
  mutate(pct = if_else(sex=='Hombres',pct,-pct))

pop |>
  ggplot(aes(
    x = pct,
    y = age_groups,
    fill = sex
  )) +
  geom_col() +
  geom_label(
    aes(
      x = 0,
      label = age_groups
    ),
    fill = 'white',
    label.size = 0
  )