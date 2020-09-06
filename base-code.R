library(tidyverse)
library(dplyr)
library(ggplot2)
library(forcats)

censo_2010_s <- censo_2010 %>%
  transmute(provincia, total_poblacion = total_poblacion / 1000000, mujeres = mujeres / 1000000, porcentaje) %>%
  arrange(porcentaje)

  
ggplot(censo_2010_s, aes(x = total_poblacion, y = porcentaje, color = provincia)) +
  geom_point() +
  scale_x_log10()

censo_2010_s %>%
  arrange(porcentaje) %>%
  mutate(provincia=factor(provincia, levels=provincia)) %>%
  ggplot(aes(x = total_poblacion, y = porcentaje, color = provincia)) +
  geom_point() +
  scale_x_log10() +
  facet_wrap(~provincia) +
  theme(legend.position="none")


censo_2010_s %>%
  arrange(desc(porcentaje)) %>%
  mutate(provincia=factor(provincia, levels=provincia)) %>%
    ggplot(aes(x = provincia, y = porcentaje, fill=provincia)) +
  geom_col() +
  coord_flip() +
  scale_fill_hue(c = 70) +
  theme(legend.position="none")
