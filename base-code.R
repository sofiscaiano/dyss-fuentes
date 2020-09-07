library(tidyverse)
library(dplyr)
library(ggplot2)
library(forcats)

censo_2010

censo_2010_s <- censo_2010 %>%
  transmute(provincia, total_poblacion = total_poblacion / 1000000, mujeres = mujeres / 1000000, porcentaje) %>%
  arrange(porcentaje)

  
ggplot(censo_2010_s, aes(x = total_poblacion, y = porcentaje, color = provincia)) +
  geom_point(size = 3) +
  scale_x_log10() +
  ggtitle("Diagrama de dispersión de la proporción de la población femenina", subtitle="Fuente: Censo Nacional de Población del año 2010")
  

censo_2010_s %>%
  arrange(porcentaje) %>%
  mutate(provincia=factor(provincia, levels=provincia)) %>%
  ggplot(aes(x = total_poblacion, y = porcentaje, color = provincia)) +
  geom_point(size = 3) +
  scale_x_log10() +
  facet_wrap(~provincia) +
  theme(legend.position="none")


censo_2010_s %>%
  arrange(desc(porcentaje)) %>%
  mutate(provincia=factor(provincia, levels=provincia)) %>%
    ggplot(aes(x = provincia, y = porcentaje, fill=provincia)) +
  geom_col() +
  coord_flip() +
  scale_fill_hue(c = 50) +
  theme(legend.position="none") +
  ggtitle("Porcentaje de la población femenina por cada provincia argentina", subtitle="Fuente: Censo Nacional de Población del año 2010")

diff_pop <- censo_2001_2010 %>%
  mutate(diff = pop_2010 - pop_2001) %>%
  mutate(per = diff / pop_2001)

arrange(diff_pop, desc(diff))

arrange(diff_pop, desc(per))

ggplot(diff_pop, aes(x= pop_2001, y= per, color=provincia)) +
  geom_point() +
  scale_x_log10()

jubilaciones_y_pensiones %>%
  filter(edad >= 65) %>%
  summarize(total_no = sum(no), total = sum(total)) %>%
  mutate(percent= total_no / total)

analfabetismo %>%
  filter(edad > 10) %>%
  summarize(total_si = sum(si), total = sum(total))

