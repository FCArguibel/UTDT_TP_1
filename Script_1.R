library(tidyverse)
library(readr)
library(readxl)


datos <- read_csv("https://cdn.produccion.gob.ar/cdn-cep/datos-por-departamento/puestos/puestos_total_depto.csv")
departamentos <- read_csv("https://cdn.produccion.gob.ar/cdn-cep/datos-por-departamento/diccionario_cod_depto.csv")

# Hago union con datos y departamentos

datos_nuevo <- datos %>% 
  left_join(., departamentos, by = c("id_provincia_indec", "codigo_departamento_indec")) %>% 
  select(-c("id_provincia_indec", "codigo_departamento_indec")) %>% 
  rename(provincia=nombre_provincia_indec, departamento=nombre_departamento_indec)


# Hago un group_by por provincia y fecha

group_1 <- datos_nuevo %>% 
  group_by(provincia, fecha) %>% 
  summarise(puestos_medios = mean(puestos, na.rm = T),
            puestos_max = max(puestos, na.rm = T),
            puestos_min = min(puestos, na.rm = T))


# Creo un data.frame wider y utilizo mutate para crear total Argy por fecha

datos_wider <- group_1 %>% 
  select(provincia, fecha, puestos_medios) %>% 
  pivot_wider(names_from = provincia,
              values_from = puestos_medios) %>% 
  mutate(total_puestos_argy = rowSums(across(2:26)))


