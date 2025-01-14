# ---- Configurações
library(tidyverse)
library(raster)
library(terra)
library(sp)
library(sf)
library(leaflet)
library(spdep)
library(geobr)
library(foreign)
library(readxl)
library(stringdist)
library(ggplot2)
library(ggspatial)
library(units)
library(RColorBrewer)
library(stringr)

#---- Obtendo bases de dados
# Pacote geobr: dados sobre municipíos e UCs do estado em formato 'sf'
# Municípios
municipios_RJ <- read_municipality() %>%
  filter(abbrev_state == "RJ") %>%
  st_as_sf() %>%
  st_make_valid() %>%
  mutate(area_munic = st_area(geom) / 1e6)

municipios_RJ$area_munic<-gsub(" [m^2]", "", municipios_RJ$area_munic)

#Unidades de Conservação
UC <- read_conservation_units() %>%
  st_as_sf() %>%
  st_make_valid()   %>%
  mutate(area_uc = st_area(geom) / 1e6)

UC$area_uc<-gsub(" [m^2]", "", UC$area_uc)
UC$area_uc<-as.numeric(UC$area_uc)


# Realizar a interseção espacial entre UCs e os municípios do Rio de Janeiro
UC_RJ <- st_intersection(UC1, municipios_RJ)%>% 
  st_as_sf() %>%
  st_make_valid()   %>%
  mutate(area_uc_munic = st_area(geom) / 1e6)

UC_RJ$area_uc_munic<-gsub(" [m^2]", "", UC_RJ$area_uc_munic)

UC_RJ$area_uc_munic<-as.numeric(UC_RJ$area_uc_munic)

UC_RJ$area_munic<-as.numeric(UC_RJ$area_munic) 

UC_RJ$percnt_uc_munic<-(UC_RJ$area_uc_munic/UC_RJ$area_munic)*100

# Agrupar por código do parque e somar as áreas
UC_RJ2<- UC_RJ %>%
  group_by(code_conservation_unit,name_conservation_unit) %>%
  summarize(
    area_total = sum(area_uc_munic, na.rm = TRUE),  # Soma das áreas
    geom = st_union(geom)                 # Combinar as geometrias
  )

# Filtrar as linhas que contêm a palavra "PARQUE" na coluna 'nome_uc'
Parque_RJ <- UC_RJ2 %>%
  filter(str_detect(name_conservation_unit, "PARQUE ESTADUAL"))



# MAPBiomas: série histórica parques estaduais RJ (1985-2023)
UC<-read_excel("Mapbiomas.xlsx", sheet=2)
