# ---- Configurações-----------------------------------------------------------------------
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
library(ggspatial)
library(units)
library(RColorBrewer)

#---- Obtendo bases de dados---------------------------------------------------------------
## Pacote geobr: dados sobre municipíos e UCs do estado em formato 'sf'
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

# Filtrar as linhas que contêm a palavra "PARQUE ESTADUAL" 
Parque_RJ <- UC_RJ2 %>%
  filter(str_detect(name_conservation_unit, "PARQUE ESTADUAL"))

#mapa das UCs com o limite dos municipios
ggplot() +
  geom_sf(data = municipios_RJ, fill = "white", color = "black") + # Fundo dos municípios
  geom_sf(data = Parque_RJ, fill = 'green', color = "black") + # Parques
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "aliceblue"),
    strip.text = element_text(size = 12, face = "bold")
  ) +
  annotation_scale(location = "br") + # Escala no canto inferior direito
  annotation_north_arrow(location = "tl", which_north = "true", style = north_arrow_fancy_orienteering)

## MAPBiomas: série histórica parques estaduais RJ (1985-2023)
UC_Mapbiomas<-read_excel("Mapbiomas.xlsx", sheet=2)

# Filtrar as linhas que contêm a palavra "PARQUE ESTADUAL" 
Parques_Mapbiomas <- UC_Mapbiomas %>%
  filter(str_detect(territory_level3, "PARQUE ESTADUAL"))
  
Parques_Mapbiomas<-Parques_Mapbiomas[,-c(12:40)]
colnames(Parques_Mapbiomas)[4]<-c("name_conservation_unit")

# Comparar os nomes de UCs entre as duas bases usando uma métrica de distância de strings
distancias <- stringdist::stringdistmatrix(Parque_RJ$name_conservation_unit, Parques_Mapbiomas$territory_level3, method = "jw")

# Agora você pode identificar quais pares têm a menor distância e, assim, encontrar correspondências
# Para fins de demonstração, vamos encontrar a correspondência mais próxima para cada nome em UC1
matches <- apply(distancias, 1, function(x) which.min(x))

# Primeiro, criar uma coluna de listas de correspondências
# Ajustar o código para usar seq_along corretamente com rowwise e mutate

Parques_Mapbiomas2<-Parques_Mapbiomas[c(180:190,622:629,1053:1060,1061:1064,612:621,1395:1405,29:35,243:254,191:193),]
# !!!!Estão faltando os parques estaduais dos três picos e da pedra branca que não aparecem em nehuma das bases de dados


##### Estatísticas Descritivas ------------------------------------------------------------------------------


