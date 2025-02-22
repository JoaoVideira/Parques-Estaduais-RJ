#---- Configurações -----------
library(tidyverse)
library(raster)
library(terra)
library(sf)
library(leaflet)
library(spdep)
library(geobr)
library(foreign)
library(readxl)
library(stringdist)
library(ggspatial)
library(units)
library(RColorBrewer)
library(stringr)
library(ggspatial)
library(units)
library(RColorBrewer)
library(gridExtra)
library(gganimate)
library(gifski)
library(av)
library(fuzzyjoin)
library(stringdist)
library(patchwork)

#---- Obtendo bases de dados---------------------------------------------------------------
## Pacote geobr: dados sobre municipíos e UCs do estado em formato 'sf'
# Municípios
municipios_RJ <- read_municipality() %>%
  filter(abbrev_state == "RJ") %>%
  st_as_sf() %>%
  st_make_valid() %>%
  mutate(area_munic = st_area(geom) / 1e6)

municipios_RJ$area_munic<-gsub(" [m^2]", "", municipios_RJ$area_munic)
municipios_RJ$area_munic<-as.numeric(municipios_RJ$area_munic)
municipios_RJ$code_muni<-as.character(municipios_RJ$code_muni)

#------ Dados INEA -------------------------------
# Normalizar o caminho para o shapefile
caminho.shp <- normalizePath("C:/Users/joaoa/OneDrive/Documentos/Parques-Estaduais-RJ/gpl_ucs_estaduais_limites_inearj_me_2023.shp")

# Ler o shapefile com a codificação correta (usando stringsAsFactors para evitar problemas de codificação)
UC_Estaduais <- st_read(caminho.shp, options = "ENCODING=LATIN1", stringsAsFactors = FALSE) %>%
  st_as_sf() %>%
  st_make_valid()

# Mapa com as UCs do estado do Rio de Janeiro com o limite dos municipios
ggplot() +
  geom_sf(data = municipios_RJ, fill = "white", color = "black") + # Fundo dos municípios
  geom_sf(data = UC_Estaduais, fill = 'green', color = "black") + # UCs
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "aliceblue"),
    strip.text = element_text(size = 12, face = "bold")
  ) +
  annotation_scale(location = "br") + # Escala no canto inferior direito
  annotation_north_arrow(location = "tl", which_north = "true", style = north_arrow_fancy_orienteering)

# Filtrando a base para obter apenas os parques
Parques_Estaduais<-filter(UC_Estaduais, UC_Estaduais$categoria=="PARQUE")
Parques_Estaduais<-Parques_Estaduais[-c(8,12),]# sairam dois parques que não são estaduais

#Mapa com os parques estado do Rio de Janeiro com o limite dos municipios
#Os parques se concentram em áreas litorâneas, muitas de alta ocupação e densidade populacional, assim como
#em áreas de alta declividade e altitude, regiões serranas, que possuem baixa custo de oportunidade da terra
ggplot() +
  geom_sf(data = municipios_RJ, fill = "white", color = "black") + # Fundo dos municípios
  geom_sf(data = Parques_Estaduais$geometry, fill = 'darkgreen', color = "black") + # Parques
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "aliceblue"),
    strip.text = element_text(size = 12, face = "bold")
  ) +
  annotation_scale(location = "br") + # Escala no canto inferior direito
  annotation_north_arrow(location = "tl", which_north = "true", style = north_arrow_fancy_orienteering)

# Filtrando a base para obter as outras UCs
Outras_UCs<-filter(UC_Estaduais, UC_Estaduais$categoria!="PARQUE")

#Mapa com os outras UCs não parques no estado do Rio de Janeiro com o limite dos municipios
# As outras UCsseguem uma distribuição espacial semelhante a dos parques 
ggplot() +
  geom_sf(data = municipios_RJ, fill = "white", color = "black") + # Fundo dos municípios
  geom_sf(data = Outras_UCs$geometry, fill = 'orange', color = "black") + # Parques
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "aliceblue"),
    strip.text = element_text(size = 12, face = "bold")
  ) +
  annotation_scale(location = "br") + # Escala no canto inferior direito
  annotation_north_arrow(location = "tl", which_north = "true", style = north_arrow_fancy_orienteering)

#Gráfico de barras com o percentual de área de cada parque, em relação à área total dos parques
Soma_total_parques<-sum(Parques_Estaduais$area_ha)/100
soma_munic<-sum(municipios_RJ$area_munic)

Percent_parques_total<-(Soma_total_parques/soma_munic)*100
#Percentual área dos parques sobre área do estado: 4,3%. Ainda que o percentual seja baixo, deve-se relativiza-lo 
#, dado que há outras cateorias de UCs e área protegidas ocupando o terrítório do estado do Rio de Janeiro

Percent_parques<-((Parques_Estaduais$area_ha/100)/Soma_total_parques)*100

Parques_Estaduais<-cbind.data.frame(Parques_Estaduais,Percent_parques)

# Calcular a média
media_percentual <- mean(Parques_Estaduais$Percent_parques)

# Remover "Parque Estadual", "DO" e "DA"
Parques_Estaduais$nome <- Parques_Estaduais$nome %>%
  str_replace_all("PARQUE ESTADUAL", "") %>% # Remove "Parque Estadual"
  str_replace_all("\\bDO\\b|\\bDA\\b|\\bDOS\\b", "")#Remove "DA", "DO", "DOS"

# Criar o gráfico de barras percentual de área dos parques estaduais
# Três parques apresentam valores acima da média, com destaqueu para PES Três Picos que apresenta 
#valores acima de tres vezes dos parques que estão abaixo da média.
#Vale ressaltar que o três primeiros colocados ( tres picos, Cunhambebe e Desengano) ocupam mais de 60% da área total
#dos parques e estão localizadas em regiões serranas.
ggplot(Parques_Estaduais, aes(x = reorder(nome, -Percent_parques), y = Percent_parques)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_hline(yintercept = media_percentual, linetype = "dotted", color = "red") +
  theme_minimal() +
  labs(
    x = "Parques Estaduais",
    y = "Percentual de Área"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

Parques_Estaduais$plano_mane<-as.factor(Parques_Estaduais$plano_mane)
Parques_Estaduais$amortecime<-as.factor(Parques_Estaduais$amortecime)

# Contagem manual das ocorrências
Plano_manejo_data <- Parques_Estaduais %>%
  group_by(plano_mane) %>%
  summarise(Frequencia = n())

Zona_Amort_data <- Parques_Estaduais %>%
  group_by(amortecime) %>%
  summarise(Frequencia = n())

## MAPBiomas: base com coberura e uso do solo das UCs (1985 a 2023)
UC_Mapbiomas<-read_excel("Mapbiomas.xlsx", sheet=2)

# Filtrar as linhas que contêm a palavra "PARQUE ESTADUAL" 
Parques_Mapbiomas <- UC_Mapbiomas %>%
  filter(str_detect(territory_level3, "PARQUE ESTADUAL"))

colnames(Parques_Mapbiomas)[4]<-c("nome")

# Comparar os nomes de UCs entre as duas bases usando uma métrica de distância de strings
distancias <- stringdist::stringdistmatrix(Parques_Estaduais$nome, Parques_Mapbiomas$nome, method = "jw")

# Identificação de quais pares têm a menor distância, para encontrar correspondências
matches <- apply(distancias, 1, function(x) which.min(x))
print(matches)

Parques_Mapbiomas2<-Parques_Mapbiomas[c(180:190,191:193,622:629,612:621,1053:1060,1061:1064,1395:1405,29:35,243:254),]
# !!!!Estão faltando os parques estaduais dos três picos e da pedra branca que não aparecem em nehuma das bases de dados

#Ajustando os nomes UCs para conseguir compatibilizar com os nomes da base do INEA
Parques_Mapbiomas2$nome <- Parques_Mapbiomas2$nome %>%
  str_replace_all("PARQUE ESTADUAL", "") %>%         # Remove "PARQUE ESTADUAL"
  str_replace_all("\\bDO\\b|\\bDA\\b|\\bDOS\\b", "") %>% # Remove "DO", "DA", "DOS"
  str_replace_all("\\s{2,}", " ") %>%                # Remove espaços extras
  str_replace_all("\\s?\\([^)]*\\)", "") %>%        # Remove conteúdo entre parênteses
  str_trim() %>%
  str_to_lower()  # Opcional: converter para minúsculas          


Parques_Estaduais$nome<- Parques_Estaduais$nome %>%
  str_replace_all("\\s{2,}", " ") %>%                # Remove espaços extras
  str_replace_all("\\s?\\([^)]*\\)", "") %>%        # Remove conteúdo entre parênteses
  str_trim() %>%
  str_to_lower()  # Opcional: converter para minúsculas     

# Transformar os dados para formato long
Parques_Mapbiomas3 <- Parques_Mapbiomas2 %>%
  pivot_longer(cols = starts_with(c("2", "1")), names_to = "ano", values_to = "uso_solo")

# Usar stringdist_join para unir as bases com base na similaridade das strings
Base <- fuzzyjoin::stringdist_join(
  Parques_Mapbiomas3, Parques_Estaduais, 
  by = "nome", 
  method = "jw",     # Método de distância Jaro-Winkler
  max_dist = 0.2,    # Limite de distância máxima (ajuste conforme necessário)
  mode = "left"      # Realizar um left join
)

Base$ano<-as.numeric(Base$ano)
glimpse(Base$class_level_2)

#Agrupando percentual de uso do solo para Parques com e sem plano de manejo
Base01 <- Base %>%
  group_by(plano_mane, ano) %>%  # Agrupar por plano de manejo e ano antes da mutação
  mutate(
    total_uso_solo = sum(uso_solo, na.rm = TRUE),  # Soma total de uso_solo por grupo
    percent_floresta = ifelse(class_level_0 == "Natural", (uso_solo / total_uso_solo) * 100, 0),
    percent_antro = ifelse(class_level_0 == "Antropic", (uso_solo / total_uso_solo) * 100, 0)
  ) %>%
  summarize(
    Valor_natural = sum(percent_floresta, na.rm = TRUE),
    Valor_antropico = sum(percent_antro, na.rm = TRUE)
  )  # Resumo em uma única chamada de summarize()


# Gráfico série histórica com valores de percentual de cobertura florestal estratificados entre parques que possuem 
#plano de manejo e parques não detém.
# Criar o gráfico de linha
ggplot(Base01, aes(x = ano, y = Valor_natural, color = plano_mane, group = plano_mane)) +
  geom_line(size = 1) +  # Adiciona linhas para cada grupo
  geom_point(size = 2) +  # Adiciona pontos para cada observação
  labs(
    x = "Ano",
    y = "Percentual de Cobertura Natural",
    color = "Plano de Manejo"
  ) +
  theme_minimal() +  # Tema limpo
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)  # Centraliza o título
  )

#Filtrando apenas para classe 0: Antropico. A finalidade é criar um gráfico com os usos de solo da classe 2 restritas
# ao uso antropico da classe 0 e ver com mais detalhes o comportamento desses usos entre com PM e sem PM.
Base_antropico <- Base %>%
  group_by(plano_mane, ano) %>%  # Agrupar por plano de manejo e ano antes da mutação
  mutate(
    total_uso_solo = sum(uso_solo, na.rm = TRUE),  # Soma total de uso_solo por grupo
    percent_pasto = ifelse(class_level_2 == "3.1. Pasture", (uso_solo / total_uso_solo) * 100, 0),
    percent_urbano= ifelse(class_level_2 == "4.2. Urban Area", (uso_solo / total_uso_solo) * 100, 0),
    percent_Mosaico=ifelse(class_level_2 =="3.4. Mosaic of Uses", (uso_solo/total_uso_solo)*100,0),
    percent_agro=ifelse(class_level_2 =="3.2. Agriculture", (uso_solo/total_uso_solo)*100,0),
    percent_FlorestaP=ifelse(class_level_2 =='3.3. Forest Plantation', (uso_solo/total_uso_solo)*100,0)
  ) %>%
  summarize(
    Valor_pasto = sum(percent_pasto, na.rm = TRUE),
    Valor_urbano = sum(percent_urbano, na.rm = TRUE),
    Valor_Mosaico = sum(percent_Mosaico, na.rm = TRUE),
    Valor_agro = sum(percent_agro, na.rm = TRUE),
    Valor_floresta_plantada = sum(percent_FlorestaP, na.rm = TRUE)
  )  # Resumo em uma única chamada de summarize()


ggplot(Base_antropico, aes(x = ano)) +
  geom_line(aes(y = Valor_pasto, color = "Pasto"), linewidth = 1) +
  geom_point(aes(y = Valor_pasto, color = "Pasto"), size = 2) +
  
  geom_line(aes(y = Valor_urbano, color = "Urbano"), linewidth = 1) +
  geom_point(aes(y = Valor_urbano, color = "Urbano"), size = 2) +
  
  geom_line(aes(y = Valor_Mosaico, color = "Mosaico"), linewidth = 1) +
  geom_point(aes(y = Valor_Mosaico, color = "Mosaico"), size = 2) +
  
  geom_line(aes(y = Valor_agro, color = "Agricultura"), linewidth = 1) +
  geom_point(aes(y = Valor_agro, color = "Agricultura"), size = 2) +
  
  geom_line(aes(y = Valor_floresta_plantada, color = "Floresta Plantada"), linewidth = 1) +
  geom_point(aes(y = Valor_floresta_plantada, color = "Floresta Plantada"), size = 2) +
  
  facet_wrap(~ plano_mane, ncol = 2) +  # Divisão por plano de manejo
  labs(
    x = "Ano",
    y = "Percentual de Cobertura de Uso",
    color = "Tipos de Uso do Solo"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  )

## Carregar shapefiles Áreas de Preservação Permanente/Áreas com Restrição Ambiental
# Normalizar o caminho para o shapefile
caminhoaAP1.shp <- normalizePath("C:/Users/joaoa/OneDrive/Documentos/Parques-Estaduais-RJ/gpl_apps_mangue_25_2022.shp")
caminhoaAP2.shp <- normalizePath("C:/Users/joaoa/OneDrive/Documentos/Parques-Estaduais-RJ/gpl_app_altitude_25_2022.shp")
caminhoaAP3.shp <- normalizePath("C:/Users/joaoa/OneDrive/Documentos/Parques-Estaduais-RJ/gpl_apps_topo_morro_25_2022.shp")
caminhoaAP4.shp <- normalizePath("C:/Users/joaoa/OneDrive/Documentos/Parques-Estaduais-RJ/gpl_apps_declividade_25_2022.shp")
caminhoaAP5.shp <- normalizePath("C:/Users/joaoa/OneDrive/Documentos/Parques-Estaduais-RJ/gpl_apps_nascentes_25_2022.shp")

# Ler o shapefile com a codificação correta (usando stringsAsFactors para evitar problemas de codificação)
Mangue <- st_read(caminhoaAP1.shp, options = "ENCODING=LATIN1", stringsAsFactors = FALSE) %>%
  st_as_sf() %>%
  st_make_valid()

Mangue<-Mangue[-2]

# Ler o shapefile com a codificação correta (usando stringsAsFactors para evitar problemas de codificação)
Altitude <- st_read(caminhoaAP2.shp, options = "ENCODING=LATIN1", stringsAsFactors = FALSE) %>%
  st_as_sf() %>%
  st_make_valid()

# Ler o shapefile com a codificação correta (usando stringsAsFactors para evitar problemas de codificação)
Topomorro <- st_read(caminhoaAP3.shp, options = "ENCODING=LATIN1", stringsAsFactors = FALSE) %>%
  st_as_sf() %>%
  st_make_valid()

Topomorro<-Topomorro[-2]

# Ler o shapefile com a codificação correta (usando stringsAsFactors para evitar problemas de codificação)
declividade<- st_read(caminhoaAP4.shp, options = "ENCODING=LATIN1", stringsAsFactors = FALSE) %>%
  st_as_sf() %>%
  st_make_valid()

declividade<-declividade[-2]

# Ler o shapefile com a codificação correta (usando stringsAsFactors para evitar problemas de codificação)
nascentes<- st_read(caminhoaAP5.shp, options = "ENCODING=LATIN1", stringsAsFactors = FALSE) %>%
  st_as_sf() %>%
  st_make_valid()

nascentes<-nascentes[-2]

#Juntando as bases de APPs
APP<-rbind.data.frame(Mangue,Altitude, Topomorro,declividade,nascentes)%>%
  st_as_sf() %>%
  st_make_valid()

# Gráfico com Mangue e Parques: sem sobreposição, mas parques adjacentes
ggplot() +
  geom_sf(data = municipios_RJ, fill = "white", color = "black")+# Fundo dos municípios
  geom_sf(data = Mangue$geometry, fill = 'brown', color = 'brown')+ # Fundo das APPs
  geom_sf(data = Parques_Estaduais$geometry, fill = 'green', color = "green") + # Parques
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "aliceblue"),
    strip.text = element_text(size = 12, face = "bold")
  ) +
  annotation_scale(location = "br") + # Escala no canto inferior direito
  annotation_north_arrow(location = "tl", which_north = "true", style = north_arrow_fancy_orienteering)


# Gráfico com Topo de Morro e Parques: bastante sobreposição e adjancências
ggplot() +
  geom_sf(data = municipios_RJ, fill = "white", color = "black")+# Fundo dos municípios
  geom_sf(data = Topomorro$geometry, fill = 'brown', color = 'brown')+ # Fundo das APPs
  geom_sf(data = Parques_Estaduais$geometry, fill = 'green', color = "green") + # Parques
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "aliceblue"),
    strip.text = element_text(size = 12, face = "bold")
  ) +
  annotation_scale(location = "br") + # Escala no canto inferior direito
  annotation_north_arrow(location = "tl", which_north = "true", style = north_arrow_fancy_orienteering)


# Gráfico com Declividade acima de 45º e Parques: muita sobreposição e adjacências
ggplot() +
  geom_sf(data = municipios_RJ, fill = "white", color = "black")+# Fundo dos municípios
  geom_sf(data = declividade$geometry, fill = 'brown', color = 'brown')+ # Fundo das APPs
  geom_sf(data = Parques_Estaduais$geometry, fill = 'green', color = "green") + # Parques
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "aliceblue"),
    strip.text = element_text(size = 12, face = "bold")
  ) +
  annotation_scale(location = "br") + # Escala no canto inferior direito
  annotation_north_arrow(location = "tl", which_north = "true", style = north_arrow_fancy_orienteering)


# Gráfico com Nascente e Parques: muita sobreposição e adjacências, porém as nascentes são 
#praticamente onipresentes no território do estado.
ggplot() +
  geom_sf(data = municipios_RJ, fill = "white", color = "black")+# Fundo dos municípios
  geom_sf(data = nascentes$geometry, fill = 'brown', color = 'brown')+ # Fundo das APPs
  geom_sf(data = Parques_Estaduais$geometry, fill = 'green', color = "green") + # Parques
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "aliceblue"),
    strip.text = element_text(size = 12, face = "bold")
  ) +
  annotation_scale(location = "br") + # Escala no canto inferior direito
  annotation_north_arrow(location = "tl", which_north = "true", style = north_arrow_fancy_orienteering)


# Gráfico com Altitude acima de 1800 metros e Parques: nenhum sobreposição, porém há adjacencias
ggplot() +
  geom_sf(data = municipios_RJ, fill = "white", color = "black")+# Fundo dos municípios
  geom_sf(data = Altitude$geometry, fill = 'brown', color = 'brown')+ # Fundo das APPs
  geom_sf(data = Parques_Estaduais$geometry, fill = 'green', color = "green") + # Parques
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "aliceblue"),
    strip.text = element_text(size = 12, face = "bold")
  ) +
  annotation_scale(location = "br") + # Escala no canto inferior direito
  annotation_north_arrow(location = "tl", which_north = "true", style = north_arrow_fancy_orienteering)


## Carregar shapefiles para Dados CAR no estado do Rio de Janeiro
# Normalizar o caminho para o shapefile: Área Rural Consolidada
Rural_consolidada.shp <- normalizePath("C:/Users/joaoa/OneDrive/Documentos/Parques-Estaduais-RJ/area_rural_consolida_25k.shp")
Rural_consolidada<- st_read(Rural_consolidada.shp, options = "ENCODING=LATIN1", stringsAsFactors = FALSE) %>%
  st_as_sf() %>%
  st_make_valid()

## Carregar shapefiles para Dados CAR no estado do Rio de Janeiro
# Normalizar o caminho para o shapefile: Área Rural não Consolidada
Rural_n_consolidada.shp <- normalizePath("C:/Users/joaoa/OneDrive/Documentos/Parques-Estaduais-RJ/area_rural_nao_consol__25k.shp")
Rural_n_consolidada<- st_read(Rural_n_consolidada.shp, options = "ENCODING=LATIN1", stringsAsFactors = FALSE) %>%
  st_as_sf() %>%
  st_make_valid()

## Carregar shapefiles para Dados CAR no estado do Rio de Janeiro
# Normalizar o caminho para o shapefile: vegetacao_nativa
Veget_nativa.shp <- normalizePath("C:/Users/joaoa/OneDrive/Documentos/Parques-Estaduais-RJ/vegetacao_nativa_25k.shp")
Veget_nativa<- st_read(Veget_nativa.shp, options = "ENCODING=LATIN1", stringsAsFactors = FALSE) %>%
  st_as_sf() %>%
  st_make_valid()

Veget_nativa<-Veget_nativa[,-c(3,4)]

#Juntando as bases de CAR
CAR<-rbind.data.frame(Rural_consolidada, Rural_n_consolidada, Veget_nativa)%>%
  st_as_sf() %>%
  st_make_valid()

CAR$class<-as.factor(CAR$class)

# Gráfico com CARs: rural consolidada e Parques
g1<-ggplot() +
  # Mapa base dos municípios
  geom_sf(data = municipios_RJ, fill = "white", color = "black", size = 0.3) +
  # Mapa do CAR com cores baseadas nas classes
  geom_sf(data = Rural_consolidada, color = "orange", size = 0.2) +
  # Mapa dos Parques Estaduais
  geom_sf(data = Parques_Estaduais$geometry, fill = "green", color = "darkgreen", alpha = 0.6) +
  # Adicionar escala e seta norte
  annotation_scale(location = "br", width_hint = 0.4) +
  annotation_north_arrow(
    location = "tl", which_north = "true",
    style = north_arrow_fancy_orienteering
  )

# Gráfico com CARs: rural não consolidada e Parques
g2<- ggplot() +
  # Mapa base dos municípios
  geom_sf(data = municipios_RJ, fill = "white", color = "black", size = 0.3) +
  # Mapa do CAR com cores baseadas nas classes
  geom_sf(data = Rural_n_consolidada, color = "orange", size = 0.2) +
  # Mapa dos Parques Estaduais
  geom_sf(data = Parques_Estaduais$geometry, fill = "green", color = "darkgreen", alpha = 0.6) +
  # Adicionar escala e seta norte
  annotation_scale(location = "br", width_hint = 0.4) +
  annotation_north_arrow(
    location = "tl", which_north = "true",
    style = north_arrow_fancy_orienteering
  )

# Combine os gráficos em um painel lado a lado
painel <- g1 + g2 + 
  plot_layout(ncol = 2) + # Define o número de colunas no painel
  plot_annotation(theme = theme(plot.title = element_text(hjust = 0.5, size = 16)))

# Visualize o painel
print(painel)

ggplot() +
  # Mapa base dos municípios
  geom_sf(data = municipios_RJ, fill = "white", color = "black", size = 0.3) +
  # Mapa do CAR com cores baseadas nas classes
  geom_sf(data = Veget_nativa, color = "orange", size = 0.2) +
  # Mapa dos Parques Estaduais
  geom_sf(data = Parques_Estaduais$geometry, fill = "green", color = "darkgreen", alpha = 0.6) +
  # Adicionar escala e seta norte
  annotation_scale(location = "br", width_hint = 0.4) +
  annotation_north_arrow(
    location = "tl", which_north = "true",
    style = north_arrow_fancy_orienteering
  )

##---- Dados SIDRA IBGE --------------------------------------------------------
# Pesquisa Pecuária Municipal (PAM): Tabela 3939 - Efetivo dos rebanhos (bovino)


# Cadastro Central de Empresas (CEMPRE): Tabela 9528 - Unidades locais, pessoal ocupado total 


# Produção Agrícola Municipal (PAM): Tabela 5457 -  Valor da produção (R$)
rm(Agricultura)
Agricultura<-read_excel("Agro.xlsx", col_names =TRUE, skip = 3)

colnames(Agricultura)[c(1,2)]<-c("code_muni","Municipio")
Agricultura<-Agricultura[-c(1,91),]

Df<-left_join(municipios_RJ,Agricultura, by="code_muni")
Df<-Df[-c(3,4),]
Df <- Df %>%
  rename_with(
    ~ paste0("ano_", .),  # Adiciona o prefixo "ano_" aos nomes
    .cols = starts_with("20")  # Seleciona colunas que começam com "19" ou "20"
  )%>%
  mutate(across(
    starts_with("ano"),
    ~ coalesce(  # Substitui NAs por 0
      as.numeric(gsub("[^0-9-]", "", as.character(.))),  # Conversão para numérico
      0
    )
  ))

# Base: Mapa dos municípios com preenchimento gradiente. O mapa indica uma leve sobreposição das áreas dos parques
# com áreas cujos valores de produção foram os mais baixos. Isso está de acordo com as observações feitas nos outros mapas
ggplot() +
  # Base: Mapa dos municípios com preenchimento gradiente
  geom_sf(
    data = Df,
    aes(fill = ano_2023),  # Variável contínua para o gradiente
    color = "black",
    size = 0.3
  ) +
  # Camada 2: Parques Estaduais
  geom_sf(
    data = Parques_Estaduais$geometry,
    fill = "green",
    color = "darkgreen",
    alpha = 0.6
  ) +
  # Escala e seta norte
  annotation_scale(location = "br", width_hint = 0.4) +
  annotation_north_arrow(
    location = "tl", which_north = "true",
    style = north_arrow_fancy_orienteering
  ) +
  # Escala de cores com formatação numérica
  scale_fill_viridis_c(
    option = "plasma",
    name = "Produção Agrícola",
    na.value = "gray90",
    labels = scales::label_number(  # Usa label_number ao invés de number
      accuracy = 1,           # Mostra números inteiros (sem decimais)
      big.mark = ".",         # Separador de milhar
      decimal.mark = ","      # Separador decimal
    )
  ) +
  theme_minimal() +
  theme(
    legend.text = element_text(size = 6),   # Ajusta o tamanho do texto da legenda
    legend.title = element_text(size = 8) # Ajusta o tamanho do título da legenda
  )

## ---- Dados Base dos Dados ---------------------------------------------------
# SNIS (MDR): População urbana, população atendida com água, população atendida com esgoto


# SEEG : informações sobre emissões por setor/atividade e municipio
