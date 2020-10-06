#Importar libraries
library(data.table)
library(ggplot2)
library(grid)
library(dplyr)
library(tibble)
library(reshape2)
library(ggpubr)
library(maps)
library(ggiraph)
library(leaflet)
library(geojsonio)
library(gganimate)
library(plotly)
library(RColorBrewer)
library(sf)
library(viridis)


#Importar dados
data <- fread("https://raw.githubusercontent.com/dssg-pt/data-data/master/data.csv")

#Data de data, de chr para Date
data$data <- as.Date(data$data,"%d-%m-%Y")

#Dados da testagem
data_testes <- fread("https://raw.githubusercontent.com/dssg-pt/data-data/master/amostras.csv")

#Data de data_testes, de chr para Date
data_testes$data <- as.Date(data_testes$data,"%d-%m-%Y")

#Dados por concelho
data_concelhos <- fread("https://raw.githubusercontent.com/dssg-pt/data-data/master/data_concelhos.csv")

#Data de data_concelhos, de chr para Date
data_concelhos$data <- as.Date(data_concelhos$data,"%d-%m-%Y")

#Mapa de Portugal Regiões
mapa_pt <- geojson_read("https://raw.githubusercontent.com/dssg-pt/data-data/master/extra/mapas/portugal.geojson",
                        what = "sp")

#Mapa de Portugal por Concelhos em <https://dados.gov.pt/pt/datasets/concelhos-de-portugal/>
mapa_concelhos <- st_read("C:/Users/rakac/Downloads/concelhos-shapefile")