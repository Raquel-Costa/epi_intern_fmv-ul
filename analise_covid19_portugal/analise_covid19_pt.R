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




#SINTOMAS

##FREQUÊNCIA RELATIVA
###Criar uma tabela com uma coluna para os sintomas, dar-lhe o nome "sintoma", mudar o nome de cada sintoma na tabela 
###e criar outra coluna para os últimos valores registados de cada sintoma e dar-lhe o nome "percentagem"
sintomas <- as.data.frame(t(data[173,41:46])) %>% 
  rownames_to_column(var = "sintoma")

names(sintomas)[2] = "frequencia"
sintomas[, 1] = c("Tosse", "Febre", "Dificuldade Respiratória", "Cefaleia", "Dores Musculares", "Fraqueza Generalizada")

###Fazer um gráfico de barras com os sintomas no eixo do x e a percentagem no eixo dos y
ggplot(sintomas, aes(x = sintoma, y = frequencia*100)) +
  geom_col(fill = "slategray3",  width = 0.7) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian( ylim = c(0, max(sintomas$frequencia*100 + 10))) +
  #Colocar seta no eixo dos y - geom_segment(aes(x=0, xend = 0 , y=0, 
   #yend = max(percentagem*100 + 4)), arrow = arrow(length = unit(0.5,"cm"))) +
  theme_classic() +
  labs(title = "Frequência de Sintomas \nnos Pacientes com COVID19", 
       x = "Sintomas",
       y = "Frequência (%)") +
  theme(plot.title = element_text(margin = margin(t = 20, b = 20), 
                                  size = 15, 
                                  color = "black", 
                                  hjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 15, b = 10), 
                                    size = 12),
        axis.title.y = element_text(margin = margin(r = 15, l = 10), 
                                    size = 12),
        axis.text.x = element_text(size=10, 
                                   color = "black"),
        axis.text.y = element_text(size=12, 
                                   color = "black")) +
  geom_text(aes(label = scales::percent(frequencia, digits = 2)), 
            vjust = -0.5, 
            size = 4) +
  #Retirar o .0 de cada valor
  scale_x_discrete(labels = c("Cefaleia", "Dificuldade\nRespiratória", "Dores\nMusculares",
                              "Febre", "Fraqueza\nGeneralizada", "Tosse"))



##EVOLUÇÃO DA FREQUÊNCIA RELATIVA
###Criar uma tabela com colunas, uma para cada sintoma, sendo o valor para cada dia a frequência desse 
###sintoma nesse dia vezes o número de confirmados até esse dia menos a frequência desse sintoma no dia anterior 
###vezes o número de confirmados até ao dia anterior, isto para nos dar o número de pessoas com esse sintoma nesse 
###dia apenas, e depois tudo a dividir pelo número de novos confirmados nesse dia para termos o resultado em percentagem. 
sintomas_tempo <- as.data.frame((data[8:173,41:46]*data$confirmados[8:173])
                                -(data[7:172, 41:46]*data$confirmados[7:172]))/data$confirmados_novos[8:173] 

###É ainda necessário acrescentar a linha 7 pois como são as primerias frequências a aparecer 
###já indicam a frequência específica para esse dia
sintomas_tempo <- rbind(data[7, 41:46], sintomas_tempo)

###Acresecentar à tabela que criámos, uma coluna com as datas
sintomas_tempo <- cbind(data$data[7:173], sintomas_tempo)

###No casos do sintoma Dificuldade Respiratória, este só começou a ser registado mais tarde pelo que 
###o primerio valor que já representa a frequência específica para esse dia é o da linha 3, coluna 4
sintomas_tempo[3, 4] <- 0.11

###Mudar os nomes das colunas
names(sintomas_tempo) <- c("Data", "Tosse", "Febre", "Dificuldade \nrespiratória", "Cefaleia", 
                           "Dores \nMusculares",  "Fraqueza \nGeneralizada")

###Fazer o melt da tablea para poder representar a ecolução temporal num gráfico e mudar o nome da coluna 2 para Sintomas
sintomas_tempo_melt <-  melt(sintomas_tempo, id.vars="Data")
names(sintomas_tempo_melt)[2] <- "sintoma"

###Fazer um gráfico de linhas com a data no eixo do x, a frequência diária dos sintomas no eixo dos y 
###e cada sintoma numa linha
ggplot(sintomas_tempo_melt, aes(x = Data, y = value*100, color = sintoma)) +
  geom_line() +
  facet_grid(sintomas_tempo_melt$sintoma) +
  guides(color = FALSE) +
  labs(title = "Frequência de Sintomas ao Longo do Tempo",
       x = "Mês",
       y = "Frequência (%)") +
  theme(plot.title = element_text(margin = margin(t = 20, b = 20), 
                                  size = 15, 
                                  color = "black", 
                                  hjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 15, b = 10), 
                                    size = 12),
        axis.title.y = element_text(margin = margin(r = 15, l = 10),
                                    size = 12),
        strip.text.y = element_text(size = 6, 
                                    angle = 0))
        




#TESTAGEM

##EVOLUÇÃO DO NÚMERO DE TESTES REALIZADOS
###Tabela com coluna para data e outra para número de testes feitos nesse dia
testes_diarios <- data_testes[,c(1, 3)]
names(testes_diarios) = c("Data", "Testes")

###Gráfico de pontos e linhas com data no eixo do x e número de testes no eixo do y
testes_diarios_grafico <- ggplot(testes_diarios, aes(x = Data, y = Testes)) + 
  geom_point(color = "cadetblue") +
  geom_line(size = 0.4, color = "cadetblue") +
  labs(title = "Testes Realizados ao Longo do Tempo",
       x = "Mês") +
  theme(plot.title = element_text(size = 15, 
                                  color = "black", 
                                  hjust = 0.5),
        axis.title.x = element_text(size = 12)) +
  scale_x_date(breaks = "months", date_labels = "%B")

###Fazer com que gráfico seja interativo
ggplotly(testes_diarios_grafico) %>% 
  layout(margin = margin(l =10),
         yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                       "Número de Testes",
                                       rep("&nbsp;", 20),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")))



##EVOLUÇÃO DA TAXA DE TESTES POSITIVOS
###Tabela com coluna para data e outra para casos desse dia a dividir por número de testes desse dia
testes_positivos <- cbind(testes_diarios[,1], as.data.frame((casos_diarios[1:nrow(testes_diarios),2]/testes_diarios[,2])*100))
names(testes_positivos) = c("Data", "Percentagem_Positivos")

###Fazer gráfico com data no eixo do x e proporção de testes positivos no eixo do y
testes_positivos_grafico <- ggplot(testes_positivos, aes(x = Data, y = Percentagem_Positivos)) +
  geom_line(color = "lightseagreen") +
  geom_point(color = "lightseagreen") +
  labs(title = "Proporção de Testes Positivos ao Longo do Tempo", 
       x = "Mês",
       y = "Proporção (%)") +
  scale_x_date(breaks = "months", date_labels = "%B")

###Fazer com que gráfico seja interativo
ggplotly(testes_positivos_grafico) %>% 
  layout(margin = margin(l =10),
         yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                       "Proporção (%)",
                                       rep("&nbsp;", 20),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")))



#CASOS

##EVOLUÇÃO DOS CASOS DIÁRIOS
###Tabela com coluna para data e coluna para número de casos confirmados nesse dia
casos_diarios <- data[, c(1, 12)]
names(casos_diarios) = c("Data", "Casos")

###Fazer gráfico de linhas com data no eixo do x e número de casos no eixo do y
casos_diarios_grafico <- ggplot(casos_diarios, aes(x = Data, y = Casos))+
  geom_point(color = "coral3") +
  geom_line(size = 0.4, color = "coral3")+
  labs(title = "Casos Confirmados ao Longo do Tempo", 
       x = "Mês") +
  theme(plot.title = element_text(size = 15, 
                                  color = "black", 
                                  hjust = 0.5),
        axis.title.x = element_text(size = 12)) +
  scale_x_date(breaks = "months", date_labels = "%B")

###Fazer com que gráfico seja interativo
ggplotly(casos_diarios_grafico) %>% 
  layout(margin = margin(l =10),
         yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                       "Número de Casos",
                                       rep("&nbsp;", 20),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")))



##CASOS POR REGIÃO
###Criar tabela com uma coluna para a região e outra para o valor mais recente do número de casos confirmados,
###mudar o nome das colunas e mudar o nome das regiões
casos_regioes <- as.data.frame(t(as.data.frame(lapply(data[,confirmados_arsnorte:confirmados_madeira],
                                                      max, na.rm = TRUE))))%>% 
  rownames_to_column(var = "Regioes")
names(casos_regioes)[2] <- "Casos"
casos_regioes[, 1] <- c("Norte", "Centro", "Lisboa e Vale \ndo Tejo", "Alentejo", "Algarve", "Açores", "Madeira")

###Fazer um gráfico de barras com as regiões no eixo do x e o número de casos no eixo do y
ggplot(casos_regioes, aes(x = Regioes, y = Casos)) +
  geom_col(fill = "salmon1") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian( ylim = c(0, max(casos_regioes$Casos + 10000))) +
  theme_classic() +
  labs(title = "Número de Casos por Região", 
       x = "Região") +
  theme(plot.title = element_text(margin = margin(t = 20, b = 20), 
                                  size = 15, 
                                  color = "Black", 
                                  hjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 15, b = 10), 
                                    size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 10), 
                                    size = 12),
        axis.text.x = element_text(size=10, color = "black"),
        
        
        axis.text.y = element_text(size=10, 
                                   color = "black")) +
  geom_text(aes(label = Casos), 
            vjust = -0.5, 
            size = 4)

###mapa
####Definir intervalos que queremos na legenda
bins =  c(0, 200, 600, 1200, 1400, 10000, Inf)

####Colocar as Regiãoes da tabela pela mesma ordem que a dos poligonos
casos_regioes_ordem <- casos_regioes[c(4, 5, 6, 2, 7, 1, 3),]

####Definir a palete de cores para o mapa
pal <- colorBin("YlOrRd", domain = casos_regioes_ordem[,2], bins = bins)

####Definir legenda que aparece quando se passa o rato pelo mapa
labels <- sprintf(
  "<strong>%s</strong><br/>%g casos",
  casos_regioes_ordem[,1], casos_regioes_ordem[,2]
) %>% lapply(htmltools::HTML)

####Fazer o mapa
leaflet(mapa_pt) %>% 
  addTiles(group = "Normal") %>% 
  addProviderTiles(providers$CartoDB.Positron, group = "Claro") %>% 
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels, group = "Escuro") %>% 
  addLayersControl(
    baseGroups = c("Normal", "Claro", "Escuro"),
    options = layersControlOptions(collapsed = TRUE)
  ) %>% 
  addPolygons(
    fillColor = ~pal(casos_regioes_ordem[,2]), 
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                             padding = "3px 8px"),
                                textsize = "15px",
                                direction = "auto")) %>% 
  addLegend(pal = pal, values = casos_regioes_ordem$Casos, opacity = 0.7, title = "Nº Casos",
            position = "bottomright")



##PREVALÊNCIA POR REGIÃO
#Valores da população de cada Região com base nas CCDRs
acores = 242796
alentejo = 503507
algarve = 450484
centro = 2217285
lisboa = 3631738
madeira = 253945
norte = 3575338

#Criar uma tabela com uma coluna para as Regiãoes e outra para o número de pessoas nessa Região
populacao_regioes <- as.data.frame(c(norte, centro, lisboa, alentejo, algarve, acores, madeira), 
                                   c("norte", "centro", "lisboa", "alentejo", "algarve", "açores", "madeira"))
colnames(populacao_regioes) <- "População"
populacao_regioes_invertido <- t(populacao_regioes)

prevalencia_regiao <- (as.data.frame(t(as.data.frame((lapply(data[,confirmados_arsnorte:confirmados_madeira], last)))))*100
                       / populacao_regioes_invertido) %>% 
  rownames_to_column(var="Regiao")
colnames(prevalencia_regiao)[2] <- "Prevalencia"
prevalencia_regiao[,1] <- c("Norte", "Centro", "LVT", "Alentejo", "Algarve", "Açores", "Madeira")

prevalencia_regiao_grafico <- ggplot(prevalencia_regiao, aes(x=Regiao, y=Prevalencia)) + 
  geom_col(fill="palegreen", width = 0.5) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  labs(y="Prevalência (%)", x="") +
  theme(axis.title.y = element_text(size = 12))

ggplotly(prevalencia_regiao_grafico)%>% 
  layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                       "Incidência",
                                       rep("&nbsp;", 20),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")))
#Mapa
prevalencia_regiao_ordem <- prevalencia_regiao[c(4,5,6,2,7,1,3),] # colocar as regioes pela ordem do mapa
prevalencia_regiao_ordem[,1] <- c("Alentejo", "Algarve", "Açores", "Centro", "Madeira", "Norte", "Lisboa e Vale do Tejo" )

pal_prevalencia_regiao <- colorBin("viridis", domain = prevalencia_regiao_ordem$Prevalencia, bins = c(0, 0.1, 0.2, 0.3, 0.5, 0.7, 0.8, Inf))

labels_prevalencia_regiao <- paste( 
  "<strong>", prevalencia_regiao_ordem[,1],"</strong><br/>", 
  round(prevalencia_regiao_ordem[,2], digits = 4), "&#37<br/>", 
  sep="") %>%
  lapply(htmltools::HTML)

leaflet(mapa_pt) %>%
  addPolygons(stroke = TRUE, 
              smoothFactor = 0.3, 
              fillOpacity = 1, 
              color = "black", 
              weight = 1,
              fillColor = ~pal_prevalencia_regiao(prevalencia_regiao_ordem$Prevalencia),
              label = labels_prevalencia_regiao, 
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), 
                                          textsize = "13px", 
                                          direction = "auto")) %>% 
  addLegend("bottomleft", 
            pal = pal_prevalencia_regiao, 
            values = prevalencia_regiao_ordem$Prevalencia , 
            opacity = 0.5, 
            title = "Prevalência de confirmados por ARS") %>% 
  addTiles(group ="Original") %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Positron") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satélite") %>%
  addLayersControl(baseGroups = c("Original", "Positron", "Satélite"), options = layersControlOptions(collapsed = FALSE))



##INCIDÊNCIA POR REGIÃO
###Fazer com que cada coluna seja uma Região e repetir cada número as vezes necessárias para ficar com o número
###igual ao das colunas da base de dados
populacao_regioes_rep <- as.data.frame(t(populacao_regioes[rep(seq_len(ncol(populacao_regioes)), each = nrow(data))]))

###Calcular a incidência em cada Região fazendo os casos novos por Região a dividir pela população da Região 
###menos os confirmados da Região menos os óbitos da Região e dar nomes a cada coluna
incidencia_regioes <- cbind(data$data, (as.data.frame(data[, confirmados_arsnorte:confirmados_madeira] 
                                                      - lag(data[, confirmados_arsnorte:confirmados_madeira]))) 
                            / (populacao_regioes_rep - as.data.frame(data[,confirmados_arsnorte:confirmados_madeira] 
                                                                     - data[,obitos_arsnorte:obitos_madeira])))
names(incidencia_regioes) <- c("Data", "Norte", "Centro", "Lisboa e Vale do Tejo", "Alentejo", "Algarve",
                               "Açores", "Madeira")

###Definir intervalos para legenda
bins_2 =  c(0, 0.002, 0.004, 0.006, 0.008, 0.01, 0.02, Inf)

###Da tabela anterior com todas as datas, selecionar apenas o valor mais recente e mudar nome coluna e Regiãoes
incidencia_regioes_ordem <- as.data.frame(t(as.data.frame(lapply(incidencia_regioes[,c(5, 6, 7, 3, 8, 2, 4)], last)))) %>% 
  rownames_to_column(var = "Regiao") 
incidencia_regioes_ordem[,1] <- c("Alentejo", "Algarve", "Açores","Centro", "Madeira", "Norte", "Lisboa e Vale do Tejo")

###Definir palete de cores para mapa
pal_2 <- colorBin("YlOrRd", domain = incidencia_regioes_ordem[,2]*100, bins = bins_2)

###Definir legenda quando se passa com o rato por cima
labels_2 <- sprintf(
  "<strong>%s</strong><br/>%g&#x25 incidência",
  incidencia_regioes_ordem[,1], round(incidencia_regioes_ordem[,2]*100, digits = 4)
) %>% lapply(htmltools::HTML)

###Fazer o mapa
leaflet(mapa_pt) %>% 
  addTiles(group = "Normal") %>% 
  addProviderTiles(providers$CartoDB.Positron, group = "Claro") %>% 
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels, group = "Escuro") %>% 
  addLayersControl(
    baseGroups = c("Normal", "Claro", "Escuro"),
    options = layersControlOptions(collapsed = TRUE)
  ) %>% 
  addPolygons(
    fillColor = ~pal_2(incidencia_regioes_ordem[,2]*100), 
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels_2,
    labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                             padding = "3px 8px"),
                                textsize = "15px",
                                direction = "auto")) %>% 
  addLegend(pal = pal, values = incidencia_regioes_ordem[,2]*100, opacity = 0.7, title = "incidência",
            position = "bottomright")



##EVOLUÇÃO DA INCIDÊNCIA POR REGIÃO
incidencia_regioes_tempo <- cbind(data$data, (as.data.frame(data[, confirmados_arsnorte:confirmados_madeira] 
                                                            - lag(data[, confirmados_arsnorte:confirmados_madeira]))))
names(incidencia_regioes_tempo) =  c("Data", "Norte", "Centro", "Lisboa e Vale do Tejo", "Alentejo", 
                                     "Algarve", "Açores", "Madeira")

incidencia_regioes_tempo_melt <- melt(incidencia_regioes_tempo, id.vars = "Data")
names(incidencia_regioes_tempo_melt) = c("Data", "Regiao", "Incidencia")

###Fazer gráfico de linhas com data no eixo do x, a incidencia no eixo do y e o género em cada linha
ggplot(incidencia_regioes_tempo_melt, aes(x = Data, y = Incidencia, color = Regiao)) +
  geom_point(size = 0.4) +
  geom_line(size = 0.4) +
  facet_grid(incidencia_regioes_tempo_melt$Regiao, scales = "free_y") +
  theme(legend.position = "none") +
  labs(title = "Incidência Diária por Regiao",
       x = "Mês", 
       y ="Incidência") +
  theme(plot.title = element_text(margin = margin(t = 20, b = 20), 
                                  size = 15, 
                                  color = "Black", 
                                  hjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 20, b = 20),
                                    size = 12),
        axis.title.y = element_text(margin = margin(r = 20, l = 20),
                                    size = 12),
        axis.text.y = element_text(size = 8),
        strip.text.y = element_text(size = 8, angle = 0)) +
  scale_x_date(breaks = "months", date_labels = "%B")

##mapa
###Fazer tabela com novos casos mais recentes
incidencia_regioes_recente <- as.data.frame(t(as.data.frame(lapply((data[, confirmados_arsnorte:confirmados_madeira])
                                                                   - lag(data[, confirmados_arsnorte:confirmados_madeira]), last)))) %>% 
  rownames_to_column(var = "Regioes")
incidencia_regioes_recente[,1] = c("Norte", "Centro", "Lisboa e Vale do Tejo", "Alentejo", 
                                   "Algarve", "Açores", "Madeira")

###Definir intervalos para legenda
bins_7 =  c(0, 25, 50, 100, 200, 300, 400, Inf)

###Definir palete de cores para mapa
pal_7 <- colorBin("YlOrRd", domain = incidencia_regioes_recente[,2], bins = bins_7)

###Da tabela anterior com todas as datas, selecionar apenas o valor mais recente e mudar nome coluna e Regiãoes
incidencia_regioes_recente <- incidencia_regioes_recente[c(4, 5, 6, 2, 7, 1, 3),] 

###Definir legenda quando se passa com o rato por cima
labels_7 <- sprintf(
  "<strong>%s</strong><br/>%g casos novos",
  incidencia_regioes_recente[,1], round(incidencia_regioes_recente[,2], digits = 4)
) %>% lapply(htmltools::HTML)

###Fazer o mapa
leaflet(mapa_pt) %>% 
  addTiles(group = "Normal") %>% 
  addProviderTiles(providers$CartoDB.Positron, group = "Claro") %>% 
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels, group = "Escuro") %>% 
  addLayersControl(
    baseGroups = c("Normal", "Claro", "Escuro"),
    options = layersControlOptions(collapsed = TRUE)
  ) %>% 
  addPolygons(
    fillColor = ~pal_7(incidencia_regioes_recente[,2]), 
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels_7,
    labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                             padding = "3px 8px"),
                                textsize = "15px",
                                direction = "auto")) %>% 
  addLegend(pal = pal_7, values = incidencia_regioes_recente[,2], opacity = 0.7, title = "incidência",
            position = "bottomright")



##EVOLUÇÃO DA TAXA INCIDÊNCIA POR REGIÃO
###Fazer melt para fazer o gráfico e dar nomes a cada coluna
incidencia_regioes_melt <- melt(incidencia_regioes, id.vars = "Data")
names(incidencia_regioes_melt) <- c("data", "regiao", "valor")

###Fazer o gráfico de linhas com a data no eixo do x, a incidência no eixo do y e a Região em cada linha
ggplot(incidencia_regioes_melt, aes(x = data, y = valor*100, color = regiao)) +
  geom_line() + 
  labs(title = "Incidência por Região ao Longo do Tempo",
       x = "Mês", 
       y = "Incidência (%)") +
  facet_grid(incidencia_regioes_melt$regiao) +
  theme(plot.title = element_text(margin = margin(t = 20, b = 20),
                                  size = 15, 
                                  color = "Black", 
                                  hjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 15, b = 10), 
                                    size = 12),
        axis.title.y = element_text(margin = margin(r = 15, l = 10), 
                                    size = 12),
        axis.text.y = element_text(size = 5),
        strip.text.y = element_text(size = 8, angle = 0)) +
  guides(color = FALSE) +
  scale_x_date(breaks = "months", date_labels = "%B")



##CASOS POR GRUPO ETÁRIO
###Selecionar as colunas de confirmados feminino para todas as idades e juntá-las numa tabela e
###fazer o mesmo para o masculino
femininos <- as.data.frame(data %>% 
                             dplyr::select(starts_with("confirmados_") & (ends_with("9_f")| ends_with("plus_f"))))

masculinos <- as.data.frame(data %>% 
                              dplyr::select((starts_with("confirmados_") & (ends_with("9_m")| ends_with("plus_m")))))

###Selecionar o valor mais recente de cada coluna de modo a ficar com o número de casos até ao momento para cada
###faixa etária e para cada género
casos_femininos_idade <- as.data.frame(lapply(femininos, last))
casos_masculinos_idade <- as.data.frame(lapply(masculinos, last))

###Somar a tabela dos femininos com a dos masculinos o que vai dar o número de casos até ao momento por idade apenas
casos_total_idade <- as.data.frame(casos_femininos_idade + casos_masculinos_idade)

###Criar tabela com uma colunda para a faixa etária e outra para o número de casos femininos e mudar coluna
###da faixa etária para os nomes adequados
casos_femininos_idade_invertido <- as.data.frame(t(casos_femininos_idade)) %>% 
  rownames_to_column(var = "Idade")
names(casos_femininos_idade_invertido)[2] <- "Femininos"
casos_femininos_idade_invertido[,1] <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

###Criar tabela com uma colunda para a faixa etária e outra para o número de casos masculinos e mudar coluna
###da faixa etária para os nomes adequados
casos_masculinos_idade_invertido <- as.data.frame(t(casos_masculinos_idade)) %>% 
  rownames_to_column(var = "Idade")
names(casos_masculinos_idade_invertido)[2] <- "Masculinos"
casos_masculinos_idade_invertido[,1] <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

###Criar tabela com uma colunda para a faixa etária e outra para o número de casos total e mudar coluna
###da faixa etária para os nomes adequados
casos_total_idade_invertido <- as.data.frame(t(casos_total_idade)) %>% 
  rownames_to_column(var = "Idade")
names(casos_total_idade_invertido)[2] <- "Total"
casos_total_idade_invertido[,1] <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

###Juntar as 3 tabelas que criámos
casos_fem_masc <- merge(casos_femininos_idade_invertido, casos_masculinos_idade_invertido, by = "Idade")
casos_fem_masc_tot <- merge(casos_fem_masc, casos_total_idade_invertido, by = "Idade")

###Fazer melt para poder fazer gráfico 
casos_fem_masc_tot_melt <- melt(casos_fem_masc_tot, id.vars = "Idade")
names(casos_fem_masc_tot_melt) = c("Idade", "Genero", "Casos")

###Fazer gráfico de barras com idade no eixo do x, o número de casos no eixo do y e o género em cada barra
casos_fem_masc_tot_grafico <- ggplot(casos_fem_masc_tot_melt, aes(x = Idade, y = Casos, fill = Genero)) +
  geom_col(position = "dodge") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, max(casos_fem_masc_tot_melt$Casos + 1000))) +
  theme_classic() +
  labs(title = "Casos por Idade e Género",
       y = "Mortes") +
  theme(plot.title = element_text(size = 15, 
                                  color = "Black", 
                                  hjust = 0.5), 
        legend.title = element_blank(),
        axis.title.x = element_text( size = 12),
        axis.text.x = element_text(size=8, 
                                   color = "black"),
        axis.text.y = element_text(size=10,
                                   color = "black")) +
  guides(fill=guide_legend(title="Género")) +
  scale_fill_manual(values = c("coral2", "lightblue", "saddlebrown"))

###Fazer gráfico interativo
ggplotly(casos_fem_masc_tot_grafico) %>% 
  layout(margin = margin(l =10),
         yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                       "Número de Casos",
                                       rep("&nbsp;", 20),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")),
         legend = list(x = 1, y = 0))



##PREVALÊNCIA POR FAIXA ETÁRIA
###Dados da população de portugal em <https://www.pordata.pt/Portugal/Popula%C3%A7%C3%A3o+residente++m%C3%A9dia+anual+
###total+e+por+grupo+et%C3%A1rio-10>
pt_0_9 = 433332	+ 461299
pt_10_19 = 507646 + 549033
pt_20_29 = 544575 + 547505
pt_30_39 = 571355 + 679093
pt_40_49 = 792670 + 782555
pt_50_59 = 747581 + 734540
pt_60_69 = 672758 + 620543
pt_70_79 = 544016 + 429107
pt_80_plus = 352218 + 316442

#Fazer tabela com população por faixa etária
populacao_idades_pt <- as.data.frame(c(pt_0_9, pt_10_19, pt_20_29, pt_30_39, pt_40_49, pt_50_59, pt_60_69, pt_70_79, 
                                       pt_80_plus))
###Calcular prevalência
prevalencia_idade <- ((as.data.frame(t(casos_total_idade)))*100 / populacao_idades_pt) %>%  
  rownames_to_column(var = "Faixa_Etaria")
prevalencia_idade[,1] = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
names(prevalencia_idade)[2] = "Prevalencia"

###Fazer gráfico com faixa etária no eixo do x e prevalência no eixo do y
prevalencia_idade_grafico <- ggplot(prevalencia_idade, aes(x = Faixa_Etaria, y = Prevalencia)) +
  geom_col(fill = "coral2") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian( ylim = c(0, max(prevalencia_idade$Prevalencia + 0.2))) +
  theme_classic() +
  labs(x = "") +
  theme(axis.text.x = element_text(size=8, color = "black"),
        axis.text.y = element_text(size=12, color = "black"))

###Tornar gráfico interativo
ggplotly(prevalencia_idade_grafico) %>% 
  layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                       "Prevalência (%)",
                                       rep("&nbsp;", 20),
                                       rep("\n&nbsp;", 1)),
                                     collapse = "")))

###Prevalência por faixa etária diária

####Repetir valores da população por faixa etária para ficar com mesmo número de linhas da base de dados
populacao_idades_pt_rep <- as.data.frame(t(populacao_idades_pt[rep(seq_len(ncol(populacao_idades_pt)), each = nrow(data))]))

####Cálculo da prevalência
prevalencia_tempo_idade <- cbind(data$data, ((femininos + masculinos) / populacao_idades_pt_rep)*100)
names(prevalencia_tempo_idade) = c("Data", "0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

####Fazer melt para fazer o gráfico
prevalencia_tempo_idade_melt <- melt(prevalencia_tempo_idade, id.vars = "Data")
names(prevalencia_tempo_idade_melt) = c("Data", "Faixa_Etaria", "Prevalencia")

####Fazer gráfico de linhas com data no eixo do x, prevalência no eixo do y e faixa etária nas cores das linhas
prevalencia_tempo_idade_grafico <- ggplot(prevalencia_tempo_idade_melt, aes(x = Data, y = Prevalencia, color = Faixa_Etaria)) +
  geom_line() +
  labs(x = "") +
  theme(legend.title = element_blank(),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 8),
        axis.text.x = element_text(size = 6)) +
  scale_x_date(breaks = "months", date_labels = "%B")

####Tornar o grafico interativo
ggplotly(prevalencia_tempo_idade_grafico) %>% 
  layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                       "Prevalência (%)",
                                       rep("&nbsp;", 20),
                                       rep("\n&nbsp;", 1)),
                                     collapse = "")),
         legend = list(x = 1, y = 0))




##CASOS POR GRUPO ETÁRIO LONGO DO TEMPO
###Pegando nas tabelas que fizémos por género or faixa etária, como os valores na base de dados são cumulativos, 
###fazemos o valor desse dia menos o valor do dia anterior para obtermos o número de novos casos por dia, por faixa etária
###por género
femininos_novos <- femininos - lag(femininos)
masculinos_novos <- masculinos - lag(masculinos)

###Criar uma tabela com uma coluna para a data e outras colunas com o número de casos por dia por faixa etária apenas
###que resultam da soma dos novos casos femininos com os novos casos masculinos
casos_total_tempo <- cbind(data$data, as.data.frame(femininos_novos + masculinos_novos))

###Como  linha 7 que é a primeira em que há registo dos casos já representa o número de casos nesse dia apenas,
###adicionámos essa linha à tabela e dar nomes às colunas
casos_total_tempo[7, 2:10] <- femininos[7,] + masculinos[7,]
names(casos_total_tempo)<- c("Data", "0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

###Fazer melt para fazer o gráfico
casos_total_tempo_melt <- melt(casos_total_tempo, id.vars = "Data")
names(casos_total_tempo_melt) = c("Data", "Faixa_Etaria", "Casos")

###Fazer o gráfico de linhas com a data no eixo do x, o número de casos no eixo do y e a faixa etária em cada linha
casos_total_tempo_grafico <- ggplot(casos_total_tempo_melt, aes(x = Data, y = Casos, fill = Faixa_Etaria)) +
  geom_area(alpha=0.6 , size=.5, colour="white") +
  scale_fill_viridis(discrete = T) +
  labs(title = "Casos por Grupo Etário ao Longo do Tempo",
       x ="Mês", 
       y = "Casos") +
  theme(plot.title = element_text(size = 15, 
                                  color = "black",
                                  hjust=50),
        legend.title = element_blank(),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 5),
        strip.text.y = element_text(size = 3, angle = 0)) +
  scale_x_date(breaks = "months", date_labels = "%B")

###Tornar gráfico interativo
ggplotly(casos_total_tempo_grafico) %>% 
  layout(margin = margin(l =10),
         legend = list(x = 1, y = 0))
 


##EVOLUÇÃO DA TAXA INCIDÊNCIA POR FAIXA ETÁRIA
###Repetir valores da população por faixa etária para ficar com mesmo número de linhas da base de dados
populacao_idades_pt_rep <- as.data.frame(t(populacao_idades_pt[rep(seq_len(ncol(populacao_idades_pt)), each = nrow(data))]))

###Selecionar as colunas de obitos feminino para todas as idades e juntá-las numa tabela e 
###fazer o mesmo para o masculino
femininos_mortes <- as.data.frame(data %>% 
                                    dplyr::select(starts_with("obitos_") & (ends_with("9_f")| ends_with("plus_f"))))

masculinos_mortes <- as.data.frame(data %>% 
                                     dplyr::select((starts_with("obitos_") & (ends_with("9_m")| ends_with("plus_m")))))

###Calcular a população de risco subtraindo à população por faixa etária o número de óbitos e o número de casos até ao momento                                     
populacao_risco_idade <- populacao_idades_pt_rep - femininos_mortes - masculinos_mortes - femininos - masculinos

###Calcular a taxa de incidência, retirando a coluna das datas
taxa_incidencia_idade <- (casos_total_tempo[,-1]/populacao_risco_idade)*100

###Voltar a adicionar a coluna da data e fazer melt para fazer o gráfico
taxa_incidencia_idade <- cbind(data$data, taxa_incidencia_idade)
names(taxa_incidencia_idade)[1] = "Data"
taxa_incidencia_idade_melt <- melt(taxa_incidencia_idade, id.vars = "Data")
names(taxa_incidencia_idade_melt) = c("Data", "Faixa_Etaria", "Taxa_Incidencia")

###Fazer gráfico de linhas com data no eixo do x, taxa de incidência no eixo do y e faixa etária nas cores das linhas
ggplot(taxa_incidencia_idade_melt, aes(x = Data, y = Taxa_Incidencia, color = Faixa_Etaria)) +
  geom_line(size = 0.8)+
  facet_grid(taxa_incidencia_idade_melt$Faixa_Etaria)+
  guides(color = FALSE) +
  labs(x = "",
       y ="Taxa de Incidência (%)") +
  theme(axis.title.y = element_text(margin = margin(r = 20, l = 20),
                                    size = 12),
        axis.text.y = element_text(size = 8),
        strip.text.y = element_text(size = 8, angle = 0)) +
  scale_x_date(breaks = "months", date_labels = "%B")



##CASOS POR GÉNERO AO LONGO DO TEMPO
###Duas tabelas com casos diários, uma para cada género
femininos_total_novos <- as.data.frame(data$confirmados_f - lag(data$confirmados_f))
masculinos_total_novos <- as.data.frame(data$confirmados_m - lag(data$confirmados_m))

###Juntar as tabelas e adicionar coluna com data
incidencia_genero <- cbind(data$data, femininos_total_novos, masculinos_total_novos)
names(incidencia_genero) = c("Data", "Feminino", "Masculino")

###Fazer melt para fazer o gráfico
incidencia_genero_melt <- melt(incidencia_genero, id.vars = "Data")
names(incidencia_genero_melt) = c("Data", "Genero", "Incidencia")

###Fazer gráfico de linhas com data no eixo do x, incidencia no eixo do y e género nas cores das linhas
incidencia_genero_grafico <- ggplot(incidencia_genero_melt, aes(x = Data, y = Incidencia, color = Genero)) +
  geom_line(size = 0.4) +
  labs(x = "") +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(size=8, color = "black"),
        axis.text.y = element_text(size=12, color = "black")) +
  scale_x_date(breaks = "months", date_labels = "%B")
  

###Tornar gráfico interativo
ggplotly(incidencia_genero_grafico) %>% 
  layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                       "Incidência",
                                       rep("&nbsp;", 20),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")),
         legend = list(x = 1, y = 0))



##TAXA INCIDÊNCIA AO LONGO DO TEMPO (novos/população de risco)
###Variáveis com a população total, feminina e masculina com base no INE
populacao_pt = 10295909
mulheres_pt = 5435932
homens_pt = 4859977

###Cálculo da incidência criando uma tabbela para o total, outra para mulheres e outra para homens
incidencia_total <- as.data.frame(data$confirmados_novos/ (populacao_pt - data$confirmados - data$obitos))
incidencia_homens <- as.data.frame((data$confirmados_m - lag(data$confirmados_m)) 
                                   / (homens_pt - data$confirmados_m - data$obitos_m)) 
incidencia_mulheres <- as.data.frame((data$confirmados_f - lag(data$confirmados_f)) 
                                     / (mulheres_pt - data$confirmados_f - data$obitos_f))

##Remover valores negativos devido a erro na base de dados original em que valor cumulativo do número casos homens 
###e mulheres era 0 e não devia
incidencia_homens[174:175,] <- NA
incidencia_mulheres[174:175,] <- NA

###Criar uma tabela com as 3 tabelas anteriores, adicionando uma coluna com a data e mudar os nomes das colunas
incidencia <- data.frame(data$data, incidencia_total*100, incidencia_mulheres*100, incidencia_homens*100)
names(incidencia) <- c("Data", "Total", "Mulheres", "Homens")

###Fazer melt para poder fazer gráfico de linhas e dar nome à coluna do género
incidencia_melt <- melt(incidencia, id.vars = "Data")
names(incidencia_melt) <- c("Data", "Genero", "Incidencia")

###Fazer gráfico de linhas com data no eixo do x, a incidencia no eixo do y e o género em cada linha
incidencia_grafico <- ggplot(incidencia_melt, aes(x = Data, y = Incidencia, color = Genero)) +
  geom_point(size = 0.4) +
  geom_line(size = 0.4) +
  facet_grid(incidencia_melt$Genero) +
  theme(legend.position = "none") +
  labs(title = "Incidência Diária por Género",
       x = "", 
       y ="") +
  theme(plot.title = element_text(size = 15, color = "Black", 
                                  hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 8)) +
  scale_x_date(breaks = "months", date_labels = "%B")

###Tornar gráfico interativo
ggplotly(incidencia_grafico) %>% 
  layout(margin = margin(l =10),
         yaxis = list(title = paste0(c(rep("&nbsp;", 4),
                                       "Incidência (%)",
                                       rep("&nbsp;", 35),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")),
         xaxis = list(title = paste0(c(rep("<br>", 3),
                                       "Mês",
                                       rep("<br>", 4),
                                       rep("<br />", 2)),
                                     collapse = "")))




#MORTES

##MORTES AO LONGO DO TEMPO
###Fazer tabela com coluna para data e outra para óbitos ocorridos em cada dia
mortes_diarias <- as.data.frame(cbind(data$data, as.data.frame(data$obitos - lag(data$obitos))))
names(mortes_diarias) = c("Data", "Mortes")

###Fazer gráfico de linhas com data no eixo do x e número de mortes no eixo do y
mortes_diarias_grafico <- ggplot(mortes_diarias, aes(x = Data, y = Mortes)) +
  geom_point() +
  geom_line(size = 0.4) +
  labs(title = "Mortes ao Longo do Tempo", 
       x = "Mês", 
       color = "") +
  theme(plot.title = element_text(size = 17, 
                                  color = "black", 
                                  hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)) +
  scale_x_date(breaks = "months", date_labels = "%B")

###Tornar gráfico interativo
ggplotly(mortes_diarias_grafico) %>% 
  layout(margin = margin(l =10),
         yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                       "Número de Mortes",
                                       rep("&nbsp;", 20),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")))



##RELAÇÃO CASOS E MORTES DIÁRIOS
###Tabela com coluna para data, outra para casos e outra para mortes
casos_mortes_diarios <- merge(casos_diarios, mortes_diarias, by="Data")

###Fazer melt para poder fazer gráfico de linhas
casos_mortes_diarios_melt <- melt(casos_mortes_diarios, id.vars = "Data")

###Fazer gráfico de linhas com data no eixo do x, número no eixo do y e mortes ou casos em cada linha
ggplot(casos_mortes_diarios_melt, aes(x = Data, y = value, color = variable)) +
  geom_line(size = 1) +
  facet_grid(casos_mortes_diarios_melt$variable, scales = "free_y") +
  guides(color = FALSE) +
  labs(title = "Casos e Mortes Diárias",
       x = "Mês", 
       y = "Número de Pessoas",
       color = "") +
  scale_color_discrete(labels = c("Casos", "Mortes")) +
  theme(plot.title = element_text(margin = margin(t = 20, b = 20),
                                  size = 17, 
                                  color = "black", 
                                  hjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 20, b = 20), 
                                    size = 12),
        axis.title.y = element_text(margin = margin(r = 20, l = 20), 
                                    size = 12)) +
  scale_x_date(breaks = "months", date_labels = "%B")



##MORTES POR GÉNERO
###Criar uma tabela com o valor mais recente, que é também o valor mais alto, do óbitos femininos e masculinos, 
###fazendo uma coluna para o género e outra para o número de óbitos
obitos_genero <- as.data.frame(t(as.data.frame(lapply(data[,obitos_f:obitos_m], max, na.rm = TRUE)))) %>% 
  rownames_to_column(var = "genero")
names(obitos_genero)[2] <- "mortes"

###Fazer um gráfico de barras com o genero no eixo do x e o número de mortes no eixo do y
ggplot(obitos_genero, aes(x = genero, y = mortes)) +
  geom_col(fill = "darksalmon") + 
  labs(title = "Número de Mortes Por género", x = "") + 
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian( ylim = c(0, max(obitos_genero$mortes + 100))) +
  theme_classic() +
  scale_x_discrete(labels = c("Feminino", "Masculino")) +
  theme(plot.title = element_text(margin = margin(t = 20, b = 20), 
                                  size = 17, 
                                  color = "Black", 
                                  hjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 20, b = 20), 
                                    size = 12),
        axis.title.y = element_text(margin = margin(r = 20, l = 20), 
                                    size = 12),
        axis.text.x = element_text(size=12, 
                                   color = "black"),
        axis.text.y = element_text(size=12, 
                                   color = "black")) +
  geom_text(aes(label = mortes), 
            vjust = -0.5, 
            size = 4)
  


##MORTES POR GRUPO ETÁRIO E GÉNERO
###Selecionar o valor mais recente de cada coluna de modo a ficar com o número de óbitos até ao momento 
###para cada faixa etária e para cada género
mortes_femininos_idade <- as.data.frame(lapply(femininos_mortes, last))
mortes_masculinos_idade <- as.data.frame(lapply(masculinos_mortes, last))

###Somar a tabela dos femininos com a dos masculinos o que vai dar o número de óbitos até ao momento por idade apenas
mortes_total_idade <- as.data.frame(mortes_femininos_idade + mortes_masculinos_idade)

###Criar tabela com uma colunda para a faixa etária e outra para o número de óbitos femininos e mudar coluna da 
###faixa etária para os nomes adequados
mortes_femininos_idade_invertido <- as.data.frame(t(mortes_femininos_idade))%>% 
  rownames_to_column(var = "Idade")
names(mortes_femininos_idade_invertido)[2] <- "Femininos"
mortes_femininos_idade_invertido[,1] <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

###Criar tabela com uma colunda para a faixa etária e outra para o número de óbitos masculinos e mudar coluna da 
###faixa etária para os nomes adequados
mortes_masculinos_idade_invertido <- as.data.frame(t(mortes_masculinos_idade)) %>% 
  rownames_to_column(var = "Idade")
names(mortes_masculinos_idade_invertido)[2] <- "Masculinos"
mortes_masculinos_idade_invertido[,1] <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

###Criar tabela com uma colunda para a faixa etária e outra para o número de óbitos total e mudar coluna da 
###faixa etária para os nomes adequados
mortes_total_idade_invertido <- as.data.frame(t(mortes_total_idade)) %>% 
  rownames_to_column(var = "Idade")
names(mortes_total_idade_invertido)[2] <- "Total"
mortes_total_idade_invertido[,1] <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

###Juntar as 3 tabelas que criámos
mortes_fem_masc <- merge(mortes_femininos_idade_invertido, mortes_masculinos_idade_invertido, by = "Idade")
mortes_fem_masc_tot <- merge(mortes_fem_masc, mortes_total_idade_invertido, by = "Idade")

###Fazer melt para poder fazer gráfico 
mortes_fem_masc_tot_melt <- melt(mortes_fem_masc_tot, id.vars = "Idade")
names(mortes_fem_masc_tot_melt) = c("Idade", "Genero", "Mortes")

###Fazer gráfico de barras com idade no eixo do x, o número de óbitos no eixo do y e o género em cada barra
mortes_fem_masc_tot_grafico <- ggplot(mortes_fem_masc_tot_melt, aes(x = Idade, y = Mortes, fill = Genero)) +
  geom_col(position = "dodge") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian( ylim = c(0, max(mortes_fem_masc_tot_melt$Mortes + 100))) +
  theme_classic() +
  labs(title = "Mortes por Idade e género",
       y = "Mortes") +
  theme(plot.title = element_text(size = 15, 
                                  color = "Black", 
                                  hjust = 0.5), 
        legend.title = element_blank(),
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size=8, color = "black"),
        axis.text.y = element_text(size=12, color = "black")) +
  guides(fill=guide_legend(title="género")) +
  scale_fill_manual(values = c("coral2", "lightblue", "saddlebrown"))

###Tornar gráfico interativo
ggplotly(mortes_fem_masc_tot_grafico) %>% 
  layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                       "Número de Mortes",
                                       rep("&nbsp;", 20),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")),
         legend = list(x = 1, y = 0))



##MORTES POR REGIÃO
###Criar tabela com uma coluna para as Regiãoes e outra para o número mais recente total de óbitos e dar nomes
mortos_regioes <- as.data.frame(t(as.data.frame(lapply(data[,obitos_arsnorte:obitos_madeira], last)))) %>% 
  rownames_to_column(var = "Regioes")
names(mortos_regioes)[2] <- "Mortes"
mortos_regioes[, 1] <- c("Norte", "Centro", "Lisboa e Vale \ndo Tejo", "Alentejo", "Algarve", "Açores", "Madeira")

###Fazer gráfico com Regiãoes no eixo do x e número de mortes no eixo do y
ggplot(mortos_regioes, aes(x = Regioes, y = Mortes)) +
  geom_col(fill = "salmon1") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian( ylim = c(0, max(mortos_regioes$Mortes + 125))) +
  theme_classic() +
  labs(title = "Número de Mortes por Região") +
  theme(plot.title = element_text(margin = margin(t = 20, b = 20), 
                                  size = 17, 
                                  color = "Black", 
                                  hjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 20, b = 20), 
                                    size = 12),
        axis.title.y = element_text(margin = margin(r = 20, l = 20), size = 12),
        axis.text.x = element_text(size=12, color = "black"),
        axis.text.y = element_text(size=12, color = "black")) +
  geom_text(aes(label = Mortes), 
            vjust = -0.5, 
            size = 4)

###mapa
####Definir intervalos da legenda
bins_3 =  c(0, 25, 50, 100, 500, 1000, Inf)

####Ordenar a tabela feita anteriormente para ficar com a mesma ordem que os poligonos do mapa
mortos_regioes_ordem <- mortos_regioes[c(4, 5, 6, 2, 7, 1, 3),]

####Definir a palete de cores
pal_3 <- colorBin("YlOrRd", domain = mortos_regioes_ordem[,2], bins = bins_3)

####Definir a legenda quando se passa com o rato por cima
labels_3 <- sprintf(
  "<strong>%s</strong><br/>%g Mortos",
  incidencia_regioes_ordem[,1], mortos_regioes_ordem[,2]
) %>% lapply(htmltools::HTML)

####Fazer o mapa
leaflet(mapa_pt) %>% 
  addTiles(group = "Normal") %>% 
  addProviderTiles(providers$CartoDB.Positron, group = "Claro") %>% 
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels, group = "Escuro") %>% 
  addLayersControl(
    baseGroups = c("Normal", "Claro", "Escuro"),
    options = layersControlOptions(collapsed = TRUE)
  ) %>% 
  addPolygons(
    fillColor = ~pal_3(mortos_regioes_ordem[,2]), 
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels_3,
    labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                             padding = "3px 8px"),
                                textsize = "15px",
                                direction = "auto")) %>% 
  addLegend(pal = pal, values = incidencia_regioes_ordem[,2]*100, opacity = 0.7, title = "Nº Mortos",
            position = "bottomright")



##MORTALIDADE (obitos/população)
###Cálculo da mortalidade total, feminina e masculina com base no valor mais recente dos óbitos, que é também o mais alto
mortalidade_total <- max(data$obitos, na.rm = TRUE) / populacao_pt
mortalidade_mulheres <- max(data$obitos_f, na.rm = TRUE) / mulheres_pt
mortalidade_homens <- max(data$obitos_m, na.rm =TRUE) / homens_pt

###Criar uma tabela com uma coluna para o género e outra para o valor da mortalidade e dar nome apropriado aos generos
mortalidade <- data.frame(t(data.frame(mortalidade_total, mortalidade_mulheres, mortalidade_homens))) %>% 
  rownames_to_column(var = "genero")
names(mortalidade)[2] <- "mortalidade"
mortalidade[, 1] <- c("Total", "Mulheres", "Homens")

###Criar um gráfico de barras com o genero no eixo do x e a mortalidade no eixo do y
ggplot(mortalidade, aes(x = genero, y = mortalidade*100)) +
  geom_col(fill = "salmon1") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian( ylim = c(0, max(mortalidade$mortalidade*106))) +
  theme_classic() +
  labs(title = "Mortalidade", 
       x = "",
       y = "%") +
  theme(plot.title = element_text(margin = margin(t = 20, b = 20), 
                                  size = 17, 
                                  color = "Black", 
                                  hjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 20, b = 20), 
                                    size = 12),
        axis.title.y = element_text(margin = margin(r = 20, l = 20), 
                                    size = 12),
        axis.text.x = element_text(size=12, color = "black"),
        axis.text.y = element_text(size=12, color = "black")) +
  geom_text(aes(label = scales::percent(mortalidade, digits = 4)), 
            vjust = -0.5, 
            size = 4)



##MORTALIDADE POR FAIXA ETÁRIA
###Calcular a taxa de mortalidade para cada faixa etária e adicionar coluna com faixas etárias
mortalidade_idade <- cbind(mortes_femininos_idade_invertido[,1], ((mortes_femininos_idade_invertido[,2] + mortes_masculinos_idade_invertido[,2])*100 
                                                                  / populacao_idades_pt))
names(mortalidade_idade) = c("Faixa_Etaria", "Taxa_Mortalidade")

###Fazer o gráfico com faixa etária no eixo do x e taxa de mortalidade no eixo do y
mortalidade_idade_grafico <- ggplot(mortalidade_idade, aes(x = Faixa_Etaria, y = Taxa_Mortalidade)) +
  geom_col(fill = "salmon1") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian( ylim = c(0, max(mortalidade_idade$Taxa_Mortalidade + 0.01))) +
  theme_classic() +
  labs(x = "") +
  theme(axis.title.y = element_text(margin = margin(r = 20, l = 20), 
                                    size = 12),
        axis.text.x = element_text(size=12, color = "black"),
        axis.text.y = element_text(size=12, color = "black"))


###Tornar gráfico interativo
ggplotly(mortalidade_idade_grafico) %>% 
  layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                     "Taxa de Mortalidade (%)",
                                     rep("&nbsp;", 20),
                                     rep("\n&nbsp;", 2)),
                                   collapse = "")))


##MORTALIDADE POR REGIÃO
###Criar tabela com uma coluna para as Regiãoes e outra para a mortalidade mais recente e dar nomes apropriados
mortalidade_regioes <- data.frame(t(as.data.frame(lapply(data[, obitos_arsnorte:obitos_madeira], last)) *100) 
                                  / populacao_regioes) %>% 
  rownames_to_column(var = "Regiao")
names(mortalidade_regioes)[2] <- "Mortalidade"
mortalidade_regioes[, 1] <- c("Norte", "Centro", "Lisboa e Vale \ndo Tejo", "Alentejo", "Algarve", "Açores", "Madeira")

###Fazer gráfico com Regiãoes no eixo do x e mortaldiade no eixo do y
ggplot(mortalidade_regioes, aes(x = Regiao, y = Mortalidade*100)) +
  geom_col(fill = "gray") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian( ylim = c(0, max(mortalidade_regioes$Mortalidade*106))) +
  theme_classic() +
  labs(title = "Mortalidade Por Região",
       y = "Mortalidade (%)") +
  theme(plot.title = element_text(margin = margin(t = 20, b = 20), 
                                  size = 17, 
                                  color = "Black", 
                                  hjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 20, b = 20), 
                                    size = 12),
        axis.title.y = element_text(margin = margin(r = 20, l = 20), 
                                    size = 12),
        axis.text.x = element_text(size=12, color = "black"),
        axis.text.y = element_text(size=12, color = "black")) +
  geom_text(aes(label = scales::percent(round(Mortalidade, digits = 5))), 
            vjust = -0.5, 
            size = 4)

###mapa
####Definir intervalos da legenda
bins_4 =  c(0, 0.002, 0.004, 0.006, 0.008, 0.01, 0.02, Inf)

####Ordenar a tabela feita anteriormente para ficar com a mesma ordem que os poligonos do mapa
mortalidade_regioes_ordem <- mortalidade_regioes[c(4, 5, 6, 2, 7, 1, 3),]

####Definir a palete de cores
pal_4 <- colorBin("YlOrRd", domain = mortalidade_regioes_ordem[,2], bins = bins_4)

####Definir a legenda quando se passa com o rato por cima
labels_4 <- sprintf(
  "<strong>%s</strong><br/>%g&#x25 Mortalidade",
  mortalidade_regioes_ordem[,1], round(mortalidade_regioes_ordem[,2], digits =  3)
) %>% lapply(htmltools::HTML)

####Fazer o map
leaflet(mapa_pt) %>% 
  addTiles(group = "Normal") %>% 
  addProviderTiles(providers$CartoDB.Positron, group = "Claro") %>% 
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels, group = "Escuro") %>% 
  addLayersControl(
    baseGroups = c("Normal", "Claro", "Escuro"),
    options = layersControlOptions(collapsed = TRUE)
  ) %>% 
  addPolygons(
    fillColor = ~pal_4(mortalidade_regioes_ordem[,2]), 
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels_4,
    labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                             padding = "3px 8px"),
                                textsize = "15px",
                                direction = "auto")) %>% 
  addLegend(pal = pal_4, values = mortalidade_regioes_ordem[,2], opacity = 0.7, title = "Mortalidade",
            position = "bottomright")



##LETALIDADE (obitos/confirmados)
###Cálculo da letalidade total, feminina e masculina com base no valor mais recente dos óbitos, 
###que é também o mais alto, e no valor mais recente de confirmados
letalidade_total <- max(data$obitos, na.rm = TRUE) / last(data$confirmados)
letalidade_mulheres <- max(data$obitos_f, na.rm = TRUE) / last(data$confirmados_f)
letalidade_homens <- max(data$obitos_m, na.rm = TRUE) / last(data$confirmados_m)

###Criar uma tabela com uma coluna para o género e outra para o valor da letalidade e dar nome apropriado aos generos
letalidade <- data.frame(t(data.frame(letalidade_total, letalidade_mulheres, letalidade_homens))) %>% 
  rownames_to_column(var = "genero")
names(letalidade)[2] <- "letalidade"
letalidade[, 1] <- c("Total", "Mulheres", "Homens")

###Criar um gráfico de barras com o genero no eixo do x e a letalidade no eixo do y
ggplot(letalidade, aes(x = genero, y = letalidade*100)) +
  geom_col(fill = "salmon1") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian( ylim = c(0, max(letalidade$letalidade*100 + 4))) +
  theme_classic() +
  labs(title = "Letalidade", 
       x = "",
       y = "%") +
  theme(plot.title = element_text(margin = margin(t = 20, b = 20), 
                                  size = 20, 
                                  color = "Black", 
                                  hjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 20, b = 20), 
                                    size = 15),
        axis.title.y = element_text(margin = margin(r = 20, l = 20), 
                                    size = 15),
        axis.text.x = element_text(size=12, 
                                   color = "black"),
        axis.text.y = element_text(size=12, color = "black")) +
  geom_text(aes(label = scales::percent(letalidade, digits = 4)), 
            vjust = -0.5, 
            size = 4)


##LETALIDADE LONGO DO TEMPO
###Calcular letalidade toal, para mulheres e para homens, criando uma tabela para cada com uma coluna para a 
###data e outra para os valores da letalidade para cada dia
letalidade_tempo_total <- cbind(data$data, as.data.frame((data$obitos / data$confirmados)*100))
letalidade_tempo_mulheres <- cbind(data$data, as.data.frame((data$obitos_f / data$confirmados_f)*100))
letalidade_tempo_homens <- cbind(data$data, as.data.frame((data$obitos_m / data$confirmados_m)*100))

###Juntar as 3 tabelas numa são mudando os nomes de cada coluna
letalidade_tempo_total_mulheres <- merge(letalidade_tempo_total, letalidade_tempo_mulheres, by ="data$data")
letalidade_tempo <- merge(letalidade_tempo_total_mulheres, letalidade_tempo_homens, by="data$data")
names(letalidade_tempo) <- c("Data", "Total", "Mulheres", "Homens")

###Fazer o melt para poder faze um gráfico de linhas
letalidade_tempo_melt <- melt(letalidade_tempo, id.vars = "Data")
names(letalidade_tempo_melt) <- c("Data", "Genero", "Letalidade")

###Fazer gráfico de linhas com a data no eixo do x, a letalidade no eixo do y e o género em cada linha
letalidade_tempo_grafico <- ggplot(letalidade_tempo_melt, aes(x = Data, y = Letalidade, color = Genero)) +
  geom_line() +
  labs(title = "Letalidade ao Longo do Tempo",
       x = "Mês",
       y = "Letalidade (%)") +
  theme(plot.title = element_text(size = 15, 
                                  color = "black", 
                                  hjust = 0.5), 
        legend.title = element_blank(),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))+
  scale_x_date(breaks = "months", date_labels = "%B")

###Tornar gráfico interativo
ggplotly(letalidade_tempo_grafico, add_tracey = "Letalidade") %>% 
  layout(margin = margin(l =10), legend = list(x = 1, y = 0))



##LETALIDADE POR GRUPO ETÁRIO E GÉNERO
###Tabela com número de casos confirmados por faixa etária por género
casos_genero_idade <- merge(casos_femininos_idade_invertido, casos_masculinos_idade_invertido, by = "Idade")
casos_genero_idade_total <-  merge(casos_genero_idade, casos_total_idade_invertido, by = "Idade")

###Tabela com número de óbitos por faixa etária por género
mortes_genero_idade <- merge(mortes_femininos_idade_invertido, mortes_masculinos_idade_invertido, by = "Idade")
mortes_genero_idade_total <-  merge(mortes_genero_idade, mortes_total_idade_invertido, by = "Idade")

###Criar tabela com uma coluna com as faixas etárioas e outra com a letalidade e dar nomes às colunas
letalidade_genero_idade <- cbind(casos_femininos_idade_invertido[,1], (mortes_genero_idade_total[,2:4]/casos_genero_idade_total[,2:4]))
names(letalidade_genero_idade) <- c("Idade", "Feminino", "Masculino", "Total")

###Fazer melt para poder fazer gráfico
letalidade_genero_idade_melt <- melt(letalidade_genero_idade, id.vars = "Idade")

###Fazer gráfico com idade no eixo do x, letalidade no eixo do y e faixa etária em cada linha
letalidade_genero_idade_grafico <- ggplot(letalidade_genero_idade_melt, aes(x = Idade, y = value*100, color = variable, 
                                                                tooltip = round(value*100, digits = 2), data_id = value)) +
  geom_point_interactive() +
  labs(title = "Letalidade por Faixa etária por Género",
       x ="Faixa etária (anos)",
       y = "Letalidade (%)") +
  theme(plot.title = element_text(margin = margin(t = 20, b = 20), 
                                  size = 15, 
                                  color = "black", 
                                  hjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 20, b = 20,), 
                                    size = 12),
        axis.title.y = element_text(margin = margin(r = 20, l = 20), 
                                    size = 12)) +
  scale_color_discrete(name= "Genéro")
        


###Animar o gráfico 
girafe(code = print(letalidade_genero_idade_grafico),
       options = list(
         opts_zoom(max = 2),
         opts_hover(css = "fill:black;"),
         opts_sizing(rescale = TRUE, width = 0.8)
       ))



##LETALIDADE POR GRUPO ETÁRIO AO LONGO DO TEMPO
###Tabela com o número de mortes total diários por faixa etária
total_mortes_novos <- femininos_mortes + masculinos_mortes

###Tabela com o número de casos totais diários por faixa etária
total_casos_novos <- femininos + masculinos

###Tabela com uma coluna com a data e outras com cada faixa etária onde tem o valor da letalidade total para cada dia e 
###dar nomes às colunas
letalidade_tempo_idade <- cbind(data$data, total_mortes_novos/total_casos_novos)
names(letalidade_tempo_idade) <- c("Data", "0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

###Fazer melt para fazer o gráfico
letalidade_tempo_idade_melt <- melt(letalidade_tempo_idade, id.vars = "Data")

###Fazer o gráfico
ggplot(letalidade_tempo_idade_melt, aes(x = Data, y = value*100, color = variable)) +
  geom_line(size = 1) +
  facet_grid(letalidade_tempo_idade_melt$variable) +
  guides(color = FALSE) +
  labs(title = "Letalidade por Faixa Etária ao Longo do Tempo",
       x ="Mês",
       y = "Letalidade (%)") +
  theme(plot.title = element_text(margin = margin(t = 20, b = 20), 
                                  size = 15,
                                  hjust = 0.5,
                                  color = "black"),
        axis.title.x = element_text(margin = margin(t = 20 ,b = 20), 
                                    size = 15),
        axis.title.y = element_text(margin = margin(r = 20, l = 20), 
                                    size = 15),
        strip.text.y = element_text(angle = 0)) +
  scale_x_date(breaks = "months", date_labels = "%B")



##LETALIDADE POR REGIÃO
###Fazer uma tabela com uma coluna com a Região e outra com a letalidade para cada Região e dar nomes adequados
letalidade_regioes <- data.frame(t(as.data.frame(lapply(data[, obitos_arsnorte:obitos_madeira], last))
                                   / (as.data.frame(lapply(data[, confirmados_arsnorte:confirmados_madeira], last))))) %>% 
  rownames_to_column(var = "Regiao")
names(letalidade_regioes)[2] <- "Letalidade"
letalidade_regioes[, 1] <- c("Norte", "Centro", "Lisboa e Vale \ndo Tejo", "Alentejo", "Algarve", "Açores", "Madeira")

###Fazer o gráfico com a Região no eixo do x e a letalidade no eixo do y
ggplot(letalidade_regioes, aes(x = Regiao, y = Letalidade*100)) +
  geom_col(fill = "gray") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian( ylim = c(0, max(letalidade_regioes$Letalidade*106))) +
  theme_classic() +
  labs(title = "Letalidade Por Região",
       x = "Região",
       y = "Letalidade (%)") +
  theme(plot.title = element_text(margin = margin(t = 20, b = 20), 
                                  size = 15, 
                                  color = "Black", 
                                  hjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 15, b = 10), 
                                    size = 12),
        axis.title.y = element_text(margin = margin(r = 15, l = 10), 
                                    size = 12),
        axis.text.x = element_text(size=10, 
                                   color = "black"),
        axis.text.y = element_text(size=12, 
                                   color = "black")) +
  geom_text(aes(label = scales::percent(round(Letalidade, digits = 5))), 
            vjust = -0.5, 
            size = 4)

###mapa
####Definir intervalos da legenda
bins_5 =  c(0, 1, 2, 3, 4, 5, 6, 7, Inf)

####Ordenar a tabela feita anteriormente para ficar com a mesma ordem que os poligonos do mapa
letalidade_regioes_ordem <- letalidade_regioes[c(4, 5, 6, 2, 7, 1, 3),]

####Definir a palete de cores
pal_5 <- colorBin("YlOrRd", domain = letalidade_regioes_ordem[,2]*100, bins = bins_5)

####Definir a legenda quando se passa com o rato por cima
labels_5 <- sprintf(
  "<strong>%s</strong><br/>%g&#x25 Letalidade",
  letalidade_regioes_ordem[,1], round(letalidade_regioes_ordem[,2]*100, digits =  2)
) %>% lapply(htmltools::HTML)

####Fazer o mapa
leaflet(mapa_pt) %>% 
  addTiles(group = "Normal") %>% 
  addProviderTiles(providers$CartoDB.Positron, group = "Claro") %>% 
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels, group = "Escuro") %>% 
  addLayersControl(
    baseGroups = c("Normal", "Claro", "Escuro"),
    options = layersControlOptions(collapsed = TRUE)
  ) %>% 
  addPolygons(
    fillColor = ~pal_5(letalidade_regioes_ordem[,2]*100), 
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels_5,
    labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                             padding = "3px 8px"),
                                textsize = "15px",
                                direction = "auto")) %>% 
  addLegend(pal = pal_5, values = letalidade_regioes_ordem[,2]*100, opacity = 0.7, title = "Letalidade",
            position = "bottomright")



##LETALIDADE POR REGIÃO AO LONGO DO TEMPO
###Criar tabela com uma coluna para data e outras colunas uma para cada região tendo lá os valores da letalidade diária
###e dar nomes às colunas
letalidade_regioes_tempo <- cbind(data$data, as.data.frame((data[,49:55]/data[,4:10]))*100)
names(letalidade_regioes_tempo) <- c("Data", "Norte", "Centro", "Lisboa e Vale do Tejo", "Alentejo", 
                                     "Alrgarve", "Açores", "Madeira")

###Fazer melt para poder fazer o gráfico
letalidade_regioes_tempo_melt <- melt(letalidade_regioes_tempo, id.vars = "Data")
names(letalidade_regioes_tempo_melt) = c("Data", "Regiao", "Letalidade")

###Fazer o gráfico de linhas com data no eixo do x, letalidade no eixo do y e regiao em cada linha
letalidade_regioes_tempo_grafico <- ggplot(letalidade_regioes_tempo_melt, aes(x = Data, y = Letalidade, color = Regiao)) +
  geom_line() +
  labs(title = "Letalidade por Região ao Longo do Tempo",
       x = "Mês",
       y ="Letalidade (%)") +
  theme(plot.title = element_text(size = 15, 
                                  color = "black", 
                                  hjust = 0.5), 
        legend.title = element_blank(),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 8),
        axis.text.x = element_text(size = 8)) +
  scale_x_date(breaks = "months", date_labels = "%B")

###Tornar gráfico interativo
ggplotly(letalidade_regioes_tempo_grafico) %>% 
  layout(margin = margin(l =10),
         yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                       "Letalidade (%)",
                                       rep("&nbsp;", 20),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")),
         legend = list(x = 1, y = 0))



#INTERNADOS

##NÚMERO DE INTERNADOS AO LONGO DO TEMPO
###Fazer melt das colunas data, internados e internados UCI para ter número de internados em cada dia
internados <- melt(data[,c(1, 15, 16)], id.vars = "data")

###Mudar o nome do conteúdo da coluna dos internados para internados ou internados UCI
levels(internados$variable)[levels(internados$variable)=="internados"] <- "Internados"
levels(internados$variable)[levels(internados$variable)=="internados_uci"] <- "Internados UCI"

###Fazer gráfico de linhas com data no eixo do x, número de internados no eixo do y e tipo de internamento nas linhas
internados_grafico <- ggplot(internados, aes(x = data, y =value, color = variable)) +
  geom_line(size = 1) +
  labs(title = "Número de Internados ao Longo do Tempo", 
       x = "Mês", 
       color = "") +
  theme(plot.title = element_text(size = 15, 
                                  color = "black", 
                                  hjust = 0.5),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 6)) +
  scale_x_date(breaks = "months", date_labels = "%B")

###Tornar gráfico interativo
ggplotly(internados_grafico) %>% 
  layout(margin = margin(l =10),
         yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                       "Número de Pessoas",
                                       rep("&nbsp;", 20),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")),
         legend = list(x = 1, y = 0))



##PERCENTAGEM DE INTERNADOS AO LONGO DO TEMPO
###Fazer melt para ter tabela com coluna da data, coluna do tipo de internamento e coluna com percentagem de internados
###que são os internados a dividir pelos confirmados e dar nomes às colunas
internados_confirmados <- melt((cbind(data$data, (as.data.frame(lapply(data[,c(15, 16)], 
                                                                       function(x) {(x/data[, 3])*100}))))), id.vars = "data$data")
names(internados_confirmados) <- c("data", "internados", "percentagem")

###Mudar o nome do conteúdo da coluna dos internados para internados ou internados UCI
levels(internados_confirmados$internados)[levels(internados_confirmados$internados)=="confirmados"] <- "Internados"
levels(internados_confirmados$internados)[levels(internados_confirmados$internados)=="confirmados.1"] <- "Internados UCI"

###Fazer gráfico de linhas com data no eixo do x, percentagem internados no eixo do y e tipo de internamento em cada linha
internados_confirmados_grafico <- ggplot(internados_confirmados, aes(x = data, y = percentagem, color = internados)) +
  geom_line(size = 1) +
  labs(title = "Percentagem Internados dentro \ndos Infetados ao Longo do Tempo", 
       x = "Mês", 
       color = "") +
  theme(plot.title = element_text(size = 15, 
                                  color = "black", 
                                  hjust = 0.5),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 6)) +
  scale_x_date(breaks = "months", date_labels = "%B")

###Tornar gráfico interativo
ggplotly(internados_confirmados_grafico) %>% 
  layout(margin = margin(l =10),
         yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                       "Percentagem (%)",
                                       rep("&nbsp;", 20),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")),
         legend = list(x = 1, y = 0))



#RECUPERADOS 

##Criar tabela com coluna para data e outra coluna para a percentagem de recuperados em cada dia e dar nomes às colunas
recuperados <- cbind(data$data, as.data.frame((data$recuperados / data$confirmados)*100))
names(recuperados) <- c("Data", "Recuperados")

##Fazer gráfico de linhas com data no eixo do x e percentagem recuperados no eixo y
recuperados_grafico <- ggplot(recuperados, aes(x = Data, y = Recuperados)) +
  geom_line(color = "salmon1", size = 1) +
  labs(title = "Percentagem de Recuperados \ndentro dos Infetados", 
       x = "Mês") +
  theme(plot.title = element_text(size = 15, 
                                  color = "black", 
                                  hjust = 0.5),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12)) +
  scale_x_date(breaks = "months", date_labels = "%B")

###Tornar gráfico interativo
ggplotly(recuperados_grafico) %>% 
  layout(margin = margin(l =10),
         yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                       "Recuperados (%)",
                                       rep("&nbsp;", 20),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")))

##POR FAZER
##CASOS POR CONCELHO

mapa_concelhos_ordem <- with(mapa_concelhos, mapa_concelhos[order(NAME_2),])



i <- amatch(data_concelhos_recente$Regiao, mapa_concelhos$NAME_2, maxDist = 10)
data_concelhos_recente$match[!is.na(i)] <- mapa_concelhos$NAME_2[i[!is.na(i)]]

m <- merge(mapa_concelhos, data_concelhos_recente, by.x='NAME_2', by.y='match')


data_concelhos_recente <- as.data.frame(t(last(data_concelhos[,2:309]))) %>% 
  rownames_to_column(var = "Regiao")
names(data_concelhos_recente) = c("Regiao", "Casos")

bins_1 =  c(0, 50, 100, 150, 250, 500, 1000, 2000, 3000, Inf)

###Definir a palete de cores para o mapa
pal_1 <- colorBin("YlOrRd", domain = data_concelhos_recente[,2], bins = bins_1)

labels_1 <- sprintf(
  "<strong>%s</strong><br/>%g casos",
  data_concelhos_recente[,1], data_concelhos_recente[,2]
) %>% lapply(htmltools::HTML)

leaflet(mapa_concelhos) %>% 
  addTiles(group = "Normal") %>% 
  addPolygons(
    fillColor = ~pal_1(data_concelhos_recente[,2]),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels_1,
    labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                             padding = "3px 8px"),
                                textsize = "15px",
                                direction = "auto")) %>% 
  addLegend(pal = pal_1, values =  data_concelhos_recente[,2], opacity = 0.7, title = "Nº Casos",
            position = "bottomright")



##LETALIDADE AJUSTADA À IDADE
###Proporção de cada faixa etária nos casos confirmados
percentagem_idades <- as.data.frame(casos_total_idade_invertido[,2]/last(data$confirmados))

###Multiplicação da letalidade de cada faixa etária pela proporção dessa fixa etária nos confirmados
cross_product_idade <- as.data.frame(letalidade_genero_idade[, 4]*percentagem_idades)

###Criar uma tabela com uma coluna para a idade, outra para a letalidade por faixa etária, outra para a proporção de cada 
###faixa etária e outra para o cross_product
letalidade_ajustada_idade <- as.data.frame(cbind((letalidade_genero_idade[,c(1, 4)]), percentagem_idades, cross_product_idade))

###Adicionar uma linha com o total sendo a coluna 4, última linha a letalidade ajustada
total_letalidade_ajustada_idade = c("Total", sum(letalidade_ajustada_idade[,2]), sum(letalidade_ajustada_idade[,3]), 
                                    sum(letalidade_ajustada_idade[,4]))
letalidade_ajustada_idade <- rbind(letalidade_ajustada_idade, total_letalidade_ajustada_idade)
names(letalidade_ajustada_idade) = c("Idade", "Letalidade", "Proporção", "Cross Product")



##LETALIDADE AJUSTADA AO GÉNERO
###Número de casos por género
casos_genero <-  as.data.frame(lapply(casos_genero_idade[,2:3], sum))

###Proporção de cada género nos casos confirmados
percentagem_genero <- as.data.frame(t(casos_genero/last(data$confirmados)))

###Letalidade por género
letalidade_fem = last(data$obitos_f)/last(data$confirmados_f)
letalidade_masc = last(data$obitos_m)/last(data$confirmados_m)
letalidade_genero <- as.data.frame(rbind(letalidade_fem, letalidade_masc))

###Multiplicação da letalidade de cada género pela proporção desse género nos confirmados
cross_product_genero <- as.data.frame(letalidade_genero*percentagem_genero)

###Criar tabela para fazer coluna com género
genero <- as.data.frame(c("Feminino", "Masculino"))

###Criar uma tabela com uma coluna para o género, outra para a letalidade por género, outra para a proporção de cada género e 
###outra para o cross_product
letalidade_ajustada_genero <- as.data.frame(cbind(genero, letalidade_genero, percentagem_genero, cross_product_genero))

###Adicionar uma linha com o total sendo a coluna 4, última linha a letalidade ajustada
total_letalidade_ajustada_genero = c("Total", sum(letalidade_ajustada_genero[,2]), sum(letalidade_ajustada_genero[,3]), 
                                     sum(letalidade_ajustada_genero[,4]))
letalidade_ajustada_genero <- rbind(letalidade_ajustada_genero, total_letalidade_ajustada_genero)
names(letalidade_ajustada_genero) = c("Género", "Letalidade", "Proporção", "Cross Product")



##LETALIDADE AJUSTADA AO DESFASAMENTO
###Número total de mortes a dividir pelo número total de casos 14 dias antes, que é o número de dias em média que vai desde infeção até morte
###sciencedirect.com/science/article/pii/S253104372030129X
letalidadade_ajustada_desfasamento = last(data$obitos)/(top_n(as.data.frame(data$confirmados), 14)[1,])

