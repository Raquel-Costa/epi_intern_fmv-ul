library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dashboardthemes)
library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinyBS)
library(reshape2)
library(rjson)
library(leaflet)
library(tibble)
library(geojsonio)

#Upload all data (portuguese data, portuguese tests data, world data, countries ISO3 codes, portuguese map)

covid19pt <- fread("https://raw.githubusercontent.com/dssg-pt/covid19pt-data/master/data.csv")

covid19pt$data <- as.Date(as.character(covid19pt$data),format = "%d-%m-%Y")


covid19pt_testes <- fread("https://raw.githubusercontent.com/dssg-pt/covid19pt-data/master/amostras.csv")

covid19pt_testes$data <- as.Date(covid19pt_testes$data,"%d-%m-%Y")


covid19world <- fread("https://raw.githubusercontent.com/datasets/covid-19/master/data/countries-aggregated.csv")

covid19world$Date <- as.Date(covid19world$Date,"%d-%m-%Y")

covid19world_recent <- covid19world %>% 
  group_by(Country) %>% 
  filter(Date == max(Date)) %>% 
  mutate("% recovered" = round(Recovered/Confirmed *100, digits = 2)) %>% 
  mutate("% dead" = round(Deaths/Confirmed *100, digits = 2)) %>% 
  select(Country, Confirmed, Deaths, Recovered, `% dead`, `% recovered`)


countriescodes <- fread("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv") %>% 
  select("name", "alpha-3", "region")

names(countriescodes) <- c("Country", "ISO3", "Region")


mapa_pt <- geojson_read("https://raw.githubusercontent.com/dssg-pt/covid19pt-data/master/extra/mapas/portugal.geojson",what = "sp")




#start shiny app ----------------------------------------------------------

shinyApp(
  
  #ui ---------------------------------------------------------------------
    ui = dashboardPagePlus(
      
     
       #header -------------------------------------------------------------
        header = dashboardHeaderPlus(
          #add title and image when the sidebar is minimized
            title = tagList(
                span(class = "logo-lg", "Covid Portugal"), 
                img(src = "pt.svg", height = 20, width = 20)),
            titleWidth = 200,
            enable_rightsidebar = FALSE,
          
          #add information icon and a pop up box when clicking the icon
            tags$li(actionLink("openModal", label = "", icon = icon("info")),
                    class = "dropdown")
        ),
        
        tags$script(HTML("
        var openTab = function(tabName){
          $('a', $('.sidebar')).each(function() {
            if(this.getAttribute('data-value') == tabName) {
              this.click()
            };
          });
        }
      ")),
        
        
      
       #sidebar --------------------------------------------------------------
        sidebar = dashboardSidebar(width = 200,
                                   sidebarMenu(id = "side_menu",
                                       menuItem("Today", tabName = "Today", icon = icon("calendar")),
                                       menuItem("Overall", tabName = "Overall", icon = icon("globe-europe")),
                                       menuItem("Tests", tabName = "Tests", icon = icon("vial")),
                                       menuItem("Mobility", tabName = "Mobility", icon = icon("car-alt"),
                                                menuSubItem("Facebook", tabName = "Facebook", icon = icon("facebook")),
                                                menuSubItem("Google", tabName = "Google", icon = icon("google"))))),
        
      
      
      
       #body ------------------------------------------------------------------
        body = dashboardBody(
          
          #define black theme
            shinyDashboardThemes(theme = "grey_dark"),
          
          #define font style
            tags$head(
                tags$style(HTML(".main-sidebar { font-size: 17px; }")),
                tags$style(HTML('.main-header .logo {
                                          font-family: "Georgia", Times, "Times New Roman", serif;
                                          font-weight: bold;
                                          font-size: 20px;
                       }'))),
          
          
          #create content for menuItem Today
            tabItems(
                tabItem(tabName = "Today", h2("Today's Numbers"),
                        br(),
                        fluidRow(
                            box(width = 9,
                                
                                #user date input where the default is today's date
                                   dateInput("date", label = "Choose a Date", value = (Sys.Date())),
                                   br(),
                                
                                #plot depending on actionbutton that user clicks
                                   wellPanel(
                                       uiOutput("plots"))),
                            
                          #create infoboxes and an actionbutton above each infobox
                            box(width = 3,
                                   infoBoxOutput("Newcases", width = NULL),
                                   actionButton("seeplot1", "See Plot",
                                           style = "position:absolute;right:2em;top:3em;"),
                                   infoBoxOutput("Newdeaths", width = NULL),
                                   actionButton("seeplot2", "See Plot",
                                           style = "position:absolute;right:2em;top:10.5em;"),
                                   infoBoxOutput("Atives", width = NULL),
                                   actionButton("seeplot3", "See Plot",
                                           style = "position:absolute;right:2em;top:18em;"),
                                   infoBoxOutput("Hospitalized", width = NULL),
                                   actionButton("seeplot4", "See Plot",
                                           style = "position:absolute;right:2em;top:25.5em;"),
                                   infoBoxOutput("Hospitalizeduci", width = NULL),
                                   actionButton("seeplot5", "See Plot",
                                           style = "position:absolute;right:2em;top:33em;"),
                                   infoBoxOutput("Newrecovered", width = NULL),
                                   actionButton("seeplot6", "See Plot",
                                           style = "position:absolute;right:2em;top:40.5em;")
                            )),
                        
                      #text that will including map information date and leaflet map
                        box(textOutput("explicationmap"),
                            leafletOutput("map"), width = 12)),
                
               
                
                 
            #create content for menuItem Overall
              tabItem(tabName = "Overall", h2("Overall Data"),
                      br(),
                      width = "100%",
                      
                    #valueboxes with overall number information
                      valueBoxOutput("Cases"),
                      valueBoxOutput("Deaths"),
                      valueBoxOutput("Recovered"),
                    
                    #second row of value boxes in the middle
                      fluidRow(
                        column(width = 12, offset = 2,
                        valueBoxOutput("% dead"),
                        valueBoxOutput("% recovered"))),
                      br(),
                      h2("Compare with Other Countries"),
                      br(),
                    
                    #data table with the values of world countries
                      box(DT::dataTableOutput("Covidworld"), width = "90%")),
            
            
            #create content for menuItem Tests
              tabItem(tabName = "Tests", h2("Tests"),
                      br(),
                      fluidRow(
                        column(width = 9,
                               
                            #user date input
                               dateInput("date2", label = "Choose a Date", value = (Sys.Date())),
                               br(),
                              
                            #plot depending on actionbutton that user clicks
                               wellPanel(
                                 uiOutput("plots2"))
                        ),
                        
                        column(width = 3,
                               br(),br(),br(),br(), br(),br(),br(),br(),br(),
                               
                            #create infoboxes and an actionbutton above each infobox
                               infoBoxOutput("Test", width = NULL),
                               actionButton("seeplot7", "See Plot",
                                            style = "position:absolute;right:2em;top:15em;"),
                               br(),
                               infoBoxOutput("Positivetest", width = NULL),
                               actionButton("seeplot8", "See Plot",
                                            style = "position:absolute;right:2em;top:24em;")
              ))),
              
            
           
            #create content for menuItem Facebook
              tabItem(tabName = "Facebook", h2("Mobility Based on Facebook Data"),
                      br(),
                      fluidRow(
                        
                        #one column with a box with explanation and upload and other column with ISO3 data table
                          box(uiOutput("explainfacebook"),
                              br(),br(),
                              fileInput("upload", "Upload the 'Mobility-Range' File", accept = "m*.txt"),
                              textOutput("explainnext")),
                          box(title = "If you don't know the ISO3 of a country", DT::dataTableOutput("codes"))),
                      br(),
                      
                    #one place for user to choose country and other to choose district
                      box(uiOutput('country'),
                          uiOutput('distrito'), width = "50%"),
                    
                    #plot according to user choices
                      box(plotOutput("mobilitydaily"), width = "90%")
                      ),

            
        
          #create content for menuItem Google
            tabItem(tabName = "Google", h2("Mobility Based on Google Data"),
                    br(),
                    fluidRow(
                      
                    #one column with a box with explanation and other with upload
                      box(uiOutput("explaingoogle")),
                      box(fileInput("upload2", "Upload the 'Global CSV' File", accept = "G*.csv"),
                          br(),
                          textOutput("explainnext2")),
                    br(),
                    
                  #one place for user to choose country, other to choose district and other to choose region
                    fluidRow(box(uiOutput('country2'),
                        uiOutput('distrito2'),
                        uiOutput('region'))),
                  
                  #plots for each category according to user choices
                    fluidRow(
                      box(plotOutput("retail"), width = 4),
                      box(plotOutput("parks"), width = 4),
                      box(plotOutput("workplaces"), width = 4)),
                    fluidRow(
                      box(plotOutput("grocery"), width = 4),
                      box(plotOutput("transit"), width = 4),
                      box(plotOutput("residential"), width = 4))
                      
            ))),

        title = "DashboardPage"
    )),
    
  
  
  #server ----------------------------------------------------------------------
    server = function(input, output) {
      
    #info icon -----------------------------------------------------------------
      #content of pop up box when user clicks on information icon
      observeEvent(input$openModal, {
        showModal(
          modalDialog(title = "Hi there!",
                      p(HTML("My name is Raquel Costa and I created this Shiny App using open source data:
                 <ul>
                <li><a href='https://github.com/dssg-pt/covid19pt-data'>Portuguese Data</a></li>
                <li><a href='https://github.com/datasets/covid-19'>World Data</a></li>
                <li><a href='https://data.humdata.org/dataset/movement-range-maps'>Facebook Mobility</a></li>
                <li><a href='https://www.google.com/covid19/mobility/?hl=en'>Google Mobility</a></li></ul>")))
        )
      })
      

      
    #Today menu -----------------------------------------------------------------
      
      
    #New Cases infobox with value shown according to user chosen date
      output$Newcases <- renderInfoBox({
        novos_casos <- covid19pt %>%
          filter(data == input$date) %>% 
          select(confirmados_novos)
        box1 <- infoBox("New Cases", novos_casos, icon = icon("head-side-cough"), color = "blue", fill = TRUE, width = 3)
        
      })
      
      
    #New Deaths infobox with value shown according to user chosen date
      output$Newdeaths <- renderInfoBox({
        lag_mortes <- covid19pt %>% 
          mutate(lag_obitos = lag(obitos))
        total_mortes <- covid19pt %>%
          filter(data == input$date) %>%
          select(obitos)
        total_mortes_antes <- lag_mortes %>%
          filter(data == input$date) %>%
          select(lag_obitos)
        novas_mortes = total_mortes - total_mortes_antes
        infoBox("New Deaths", novas_mortes, icon = icon("skull"), color = "light-blue",
                fill = TRUE, width = 3)
      })
      
      
    #Active Cases infobox with value shown according to user chosen date
      output$Atives <- renderInfoBox({
        ativos <- covid19pt %>%
          filter(data == input$date) %>% 
          select(ativos)
        infoBox("Active Cases", ativos, icon = icon("virus"), color = "teal",
                fill = TRUE, width = 3)
      })
      
    #Hospitalized infobox with value shown according to user chosen date
      output$Hospitalized <- renderInfoBox({
        hospitalizados <- covid19pt %>%
          filter(data == input$date) %>% 
          select(internados)
        infoBox("Hospitalized", hospitalizados, icon = icon("hospital"), color = "yellow",
                fill = TRUE, width = 3)
      })
      
      
    #Hospitalized ICU infobox with value shown according to user chosen date
      output$Hospitalizeduci <- renderInfoBox({
        uci <- covid19pt %>%
          filter(data == input$date) %>% 
          select(internados_uci)
        infoBox("Hospitalized ICU", uci, icon = icon("procedures"), color = "orange",
                fill = TRUE, width = 3)
      })
      
      
    #New recovered infobox with value shown according to user chosen date
      output$Newrecovered <- renderInfoBox({
        lag_recuperados <- covid19pt %>% 
          mutate(lag_recuperados = lag(recuperados))
        total_recuperados <- covid19pt %>%
          filter(data == input$date) %>%
          select(recuperados)
        total_recuperados_antes <- lag_recuperados %>%
          filter(data == input$date) %>%
          select(lag_recuperados)
        novos_recuperados = total_recuperados - total_recuperados_antes
        infoBox("New Recovered", novos_recuperados, icon = icon("heartbeat"), color = "red",
                fill = TRUE, width = 3)
      })
      
      
    
    #data and plot if user clicks on actionbutton in New Cases infobox, with a vertical line according to user chosen date
      casos_diarios <- covid19pt[, c(1, 12)]
      names(casos_diarios) = c("Date", "New Cases")

        
        observeEvent(input$seeplot1, {
            output$plots=renderUI({plotlyOutput("plot1")})
            output$plot1 <- renderPlotly({
                ggplotly(ggplot(casos_diarios, aes(x = Date, y = `New Cases`))+
                    theme_minimal()+
                    geom_point(color = "dodgerblue3") +
                    geom_line(color = "dodgerblue3")+
                    geom_vline(color = "black", aes(xintercept = as.numeric(input$date))) +
                    labs(x = "") +
                    theme(axis.title.y = element_text(size = 15),
                          axis.text.x = element_text(size = 15),
                          axis.text.y = element_text(size = 15)) +
                    scale_x_date(breaks = "months", date_labels = "%b"))
            })
            
        })
        
        
        
      #data and plot if user clicks on actionbutton in New Deaths infobox, with a vertical line according to user chosen date
        mortes_diarias <- as.data.frame(cbind(covid19pt$data, as.data.frame(covid19pt$obitos - lag(covid19pt$obitos))))
        names(mortes_diarias) = c("Date", "New Deaths")
        
        
        observeEvent(input$seeplot2, {
            output$plots=renderUI({plotlyOutput("plot2")})
            output$plot2 <- renderPlotly({
                ggplotly(ggplot(mortes_diarias, aes(x = Date, y = `New Deaths`)) +
                  theme_minimal()+
                  geom_point(color = "dodgerblue2") +
                  geom_line(color = "dodgerblue2")  +
                    geom_vline(color = "black", aes(xintercept = as.numeric(input$date))) +
                  labs(x = "", 
                       y = "New Deaths") +
                  theme(axis.title.y = element_text(size = 15),
                        axis.text.x = element_text(size = 15),
                        axis.text.y = element_text(size = 15)) +
                  scale_x_date(breaks = "months", date_labels = "%b"))
            })
        })
        
        
      
      #data and plot if user clicks on actionbutton in Active Cases infobox, with a vertical line according to user chosen date
        casos_ativos <- covid19pt[, c("data", "ativos")]
        names(casos_ativos) = c("Date", "Active Cases")
        
        observeEvent(input$seeplot3, {
          output$plots=renderUI({plotlyOutput("plot3")})
          output$plot3 <- renderPlotly({
            ggplotly(ggplot(casos_ativos, aes(x = Date, y = `Active Cases`)) +
              theme_minimal()+
              geom_point(color = "cyan3") +
              geom_line(color = "cyan3")  +
              geom_vline(color = "black", aes(xintercept = as.numeric(input$date))) +
              labs(x = "", 
                   y = "Active Cases") +
              theme(axis.title.y = element_text(size = 15),
                    axis.text.x = element_text(size = 15),
                    axis.text.y = element_text(size = 15)) +
              scale_x_date(breaks = "months", date_labels = "%b"))
          })
        })
        
        
        
      #data and plot if user clicks on actionbutton in Hospitalized infobox, with a vertical line according to user chosen date
        internados <- covid19pt[, c("data", "internados")]
        names(internados) = c("Date", "Hospitalized")
        
        observeEvent(input$seeplot4, {
          output$plots=renderUI({plotlyOutput("plot4")})
          output$plot4 <- renderPlotly({
            ggplotly(ggplot(internados, aes(x = Date, y = Hospitalized)) +
              theme_minimal()+
              geom_point(color = "darkgoldenrod1") +
              geom_line(color = "darkgoldenrod1")  +
              geom_vline(color = "black", aes(xintercept = as.numeric(input$date))) +
              labs(x = "", 
                   y = "Hospitalized") +
              theme(axis.title.y = element_text(size = 15),
                    axis.text.x = element_text(size = 15),
                    axis.text.y = element_text(size = 15)) +
              scale_x_date(breaks = "months", date_labels = "%b"))
          })
        })
        
        
        
      #data and plot if user clicks on actionbutton in Hospitalized ICU infobox, with a vertical line according to user chosen date
        internados_uci <- covid19pt[, c("data", "internados_uci")]
        names(internados_uci) = c("Date", "Hospitalized ICU")
        
        observeEvent(input$seeplot5, {
          output$plots=renderUI({plotlyOutput("plot5")})
          output$plot5 <- renderPlotly({
            ggplotly(ggplot(internados_uci, aes(x = Date, y = `Hospitalized ICU`)) +
              theme_minimal()+
              geom_point(color = "chocolate1") +
              geom_line(color = "chocolate1")  +
              geom_vline(color = "black", aes(xintercept = as.numeric(input$date))) +
              labs(x = "", 
                   y = "Hospitalized UCI") +
              theme(axis.title.y = element_text(size = 15),
                    axis.text.x = element_text(size = 15),
                    axis.text.y = element_text(size = 15)) +
              scale_x_date(breaks = "months", date_labels = "%b"))
          })
        })
        
        
        
      #data and plot if user clicks on actionbutton in New Recovered infobox, with a vertical line according to user chosen date
        recuperados_diarios <- as.data.frame(cbind(covid19pt$data, as.data.frame(covid19pt$recuperados - lag(covid19pt$recuperados))))
        names(recuperados_diarios) = c("Date", "New Recovered")
        
        
        observeEvent(input$seeplot6, {
          output$plots=renderUI({plotlyOutput("plot6")})
          output$plot6 <- renderPlotly({
            ggplotly(ggplot(recuperados_diarios, aes(x = Date, y = `New Recovered`)) +
              theme_minimal()+
              geom_point(color = "firebrick1") +
              geom_line(color = "firebrick1")  +
              geom_vline(color = "black", aes(xintercept = as.numeric(input$date))) +
              labs(x = "", 
                   y = "New Recovered") +
              theme(axis.title.y = element_text(size = 15),
                    axis.text.x = element_text(size = 15),
                    axis.text.y = element_text(size = 15)) +
              scale_x_date(breaks = "months", date_labels = "%b"))
          })
        })
        
        
        
      #explanation of the map with the date of the most recent data available which was the data used to create the map
        output$explicationmap <- renderText({
          paste("This plot shows the most recent number of new cases available (",last(covid19pt$data), ") for each ARS region")
        })
        
        
      #data and map
        output$map <- renderLeaflet({
          
        #new cases by region and create a table with 2 columns: regions and cases
          novos_casos_regiao <- as.data.frame(t(as.data.frame(lapply(covid19pt[,confirmados_arsnorte:confirmados_madeira] 
                                                     - lag(covid19pt[,confirmados_arsnorte:confirmados_madeira]), last)))) %>%
            rownames_to_column(var = "Regioes")
          names(novos_casos_regiao)[2] = "Casos"
          
        #give better names to the regions and put them in the correct order (equal to mapa_pt) to do the map
          novos_casos_regiao[,1] <- c("Norte", "Centro", "LVT", "Alentejo", "Algarve", "Acores", "Madeira")
          novos_casos_regiao_ordem <- novos_casos_regiao[c(4, 5, 6, 2, 7, 1, 3),]
          
        #define the bins, the color pallete and the labels
          bins =  c(0, 100, 200, 400, 500, 600, 700, 800, 900, 1000, Inf)
          
          pal <- colorBin("YlOrRd", domain = novos_casos_regiao_ordem[,2], bins = bins)
          
          labels <- sprintf(
            "<strong>%s</strong><br/>%g casos",
            novos_casos_regiao_ordem[,1], novos_casos_regiao_ordem[,2]
          ) %>% lapply(htmltools::HTML)
          
        #create the map
          leaflet(mapa_pt) %>%
            addTiles(group = "Normal") %>%
            addProviderTiles(providers$CartoDB.Positron, group = "Claro") %>%
            addProviderTiles(providers$CartoDB.DarkMatterNoLabels, group = "Escuro") %>%
            addLayersControl(
              baseGroups = c("Normal", "Claro", "Escuro"),
              options = layersControlOptions(collapsed = TRUE)
            ) %>%
            addPolygons(
              fillColor = ~pal(novos_casos_regiao_ordem[,2]),
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
            addLegend(pal = pal, values = novos_casos_regiao_ordem[,2], opacity = 0.7, title = "Number of New Cases by ARS",
                      position = "bottomright")
        })


    
    #Overall menu ----------------------------------------------------------------------------------    
        
        
      #Total Cases valuebox
        output$Cases <- renderValueBox({
          valueBox(
            last(covid19pt$confirmados), subtitle = "Total Cases", icon = icon("head-side-cough"),
            color = "blue")
        })
        
        #Total Deaths valuebox
        output$Deaths <- renderValueBox({
          valueBox(
            last(covid19pt$obitos), subtitle = "Total Deaths", icon = icon("skull"),
            color = "light-blue")
        })
        
        
      #Total Recivered valuebox
        output$Recovered <- renderValueBox({
          valueBox(
            last(covid19pt$recuperados), subtitle = "Total Recovered", icon = icon("heartbeat"),
            color = "teal")
        })
        
      
      #Total Deaths/Total Cases valuebox
        output$`% dead` <- renderValueBox({
          valueBox(
            round(last(covid19pt$obitos)/last(covid19pt$confirmados) *100, digits = 2), subtitle = "% dead", 
            icon = icon("percent"), color = "yellow"
          )
        })
        
        
      #Total recovered/Total Cases valuebox
        output$`% recovered` <- renderValueBox({
          valueBox(
            
            round(last(covid19pt$recuperados)/last(covid19pt$confirmados)*100, digits = 2), subtitle = "% recovered", 
            icon = icon("percent"), color = "orange"
          )
        })
        
      
      #data table with multiple coountries covid19 numbers  
        output$Covidworld <- DT::renderDataTable({
          covid19world_recent
        })
        

        
    #Tests menu ------------------------------------------------------------------------------------
      
      #New Tests infobox with value shown according to user chosen date 
        output$Test<- renderInfoBox({
          novas_amostras <- covid19pt_testes %>%
            filter(data == input$date2) %>%
            select(amostras_novas)
          infoBox("New Tests", novas_amostras, icon = icon("vials"), color = "green",
                  fill = TRUE, width = 3)
        })
        
      
      #Positive Tests Percentage infobox with value shown according to user chosen date 
        output$Positivetest<- renderInfoBox({
          novos_positivos <- testes_positivos %>%
            filter(Date == input$date2) %>%
            select(`Positives (%)`)
          infoBox("Positives Percentage", round(novos_positivos, digits = 1), icon = icon("vial"), color = "red",
                  fill = TRUE, width = 3)
        })
      
      #data and plot of daily tests number if user clicks on actionbutton in New Tests infobox, with a vertical line according to user chosen date
        testes <- covid19pt_testes[, c("data", "amostras_novas")]
        names(testes) = c("Date", "New Tests")

        observeEvent(input$seeplot7, {
          output$plots2=renderUI({plotlyOutput("plot7")})
          output$plot7 <- renderPlotly({
            ggplotly(ggplot(testes, aes(x = Date, y = `New Tests`)) +
              theme_minimal()+
              geom_point(color = "forestgreen") +
              geom_line(color = "forestgreen")  +
                geom_vline(color = "black", aes(xintercept = as.numeric(input$date2))) +
              labs(x = "",
                   y = "New Tests") +
              theme(axis.title.y = element_text(size = 15),
                    axis.text.x = element_text(size = 15),
                    axis.text.y = element_text(size = 15)) +
              scale_x_date(breaks = "months", date_labels = "%b"))
          })
        })
        
        
      #data and plot of daily positive tests percentage if user clicks on actionbutton in Positive Percentage infobox, with a vertical line according to user chosen date
        testes_positivos <- cbind(testes[,1], as.data.frame((casos_diarios[1:nrow(testes),2]/testes[,2])*100))
        
        names(testes_positivos) = c("Date", "Positives (%)")

        observeEvent(input$seeplot8, {
          output$plots2=renderUI({plotlyOutput("plot8")})
          output$plot8 <- renderPlotly({
            ggplotly(ggplot(testes_positivos, aes(x = Date, y = `Positives (%)`)) +
              theme_minimal()+
              geom_point(color = "red") +
              geom_line(color = "red")  +
              geom_vline(color = "black", aes(xintercept = as.numeric(input$date2))) +
              labs(x = "",
                   y = "Positives Percentage") +
              theme(axis.title.y = element_text(size = 15),
                    axis.text.x = element_text(size = 15),
                    axis.text.y = element_text(size = 15)) +
              scale_x_date(breaks = "months", date_labels = "%b"))
          })
        })


    
        
    #Mobility Facebook --------------------------------------------------------------------------------

      #explanation of the facebook database, how to get the data and how to upload it

        output$explainfacebook <- renderUI({
                HTML("<di>Facebook has a database available where it compares the movements of the population now, during the pandemic, with the movements of the baseline
                (February 2020). Positive numbers indicate and increase in movement compared to the baseline and negative numbers indicate a decrease.
                If you want to visualize this database follow the next steps:
                
                    <ul>
                      <li>Go to this link: <a href='https://data.humdata.org/dataset/movement-range-maps'>Facebook</a></li>
                      <li>In 'Data and Resources' download the 'movement-range' zip folder</li>
                      <li>When the download is done go to your transfers and unzip the folder. This should open 2 files: README and movement-range.</li>
                      <li>Down here browse for the 'movement-range' file.</li></ul>
                      
                It can take a few seconds to show the plot after the upload because the database is very large.</div>")
                
              })
        
        
      #place to browse for the file and upload it
        observeEvent(input$process, {
          files <- unz(input$upload$name, "m*.txt")
        
        })

        
      #more information on the next steps after the upload
        output$explainnext<- renderText({
          "It's all done! Now it should appear 2 boxes where you 
          can choose a country (using the ISO3 code, if you don't know it see it below) and a district."
          
        })
        
        
      #data table with the country's names and their ISO3 code
        output$codes<- DT::renderDataTable({
          countriescodes
        })
        
        
      #read the uploaded file
        myData <- reactive({
          inFile <- input$upload
          if (is.null(inFile)) return(NULL)
          read.delim(inFile$datapath, header = TRUE)
        })
        

      
      #place for the user to choose the country where the options are the countries present in the data that was uploaded
        output$country <- renderUI({
          data <- myData()

          if(is.null(data)){return(NULL)}

          selectInput(inputId = "country",
                      label = "Select Country using ISO3 code",
                      choice = unique(data$country),
                      multiple = FALSE)
        })

        
      #filter the data to only include the country chosen
        country <- reactive({
          data <- myData()

          if(is.null(data)){return(NULL)}

          data %>%
            filter(country %in% input$country)

        })

      
      #place for the user to choose the district where the options are the districts present in the data that was uploaded
        output$distrito <- renderUI({
          data <- country()

          if(is.null(data)){return(NULL)}

          selectInput(inputId = "distrito",
                      label = "Select District",
                      choice = unique(data$polygon_name),
                      multiple = FALSE)
        })

        
      #filter the data to only include the district chosen
        mobilidade <- reactive({
          data <- country()

          if(is.null(data)){return(NULL)}

          data %>%
            filter(polygon_name %in% input$distrito)
        })


      #create a plot with the filtered data according to the user choices
        output$mobilitydaily <- renderPlot({
          ggplot(mobilidade(), aes(x = as.Date(mobilidade()[,1]), y = mobilidade()[,6]*100)) +
            geom_line(color = "coral3", size = 0.9) +
            geom_hline(yintercept = 0, color = "black", size = 0.9) +
            labs(title = "Movement Change Compared to Baseline (February 2020)",
                 x = "",
                 y = "Percentage of Movement Change") +
            geom_text(x = as.Date("2020-04-01"), label = "Baseline", y = 3, color = "black", text=element_text(size=11))+
            theme(plot.title = element_text(size=22)) + 
            scale_x_date(breaks = "months", date_labels = "%b")
        })
        

        
          
    #Mobility Google -------------------------------------------------------------------------------- 
        
      #explanation of the google database, how to get the data and how to upload it
        output$explaingoogle <- renderUI({
          HTML("Google has a database available where it compares the movements of the population now, during the pandemic, with the movements of the baseline
          (January and February 2020) for different categories of places. Positive numbers indicate and increase in movement compared to the baseline and negative numbers indicate a decrease.
          If you want to visualize this database follow the next steps:
          
              <ul>
                <li>Go to this link: <a href='https://www.google.com/covid19/mobility/?hl=en'>Google</a></li>
                <li>In 'Community Mobility Reports' download the 'Global CSV' file.</li>
                <li> When the download is done, browse here for the 'Global CSV' file.</li></ul>
          It can take a few seconds to show the plot after the upload because the database is very large.")
        })
        
        
      #more information on the next steps after the upload
        output$explainnext2 <- renderText({
          "It's all done! Now it should appear 3 boxes where you can choose a country, a district and a county. "
        })
         
        
      #read the uploaded file
        myData2 <- reactive({
          inFile2 <- input$upload2
          if (is.null(inFile2)) return(NULL)
          read.csv(inFile2$datapath, header = TRUE)
        })
        
        
    #place for the user to choose the country where the options are the countries present in the data that was uploaded
        output$country2 <- renderUI({
          data2 <- myData2()

          if(is.null(data2)){return(NULL)}

          selectInput(inputId = "country2",
                      label = "Select Country",
                      choice = unique(data2$country_region),
                      multiple = FALSE)
        })

        
      #filter the data to only include the country chosen
        country2 <- reactive({
          data2 <- myData2()

          if(is.null(data2)){return(NULL)}

          data2 %>%
            filter(country_region %in% input$country2)

        })

      
      #place for the user to choose the district where the options are the districts present in the data that was uploaded
        output$distrito2 <- renderUI({
          data2 <- country2()

          if(is.null(data2)){return(NULL)}

          selectInput(inputId = "distrito2",
                      label = "Select District",
                      choice = unique(data2$sub_region_1),
                      multiple = FALSE)
        })

      
      #filter the data to only include the district chosen
        distrito2 <- reactive({
          data2 <- country2()

          if(is.null(data2)){return(NULL)}

          data2 %>%
            filter(sub_region_1 %in% input$distrito2)
        })

        
      #place for the user to choose the region (county) where the options are the regions present in the data that was uploaded
        output$region <- renderUI({
          data2 <- distrito2()

          if(is.null(data2)){return(NULL)}

          selectInput(inputId = "region",
                      label = "Select County",
                      choice = unique(data2$sub_region_2),
                      multiple = FALSE)
        })

        
      #filter the data to only include the region chosen
        region <- reactive({
          data2 <- distrito2()

          if(is.null(data2)){return(NULL)}

          data2 %>%
            filter(sub_region_2 %in% input$region)
        })

        
      #create a plot for each category with the filtered data according to the user choices
        output$retail <- renderPlot({
          ggplot(region(), aes(x = as.Date(region()[,8]), y = region()[,9])) +
            geom_line(color = "coral3", size = 0.9) +
            geom_hline(yintercept = 0, color = "black", size = 0.9) +
            geom_text(x = as.Date("2020-04-01"), label = "Baseline", y = 3, color = "black", text=element_text(size=11))+
            labs(title = "Retail and Recreation",
                 x = "",
                 y = "Percentage of Movement Change") +
            theme(plot.title = element_text(size=22)) + 
            scale_x_date(breaks = "months", date_labels = "%b")
        })
        
        output$grocery <- renderPlot({
          ggplot(region(), aes(x = as.Date(region()[,8]), y = region()[,10])) +
            geom_line(color = "coral3", size = 0.9) +
            geom_hline(yintercept = 0, color = "black", size = 0.9) +
            geom_text(x = as.Date("2020-04-01"), label = "Baseline", y = 3, color = "black", text=element_text(size=11))+
            labs(title = "Grocery and Pharmacy",
                 x = "",
                 y = "Percentage of Movement Change") +
            theme(plot.title = element_text(size=22)) + 
            scale_x_date(breaks = "months", date_labels = "%b")
        })
        
        output$parks <- renderPlot({
          ggplot(region(), aes(x = as.Date(region()[,8]), y = region()[,11])) +
            geom_line(color = "coral3", size = 0.9) +
            geom_hline(yintercept = 0, color = "black", size = 0.9) +
            geom_text(x = as.Date("2020-04-01"), label = "Baseline", y = 5, color = "black", text=element_text(size=11))+
            labs(title = "Parks",
                 x = "",
                 y = "Percentage of Movement Change") +
            theme(plot.title = element_text(size=22)) + 
            scale_x_date(breaks = "months", date_labels = "%b")
        })
        
        output$transit <- renderPlot({
          ggplot(region(), aes(x = as.Date(region()[,8]), y = region()[,12])) +
            geom_line(color = "coral3", size = 0.9) +
            geom_hline(yintercept = 0, color = "black", size = 0.9) +
            geom_text(x = as.Date("2020-04-01"), label = "Baseline", y = 3, color = "black", text=element_text(size=11))+
            labs(title = "Transit Stations",
                 x = "",
                 y = "Percentage of Movement Change") +
            theme(plot.title = element_text(size=22)) + 
            scale_x_date(breaks = "months", date_labels = "%b")
        })
        
        output$workplaces <- renderPlot({
          ggplot(region(), aes(x = as.Date(region()[,8]), y = region()[,13])) +
            geom_line(color = "coral3", size = 0.9) +
            geom_hline(yintercept = 0, color = "black", size = 0.9) +
            geom_text(x = as.Date("2020-04-01"), label = "Baseline", y = 3, color = "black", text=element_text(size=11))+
            labs(title = "Workplaces",
                 x = "",
                 y = "Percentage of Movement Change") +
            theme(plot.title = element_text(size=22)) + 
            scale_x_date(breaks = "months", date_labels = "%b")
        })
        
        output$residential <- renderPlot({
          ggplot(region(), aes(x = as.Date(region()[,8]), y = region()[,14])) +
            geom_line(color = "coral3", size = 0.9) +
            geom_hline(yintercept = 0, color = "black", size = 0.9) +
            geom_text(x = as.Date("2020-04-01"), label = "Baseline", y = 3, color = "black", text=element_text(size=11))+
            labs(title = "Residential",
                 x = "",
                 y = "Percentage of Movement Change") +
            theme(plot.title = element_text(size=22)) + 
            scale_x_date(breaks = "months", date_labels = "%b")
        })
        
        
    #Extras ------------------------------------------------------------------------    
      #increase upload limit to 320 MB so that this databases can be uploaded
         options(shiny.maxRequestSize = 320*1024^2)     

    })




