## tentativa de usar shiny modules para as secoes no server

library(tidyverse)
library(sf)
library(mapview)
library(lubridate)
library(forecast)
library(tseries)
library(curl)
library(plotly)
library(leaflet)
library(geosphere)
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(DT)
library(zoo)
library(xts)
library(rsconnect)
library(dygraphs)
library(htmltools)
library(readxl)

source("server.R")
source("Tabs.R")
source("ui.R")
source("Temas.R")


## por enquanto nao sei ainda se vou fazer um module 
## funcao das seções 
# histogramServer <- function(id) {
#   moduleServer(id, function(input, output, session) {
#     data <- reactive(mtcars[[input$var]])
#     output$hist <- renderPlot({
#       hist(data(), breaks = input$bins, main = input$var)
#     }, res = 96)
#   })
# }
# 
# faxinasgeraisServer <- function(id) {
#   moduleServer(id, function(input, output, session, faxinas){ 
#     
#     observeEvent(input$escolhido, {
#       if (input$grafico == "Barras") {
#         if (input$eixo_y == "Quantidade" & input$eixo_x == "Dia da Semana") {
#           faxinas_g <- reactive(
#             faxinas %>%
#               mutate(Quantidade = 1, ano = year(Data)) %>%
#               filter(Mulher != "NA" & `Dia da Semana` != "NA" &
#                        ano %in% input$ano) %>%
#               group_by(ano, `Dia da Semana`)  %>%
#               summarize(Quantidade = sum(Quantidade)) %>%
#               arrange(ano, Quantidade)
#           )
#           output$infgeral1parte1 <- renderPlotly({
#             g1a <- ggplot(faxinas_g(),
#                    aes(
#                      x = `Dia da Semana`,
#                      y = Quantidade,
#                      fill = `Dia da Semana`
#                    )) +
#               geom_bar(stat = "identity", position = "stack") +
#               facet_grid(~ ano, scales = "free_x") +
#               xlab("Dia da Semana") +
#               ylab("Quantidade de Faxinas") +
#               ggtitle("Quantidade de Faxinas por Dia da Semana e Ano") +
#               scale_fill_viridis_d(aesthetics = "fill") +
#               tema_facets
# 
#             g1a <- ggplotly(g1a) %>%
#               layout(showlegend = FALSE)
# 
#             g1a
#           })
#         }
#       }
#     })
#   })
# }