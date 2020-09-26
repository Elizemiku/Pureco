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

source("faxinas.R")
source("Temas.R")


## funcoes de graficos da secao1

# barplot_secao1


# lineplot_secao1

# boxplot
boxplot_secao1 <- function(dados, eixo_x, eixo_y, grupo){
  ggplot(dados %>% summarize(eixo_y = cumsum(eixo_y)),
         aes(x = .data[[eixo_x]], y = .data[[eixo_y]], 
                   fill = .data[[grupo]])) +
    geom_boxplot() +
    facet_grid(~ano) + 
    labs(x = paste0(input$eixo_x), 
         y = paste0(input$eixo_y, " de Faxinas", sep = " ", collapse = " "), 
         title = paste0(input$eixo_y, " de Faxinas por ", 
                        input$eixo_x, " e Ano",
                        sep = " ", collapse = " "))  +
    scale_fill_viridis_d() +
    tema_facets
  
}



