# Códigos do server shiny

## Carregando pacotes
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
library(tsibble)
library(zoo)
library(xts)
library(rsconnect)
library(dygraphs)

# carregando o codigo com todas configuracoes para o ui
source("Tabs.R")

# ui: interface do usuário
# navbarPage: layout de navegação da pagina por abas
ui <- fluidPage( 
  navbarPage(
  # titulo do app
  title = "Pureco - Serviços de Limpeza",
  # tema escolhido para o site
  theme = shinytheme("flatly"),
  Tab1,
  Tab2,
  Tab3,
  Tab4,
  Tab5)
)  
                         